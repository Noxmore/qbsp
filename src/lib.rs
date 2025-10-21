#![doc = include_str!("../readme.md")]

#[cfg(feature = "bevy_reflect")]
use bevy_reflect::Reflect;
use glam::Vec3;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use thiserror::Error;

pub mod prelude;
use std::mem;

pub mod data;

#[cfg(feature = "meshing")]
pub mod mesh;

#[cfg(test)]
pub mod loading_tests;
pub mod query;
pub mod reader;
pub mod util;

pub use data::bspx;

// Re-exports
pub use glam;
pub use image;
pub use qbsp_macros::{BspValue, BspVariableValue};
pub use smallvec;

// Re-export since this will be one of the most-used types when configuring `qbsp`.
pub use data::texture::Palette;

use crate::{
	data::{
		bspx::BspxData,
		lighting::BspLighting,
		models::{BspEdge, BspFace, BspModel},
		nodes::{BspClipNode, BspLeaf, BspNode, BspPlane},
		texture::{BspMipTexture, BspTexInfo},
		util::UBspValue,
		visdata::BspVisData,
		LumpDirectory, LumpEntry,
	},
	reader::{BspByteReader, BspParseContext, BspValue},
	util::display_magic_number,
};

/// The default quake palette.
pub static QUAKE_PALETTE: Palette = unsafe { mem::transmute_copy(include_bytes!("../palette.lmp")) };

pub struct BspParseInput<'a> {
	/// The data for the BSP file itself.
	pub bsp: &'a [u8],
	/// The optional .lit file for external colored lighting.
	pub lit: Option<&'a [u8]>,

	pub settings: BspParseSettings,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BspParseSettings {
	/// If `true`, will use the `RGBLIGHTING` BSPX lump if it exists to supply [`BspData::lighting`]. (Default: `true`)
	pub use_bspx_rgb_lighting: bool,
}
impl Default for BspParseSettings {
	fn default() -> Self {
		Self { use_bspx_rgb_lighting: true }
	}
}

#[derive(Debug, Clone, Error)]
pub enum BspParseError {
	#[error("Palette byte length {0} instead of 768.")]
	InvalidPaletteLength(usize),
	#[error("Lump ({0:?}) out of bounds of data! Malformed/corrupted BSP?")]
	LumpOutOfBounds(LumpEntry),
	#[error("Tried to read bytes from {from} to {to} from buffer of size {size}")]
	BufferOutOfBounds { from: usize, to: usize, size: usize },
	#[error("Failed to parse string at index {index}, invalid utf-8 sequence: {sequence:?}")]
	InvalidString { index: usize, sequence: Vec<u8> },
	#[error("Wrong magic number! Expected {expected}, found \"{}\"", display_magic_number(found))]
	WrongMagicNumber { found: [u8; 4], expected: &'static str },
	#[error("Unsupported IBSP version! Expected {expected}, found {found}")]
	UnsupportedIbspVersion { found: u32, expected: &'static str },
	#[error("Invalid color data, size {0} is not devisable by 3!")]
	ColorDataSizeNotDevisableBy3(usize),
	#[error("Invalid value: {value}, acceptable:\n{acceptable}")]
	InvalidVariant { value: i32, acceptable: &'static str },
	/// This is to be gracefully handled in-crate.
	#[error("No BSPX directory")]
	NoBspxDirectory,
	#[error("No BSPX lump: {0}")]
	NoBspxLump(String),

	/// For telling the user exactly where the error occurred in the process.
	#[error("{0} - {1}")]
	DoingJob(String, Box<BspParseError>),
}
impl BspParseError {
	/// The error error behind any [`BspParseError::DoingJob`].
	pub fn root(&self) -> &BspParseError {
		let mut err = self;
		loop {
			match err {
				Self::DoingJob(_, child) => err = child,
				_ => return err,
			}
		}
	}

	#[inline]
	pub fn map_utf8_error(data: &[u8]) -> impl FnOnce(std::str::Utf8Error) -> Self + '_ {
		|err| BspParseError::InvalidString {
			index: err.valid_up_to(),
			sequence: data[err.valid_up_to()..err.valid_up_to() + err.error_len().unwrap_or(1)].to_vec(),
		}
	}
}

pub type BspResult<T> = Result<T, BspParseError>;

pub trait BspParseResultDoingJobExt {
	/// Like `map_err`, but specifically for adding messages to BSP errors to tell the user exactly what was going on when the error occurred.
	fn job(self, job: impl ToString) -> Self;
}
impl<T> BspParseResultDoingJobExt for BspResult<T> {
	#[inline]
	fn job(self, job: impl ToString) -> Self {
		match self {
			Ok(v) => Ok(v),
			Err(err) => Err(BspParseError::DoingJob(job.to_string(), Box::new(err))),
		}
	}
}

/// The format of a BSP file. This is determined by the magic number made up of the first 4 bytes of the file, and governs how the rest of the file attempts to parse.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum BspFormat {
	/// Modern BSP format with expanded limits
	#[default]
	BSP2,

	/// Original quake format, in most cases, you should use BSP2 over this.
	BSP29,

	/// GoldSrc format. For the sake of `BspVariableValue`, this is usually the same as `BSP38`,
	/// but differs in some cases (e.g. each model having up to 4 hulls).
	BSP30,

	/// Quake 2 format.
	BSP38,
}

impl BspValue for BspFormat {
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		let magic_number: [u8; 4] = reader.read()?;

		match &magic_number {
			b"BSP2" => Ok(Self::BSP2),
			[0x1D, 0x00, 0x00, 0x00] => Ok(Self::BSP29),
			[0x1E, 0x00, 0x00, 0x00] => Ok(Self::BSP30),
			b"IBSP" => {
				// "IBSP" is shared among formats, like Quake 3. Instead, it is differentiated by a version number read after the magic number.
				let version: u32 = reader.read()?;
				// Currently, we only support version 38, the Quake2 format.
				match version {
					38 => Ok(Self::BSP38),
					_ => Err(BspParseError::UnsupportedIbspVersion {
						found: version,
						expected: "38 (Quake 2)",
					}),
				}
			}
			_ => Err(BspParseError::WrongMagicNumber {
				found: magic_number,
				expected: "BSP2, 0x1D000000 (BSP29), 0x1E000000 (BSP30), or IBSP (BSP38)",
			}),
		}
	}

	fn bsp_struct_size(_ctx: &BspParseContext) -> usize {
		unimplemented!("BspFormat can be of 4 or 8 bytes depending on whether it needs to read version number.");
	}
}

impl std::fmt::Display for BspFormat {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			BspFormat::BSP2 => write!(f, "BSP2"),
			BspFormat::BSP29 => write!(f, "BSP29"),
			BspFormat::BSP30 => write!(f, "BSP30"),
			BspFormat::BSP38 => write!(f, "BSP38"),
		}
	}
}

/// Helper function to read an array of data of type `T` from a lump. Takes in the BSP file data, the lump directory, and the lump to read from.
pub fn read_lump<T: BspValue>(data: &[u8], entry: LumpEntry, lump_name: &'static str, ctx: &BspParseContext) -> BspResult<Vec<T>> {
	let lump_data = entry.get(data)?;
	assert_eq!(
		entry.len as usize % T::bsp_struct_size(ctx),
		0,
		"Lump {lump_name} is the wrong size for {}",
		std::any::type_name::<T>()
	);
	let lump_entries = entry.len as usize / T::bsp_struct_size(ctx);

	let mut reader = BspByteReader::new(lump_data, ctx);
	let mut out = Vec::with_capacity(lump_entries);

	for i in 0..lump_entries {
		out.push(reader.read().job(format!("Parsing {lump_name} lump entry {i}"))?);
	}

	Ok(out)
}

/// The texture lump is more complex than just a vector of the same type of item, so it needs its own function.
pub fn read_mip_texture_lump(reader: &mut BspByteReader) -> BspResult<Vec<Option<BspMipTexture>>> {
	let mut textures = Vec::new();
	let num_mip_textures: u32 = reader.read()?;

	for _ in 0..num_mip_textures {
		let offset: i32 = reader.read()?;
		if offset.is_negative() {
			textures.push(None);
			continue;
		}
		textures.push(Some(BspMipTexture::bsp_parse(&mut reader.with_pos(offset as usize))?));
	}

	Ok(textures)
}

/// A BSP files contents parsed into structures for easy access.
#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspData {
	pub format: BspFormat,
	/// Essentially an embedded .map file, the differences being:
	/// - Brush data has been stripped.
	/// - Brush entities have a `model` property indexing into the `models` field of this struct.
	pub entities: String,
	pub planes: Vec<BspPlane>,
	pub textures: Vec<Option<BspMipTexture>>,
	/// All vertex positions.
	pub vertices: Vec<Vec3>,
	/// RLE encoded bit array. For BSP38, this is cluster-based. For BSP29, BSP2 and BSP30 this is leaf-based,
	/// and models will have their own indices into the visdata array.
	///
	/// Use [`potentially_visible_set_at()`](Self::potentially_visible_set_at) and related functions to query this data.
	///
	/// See [the specification](https://www.gamers.org/dEngine/quake/spec/quake-spec34/qkspec_4.htm#BL4) for more info.
	pub visibility: BspVisData,
	pub nodes: Vec<BspNode>,
	pub tex_info: Vec<BspTexInfo>,
	pub faces: Vec<BspFace>,
	pub lighting: Option<BspLighting>,
	pub clip_nodes: Vec<BspClipNode>,
	pub leaves: Vec<BspLeaf>,
	// TODO: Leaf brushes, used for collision in Quake 2 (BSP38) maps as they don't use hulls.
	// We should implement this but for now, `bevy_trenchbroom` can recalculate collision from
	// the visible mesh.
	// pub leaf_brushes: (),
	/// Indices into the face list, pointed to by leaves.
	pub mark_surfaces: Vec<UBspValue>,
	pub edges: Vec<BspEdge>,
	pub surface_edges: Vec<i32>,
	pub models: Vec<BspModel>,
	// TODO: Are brushes/brush sides actually used in-game?
	// pub brushes: (),
	// pub brush_sides: (),
	// TODO: Areas/area portals are used by Q2 to stop rendering areas after
	// doors close - useful but not required to behave correctly.
	// pub areas: (),
	// pub area_portals: (),
	pub bspx: BspxData,

	/// Additional information from the BSP parsed. For example, contains the [BspFormat] of the file.
	pub parse_ctx: BspParseContext,
}

impl BspData {
	/// Parses the data from BSP input.
	pub fn parse(input: BspParseInput) -> BspResult<Self> {
		let BspParseInput { bsp, lit, settings } = input;
		if bsp.len() < 4 {
			return Err(BspParseError::BufferOutOfBounds {
				from: 0,
				to: 4,
				size: bsp.len(),
			});
		}

		// To parse the format version and form the BspParseContext, we need one with a default parse context where it won't be used.
		let dummy_ctx = BspParseContext::default();
		let mut reader = BspByteReader::new(bsp, &dummy_ctx);

		let ctx = BspParseContext { format: reader.read()? };
		let mut reader = reader.with_context(&ctx);

		let lump_dir: LumpDirectory = reader.read()?;

		let mut entities_bytes = lump_dir.entities.get(bsp)?.to_vec();
		for (i, byte) in entities_bytes.iter_mut().enumerate() {
			if *byte > 127 {
				// For some reason some characters in the entities lump are offset to the second half of byte values, no idea why.
				*byte -= 128;
			} else if *byte == 0 {
				// Also, sometimes the entity lump ends early, so this just truncates it if that is the case.
				entities_bytes.truncate(i);
				break;
			}
		}

		let bspx = BspxData::new(bsp, &lump_dir.bspx).job("Reading BSPX data")?;

		let data = Self {
			format: ctx.format,
			entities: std::str::from_utf8(&entities_bytes)
				.map_err(BspParseError::map_utf8_error(&entities_bytes))
				.job("Reading entities lump")?
				.to_string(),
			planes: read_lump(bsp, lump_dir.planes, "planes", &ctx)?,
			textures: if let Some(tex_lump) = *lump_dir.textures {
				read_mip_texture_lump(&mut BspByteReader::new(tex_lump.get(bsp)?, &ctx)).job("Reading texture lump")?
			} else {
				vec![]
			},
			vertices: read_lump(bsp, lump_dir.vertices, "vertices", &ctx).job("vertices")?,
			visibility: BspByteReader::new(lump_dir.visibility.get(bsp)?, &ctx).read().job("visibiliy")?,
			nodes: read_lump(bsp, lump_dir.nodes, "nodes", &ctx)?,
			tex_info: read_lump(bsp, lump_dir.tex_info, "texture infos", &ctx)?,
			faces: read_lump(bsp, lump_dir.faces, "faces", &ctx)?,
			lighting: if let Some(lit) = lit {
				Some(BspLighting::read_lit(lit, &ctx, false).job("Parsing .lit file")?)
			} else if let Some(lighting) = bspx.parse_rgb_lighting(&ctx).transpose()?.filter(|_| settings.use_bspx_rgb_lighting) {
				Some(lighting)
			} else if !lump_dir.lighting.is_empty() {
				Some(BspByteReader::new(lump_dir.lighting.get(bsp)?, &ctx).read()?)
			} else {
				None
			},
			clip_nodes: if let Some(clip_node_lump) = *lump_dir.clip_nodes {
				read_lump(bsp, clip_node_lump, "clip nodes", &ctx)?
			} else {
				vec![]
			},
			leaves: read_lump(bsp, lump_dir.leaves, "leaves", &ctx)?,
			mark_surfaces: read_lump(bsp, lump_dir.mark_surfaces, "mark surfaces", &ctx)?,
			edges: read_lump(bsp, lump_dir.edges, "edges", &ctx)?,
			surface_edges: read_lump(bsp, lump_dir.surf_edges, "surface edges", &ctx)?,
			models: read_lump(bsp, lump_dir.models, "models", &ctx)?,

			bspx,

			parse_ctx: ctx,
		};

		Ok(data)
	}

	/// Parses embedded textures using the provided palette. Use [`QUAKE_PALETTE`] for the default Quake palette.
	pub fn parse_embedded_textures<'a, 'p: 'a>(&'a self, palette: &'p Palette) -> impl Iterator<Item = (&'a str, image::RgbImage)> + 'a {
		self.textures.iter().flatten().filter_map(|texture| {
			let Some(data) = &texture.data.full else {
				return None;
			};

			let image = image::RgbImage::from_fn(texture.header.width, texture.header.height, |x, y| {
				image::Rgb(palette.colors[data[(y * texture.header.width + x) as usize] as usize])
			});

			Some((texture.header.name.as_str(), image))
		})
	}
}
