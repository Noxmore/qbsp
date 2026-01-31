#![doc = include_str!("../readme.md")]

// For proc macros to be able to use the `qbsp` path.
extern crate self as qbsp;

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
		LumpDirectory, LumpEntry,
		brush::{BspBrush, BspBrushSide},
		bspx::BspxData,
		lighting::{BspLighting, read_lit},
		models::{BspEdge, BspFace, BspModel},
		nodes::{BspClipNode, BspLeaf, BspNode, BspPlane},
		texture::{BspMipTexture, BspTexInfo},
		util::UBspValue,
		visdata::BspVisData,
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
	/// If `true`, will use the `RGBLIGHTING` BSPX lump if it exists to supply [`BspData::lighting`].
	/// This will not work if [`parse_bspx_structures`](Self::parse_bspx_structures) if `false`.
	///
	/// NOTE: This moves out of [`BspxData::rgb_lighting`], and is a lossy operation. This should only be used for reading.
	///
	/// (Default: `true`)
	pub use_bspx_rgb_lighting: bool,
	/// Automatically parses BSPX structures into fields in [`BspxData`]. If `false`, all BSPX lumps will be in [`BspxData::unparsed`]. (Default: `true`)
	pub parse_bspx_structures: bool,
}
impl Default for BspParseSettings {
	fn default() -> Self {
		Self {
			use_bspx_rgb_lighting: true,
			parse_bspx_structures: true,
		}
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
	#[error("Unsupported BSP version! Expected {expected}, found {found}")]
	UnsupportedBspVersion { found: u32, expected: &'static str },
	#[error("Invalid color data, size {0} is not devisable by 3!")]
	ColorDataSizeNotDevisableBy3(usize),
	#[error("Invalid value: {value}, acceptable:\n{acceptable}")]
	InvalidVariant { value: i32, acceptable: &'static str },
	/// This is to be gracefully handled in-crate.
	#[error("No BSPX directory")]
	NoBspxDirectory,
	#[error("No BSPX lump: {0}")]
	NoBspxLump(String),
	#[error("Duplicate BSPX lump: {0}")]
	DuplicateBspxLump(String),

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

pub trait BspParseResultDoingJobExt<T> {
	/// Like `map_err`, but specifically for adding messages to BSP errors to tell the user exactly what was going on when the error occurred.
	fn job(self, job: T) -> Self;
}
impl<T> BspParseResultDoingJobExt<&str> for BspResult<T> {
	#[inline]
	fn job(self, job: &str) -> Self {
		match self {
			Ok(v) => Ok(v),
			Err(err) => Err(BspParseError::DoingJob(job.to_owned(), Box::new(err))),
		}
	}
}
impl<T, F: FnOnce() -> String> BspParseResultDoingJobExt<F> for BspResult<T> {
	#[inline]
	fn job(self, job: F) -> Self {
		match self {
			Ok(v) => Ok(v),
			Err(err) => Err(BspParseError::DoingJob((job)(), Box::new(err))),
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

	/// Quake 2 format with expanded limits, similar to what BSP2 is to BSP29.
	BSP38Qbism,
}
impl BspFormat {
	/// Returns the character used to denote liquids by prefixing the texture name in the engine that uses this format.
	pub fn liquid_prefix(self) -> Option<char> {
		match self {
			// https://quakewiki.org/wiki/Textures
			Self::BSP2 | Self::BSP29 => Some('*'),
			// https://developer.valvesoftware.com/wiki/Texture_prefixes
			Self::BSP30 => Some('!'),
			Self::BSP38 | Self::BSP38Qbism => None,
		}
	}

	pub const fn is_quake1(self) -> bool {
		matches!(self, Self::BSP29 | Self::BSP2)
	}

	pub const fn is_goldsrc(self) -> bool {
		matches!(self, Self::BSP30)
	}

	pub const fn is_quake2(self) -> bool {
		matches!(self, Self::BSP38 | Self::BSP38Qbism)
	}
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
					_ => Err(BspParseError::UnsupportedBspVersion {
						found: version,
						expected: "38 (Quake 2)",
					}),
				}
			}
			b"QBSP" => {
				let version: u32 = reader.read()?;
				match version {
					38 => Ok(Self::BSP38Qbism),
					_ => Err(BspParseError::UnsupportedBspVersion {
						found: version,
						expected: "38 (Quake 2)",
					}),
				}
			}
			_ => Err(BspParseError::WrongMagicNumber {
				found: magic_number,
				expected: "BSP2, 0x1D000000 (BSP29), 0x1E000000 (BSP30), IBSP (BSP38), or QBSP (QBISM)",
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
			BspFormat::BSP38Qbism => write!(f, "BSP38 (Qbism)"),
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
		out.push(reader.read().job(|| format!("Parsing {lump_name} lump entry {i}"))?);
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
	/// Essentially an embedded .map file, the differences being:
	/// - Brush data has been stripped.
	/// - Brush entities have a `model` property indexing into the `models` field of this struct.
	/// - Non UTF-8 text format. Use [`quake_string_to_utf8(...)`](util::quake_string_to_utf8) and [`quake_string_to_utf8_lossy(...)`](util::quake_string_to_utf8_lossy) to convert.
	pub entities: Vec<u8>,
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
	/// Used for collision in Quake 2 (BSP38) maps as they don't use hulls.
	/// Indexes into the [`brushes`](Self::brushes) vector.
	/// Index into this vector via [`BspLeaf::leaf_brushes`].
	///
	/// If this isn't a Quake 2 map, this vector should be empty.
	pub leaf_brushes: Vec<UBspValue>,
	/// Indices into the face list, pointed to by leaves.
	pub mark_surfaces: Vec<UBspValue>,
	pub edges: Vec<BspEdge>,
	pub surface_edges: Vec<i32>,
	pub models: Vec<BspModel>,
	pub brushes: Vec<BspBrush>,
	pub brush_sides: Vec<BspBrushSide>,
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

		let mut data = Self {
			entities: lump_dir.entities.get(bsp)?.to_vec(),
			planes: read_lump(bsp, lump_dir.planes, "planes", &ctx)?,
			textures: if let Some(tex_lump) = *lump_dir.textures {
				read_mip_texture_lump(&mut BspByteReader::new(tex_lump.get(bsp)?, &ctx)).job("Reading texture lump")?
			} else {
				Vec::new()
			},
			vertices: read_lump(bsp, lump_dir.vertices, "vertices", &ctx).job("vertices")?,
			visibility: BspByteReader::new(lump_dir.visibility.get(bsp)?, &ctx).read().job("visibility")?,
			nodes: read_lump(bsp, lump_dir.nodes, "nodes", &ctx)?,
			tex_info: read_lump(bsp, lump_dir.tex_info, "texture infos", &ctx)?,
			faces: read_lump(bsp, lump_dir.faces, "faces", &ctx)?,
			lighting: if let Some(lit) = lit {
				Some(BspLighting::Colored(read_lit(lit, &ctx, false).job("Parsing .lit file")?))
			} else if !lump_dir.lighting.is_empty() {
				Some(BspByteReader::new(lump_dir.lighting.get(bsp)?, &ctx).read()?)
			} else {
				None
			},
			clip_nodes: if let Some(clip_node_lump) = *lump_dir.clip_nodes {
				read_lump(bsp, clip_node_lump, "clip nodes", &ctx)?
			} else {
				Vec::new()
			},
			leaves: read_lump(bsp, lump_dir.leaves, "leaves", &ctx)?,
			leaf_brushes: if let Some(lump_entry) = *lump_dir.leaf_brushes {
				read_lump(bsp, lump_entry, "leaf brushes", &ctx)?
			} else {
				Vec::new()
			},
			mark_surfaces: read_lump(bsp, lump_dir.mark_surfaces, "mark surfaces", &ctx)?,
			edges: read_lump(bsp, lump_dir.edges, "edges", &ctx)?,
			surface_edges: read_lump(bsp, lump_dir.surf_edges, "surface edges", &ctx)?,
			models: read_lump(bsp, lump_dir.models, "models", &ctx)?,
			brushes: if let Some(lump_entry) = *lump_dir.brushes {
				read_lump(bsp, lump_entry, "brushes", &ctx)?
			} else {
				Vec::new()
			},
			brush_sides: if let Some(lump_entry) = *lump_dir.brush_sides {
				read_lump(bsp, lump_entry, "brush sides", &ctx)?
			} else {
				Vec::new()
			},

			bspx: BspxData::default(), // To be set in a moment.

			parse_ctx: ctx,
		};

		if let Some(bspx_dir) = &lump_dir.bspx {
			let mut bspx = BspxData::parse(bsp, bspx_dir, &data).job("Reading BSPX data")?;

			if settings.use_bspx_rgb_lighting
				&& let Some(lighting) = mem::take(&mut bspx.rgb_lighting)
			{
				data.lighting = Some(BspLighting::Colored(lighting));
			}

			data.bspx = bspx;
		}

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
