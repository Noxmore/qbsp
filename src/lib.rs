#![doc = include_str!("../README.md")]

pub mod prelude;
pub(crate) use prelude::*;

pub mod data;
pub(crate) use data::{bsp::*, bspx::*, *};

#[cfg(feature = "meshing")]
pub mod mesh;

pub mod query;
pub mod util;

// Re-exports
pub use glam;
pub use image;
pub use q1bsp_macros::*;
pub use smallvec;

/// The default quake palette.
pub static QUAKE_PALETTE: Palette = unsafe { mem::transmute_copy(include_bytes!("../palette.lmp")) };

pub struct BspParseInput<'a> {
	/// The data for the BSP file itself.
	pub bsp: &'a [u8],
	/// The optional .lit file for external colored lighting.
	pub lit: Option<&'a [u8]>,
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
	/// The error error behind any [BspParseError::DoingJob].
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum BspFormat {
	/// Modern BSP format with expanded limits
	#[default]
	BSP2,
	/// Original quake format, in most cases, you should use BSP2 over this.
	BSP29,
}
impl BspFormat {
	pub fn from_magic_number(data: [u8; 4]) -> Result<Self, BspParseError> {
		match &data {
			b"BSP2" => Ok(Self::BSP2),
			[0x1D, 0x00, 0x00, 0x00] => Ok(Self::BSP29),
			_ => Err(BspParseError::WrongMagicNumber {
				found: data,
				expected: "BSP2 or 0x1D for BSP29",
			}),
		}
	}
}

#[derive(Debug, Clone, Default)]
pub struct BspParseContext {
	pub format: BspFormat,
}

/// An Id Tech 1 palette to use for embedded images.
#[repr(C)] // Because we transmute data with QUAKE_PALETTE, don't want Rust to pull any shenanigans
#[derive(Debug, Clone)]
pub struct Palette {
	pub colors: [[u8; 3]; 256],
}
impl Default for Palette {
	fn default() -> Self {
		QUAKE_PALETTE.clone()
	}
}
impl Palette {
	/// Parses a palette from data. Palettes must be 768 bytes in length exactly.
	pub fn parse(data: &[u8]) -> BspResult<Self> {
		if data.len() != 768 {
			return Err(BspParseError::InvalidPaletteLength(data.len()));
		}

		Ok(Self {
			colors: data
				.chunks_exact(3)
				.map(|col| [col[0], col[1], col[2]])
				.collect::<Vec<_>>()
				.try_into()
				.unwrap(),
		})
	}
}

/// Helper function to read an array of data of type `T` from a lump. Takes in the BSP file data, the lump directory, and the lump to read from.
pub fn read_lump<T: BspValue>(data: &[u8], entry: LumpEntry, lump_name: &'static str, ctx: &BspParseContext) -> BspResult<Vec<T>> {
	// let entry = lump_dir.get(lump);
	let lump_data = entry.get(data)?;
	let lump_entries = entry.len as usize / T::bsp_struct_size(ctx);

	let mut reader = BspByteReader::new(lump_data, ctx);
	let mut out = Vec::with_capacity(lump_entries);

	for i in 0..lump_entries {
		out.push(reader.read().job(format!("Parsing {lump_name} lump entry {i}"))?);
	}

	Ok(out)
}

/// A BSP files contents parsed into structures for easy access.
#[derive(Debug, Clone, Default)]
pub struct BspData {
	/// Essentially an embedded .map file, the differences being:
	/// - Brush data has been stripped.
	/// - Brush entities have a `model` property indexing into the `models` field of this struct.
	pub entities: String,
	pub planes: Vec<BspPlane>,
	pub textures: Vec<Option<BspTexture>>,
	/// All vertex positions.
	pub vertices: Vec<Vec3>,
	/// RLE encoded bit array.
	/// 
	/// See [the specification](https://www.gamers.org/dEngine/quake/spec/quake-spec34/qkspec_4.htm#BL4) for more info.
	/// 
	/// TODO in the future, this crate might support visibility operations
	pub visibility: Vec<u8>,
	pub nodes: Vec<BspNode>,
	pub tex_info: Vec<BspTexInfo>,
	pub faces: Vec<BspFace>,
	pub lighting: Option<BspLighting>,
	pub clip_nodes: Vec<BspClipNode>,
	pub leaves: Vec<BspLeaf>,
	/// Indices into the face list, pointed to by leaves.
	pub mark_surfaces: Vec<UBspValue>,
	pub edges: Vec<BspEdge>,
	pub surface_edges: Vec<i32>,
	pub models: Vec<BspModel>,

	pub bspx: BspxData,

	/// Additional information from the BSP parsed. For example, contains the [BspFormat] of the file.
	pub parse_ctx: BspParseContext,
}
impl BspData {
	/// Parses the data from BSP input.
	pub fn parse(input: BspParseInput) -> BspResult<Self> {
		let BspParseInput { bsp, lit } = input;
		if bsp.len() < 4 {
			return Err(BspParseError::BufferOutOfBounds {
				from: 0,
				to: 4,
				size: bsp.len(),
			});
		}

		let ctx = BspParseContext {
			format: BspFormat::from_magic_number(bsp[0..4].try_into().unwrap())?,
		};
		let mut reader = BspByteReader::new(&bsp[4..], &ctx);

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
			entities: std::str::from_utf8(&entities_bytes)
				.map_err(BspParseError::map_utf8_error(&entities_bytes))
				.job("Reading entities lump")?
				.to_string(),
			planes: read_lump(bsp, lump_dir.planes, "planes", &ctx)?,
			textures: read_texture_lump(&mut BspByteReader::new(lump_dir.textures.get(bsp)?, &ctx)).job("Reading texture lump")?,
			vertices: read_lump(bsp, lump_dir.vertices, "vertices", &ctx)?,
			visibility: lump_dir.vertices.get(bsp)?.to_vec(),
			nodes: read_lump(bsp, lump_dir.nodes, "nodes", &ctx)?,
			tex_info: read_lump(bsp, lump_dir.tex_info, "texture infos", &ctx)?,
			faces: read_lump(bsp, lump_dir.faces, "faces", &ctx)?,
			lighting: if let Some(lit) = lit {
				Some(BspLighting::read_lit(lit, &ctx, false).job("Parsing .lit file")?)
			} else if let Some(lighting) = bspx.parse_rgb_lighting(&ctx) {
				Some(lighting?)
			} else {
				let lighting = lump_dir.lighting.get(bsp)?;

				if lighting.is_empty() {
					None
				} else {
					Some(BspLighting::White(lighting.to_vec()))
				}
			},
			clip_nodes: read_lump(bsp, lump_dir.clip_nodes, "clip nodes", &ctx)?,
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

	/// Parses embedded textures using the provided palette. Use [QUAKE_PALETTE] for the default Quake palette.
	pub fn parse_embedded_textures<'a, 'p: 'a>(&'a self, palette: &'p Palette) -> impl Iterator<Item = (&'a str, image::RgbImage)> + 'a {
		self.textures.iter().flatten().filter(|texture| texture.data.is_some()).map(|texture| {
			let Some(data) = &texture.data else { unreachable!() };

			let image = image::RgbImage::from_fn(texture.header.width, texture.header.height, |x, y| {
				image::Rgb(palette.colors[data[(y * texture.header.width + x) as usize] as usize])
			});

			(texture.header.name.as_str(), image)
		})
	}
}
