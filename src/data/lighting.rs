//! Types related to lightmaps.

#[cfg(feature = "bevy_reflect")]
use bevy_reflect::Reflect;
use qbsp_macros::BspValue;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::{
	BspFormat, BspParseError, BspResult,
	reader::{BspByteReader, BspParseContext, BspValue, BspVariableValue},
};

pub type GrayscaleLighting = Vec<u8>;
pub type RgbLighting = Vec<[u8; 3]>;

/// Lighting data stored in a BSP file or a neighboring LIT file.
#[derive(Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum BspLighting {
	Grayscale(GrayscaleLighting),
	Colored(RgbLighting),
}

impl BspValue for BspLighting {
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		match reader.ctx.format {
			BspFormat::BSP2 | BspFormat::BSP29 => Ok(Self::Grayscale(reader.read_rest().to_vec())),
			BspFormat::BSP30 | BspFormat::BSP38 | BspFormat::BSP38Qbism => {
				let total_len = reader.len();
				let (rgb_pixels, rest) = reader.read_bytes(total_len)?.as_chunks::<3>();
				if !rest.is_empty() {
					return Err(BspParseError::ColorDataSizeNotDevisableBy3(total_len));
				}

				Ok(BspLighting::Colored(rgb_pixels.to_vec()))
			}
		}
	}

	fn bsp_struct_size(_ctx: &BspParseContext) -> usize {
		0
	}
}

impl BspLighting {
	/// Convince function to get a location as an RGB color.
	#[inline]
	pub fn get(&self, i: usize) -> Option<[u8; 3]> {
		match self {
			Self::Grayscale(v) => {
				let v = *v.get(i)?;
				Some([v, v, v])
			}
			Self::Colored(v) => v.get(i).copied(),
		}
	}

	/// The total number of pixels in the lightmap.
	#[inline]
	pub fn len(&self) -> usize {
		match self {
			Self::Grayscale(vec) => vec.len(),
			Self::Colored(vec) => vec.len(),
		}
	}

	/// The total number of bytes in the lightmap.
	#[inline]
	pub fn bytes(&self) -> usize {
		match self {
			Self::Grayscale(vec) => vec.len(),
			Self::Colored(vec) => vec.len() * 3,
		}
	}

	/// If `true`, the lightmap has no data.
	#[inline]
	pub fn is_empty(&self) -> bool {
		self.len() == 0
	}
}
impl std::fmt::Debug for BspLighting {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Grayscale(vec) => write!(f, "White(...) (len: {})", vec.len()),
			Self::Colored(vec) => write!(f, "Colored(...) (len: {})", vec.len()),
		}
	}
}

/// Parses colored lighting from a LIT file.
pub fn read_lit(data: &[u8], ctx: &BspParseContext, ignore_header: bool) -> BspResult<RgbLighting> {
	let mut reader = BspByteReader::new(data, ctx);

	if !ignore_header {
		let magic: [u8; 4] = reader.read()?;
		if &magic != b"QLIT" {
			return Err(BspParseError::WrongMagicNumber {
				found: magic,
				expected: "QLIT",
			});
		}

		let _version: i32 = reader.read()?;
	}

	if !reader.len().is_multiple_of(3) {
		return Err(BspParseError::ColorDataSizeNotDevisableBy3(reader.len()));
	}

	Ok(reader.read_rest().chunks_exact(3).map(|v| [v[0], v[1], v[2]]).collect())
}

/// An offset into the lightmap. Specified as a number of bytes for BSP30 and BSP38, as they always have RGB lighting.
/// Specified as a number of pixels for BSP29 and BSP2. This is normalized to a number of pixels, as the number of bytes will depend on the exact lightmap format used.
#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct LightmapOffset {
	/// The offset, in pixels.
	pub pixels: i32,
}

impl BspVariableValue for LightmapOffset {
	type Bsp29 = PixelLightmapOffset;
	type Bsp2 = PixelLightmapOffset;
	type Bsp30 = ByteLightmapOffset;
	type Bsp38 = ByteLightmapOffset;
	type Qbism = ByteLightmapOffset;
}

/// An offset specified in number of pixels.
#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct PixelLightmapOffset {
	/// The offset from the start of the lightmap, in pixels, that the lightmap for this face starts.
	pub pixel_offset: i32,
}

/// An offset specified in number of bytes.
#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ByteLightmapOffset {
	/// The offset from the start of the lightmap, in bytes, that the lightmap for this face starts.
	pub byte_offset: i32,
}

impl From<PixelLightmapOffset> for LightmapOffset {
	fn from(value: PixelLightmapOffset) -> Self {
		Self { pixels: value.pixel_offset }
	}
}

impl From<ByteLightmapOffset> for LightmapOffset {
	fn from(value: ByteLightmapOffset) -> Self {
		Self {
			pixels: value.byte_offset / 3,
		}
	}
}

/// Byte that dictates how a specific BSP lightmap appears:
/// - 255 means there is no lightmap.
/// - 0 means normal, unanimated lightmap.
/// - 1 through 254 are programmer-defined animated styles, including togglable lights. In Quake though, 1 produces a fast pulsating light, and 2 produces a slow pulsating light, so those might be good defaults.
///
/// It is recommended to compare these values via the provided methods and constants of this type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct LightmapStyle(pub u8);

impl LightmapStyle {
	/// Unanimated lightmap.
	pub const NORMAL: Self = Self(0);
	/// No lightmap.
	pub const NONE: Self = Self(u8::MAX);
}

impl BspValue for LightmapStyle {
	#[inline]
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		reader.read().map(Self)
	}

	#[inline]
	fn bsp_struct_size(_ctx: &BspParseContext) -> usize {
		1
	}
}

impl std::fmt::Display for LightmapStyle {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self.0 {
			0 => write!(f, "0 (normal)"),
			255 => write!(f, "255 (no lightmap)"),
			n => n.fmt(f),
		}
	}
}
