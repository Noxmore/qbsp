//! Data definitions for textures and surface flags.

#[cfg(feature = "bevy_reflect")]
use bevy_reflect::Reflect;
use bitflags::bitflags;
use glam::{dvec2, Vec2, Vec3};
use qbsp_macros::{BspValue, BspVariableValue};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::{
	data::util::{FixedStr, NoField},
	reader::{BspByteReader, BspParseContext, BspValue, BspVariableValue},
	BspFormat, BspParseError, BspParseResultDoingJobExt, BspResult, QUAKE_PALETTE,
};

/// An id Tech 2 palette to use for embedded images.
#[repr(C)] // Because we transmute data with QUAKE_PALETTE, don't want Rust to pull any shenanigans
#[derive(Debug, Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Palette {
	#[cfg_attr(feature = "serde", serde(with = "serde_big_array::BigArray"))]
	pub colors: [[u8; 3]; 256],
}

impl BspValue for Palette {
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		let num_colors = reader.read::<i16>()?;

		if num_colors != 256 {
			return Err(BspParseError::InvalidPaletteLength(num_colors as usize));
		}

		let colors = reader.read_bytes(num_colors as usize * 3)?;

		Palette::parse(colors)
	}

	fn bsp_struct_size(ctx: &BspParseContext) -> usize {
		i16::bsp_struct_size(ctx) + 3 * 256
	}
}

impl Default for Palette {
	fn default() -> Self {
		QUAKE_PALETTE.clone()
	}
}

impl Palette {
	/// Parses a palette from data. Palettes must be 768 bytes in length exactly.
	pub fn parse(data: &[u8]) -> BspResult<Self> {
		let (pixels, rest) = data.as_chunks::<3>();
		if !rest.is_empty() {
			return Err(BspParseError::InvalidPaletteLength(data.len()));
		}
		Ok(Self {
			colors: pixels.try_into().map_err(|_| BspParseError::InvalidPaletteLength(data.len()))?,
		})
	}
}

#[derive(BspVariableValue, Debug, Copy, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[bsp29(u32)]
#[bsp2(u32)]
#[bsp30(u32)]
#[bsp38(NoField)]
pub struct TextureIdxField(pub Option<u32>);

#[derive(BspVariableValue, Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[bsp29(NoField)]
#[bsp2(NoField)]
#[bsp30(NoField)]
#[bsp38(BspTexExtraInfo)]
pub struct ExtraInfoField(pub Option<BspTexExtraInfo>);

#[derive(BspValue, Debug, Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspTexInfo {
	/// The format that was used to parse this type.
	pub format: BspFormat,

	pub projection: PlanarTextureProjection,

	pub texture_idx: TextureIdxField,

	/// GoldSrc and Q2 have surface flags, Q1 and BSP2 have texture flags.
	pub flags: BspTexInfoFlags,

	/// Extra info stored directly on the `TexInfo` - for Quake 2 (which does not have a lump for embedded textures).
	pub extra_info: ExtraInfoField,
}

impl BspTexExtraInfo {
	pub const UNIT_TEXTURE_BRIGHTNESS: u32 = 255;

	/// The brightness of the texture - if in the range 0..1 this material is diffuse, if it's
	/// more than 1 then this texture is emissive. By far the most-common brightness is 1, and
	/// most maps will display reasonably if clamping this value in the 0..1 range and making
	/// all textures diffuse. If doing HDR rendering, the material should have a diffuse
	/// layer at normal brightness plus an emissive layer with the same texture, with emission
	/// set to `1. - brightness`.
	pub fn brightness(&self) -> f64 {
		self.value as f64 / Self::UNIT_TEXTURE_BRIGHTNESS as f64
	}

	/// The scale to apply to the diffuse layer (always in the range 0..1).
	pub fn diffuse_scale(&self) -> f64 {
		self.brightness().min(1.)
	}

	/// If this texture has an emissive component (brightness > 1), this is the emissive scale
	/// for that texture. If this method returns `Some(scale)`, then `scale` will always be
	/// greater than 0.
	pub fn emissive_scale(&self) -> Option<f64> {
		self.value
			// Subtract `UNIT_TEXTURE_BRIGHTNESS + 1` so we return `None` if this is zero...
			.checked_sub(Self::UNIT_TEXTURE_BRIGHTNESS + 1)
			// ...then add 1 back to get the actual scale.
			.map(|brightness| (brightness + 1) as f64 / Self::UNIT_TEXTURE_BRIGHTNESS as f64)
	}
}

impl From<BspTexFlags> for BspTexInfoFlags {
	fn from(value: BspTexFlags) -> Self {
		Self {
			texture_flags: Some(value),
			surface_flags: BspSurfaceFlags::empty(),
		}
	}
}

impl From<BspSurfaceFlags> for BspTexInfoFlags {
	fn from(value: BspSurfaceFlags) -> Self {
		Self {
			texture_flags: None,
			surface_flags: value,
		}
	}
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspTexInfoFlags {
	/// If this is `None`, then the name should be used to check the texture flags (for GoldSrc and Quake 2)
	pub texture_flags: Option<BspTexFlags>,
	/// For BSP2 and BSP29, this is always zero.
	pub surface_flags: BspSurfaceFlags,
}

impl BspVariableValue for BspTexInfoFlags {
	type Bsp29 = BspTexFlags;
	type Bsp2 = BspTexFlags;
	type Bsp30 = BspSurfaceFlags;
	type Bsp38 = BspSurfaceFlags;
}

/// Quake 1-style texture flags. Quake 2 ditches this entirely, GoldSrc
/// mostly handles this on a per-brush basis using `rendermode` and
/// related entity keys.
#[derive(BspValue, Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[repr(u32)]
pub enum BspTexFlags {
	#[default]
	/// Normal lightmapped surface.
	Normal = 0,
	/// No lighting or 256 subdivision.
	Special = 1,
	/// Texture cannot be found.
	Missing = 2,
}

bitflags! {
	#[derive(Debug, Copy, Clone)]
	#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
	#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
	#[repr(transparent)]
	#[cfg_attr(feature = "bevy_reflect", reflect(opaque))]
	pub struct BspSurfaceFlags: u32 {
		/// Never give falling damage
		const NO_DAMAGE = 0b0000_0000_0000_0000_0001;
		/// Affects game physics
		const SLICK = 0b0000_0000_0000_0000_0010;
		/// Lighting from environment map
		const SKY = 0b0000_0000_0000_0000_0100;
		/// Climbable ladder
		const WARP = 0b0000_0000_0000_0000_1000;
		/// Don't make missile explosions
		const NO_IMPACT = 0b0000_0000_0000_0001_0000;
		/// Don't leave missile marks
		const NO_MARKS = 0b0000_0000_0000_0010_0000;
		/// Make flesh sounds and effects
		const FLESH = 0b0000_0000_0000_0100_0000;
		/// Don't generate a drawsurface at all
		const NO_DRAW = 0b0000_0000_0000_1000_0000;
		/// Make a primary bsp splitter
		const HINT = 0b0000_0000_0001_0000_0000;
		/// Completely ignore, allowing non-closed brushes
		const SKIP = 0b0000_0000_0010_0000_0000;
		/// Surface doesn't need a lightmap
		const NO_LIGHTMAP = 0b0000_0000_0100_0000_0000;
		/// Generate lighting info at vertices
		const POINT_LIGHT = 0b0000_0000_1000_0000_0000;
		/// Clanking footsteps
		const METAL_STEPS = 0b0000_0001_0000_0000_0000;
		/// No footstep sounds
		const NO_STEPS = 0b0000_0010_0000_0000_0000;
		/// Don't collide against curves with this set
		const NON_SOLID = 0b0000_0100_0000_0000_0000;
		/// Act as a light filter during q3map -light
		const LIGHT_FILTER = 0b0000_1000_0000_0000_0000;
		/// Do per-pixel light shadow casting in q3map
		const ALPHA_SHADOW = 0b0001_0000_0000_0000_0000;
		/// Never add dynamic lights
		const NO_DLIGHT = 0b0010_0000_0000_0000_0000;
		/// Unused in Quake 2, but included in case you want to add an extension.
		const UNUSED1 = 0b0100_0000_0000_0000_0000;
		/// Unused in Quake 2, but included in case you want to add an extension.
		const UNUSED2 = 0b1000_0000_0000_0000_0000;
	}
}

impl BspValue for BspSurfaceFlags {
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		u32::bsp_parse(reader).map(BspSurfaceFlags::from_bits_truncate)
	}

	fn bsp_struct_size(ctx: &BspParseContext) -> usize {
		u32::bsp_struct_size(ctx)
	}
}

/// Extra info stored directly on the `TexInfo` - for Quake 2 (which does not have a lump for embedded textures).
#[derive(BspValue, Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspTexExtraInfo {
	/// Texture brightness - 255 is "normal" brightness (should display the same as Quake 1),
	/// higher is emissive, lower is darker. To get this as a floating-point number, see
	/// [`BspTexExtraInfo::brightness`].
	pub value: u32,

	/// The name of the texture.
	pub name: FixedStr<32>,

	/// For animated textures.
	pub next: i32,
}

/// Texture projection information.
#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct PlanarTextureProjection {
	pub u_axis: Vec3,
	pub u_offset: f32,

	pub v_axis: Vec3,
	pub v_offset: f32,
}

impl PlanarTextureProjection {
	/// Projects a position onto this plane.
	///
	/// Converts to double for calculation to minimise floating-point imprecision as demonstrated [here](https://github.com/Novum/vkQuake/blob/b6eb0cf5812c09c661d51e3b95fc08d88da2288a/Quake/gl_model.c#L1315).
	pub fn project(&self, point: Vec3) -> Vec2 {
		dvec2(
			point.as_dvec3().dot(self.u_axis.as_dvec3()) + self.u_offset as f64,
			point.as_dvec3().dot(self.v_axis.as_dvec3()) + self.v_offset as f64,
		)
		.as_vec2()
	}
}

#[derive(BspVariableValue, Default, Debug, Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[bsp29(NoField)]
#[bsp2(NoField)]
#[bsp30(Palette)]
#[bsp38(NoField)]
pub struct Wad3Palette(pub Option<Palette>);

#[derive(Default, Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspTextureData {
	pub full: Option<Vec<u8>>,
	pub half: Option<Vec<u8>>,
	pub quarter: Option<Vec<u8>>,
	pub eighth: Option<Vec<u8>>,
	pub palette: Wad3Palette,
}

impl std::fmt::Debug for BspTextureData {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("BspTextureData")
			.field("full", &self.full.as_ref().map(|_| ..))
			.field("half", &self.half.as_ref().map(|_| ..))
			.field("quarter", &self.quarter.as_ref().map(|_| ..))
			.field("eighth", &self.eighth.as_ref().map(|_| ..))
			.finish()
	}
}

/// Embedded texture data. GoldSrc stores these in a separate lump.
#[derive(Debug, Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspMipTexture {
	pub header: BspTextureHeader,
	pub data: BspTextureData,
}

impl BspValue for BspMipTexture {
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		let reader_start = reader.pos();

		let header: BspTextureHeader = reader.read()?;

		macro_rules! read_data {
			($offset:ident, $res:literal $(, $($res_operator:tt)+)?) => {{
				if header.$offset == 0 {
					None
				} else {
					// The offsets are relative to start of the header, so reset state before continuing.
					*reader = reader.with_pos(reader_start + header.$offset as usize);

					Some(
						reader
							.read_bytes((header.width as usize $($($res_operator)+)?) * (header.height as usize $($($res_operator)+)?))
							.job(format!(concat!("Reading texture (", $res, "res) with header {:#?}"), header))?
							.to_vec(),
					)
				}
			}};
		}

		if [header.offset_full, header.offset_half, header.offset_quarter, header.offset_eighth]
			.into_iter()
			.all(|o| o == 0)
		{
			Ok(Self {
				data: Default::default(),
				header,
			})
		} else {
			Ok(Self {
				data: BspTextureData {
					full: read_data!(offset_full, "full"),
					half: read_data!(offset_half, "half", / 2),
					quarter: read_data!(offset_quarter, "quarter", / 4),
					eighth: read_data!(offset_eighth, "eighth", / 8),
					// We do not reset state after the last read, as the next should start directly after.
					palette: reader.read()?,
				},
				header,
			})
		}
	}

	fn bsp_struct_size(ctx: &BspParseContext) -> usize {
		BspTextureHeader::bsp_struct_size(ctx)
	}
}

#[derive(BspValue, Debug, Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspTextureHeader {
	/// What format was this parsed with.
	pub format: BspFormat,

	pub name: FixedStr<16>,

	pub width: u32,
	pub height: u32,

	pub offset_full: u32,
	#[allow(unused)]
	pub offset_half: u32,
	#[allow(unused)]
	pub offset_quarter: u32,
	#[allow(unused)]
	pub offset_eighth: u32,
}
