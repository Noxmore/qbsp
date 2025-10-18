use enumflags2::{bitflags, BitFlags};
use glam::Vec3;

use crate::{
	BoundingBox, BspByteReader, BspNodeRef, BspParseContext, BspParseResultDoingJobExt, BspResult, BspValue, FixedStr, PlanarTextureProjection,
};

/// The maximum number of characters in a `q2bsp::BspTexInfo` name.
pub const MAX_TEXTURE_NAME_BYTES: usize = 32;

/// Extra info stored directly on the `TexInfo` - for Quake 2 (which does not have a lump for embedded textures).
#[derive(BspValue, Debug, Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspTexExtraInfo {
	/// Texture brightness - 255 is "normal" brightness (should display the same as Quake 1),
	/// higher is emissive, lower is darker. To get this as a floating-point number, see
	/// [`BspTexInfo::brightness`].
	pub value: u32,

	/// The name of the texture.
	pub name: FixedStr<32>,

	/// For animated textures.
	pub next: i32,
}

#[bitflags]
#[derive(Debug, Copy, Clone)]
#[repr(u32)]
pub enum BspSurfaceFlags {
	/// Never give falling damage
	Nodamage = 0b0000_0000_0000_0000_0001,
	/// Affects game physics
	Slick = 0b0000_0000_0000_0000_0010,
	/// Lighting from environment map
	Sky = 0b0000_0000_0000_0000_0100,
	/// Climbable ladder
	Warp = 0b0000_0000_0000_0000_1000,
	/// Don't make missile explosions
	NoImpact = 0b0000_0000_0000_0001_0000,
	/// Don't leave missile marks
	NoMarks = 0b0000_0000_0000_0010_0000,
	/// Make flesh sounds and effects
	Flesh = 0b0000_0000_0000_0100_0000,
	/// Don't generate a drawsurface at all
	NoDraw = 0b0000_0000_0000_1000_0000,
	/// Make a primary bsp splitter
	Hint = 0b0000_0000_0001_0000_0000,
	/// Completely ignore, allowing non-closed brushes
	Skip = 0b0000_0000_0010_0000_0000,
	/// Surface doesn't need a lightmap
	NoLightmap = 0b0000_0000_0100_0000_0000,
	/// Generate lighting info at vertices
	PointLight = 0b0000_0000_1000_0000_0000,
	/// Clanking footsteps
	MetalSteps = 0b0000_0001_0000_0000_0000,
	/// No footstep sounds
	NoSteps = 0b0000_0010_0000_0000_0000,
	/// Don't collide against curves with this set
	NonSolid = 0b0000_0100_0000_0000_0000,
	/// Act as a light filter during q3map -light
	LightFilter = 0b0000_1000_0000_0000_0000,
	/// Do per-pixel light shadow casting in q3map
	AlphaShadow = 0b0001_0000_0000_0000_0000,
	/// Never add dynamic lights
	NoDlight = 0b0010_0000_0000_0000_0000,
	/// Unused in Quake 2, but included in case you want to add an extension.
	Unused1 = 0b0100_0000_0000_0000_0000,
	/// Unused in Quake 2, but included in case you want to add an extension.
	Unused2 = 0b1000_0000_0000_0000_0000,
}

impl BspValue for BitFlags<BspSurfaceFlags> {
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		u32::bsp_parse(reader).map(BitFlags::from_bits_truncate)
	}

	fn bsp_struct_size(ctx: &crate::BspParseContext) -> usize {
		u32::bsp_struct_size(ctx)
	}
}

#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Q2BspTexture {
	/// The UV projection for this texture.
	pub projection: PlanarTextureProjection,

	/// Any surface flags for this texture.
	pub flags: BitFlags<BspSurfaceFlags>,

	/// Texture brightness - 255 is "normal" brightness (should display the same as Quake 1),
	/// higher is emissive, lower is darker. To get this as a floating-point number, see
	/// [`BspTexInfo::brightness`].
	pub value: u32,

	/// The name of the texture.
	pub name: FixedStr<MAX_TEXTURE_NAME_BYTES>,

	/// For animated textures.
	pub next: i32,
}

#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspModel {
	pub bound: BoundingBox,

	/// Origin of model, usually (0,0,0)
	pub origin: Vec3,

	pub head_bsp_node: BspNodeRef,

	/// The index of the first face in the list.
	pub first_face: u32,
	pub num_faces: u32,
}
