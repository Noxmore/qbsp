use crate::prelude::*;
use crate::{
	Bsp29LeafContents, BspByteReader, BspParseContext, BspParseResultDoingJobExt, BspResult, BspValue, FixedStr, PlanarTextureProjection,
	ShortBsp29LeafContents,
};
use bitflags::bitflags;

/// The maximum number of characters in a `q2bsp::BspTexInfo` name.
pub const MAX_TEXTURE_NAME_BYTES: usize = 32;

/// Extra info stored directly on the `TexInfo` - for Quake 2 (which does not have a lump for embedded textures).
#[derive(BspValue, Debug, Clone, PartialEq, Eq)]
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

bitflags! {
	#[derive(Debug, Clone, Copy, PartialEq, Eq)]
	#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
	#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
	#[repr(transparent)]
	#[cfg_attr(feature = "bevy_reflect", reflect(opaque))]
	pub struct BspLeafContentFlags: u32 {
		// An eye is never valid in a solid
		const SOLID = 0b0000_0000_0000_0000_0000_0000_0000_0001;
		const WINDOW = 0b0000_0000_0000_0000_0000_0000_0000_0010;
		const AUX = 0b0000_0000_0000_0000_0000_0000_0000_0100;
		const LAVA = 0b0000_0000_0000_0000_0000_0000_0000_1000;
		const SLIME = 0b0000_0000_0000_0000_0000_0000_0001_0000;
		const WATER = 0b0000_0000_0000_0000_0000_0000_0010_0000;
		const MIST = 0b0000_0000_0000_0000_0000_0000_0100_0000;

		const AREA_PORTAL = 0b0000_0000_0000_0000_1000_0000_0000_0000;

		const PLAYER_CLIP = 0b0000_0000_0000_0001_0000_0000_0000_0000;
		const MONSTER_CLIP = 0b0000_0000_0000_0010_0000_0000_0000_0000;

		// Bot-specific contents types
		const CURRENT_0 = 0b0000_0000_0000_0100_0000_0000_0000_0000;
		const CURRENT_90 = 0b0000_0000_0000_1000_0000_0000_0000_0000;
		const CURRENT_180 = 0b0000_0000_0001_0000_0000_0000_0000_0000;
		const CURRENT_270 = 0b0000_0000_0010_0000_0000_0000_0000_0000;
		const CURRENT_UP = 0b0000_0000_0100_0000_0000_0000_0000_0000;
		const CURRENT_DOWN = 0b0000_0000_1000_0000_0000_0000_0000_0000;

		// Removed before bsping an entity
		const ORIGIN = 0b0000_0001_0000_0000_0000_0000_0000_0000;

		// Should never be on a brush; only in game
		const MONSTER = 0b0000_0010_0000_0000_0000_0000_0000_0000;
		const DEAD_MONSTER = 0b0000_0100_0000_0000_0000_0000_0000_0000;
		// Brushes not used for the bsp
		const DETAIL = 0b0000_1000_0000_0000_0000_0000_0000_0000;
		// Don't consume surface fragments inside
		const TRANSLUCENT = 0b0001_0000_0000_0000_0000_0000_0000_0000;
		const LADDER = 0b0010_0000_0000_0000_0000_0000_0000_0000;
		/// Unused in Quake 2; but included in case you want to add an extension.
		const UNUSED1 = 0b0100_0000_0000_0000_0000_0000_0000_0000;
		/// Unused in Quake 2; but included in case you want to add an extension.
		const UNUSED2 = 0b1000_0000_0000_0000_0000_0000_0000_0000;
	}
}

impl BspValue for BspLeafContentFlags {
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		u32::bsp_parse(reader).map(Self::from_bits_truncate)
	}

	fn bsp_struct_size(ctx: &crate::BspParseContext) -> usize {
		u32::bsp_struct_size(ctx)
	}
}

impl From<Bsp29LeafContents> for BspLeafContentFlags {
	fn from(value: Bsp29LeafContents) -> Self {
		match value {
			Bsp29LeafContents::Empty => Self::empty(),
			Bsp29LeafContents::Solid => BspLeafContentFlags::SOLID,
			Bsp29LeafContents::Water => BspLeafContentFlags::WATER,
			Bsp29LeafContents::Slime => BspLeafContentFlags::SLIME,
			Bsp29LeafContents::Lava => BspLeafContentFlags::LAVA,
			// Sky is considered solid for the purposes of tracing.
			Bsp29LeafContents::Sky => BspLeafContentFlags::SOLID,
			// Clip is considered solid for the purposes of tracing.
			Bsp29LeafContents::Clip => BspLeafContentFlags::SOLID,
			Bsp29LeafContents::Current0 => BspLeafContentFlags::CURRENT_0,
			Bsp29LeafContents::Current90 => BspLeafContentFlags::CURRENT_90,
			Bsp29LeafContents::Current180 => BspLeafContentFlags::CURRENT_180,
			Bsp29LeafContents::Current270 => BspLeafContentFlags::CURRENT_270,
			Bsp29LeafContents::CurrentUp => BspLeafContentFlags::CURRENT_UP,
			Bsp29LeafContents::CurrentDown => BspLeafContentFlags::CURRENT_DOWN,
		}
	}
}

impl From<ShortBsp29LeafContents> for BspLeafContentFlags {
	fn from(value: ShortBsp29LeafContents) -> Self {
		value.0.into()
	}
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
	pub flags: BspSurfaceFlags,

	/// Texture brightness - 255 is "normal" brightness (should display the same as Quake 1),
	/// higher is emissive, lower is darker. To get this as a floating-point number, see
	/// [`BspTexInfo::brightness`].
	pub value: u32,

	/// The name of the texture.
	pub name: FixedStr<MAX_TEXTURE_NAME_BYTES>,

	/// For animated textures.
	pub next: i32,
}
