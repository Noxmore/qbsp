//! Data definitions for the BSP node tree.

use crate::{
	BspParseError,
	data::{util::UBspValue, visdata::VisDataRef},
};
#[cfg(feature = "bevy_reflect")]
use bevy_reflect::Reflect;
use glam::{U16Vec3, Vec3};
use qbsp_macros::{BspValue, BspVariableValue};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use bitflags::bitflags;

use crate::{
	BspResult,
	data::util::NoField,
	reader::{BspByteReader, BspParseContext, BspValue, BspVariableValue},
};

/// A reference to a [`BspNode`].
///
/// Reads an `i32` or `i16` (depending on context and format). Negative indices are treated as a leaf index
/// of `1 - signed_node_ref`. `-1` (leaf 0) means out-of-bounds.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum BspNodeRef<T = u32> {
	/// A reference to a node.
	Node(T),
	/// A reference to a leaf.
	Leaf(T),
}

impl BspValue for BspNodeRef<u32> {
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		Ok(BspNodeRef::from_i32(i32::bsp_parse(reader)?))
	}

	fn bsp_struct_size(ctx: &BspParseContext) -> usize {
		i32::bsp_struct_size(ctx)
	}
}

impl BspValue for BspNodeRef<u16> {
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		Ok(BspNodeRef::from_i16(i16::bsp_parse(reader)?))
	}

	fn bsp_struct_size(ctx: &BspParseContext) -> usize {
		i16::bsp_struct_size(ctx)
	}
}

/// A reference to a [`BspNode`]. Reads an `i32`, if positive it's an index of a node, if negative it's the index of a leaf.
#[derive(BspVariableValue, Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[bsp29(BspNodeRef<u16>)]
#[bsp2(BspNodeRef<u32>)]
#[bsp30(BspNodeRef<u16>)]
#[bsp38(BspNodeRef<u32>)]
#[qbism(BspNodeRef<u32>)]
pub struct BspNodeSubRef(BspNodeRef);

impl From<BspNodeRef<u16>> for BspNodeRef<u32> {
	fn from(value: BspNodeRef<u16>) -> Self {
		match value {
			BspNodeRef::Node(val) => BspNodeRef::Node(val.into()),
			BspNodeRef::Leaf(val) => BspNodeRef::Leaf(val.into()),
		}
	}
}

impl<T> BspNodeRef<T>
where
	T: Copy,
{
	/// If this reference points to a node, get the index of the node.
	pub fn node(&self) -> Option<T> {
		match *self {
			Self::Node(i) => Some(i),
			Self::Leaf(_) => None,
		}
	}

	/// If this reference points to a leaf, get the index of the leaf. Note that `Some(0)`
	/// means out-of-bounds.
	pub fn leaf(&self) -> Option<T> {
		match *self {
			Self::Leaf(i) => Some(i),
			Self::Node(_) => None,
		}
	}
}

impl From<i32> for BspNodeRef {
	fn from(value: i32) -> Self {
		Self::from_i32(value)
	}
}

impl From<i16> for BspNodeRef<u16> {
	fn from(value: i16) -> Self {
		Self::from_i16(value as _)
	}
}

impl BspNodeRef<u32> {
	/// Create a 32-bit `BspNodeRef` from an `i32`. Negative indices are treated as a leaf index
	/// of `1 - signed_node_ref`. `-1` (leaf 0) means out-of-bounds.
	pub const fn from_i32(value: i32) -> Self {
		if value.is_negative() {
			// Bitwise not handles integer asymmetry and overflow.
			Self::Leaf(!value as u32)
		} else {
			Self::Node(value as u32)
		}
	}
}

impl BspNodeRef<u16> {
	/// Create a 16-bit `BspNodeRef` from an `i16`. Negative indices are treated as a leaf index
	/// of `1 - signed_node_ref`. `-1` (leaf 0) means out-of-bounds.
	pub const fn from_i16(value: i16) -> Self {
		if value.is_negative() {
			// Bitwise not handles integer asymmetry and overflow.
			Self::Leaf(!value as u16)
		} else {
			Self::Node(value as u16)
		}
	}
}

/// Leaf contents for BSP29 and BSP2, descriptions taken from
/// [the community BSP29 specification](https://www.gamers.org/dEngine/quake/spec/quake-spec34/qkspec_4.htm),
/// and refer to the behavior when the camera is inside the given leaf contents.
#[derive(BspValue, Debug, Clone, Copy, Default, PartialEq, Eq)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[repr(i32)]
pub enum Bsp29LeafContents {
	/// Ordinary leaf (air).
	#[default]
	Empty = -1,
	/// The leaf is entirely inside a solid (nothing is displayed).
	Solid = -2,
	/// Water, the vision is troubled.
	Water = -3,
	/// Slime, green acid that hurts the player.
	Slime = -4,
	/// Lava, vision turns red and the player is badly hurt.
	Lava = -5,
	/// Behaves like water, but is used for sky.
	///
	/// > *Note*: For clarification, this is labeled "sky" internally, but Quake 1 treats
	/// > any leaf contants of value `-3` or less to be water, with extra effects only
	/// > defined for `-4` and `-5`. As a result, despite the below contents having specific
	/// > meanings while compiling the map, if found in a map at runtime they just behave
	/// > like water. See the Quake GPL release:
	/// >
	/// > - [Rendering functions](https://github.com/id-Software/Quake/blob/0023db327bc1db00068284b70e1db45857aeee35/WinQuake/r_misc.c#L439)
	/// > - [Server-side](https://github.com/id-Software/Quake/blob/0023db327bc1db00068284b70e1db45857aeee35/WinQuake/world.c#L532-L533).
	Sky = -6,
	// Origin = -7, removed at csg time
	/// Clip brushes
	///
	/// > *Note*: Behaves like water in Quake 1 if found at runtime, see comment for [`Self::Sky`].
	Clip = -8,
	/// *TODO*: Only has meaning at BSP compile-time.
	///
	/// > *Note*: Behaves like water in Quake 1 if found at runtime, see comment for [`Self::Sky`].
	Current0 = -9,
	/// *TODO*: Only has meaning at BSP compile-time.
	///
	/// > *Note*: Behaves like water in Quake 1 if found at runtime, see comment for [`Self::Sky`].
	Current90 = -10,
	/// *TODO*: Only has meaning at BSP compile-time.
	///
	/// > *Note*: Behaves like water in Quake 1 if found at runtime, see comment for [`Self::Sky`].
	Current180 = -11,
	/// *TODO*: Only has meaning at BSP compile-time.
	///
	/// > *Note*: Behaves like water in Quake 1 if found at runtime, see comment for [`Self::Sky`].
	Current270 = -12,
	/// *TODO*: Only has meaning at BSP compile-time.
	///
	/// > *Note*: Behaves like water in Quake 1 if found at runtime, see comment for [`Self::Sky`].
	CurrentUp = -13,
	/// *TODO*: Only has meaning at BSP compile-time.
	///
	/// > *Note*: Behaves like water in Quake 1 if found at runtime, see comment for [`Self::Sky`].
	CurrentDown = -14,
}

/// Wrapper for [`BspLeafContents`] that reads an [`prim@i16`] rather than an [`prim@i32`].
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ShortBsp29LeafContents(pub Bsp29LeafContents);

impl BspValue for ShortBsp29LeafContents {
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		let value = reader.read::<i16>()? as i32;

		Bsp29LeafContents::bsp_parse(&mut BspByteReader::new(&value.to_le_bytes(), reader.ctx)).map(Self)
	}

	fn bsp_struct_size(_ctx: &BspParseContext) -> usize {
		size_of::<i16>()
	}
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

	fn bsp_struct_size(ctx: &BspParseContext) -> usize {
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

/// If loading a BSP2, parses a float-based bounding box, otherwise parses a short-based bounding box.
#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BoundingBox {
	pub min: Vec3,
	pub max: Vec3,
}

impl From<FloatBoundingBox> for BoundingBox {
	fn from(value: FloatBoundingBox) -> Self {
		Self {
			min: value.min,
			max: value.max,
		}
	}
}

impl From<ShortBoundingBox> for BoundingBox {
	fn from(value: ShortBoundingBox) -> Self {
		Self {
			min: value.min.as_vec3(),
			max: value.max.as_vec3(),
		}
	}
}

impl BspVariableValue for BoundingBox {
	type Bsp29 = ShortBoundingBox;
	type Bsp2 = FloatBoundingBox;
	type Bsp30 = ShortBoundingBox;
	type Bsp38 = ShortBoundingBox;
	type Qbism = FloatBoundingBox;
}

#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct FloatBoundingBox {
	pub min: Vec3,
	pub max: Vec3,
}

#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ShortBoundingBox {
	pub min: U16Vec3,
	pub max: U16Vec3,
}

#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspNode {
	/// Index of the [`BspPlane`] that splits the node.
	pub plane_idx: u32,

	pub front: BspNodeSubRef,
	pub back: BspNodeSubRef,

	/// Bounding box of the node and all its children.
	pub bound: BoundingBox,
	/// Index of the first [`BspFace`](crate::data::models::BspFace) the node contains.
	pub face_idx: UBspValue,
	/// Number of faces this node contains.
	pub face_num: UBspValue,
}

/// According to the [`gamers.org` specification](https://www.gamers.org/dEngine/quake/spec/quake-spec34/qkspec_4.htm#BL9):
///
/// > This structure is used to give a rough and somewhat exaggerated boundary to a given model. It does not separate models from each others, and is not used at all in the rendering of the levels
///
/// > Actually, the clip nodes are only used as a first and primitive collision checking method.
///
/// I think it is also used for shape casting.
#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspClipNode {
	/// Index of the [`BspPlane`] that splits the clip node.
	pub plane_idx: u32,

	/// If positive, id of Front child node. If -2, the Front part is inside the model. If -1, the Front part is outside the model.
	pub front: BspNodeSubRef,
	/// If positive, id of Back child node. If -2, the Back part is inside the model. If -1, the Back part is outside the model.
	pub back: BspNodeSubRef,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspAreaIdx(pub Option<u32>);

impl BspVariableValue for BspAreaIdx {
	type Bsp29 = NoField;
	type Bsp2 = NoField;
	type Bsp30 = NoField;
	type Bsp38 = u16;
	type Qbism = u32;
}

impl From<u16> for BspAreaIdx {
	fn from(value: u16) -> Self {
		Self(Some(value as u32))
	}
}
impl From<u32> for BspAreaIdx {
	fn from(value: u32) -> Self {
		Self(Some(value))
	}
}
impl From<NoField> for BspAreaIdx {
	fn from(_: NoField) -> Self {
		Self(None)
	}
}

#[derive(BspVariableValue, Debug, Copy, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[bsp29(Bsp29LeafContents)]
#[bsp2(Bsp29LeafContents)]
#[bsp30(BspLeafContentFlags)]
#[bsp38(BspLeafContentFlags)]
#[qbism(BspLeafContentFlags)]
pub struct BspLeafContents(pub BspLeafContentFlags);

#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspLeaf {
	pub contents: BspLeafContents,

	/// For BSP29, BSP2, and BSP30, this is an index into the `visibility` byte array where the
	/// visdata for this leaf starts. For BSP38, this is a "cluster". Leafs with the same cluster
	/// should be put within the same model, and are considered potentially visible or not as a
	/// group.
	pub visdata: VisDataRef,

	pub area: BspAreaIdx,

	/// The AABB bounding box of this leaf.
	pub bound: BoundingBox,

	/// Index in the `mark_surfaces` list.
	pub face_idx: UBspValue,
	/// Number of elements in the `mark_surfaces` list.
	pub face_num: UBspValue,

	pub ambience: Option<BspAmbience>,
	pub leaf_brushes: Option<BspLeafBrushes>,
}

#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspLeafBrushes {
	pub idx: UBspValue,
	pub num: UBspValue,
}

impl BspVariableValue for Option<BspLeafBrushes> {
	type Bsp29 = NoField;
	type Bsp2 = NoField;
	type Bsp30 = NoField;
	type Bsp38 = BspLeafBrushes;
	type Qbism = BspLeafBrushes;
}

#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspAmbience {
	pub water: u8,
	pub sky: u8,
	pub slime: u8,
	pub lava: u8,
}

impl BspVariableValue for Option<BspAmbience> {
	type Bsp29 = BspAmbience;
	type Bsp2 = BspAmbience;
	type Bsp30 = BspAmbience;
	// Quake 2 does not have the `ambience` field.
	type Bsp38 = NoField;
	type Qbism = NoField;
}

#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspPlane {
	pub normal: Vec3,
	pub dist: f32,
	/// Type of plane depending on normal vector.
	pub ty: BspPlaneType,
}

impl BspPlane {
	/// `>0` = front, `<0` = back, `0` = on plane
	pub fn point_side(&self, point: Vec3) -> f32 {
		let plane_axis = self.ty as usize;

		// If the plane lies on a cardinal axis, the computation is much simpler.
		if plane_axis < 3 {
			point[plane_axis] - self.dist
		} else {
			(self.normal.as_dvec3().dot(point.as_dvec3()) - self.dist as f64) as f32
		}
	}
}

/// Type of plane depending on normal vector.
///
/// Referenced from [this specification](https://www.gamers.org/dEngine/quake/spec/quake-spec34/qkspec_4.htm#BL1).
#[derive(BspValue, Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[repr(u32)]
pub enum BspPlaneType {
	/// Axial plane, in X
	AxialX = 0,
	/// Axial plane, in Y
	AxialY = 1,
	/// Axial plane, in Z
	AxialZ = 2,
	/// Non axial plane, roughly toward X
	AroundX = 3,
	/// Non axial plane, roughly toward Y
	AroundY = 4,
	/// Non axial plane, roughly toward Z
	AroundZ = 5,
}
