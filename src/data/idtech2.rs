use glam::Vec3;

use crate::prelude::*;

use crate::{BspByteReader, BspData, BspParseContext, BspParseResultDoingJobExt, BspResult, BspValue, BspVariableValue, UBspValue};

/// A reference to a [`BspNode`]. Reads an `i32`, if positive it's an index of a node, if negative it's the index of a leaf.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum BspNodeRef<T = u32> {
	Node(T),
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
pub struct BspNodeSubRef(BspNodeRef);

impl From<BspNodeRef<u16>> for BspNodeRef<u32> {
	fn from(value: BspNodeRef<u16>) -> Self {
		match value {
			BspNodeRef::Node(val) => BspNodeRef::Node(val.into()),
			BspNodeRef::Leaf(val) => BspNodeRef::Leaf(val.into()),
		}
	}
}

impl BspNodeRef {
	pub fn node(&self) -> Option<u32> {
		match *self {
			Self::Node(i) => Some(i),
			Self::Leaf(_) => None,
		}
	}

	pub fn leaf(&self) -> Option<u32> {
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
	pub fn from_i32(value: i32) -> Self {
		if value.is_negative() {
			Self::Leaf((-value) as u32 - 1) // - 1 because you can't have -0
		} else {
			Self::Node(value as u32)
		}
	}
}

impl BspNodeRef<u16> {
	pub fn from_i16(value: i16) -> Self {
		if value.is_negative() {
			Self::Leaf((-value) as u16 - 1) // - 1 because you can't have -0
		} else {
			Self::Node(value as u16)
		}
	}
}

#[derive(BspValue, Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspEdge {
	/// The index to the first vertex this edge connects
	pub a: UBspValue,
	/// The index to the second vertex this edge connects
	pub b: UBspValue,
}

#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspFace {
	/// Index of the plane the face is parallel to
	pub plane_idx: UBspValue,
	/// If not zero, seems to indicate that the normal should be inverted when creating meshes
	pub plane_side: UBspValue,

	/// Index of the first edge (in the face edge array)
	pub first_edge: u32,
	/// Number of consecutive edges (in the face edge array)
	pub num_edges: UBspValue,

	/// Index of the texture info structure
	pub texture_info_idx: UBspValue,

	/// Each face can have up to 4 lightmaps, the additional 3 are positioned right after the lightmap at `lightmap_offset`.
	///
	/// Each element in this array is the style in which these lightmaps appear, see docs for [`LightmapStyle`].
	///
	/// You can also short-circuit when looping through these styles, if `lightmap_styles[2]` is 255, there isn't a possibility that `lightmap_styles[3]` isn't.
	pub lightmap_styles: [LightmapStyle; 4],

	/// Offset of the lightmap (in bytes) in the lightmap lump, or -1 if no lightmap
	pub lightmap_offset: i32,
}

impl BspFace {
	/// Returns an iterator that retrieves the vertex positions that make up this face from `bsp`.
	#[inline]
	pub fn vertices<'a>(&self, bsp: &'a BspData) -> impl Iterator<Item = Vec3> + 'a {
		(self.first_edge..self.first_edge + self.num_edges.0).map(|i| {
			let surf_edge = bsp.surface_edges[i as usize];
			let edge = bsp.edges[surf_edge.unsigned_abs() as usize];
			let vert_idx = if surf_edge.is_negative() { (edge.b, edge.a) } else { (edge.a, edge.b) };

			bsp.vertices[*vert_idx.0 as usize]
		})
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
