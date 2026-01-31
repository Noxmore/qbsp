//! Data definitions related to model and mesh data.

use crate::{
	BspData,
	data::{
		lighting::{LightmapOffset, LightmapStyle},
		nodes::{BspNodeRef, FloatBoundingBox},
		util::{NoField, UBspValue},
	},
	reader::BspVariableValue,
};
#[cfg(feature = "bevy_reflect")]
use bevy_reflect::Reflect;
use glam::Vec3;
use qbsp_macros::{BspValue, BspVariableValue};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Hulls used for collision. BSP38 only has the root node (for point traces), as it uses brush-based
/// instead of precalculating hulls for different-sized entities.
#[derive(BspValue, Clone, PartialEq, Debug, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Hulls {
	/// The root hull, used for visibility and point traces.
	pub root: BspNodeRef,
	/// Hulls used for collision with non-point entities.
	pub for_size: Option<PerSizeHulls>,
}

/// Hulls for collision against objects with bounding boxes of different sizes.
///
/// [See the Quake GPL release](https://github.com/id-Software/Quake/blob/bf4ac424ce754894ac8f1dae6a3981954bc9852d/WinQuake/world.c#L129-L171)
#[derive(BspValue, Clone, PartialEq, Debug, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct PerSizeHulls {
	/// Used for entities of size <= 32 units, such as players. Quake only checks the X axis for the purposes
	/// of this check, as all entities are assumed to have hulls with a square horizontal cross-section.
	pub small: BspNodeRef,
	/// Used for entities of size > 32 units. As with the above field, Quake only checks the X axis.
	pub large: BspNodeRef,
	/// Unused in almost all known BSP files. This is hard-coded to be unused in the Quake 1 engine, and cannot be
	/// overridden in the QuakeC code of mods, so it is extremely rare to find this field used even in unconventional
	/// Quake mods.
	///
	/// [The community specification](https://www.gamers.org/dEngine/quake/spec/quake-spec34/qkspec_4.htm#BLE) notes
	/// that the 4th hull is "usually zero" in Quake 1.
	/// [The Valve Developer Wiki](https://developer.valvesoftware.com/wiki/BSP_(GoldSrc)) says "hull
	/// 3 is unused in Quake". This refers to the 4th hull (with the first hull being hull 0).
	pub unused: BspNodeRef,
}

impl BspVariableValue for Option<PerSizeHulls> {
	type Bsp29 = PerSizeHulls;
	type Bsp2 = PerSizeHulls;
	type Bsp30 = PerSizeHulls;
	type Bsp38 = NoField;
	type Qbism = NoField;
}

/// Number of visleafs in the model. Quake 2 uses visclusters, and so
/// this field is omitted.
#[derive(BspVariableValue, Debug, Copy, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[bsp29(u32)]
#[bsp2(u32)]
#[bsp30(u32)]
#[bsp38(NoField)]
#[qbism(NoField)]
pub struct VisleafsField(pub Option<u32>);

/// A single model in the BSP file. Model 0 is worldspawn, other models
/// are used for entities using `*N` where N is the model number.
#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspModel {
	/// The bounding box of the model. Unlike other bounding boxes in BSP files, this is always floating-point-based,
	/// even in BSP29.
	pub bound: FloatBoundingBox,

	/// Origin of model, usually (0,0,0)
	pub origin: Vec3,

	/// The indices for the BSP and clip roots.
	pub hulls: Hulls,

	/// Number of visleafs, not including the out-of-bounds leaf 0. Quake uses a cluster system for visibility,
	/// and so this field is not included.
	pub visleafs: VisleafsField,

	/// The first face in the model.
	pub first_face: u32,

	/// The total number of faces in the model.
	pub num_faces: u32,
}

/// A single edge in a BSP model.
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

	/// Offset of the lightmap in the lightmap lump, or -1 if no lightmap.
	///
	/// This is stored as bytes for BSP30 and BSP38.
	pub lightmap_offset: LightmapOffset,
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
