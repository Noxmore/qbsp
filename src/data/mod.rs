//! Datatype definitions for lumps within a BSP file.

#[cfg(feature = "bevy_reflect")]
use bevy_reflect::Reflect;
use qbsp_macros::{BspValue, BspVariableValue};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::reader::{BspByteReader, BspParseContext, BspValue, BspVariableValue};
use crate::{data::util::NoField, BspFormat, BspParseError, BspParseResultDoingJobExt, BspResult};

pub mod bspx;
pub mod lighting;
pub mod models;
pub mod nodes;
pub mod texture;
pub mod util;
pub mod visdata;

// Re-exports for convenience and to reduce the load of refactoring when upgrading.
pub use self::{
	bspx::{BspxData, ModelBrush, ModelBrushPlane, ModelBrushes},
	lighting::{BspLighting, LightmapOffset, LightmapStyle},
	models::{BspEdge, BspFace, BspModel},
	nodes::{BspClipNode, BspLeaf, BspNode, BspNodeRef, BspPlane},
	texture::{BspMipTexture, BspTexFlags, BspTexInfo, Palette},
	visdata::BspVisData,
};

/// Points to the chunk of data in the file a lump resides in.
#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct LumpEntry {
	pub offset: u32,
	pub len: u32,
}

impl LumpEntry {
	/// Returns the slice of `data` (BSP file input) that this entry points to.
	pub fn get<'a>(&self, data: &'a [u8]) -> BspResult<&'a [u8]> {
		let (from, to) = (self.offset as usize, self.offset as usize + self.len as usize);
		if to > data.len() {
			Err(BspParseError::LumpOutOfBounds(*self))
		} else {
			Ok(&data[from..to])
		}
	}

	/// If `true`, this lump contains no data.
	pub fn is_empty(&self) -> bool {
		self.len == 0
	}
}

/// A `LumpEntry` that doesn't exist for BSP38.
#[derive(BspVariableValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[bsp2(LumpEntry)]
#[bsp29(LumpEntry)]
#[bsp30(LumpEntry)]
#[bsp38(NoField)]
pub struct PreBsp38LumpEntry(Option<LumpEntry>);

/// A `LumpEntry` that only exists for BSP38.
#[derive(BspVariableValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[bsp2(NoField)]
#[bsp29(NoField)]
#[bsp30(NoField)]
#[bsp38(LumpEntry)]
pub struct Bsp38OnlyLumpEntry(Option<LumpEntry>);

/// Contains the list of lump entries
#[derive(Debug, Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct LumpDirectory {
	/// String entity key/value map.
	pub entities: LumpEntry,
	/// Planes map for splitting the BSP tree.
	pub planes: LumpEntry,
	/// Embedded textures (not used for Quake 2).
	pub textures: PreBsp38LumpEntry,
	/// Mesh vertices for each leaf.
	pub vertices: LumpEntry,
	/// Visdata (cluster-based for Quake 2, leaf-basd for other formats).
	pub visibility: LumpEntry,
	/// Non-leaf nodes in the BSP tree.
	pub nodes: LumpEntry,
	/// Texture info.
	pub tex_info: LumpEntry,
	/// Faces of each mesh.
	pub faces: LumpEntry,
	/// Lightmap data.
	pub lighting: LumpEntry,
	/// Clip nodes. Not used for Quake 2, as it does collision using `leaf_brushes` and doesn't
	/// have a concept of clip hulls.
	pub clip_nodes: PreBsp38LumpEntry,
	/// Leaf nodes in the BSP tree.
	pub leaves: LumpEntry,
	/// Leaves use this to index into the face lump.
	pub mark_surfaces: LumpEntry,
	/// Leaf brushes, for Quake 2 BSP collision checks.
	pub leaf_brushes: Bsp38OnlyLumpEntry,
	pub edges: LumpEntry,
	pub surf_edges: LumpEntry,
	pub models: LumpEntry,
	pub brushes: Bsp38OnlyLumpEntry,
	pub brush_sides: Bsp38OnlyLumpEntry,
	/// This field is unused in Quake 2, and it's unclear what its intention is.
	/// We need to include it anyway though, otherwise areas and area portals
	/// won't parse correctly.
	pub pop: Bsp38OnlyLumpEntry,
	pub areas: Bsp38OnlyLumpEntry,
	pub area_portals: Bsp38OnlyLumpEntry,

	pub bspx: bspx::BspxDirectory,
}
impl LumpDirectory {
	/// Does not include `bspx`, as this method is used to calculate the offset
	/// to the BSPX lump.
	pub fn bsp_entries(&self) -> impl Iterator<Item = LumpEntry> {
		[
			Some(self.entities),
			Some(self.planes),
			*self.textures,
			Some(self.vertices),
			Some(self.visibility),
			Some(self.nodes),
			Some(self.tex_info),
			Some(self.faces),
			Some(self.lighting),
			*self.clip_nodes,
			Some(self.leaves),
			Some(self.mark_surfaces),
			*self.leaf_brushes,
			Some(self.edges),
			Some(self.surf_edges),
			Some(self.models),
			*self.brushes,
			*self.brush_sides,
			*self.pop,
			*self.areas,
			*self.area_portals,
		]
		.into_iter()
		.flatten()
	}
}

impl BspValue for LumpDirectory {
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		let mut dir = Self {
			entities: reader.read().job("Reading entities entry")?,
			planes: reader.read().job("Reading planes entry")?,
			textures: reader.read().job("Reading textures entry")?,
			vertices: reader.read().job("Reading vertices entry")?,
			visibility: reader.read().job("Reading visibility entry")?,
			nodes: reader.read().job("Reading nodes entry")?,
			tex_info: reader.read().job("Reading tex_info entry")?,
			faces: reader.read().job("Reading faces entry")?,
			lighting: reader.read().job("Reading lighting entry")?,
			clip_nodes: reader.read().job("Reading clip_nodes entry")?,
			leaves: reader.read().job("Reading leaves entry")?,
			mark_surfaces: reader.read().job("Reading mark_surfaces entry")?,
			leaf_brushes: reader.read().job("Reading leaf brushes entry")?,
			edges: reader.read().job("Reading edges entry")?,
			surf_edges: reader.read().job("Reading surf_edges entry")?,
			models: reader.read().job("Reading models entry")?,
			brushes: reader.read().job("Reading brushes entry")?,
			brush_sides: reader.read().job("Reading brush_sides entry")?,
			pop: reader.read().job("Reading pop entry")?,
			areas: reader.read().job("Reading areas entry")?,
			area_portals: reader.read().job("Reading area_portals entry")?,

			bspx: bspx::BspxDirectory::default(),
		};

		// BSP30/BSP38 never have BSPX dir.
		if [BspFormat::BSP29, BspFormat::BSP2].contains(&reader.ctx.format) {
			// TODO why subtract 4??
			// > Probably the magic number
			let bspx_offset = dir.bsp_entries().map(|entry| entry.offset + entry.len).max().unwrap() - 4;

			match reader.with_pos(bspx_offset as usize).read() {
				Ok(bspx_dir) => dir.bspx = bspx_dir,
				Err(BspParseError::NoBspxDirectory) => {}
				Err(err) => return Err(BspParseError::DoingJob("Reading BSPX directory".to_string(), Box::new(err))),
			}
		}

		Ok(dir)
	}
	fn bsp_struct_size(_ctx: &BspParseContext) -> usize {
		unimplemented!("LumpDirectory is of variable size")
	}
}
