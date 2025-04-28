//! [BSPX](https://developer.valvesoftware.com/wiki/BSPX) data definitions.

use super::*;
use crate::*;

pub const BSPX_ENTRY_NAME_LEN: usize = 24;

#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspxLumpEntry {
	pub name: FixedStr<BSPX_ENTRY_NAME_LEN>,
	pub entry: LumpEntry,
}

#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspxDirectory {
	pub inner: HashMap<FixedStr<BSPX_ENTRY_NAME_LEN>, LumpEntry>,
}
impl BspValue for BspxDirectory {
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		match reader.read().and_then(|magic| {
			if &magic != b"BSPX" {
				Err(BspParseError::WrongMagicNumber {
					found: magic,
					expected: "BSPX",
				})
			} else {
				Ok(())
			}
		}) {
			Ok(()) => {}
			Err(BspParseError::BufferOutOfBounds { .. }) => return Err(BspParseError::NoBspxDirectory),
			Err(err) => return Err(err),
		}

		let num_lumps: u32 = reader.read().job("lump count")?;

		let mut inner = HashMap::new();

		for i in 0..num_lumps {
			let entry: BspxLumpEntry = reader.read().job(format!("lump entry {i}/{num_lumps}"))?;

			inner.insert(entry.name, entry.entry);
		}

		Ok(Self { inner })
	}
	fn bsp_struct_size(_ctx: &BspParseContext) -> usize {
		unimplemented!("BspxDirectory is of variable size")
	}
}

/// Owned version of [`BspxDirectory`]. Convert via [`BspxData::new`].
#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspxData {
	pub inner: HashMap<FixedStr<BSPX_ENTRY_NAME_LEN>, Vec<u8>>,
}
impl BspxData {
	pub fn new(bsp: &[u8], dir: &BspxDirectory) -> BspResult<Self> {
		let mut data = Self::default();

		for (name, entry) in &dir.inner {
			data.inner.insert(*name, entry.get(bsp)?.into());
		}

		Ok(data)
	}

	/// Retrieves a lump entry from the directory, returns `None` if the entry does not exist.
	#[inline]
	pub fn get(&self, s: &str) -> Option<&[u8]> {
		self.inner.get(&FixedStr::from_str(s).ok()?).map(|v| &**v)
	}

	/// Parses the `RGBLIGHTING` lump. Returns `None` if the lump does not exist, else returns `Some` with the parse result.
	pub fn parse_rgb_lighting(&self, ctx: &BspParseContext) -> Option<BspResult<BspLighting>> {
		Some(BspLighting::read_lit(self.get("RGBLIGHTING")?, ctx, true).job("Parsing RGBLIGHTING BSPX lump"))
	}

	/// Parses the `LIGHTGRID_OCTREE` lump. Returns `None` if the lump does not exist, else returns `Some` with the parse result.
	pub fn parse_light_grid_octree(&self, ctx: &BspParseContext) -> Option<BspResult<LightGridOctree>> {
		let mut reader = BspByteReader::new(self.get("LIGHTGRID_OCTREE")?, ctx);
		Some(reader.read().job("Parsing LIGHTGRID_OCTREE BSPX lump"))
	}

	/// Parses the `BRUSHLIST` lump. Returns `None` if the lump does not exist, else returns `Some` with the parse result.
	pub fn parse_brush_list(&self, ctx: &BspParseContext) -> Option<BspResult<BrushList>> {
		let mut reader = BspByteReader::new(self.get("BRUSHLIST")?, ctx);
		let mut brush_list = BrushList::new();

		let mut i: usize = 0;
		while reader.in_bounds() {
			let brushes = match reader.read().job(format!("Parsing BRUSHLIST BSPX lump element {i}")) {
				Ok(v) => v,
				Err(err) => return Some(Err(err)),
			};

			brush_list.push(brushes);
			i += 1;
		}

		Some(Ok(brush_list))
	}

	/// Parses the `DECOUPLED_LM` lump. Returns `None` if the lump does not exist, else returns `Some` with the parse result.
	pub fn parse_decoupled_lm(&self, ctx: &BspParseContext) -> Option<BspResult<DecoupledLightmaps>> {
		let lump_data = self.get("DECOUPLED_LM")?;
		let mut reader = BspByteReader::new(lump_data, ctx);
		let entries_count = lump_data.len() / DecoupledLightmap::bsp_struct_size(ctx);
		let mut lm_infos = DecoupledLightmaps::with_capacity(entries_count);

		for i in 0..entries_count {
			let lm_info = match reader.read().job(format!("Parsing DECOUPLED_LM BSPX lump element {i}")) {
				Ok(v) => v,
				Err(err) => return Some(Err(err)),
			};

			lm_infos.push(lm_info);
		}

		Some(Ok(lm_infos))
	}
}

/// 3d lighting data stored in an octree. Referenced from the [FTE BSPX specification](https://github.com/fte-team/fteqw/blob/master/specs/bspx.txt) and ericw-tools source code.
#[derive(BspValue, Debug, Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct LightGridOctree {
	pub step: Vec3,
	pub size: UVec3,
	pub mins: Vec3,
	pub num_styles: u8,
	pub root_idx: u32,
	pub nodes: BspVariableArray<LightGridNode, u32>,
	pub leafs: BspVariableArray<LightGridLeaf, u32>,
}

#[derive(BspValue, Debug, Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct LightGridNode {
	pub division_point: UVec3,
	pub children: [u32; 8],
}
impl LightGridNode {
	pub const LEAF: u32 = 1 << 31;
	pub const MISSING: u32 = 1 << 30;

	#[rustfmt::skip]
	#[allow(clippy::identity_op)]
	pub fn get_child_index_towards(&self, point: Vec3) -> u32 {
		self.children[
			(((point.z >= self.division_point.z as f32) as usize) << 0) |
			(((point.y >= self.division_point.y as f32) as usize) << 1) |
			(((point.x >= self.division_point.x as f32) as usize) << 2)
		]
	}
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct LightGridLeaf {
	pub mins: UVec3,
	size: UVec3,

	data: Vec<LightGridCell>,
}
impl BspValue for LightGridLeaf {
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		let mins: UVec3 = reader.read().job("position")?;
		let size: UVec3 = reader.read().job("size")?;

		let mut data = Vec::with_capacity(size.element_product() as usize);

		for _ in 0..size.element_product() {
			data.push(reader.read().job("Reading cell")?);
		}

		Ok(Self { mins, size, data })
	}

	fn bsp_struct_size(_ctx: &BspParseContext) -> usize {
		unimplemented!("LightGridLeaf is of variable size")
	}
}
impl LightGridLeaf {
	#[inline]
	pub fn cells(&self) -> &[LightGridCell] {
		&self.data
	}

	/// Returns the index into `data` of the cell at the position specified.
	#[inline]
	pub const fn cell_idx(&self, x: u32, y: u32, z: u32) -> usize {
		((z * self.size.x * self.size.y) + (y * self.size.x) + x) as usize
	}

	/// Returns the cell at the specified position, panics if the position is out of bounds.
	pub fn get_cell(&self, x: u32, y: u32, z: u32) -> &LightGridCell {
		&self.data[self.cell_idx(x, y, z)]
	}
	/// Returns the cell at the specified position, panics if the position is out of bounds.
	pub fn get_cell_mut(&mut self, x: u32, y: u32, z: u32) -> &mut LightGridCell {
		let idx = self.cell_idx(x, y, z);
		&mut self.data[idx]
	}

	#[inline]
	pub const fn size(&self) -> UVec3 {
		self.size
	}
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum LightGridCell {
	/// Cell is filled by geometry.
	Occluded,
	/// Cell is filled,
	Filled(SmallVec<[LightmapCellSample; 4]>),
}
impl BspValue for LightGridCell {
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		let style_count: u8 = reader.read().job("style count")?;
		if style_count == 255 {
			return Ok(Self::Occluded);
		}

		let mut samples = SmallVec::with_capacity(style_count as usize);
		for _ in 0..style_count {
			samples.push(reader.read().job("cell sample")?);
		}

		Ok(Self::Filled(samples))
	}

	fn bsp_struct_size(_ctx: &BspParseContext) -> usize {
		unimplemented!("LightGridCell is of variable size")
	}
}

#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct LightmapCellSample {
	pub style: LightmapStyle,
	pub color: [u8; 3],
}

#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ModelBrushesIdx {
	pub brush_count: u32,
	/// Total plane count, for validation.
	pub plane_count: u32,
}
impl TryFrom<ModelBrushesIdx> for usize {
	type Error = ();

	fn try_from(value: ModelBrushesIdx) -> Result<Self, Self::Error> {
		Ok(value.brush_count as usize)
	}
}

/// The output of reading the `BRUSHLIST` BSPX lump.
pub type BrushList = Vec<ModelBrushes>;

/// Per-model brush information stored in the `BRUSHLIST` BSPX lump.
#[derive(BspValue, Debug, Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ModelBrushes {
	/// Should be `1`.
	pub version: u32,
	pub model_idx: u32,
	pub brushes: BspVariableArray<ModelBrush, ModelBrushesIdx>,
}

#[derive(BspValue, Debug, Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ModelBrush {
	pub bound: BoundingBox,
	pub contents: ShortBspLeafContents,
	/// Non-axial faces only. It's on you to add axial planes via the bounding box.
	pub planes: BspVariableArray<ModelBrushPlane, u16>,
}

#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ModelBrushPlane {
	pub normal: Vec3,
	pub dist: f32,
}

/// For the `DECOUPLED_LM` BSPX lump. Stores lightmap sizes and axes separately to textures. This vector is per-surface.
pub type DecoupledLightmaps = Vec<DecoupledLightmap>;

#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct DecoupledLightmap {
	// TODO tmp fix until bevy_reflect 0.16, where i can make this U16Vec3 again
	pub size: [u16; 2],
	/// Offset into the lighting lump, or -1
	pub offset: i32,

	pub projection: PlanarTextureProjection,
}
