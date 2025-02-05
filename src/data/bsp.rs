//! BSP data definitions.

use super::*;
use crate::*;

#[derive(BspValue, Debug, Clone, Copy)]
pub struct BspEdge {
	/// The index to the first vertex this edge connects
	pub a: UBspValue,
	/// The index to the second vertex this edge connects
	pub b: UBspValue,
}

/// Byte that dictates how a specific BSP lightmap appears:
/// - 255 means there is no lightmap.
/// - 0 means normal, unanimated lightmap.
/// - 1 through 254 are programmer-defined animated styles, including togglable lights. In Quake though, 1 produces a fast pulsating light, and 2 produces a slow pulsating light, so those might be good defaults.
///
/// It is recommended to compare these values via the provided methods and constants of this type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

#[derive(BspValue, Debug, Clone, Copy)]
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
	/// Each element in this array is the style in which these lightmaps appear, see docs for [LightmapStyle].
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

			bsp.vertices[vert_idx.0.0 as usize]
		})
	}
}

#[derive(BspValue, Debug, Clone, Copy)]
pub struct BspTexInfo {
	pub u_axis: Vec3,
	pub u_offset: f32,

	pub v_axis: Vec3,
	pub v_offset: f32,

	pub texture_idx: u32,
	pub flags: BspTexFlags,
}

#[derive(BspValue, Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
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

#[derive(BspValue, Debug, Clone, Copy)]
pub struct BspModel {
	pub bound: BoundingBox,
	/// Origin of model, usually (0,0,0)
	pub origin: Vec3,

	pub head_bsp_node: BspNodeRef,
	pub first_clip_node: BspNodeRef,
	pub second_clip_node: BspNodeRef,
	/// [The specification](https://www.gamers.org/dEngine/quake/spec/quake-spec34/qkspec_4.htm#BLE) notes this as "usually zero".
	pub node_id3: BspNodeRef,

	/// Number of visleafs not including the solid leaf 0
	pub visleafs: u32,
	pub first_face: u32,
	pub num_faces: u32,
}

#[derive(BspValue, Debug, Clone, Copy)]
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

/// The texture lump is more complex than just a vector of the same type of item, so it needs its own function.
pub fn read_texture_lump(reader: &mut BspByteReader) -> BspResult<Vec<Option<BspTexture>>> {
	let mut textures = Vec::new();
	let num_mip_textures: u32 = reader.read()?;

	for _ in 0..num_mip_textures {
		let offset: i32 = reader.read()?;
		if offset.is_negative() {
			textures.push(None);
			continue;
		}
		textures.push(Some(BspTexture::bsp_parse(&mut reader.with_pos(offset as usize))?));
	}

	Ok(textures)
}

#[derive(BspValue, Debug, Clone, Copy)]
pub struct BspNode {
	/// Index of the [BspPlane] that splits the node.
	pub plane_idx: u32,

	pub front: VariableBspNodeRef,
	pub back: VariableBspNodeRef,

	/// Bounding box of the node and all its children.
	pub bound: VariableBoundingBox,
	/// Index of the first [BspFace] the node contains.
	pub face_idx: UBspValue,
	/// Number of faces this node contains.
	pub face_num: UBspValue,
}

/// A reference to a [BspNode]. Reads an `i32`, if positive it's an index of a node, if negative it's the index of a leaf.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BspNodeRef {
	Node(u32),
	Leaf(u32),
}
impl BspNodeRef {
	pub fn from_i32(value: i32) -> Self {
		if value.is_negative() {
			Self::Leaf((-value) as u32 - 1) // - 1 because you can't have -0
		} else {
			Self::Node(value as u32)
		}
	}
}
impl BspValue for BspNodeRef {
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		Ok(Self::from_i32(reader.read()?))
	}
	fn bsp_struct_size(_ctx: &BspParseContext) -> usize {
		size_of::<i32>()
	}
}

/// Wrapper over [BspNodeRef] that reads a [IBspValue] instead of an `i32`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VariableBspNodeRef(pub BspNodeRef);
impl BspValue for VariableBspNodeRef {
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		let value = IBspValue::bsp_parse(reader)?;

		Ok(Self(BspNodeRef::from_i32(value.0)))
	}
	fn bsp_struct_size(ctx: &BspParseContext) -> usize {
		IBspValue::bsp_struct_size(ctx)
	}
}

/// According to the [`gamers.org` specification](https://www.gamers.org/dEngine/quake/spec/quake-spec34/qkspec_4.htm#BL9):
/// 
/// > This structure is used to give a rough and somewhat exaggerated boundary to a given model. It does not separate models from each others, and is not used at all in the rendering of the levels
///
/// > Actually, the clip nodes are only used as a first and primitive collision checking method.
/// 
/// I think it is also used for shape casting.
#[derive(BspValue, Debug, Clone, Copy)]
pub struct BspClipNode {
	/// Index of the [BspPlane] that splits the clip node.
	pub plane_idx: u32,

	/// If positive, id of Front child node. If -2, the Front part is inside the model. If -1, the Front part is outside the model.
	pub front: IBspValue,
	/// If positive, id of Back child node. If -2, the Back part is inside the model. If -1, the Back part is outside the model.
	pub back: IBspValue,
}

#[derive(BspValue, Debug, Clone, Copy, Default, PartialEq, Eq)]
#[repr(i32)]
pub enum BspLeafContents {
	#[default]
	Empty = -1,
	Solid = -2,
	Water = -3,
	Slime = -4,
	Lava = -5,
	Sky = -6,
	// Origin = -7, removed at csg time
	// Clip = -8, changed to contents_solid
	Current0 = -9,
	Current90 = -10,
	Current180 = -11,
	Current270 = -12,
	CurrentUp = -13,
	CurrentDown = -14,
}

/// Wrapper for [`BspLeafContents`] that reads an [`i16`] rather than an [`i32`].
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct ShortBspLeafContents(pub BspLeafContents);
impl BspValue for ShortBspLeafContents {
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		let value = reader.read::<i16>()? as i32;

		BspLeafContents::bsp_parse(&mut BspByteReader::new(&value.to_le_bytes(), reader.ctx)).map(Self)
	}

	fn bsp_struct_size(_ctx: &BspParseContext) -> usize {
		size_of::<i16>()
	}
}

#[derive(BspValue, Debug, Clone, Copy)]
pub struct BspLeaf {
	pub contents: BspLeafContents,
	/// Beginning of visibility lists, or `-1`.
	pub vis_list: i32,

	pub bound: VariableBoundingBox,

	/// Index in the `mark_surfaces` list.
	pub face_idx: UBspValue,
	/// Number of elements in the `mark_surfaces` list.
	pub face_num: UBspValue,

	pub ambience_water: u8,
	pub ambience_sky: u8,
	pub ambience_slime: u8,
	pub ambience_lava: u8,
}

#[derive(Clone)]
pub struct BspTexture {
	pub header: BspTextureHeader,
	pub data: Option<Vec<u8>>,
	pub data_half: Option<Vec<u8>>,
	pub data_quarter: Option<Vec<u8>>,
	pub data_eighth: Option<Vec<u8>>,
}
impl BspValue for BspTexture {
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		let start_pos = reader.pos;
		let header: BspTextureHeader = reader.read()?;

		macro_rules! read_data {
			($offset:ident, $res:literal $(, $($res_operator:tt)+)?) => {
				if header.$offset == 0 {
					None
				} else {
					// From my testing, it seems the data starts at the end of the header, but this is just making sure
					reader.pos = start_pos + header.$offset as usize;
					
					Some(
						reader
							.read_bytes((header.width as usize $($($res_operator)+)?) * (header.height as usize $($($res_operator)+)?))
							.job(format!(concat!("Reading texture (", $res, "res) with header {:#?}"), header))?
							.to_vec(),
					)
				}
			};
		}
		
		Ok(Self {
			data: read_data!(offset_full, "full"),
			data_half: read_data!(offset_half, "half", / 2),
			data_quarter: read_data!(offset_quarter, "quarter", / 4),
			data_eighth: read_data!(offset_eighth, "eighth", / 8),
			header,
		})
	}
	fn bsp_struct_size(ctx: &BspParseContext) -> usize {
		BspTextureHeader::bsp_struct_size(ctx)
	}
}
impl std::fmt::Debug for BspTexture {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		self.header.fmt(f)
	}
}

#[derive(BspValue, Debug, Clone)]
pub struct BspTextureHeader {
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

/// Lighting data stored in a BSP file or a neighboring LIT file.
#[derive(Clone)]
pub enum BspLighting {
	White(Vec<u8>),
	Colored(Vec<[u8; 3]>),
}
impl BspLighting {
	/// Parses colored lighting from a LIT file.
	pub fn read_lit(data: &[u8], ctx: &BspParseContext, ignore_header: bool) -> BspResult<Self> {
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

		if data[reader.pos..].len() % 3 != 0 {
			return Err(BspParseError::ColorDataSizeNotDevisableBy3(data[reader.pos..].len()));
		}

		Ok(Self::Colored(data[reader.pos..].chunks_exact(3).map(|v| [v[0], v[1], v[2]]).collect()))
	}

	/// Convince function to get a location as an RGB color.
	#[inline]
	pub fn get(&self, i: usize) -> Option<[u8; 3]> {
		match self {
			Self::White(v) => {
				let v = *v.get(i)?;
				Some([v, v, v])
			}
			Self::Colored(v) => v.get(i).copied(),
		}
	}

	#[inline]
	pub fn len(&self) -> usize {
		match self {
			Self::White(vec) => vec.len(),
			Self::Colored(vec) => vec.len(),
		}
	}

	#[inline]
	pub fn is_empty(&self) -> bool {
		self.len() == 0
	}
}
impl std::fmt::Debug for BspLighting {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::White(vec) => write!(f, "White(...) (len: {})", vec.len()),
			Self::Colored(vec) => write!(f, "Colored(...) (len: {})", vec.len()),
		}
	}
}
