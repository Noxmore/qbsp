//! BSP data definitions.

use super::*;
use crate::{
	bsp3x::{BspSurfaceFlags, BspTexExtraInfo},
	idtech2::BspNodeRef,
	*,
};

use enumflags2::BitFlags;
pub use idtech2::{BspEdge, BspFace};

#[derive(BspValue, Debug, Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspTexInfo {
	pub projection: PlanarTextureProjection,

	pub texture_idx: BspVariableValue<Option<u32>, u32, u32, NoField, u32>,

	/// Goldsrc and Q2 have surface flags, Q1 and BSP2 have texture flags.
	pub flags: BspVariableValue<BspTexInfoFlags, BspTexFlags, BspTexFlags, BitFlags<BspSurfaceFlags>>,

	/// Extra info stored directly on the `TexInfo` - for Quake 2 (which does not have a lump for embedded textures).
	pub extra_info: BspVariableValue<Option<BspTexExtraInfo>, NoField, NoField, BspTexExtraInfo, NoField>,
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
			surface_flags: Default::default(),
		}
	}
}

impl From<BitFlags<BspSurfaceFlags>> for BspTexInfoFlags {
	fn from(value: BitFlags<BspSurfaceFlags>) -> Self {
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
	/// If this is `None`, then the name should be used to check the texture flags (for Goldsrc and Quake 2)
	texture_flags: Option<BspTexFlags>,
	/// For BSP2 and BSP29, this is always zero.
	surface_flags: BitFlags<BspSurfaceFlags>,
}

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

#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspModel {
	pub bound: BoundingBox,
	/// Origin of model, usually (0,0,0)
	pub origin: Vec3,

	/// If positive, index of clip node. If -2, the Front part is inside the model. If -1, the Front part is outside the model.
	pub root_hulls: VariableRootHulls,

	/// Number of visleafs, not including the out-of-bounds leaf 0. Quake uses a cluster system for visibility,
	/// and so this field is not included.
	pub visleafs: BspVariableValue<Option<u32>, u32, u32, NoField, u32>,

	pub first_face: u32,
	pub num_faces: u32,
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

/// The texture lump is more complex than just a vector of the same type of item, so it needs its own function.
pub fn read_mip_texture_lump(reader: &mut BspByteReader) -> BspResult<Vec<Option<BspMipTexture>>> {
	// Quake 2 doesn't have this lump at all.
	if reader.ctx.format == BspFormat::BSP38 {
		return Ok(vec![]);
	}

	let mut textures = Vec::new();
	let num_mip_textures: u32 = reader.read()?;

	for _ in 0..num_mip_textures {
		let offset: i32 = reader.read()?;
		if offset.is_negative() {
			textures.push(None);
			continue;
		}
		textures.push(Some(BspMipTexture::bsp_parse(&mut reader.with_pos(offset as usize))?));
	}

	Ok(textures)
}

#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspNode {
	/// Index of the [`BspPlane`] that splits the node.
	pub plane_idx: u32,

	pub front: VariableBspNodeRef,
	pub back: VariableBspNodeRef,

	/// Bounding box of the node and all its children.
	pub bound: VariableBoundingBox,
	/// Index of the first [`BspFace`] the node contains.
	pub face_idx: UBspValue,
	/// Number of faces this node contains.
	pub face_num: UBspValue,
}

/// Wrapper over [`BspNodeRef`] that reads a [`IBspValue`] instead of an `i32`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
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
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspClipNode {
	/// Index of the [`BspPlane`] that splits the clip node.
	pub plane_idx: u32,

	/// If positive, id of Front child node. If -2, the Front part is inside the model. If -1, the Front part is outside the model.
	pub front: IBspValue,
	/// If positive, id of Back child node. If -2, the Back part is inside the model. If -1, the Back part is outside the model.
	pub back: IBspValue,
}

#[derive(BspValue, Debug, Clone, Copy, Default, PartialEq, Eq)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
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
	Clip = -8,
	Current0 = -9,
	Current90 = -10,
	Current180 = -11,
	Current270 = -12,
	CurrentUp = -13,
	CurrentDown = -14,
}

/// Wrapper for [`BspLeafContents`] that reads an [`prim@i16`] rather than an [`prim@i32`].
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum VisdataRef {
	/// Unlike all other IDTech2-derived formats, Quake 2 uses clusters, like Quake 3 and Source.
	Cluster(i16),
	/// Most IDTech2 formats store visdata on a per-leaf basis.
	VisLeaves(i32),
}

impl From<i16> for VisdataRef {
	fn from(value: i16) -> Self {
		Self::Cluster(value)
	}
}

impl From<i32> for VisdataRef {
	fn from(value: i32) -> Self {
		Self::VisLeaves(value)
	}
}

#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspLeaf {
	pub contents: BspLeafContents,
	/// Beginning of visibility lists, or `-1`.
	pub vis_list: BspVariableValue<VisdataRef, i32, i32, i16, i32>,

	pub bound: VariableBoundingBox,

	/// Index in the `mark_surfaces` list.
	pub face_idx: UBspValue,
	/// Number of elements in the `mark_surfaces` list.
	pub face_num: UBspValue,

	pub ambience: BspVariableValue<Option<BspAmbience>, BspAmbience, BspAmbience, NoField, BspAmbience>,
	pub leaf_brushes: BspVariableValue<Option<BspLeafBrushes>, NoField, NoField, BspLeafBrushes, NoField>,
}

#[derive(BspValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspLeafBrushes {
	pub idx: u16,
	pub num: u16,
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

#[derive(Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspTextureData {
	pub full: Option<Vec<u8>>,
	pub half: Option<Vec<u8>>,
	pub quarter: Option<Vec<u8>>,
	pub eighth: Option<Vec<u8>>,
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

/// Embedded texture data. Goldsrc stores these in a separate lump.
#[derive(Debug, Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspMipTexture {
	pub header: BspTextureHeader,
	pub data: BspTextureData,
}

impl BspValue for BspMipTexture {
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
			data: BspTextureData {
				full: read_data!(offset_full, "full"),
				half: read_data!(offset_half, "half", / 2),
				quarter: read_data!(offset_quarter, "quarter", / 4),
				eighth: read_data!(offset_eighth, "eighth", / 8),
			},
			header,
		})
	}
	fn bsp_struct_size(ctx: &BspParseContext) -> usize {
		BspTextureHeader::bsp_struct_size(ctx)
	}
}

#[derive(BspValue, Debug, Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
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
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
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

		if !data[reader.pos..].len().is_multiple_of(3) {
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
