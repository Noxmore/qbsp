//! BSP file data parsing.

pub mod bsp3x;
pub mod bspx;
pub mod idtech2;
pub mod q1bsp;

pub use q1bsp as bsp;

use std::{marker::PhantomData, str::FromStr};

use crate::{bsp3x::BspTexExtraInfo, idtech2::BspNodeRef, *};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct NoField;

impl BspValue for NoField {
	fn bsp_parse(_reader: &mut BspByteReader) -> BspResult<Self> {
		Ok(NoField)
	}

	fn bsp_struct_size(_ctx: &BspParseContext) -> usize {
		0
	}
}

/// Workaround for `impl From<T> for Option<T>` being in the stdlib
macro_rules! impl_from_no_field_for_option {
	($opt_inner:ty) => {
		impl From<NoField> for Option<$opt_inner> {
			fn from(_: NoField) -> Self {
				None
			}
		}
	};
}

impl_from_no_field_for_option!(u32);
impl_from_no_field_for_option!(u16);
impl_from_no_field_for_option!(BspTexExtraInfo);
impl_from_no_field_for_option!(BspAmbience);
impl_from_no_field_for_option!(BspLeafBrushes);
impl_from_no_field_for_option!(LumpEntry);
impl_from_no_field_for_option!([BspNodeRef; 3]);

/// Like a [`Cursor`](std::io::Cursor), but i don't have to constantly juggle buffers.
pub struct BspByteReader<'a> {
	pub ctx: &'a BspParseContext,
	bytes: &'a [u8],
	pos: usize,
}
impl<'a> BspByteReader<'a> {
	#[inline]
	pub fn new(bytes: &'a [u8], ctx: &'a BspParseContext) -> Self {
		Self { ctx, bytes, pos: 0 }
	}

	#[inline]
	pub fn read<T: BspValue>(&mut self) -> BspResult<T> {
		T::bsp_parse(self)
	}

	pub fn read_bytes(&mut self, count: usize) -> BspResult<&[u8]> {
		let (from, to) = (self.pos, self.pos + count);
		if to > self.bytes.len() {
			return Err(BspParseError::BufferOutOfBounds {
				from,
				to,
				size: self.bytes.len(),
			});
		}
		let bytes = &self.bytes[from..to];
		self.pos += count;
		Ok(bytes)
	}

	#[inline]
	pub fn with_pos(&self, pos: usize) -> Self {
		Self {
			ctx: self.ctx,
			bytes: self.bytes,
			pos,
		}
	}

	/// Returns `true` if `pos` is less than `bytes.len()`.
	#[inline]
	pub fn in_bounds(&self) -> bool {
		self.pos < self.bytes.len()
	}
}

/// Defines how a type should be read from a BSP file.
pub trait BspValue: Sized {
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self>;
	fn bsp_struct_size(ctx: &BspParseContext) -> usize;
}
macro_rules! impl_bsp_parse_primitive {
	($ty:ty) => {
		impl BspValue for $ty {
			#[inline]
			fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
				Ok(<$ty>::from_le_bytes(reader.read_bytes(size_of::<$ty>())?.try_into().unwrap()))
			}
			#[inline]
			fn bsp_struct_size(_ctx: &BspParseContext) -> usize {
				size_of::<$ty>()
			}
		}
	};
}
macro_rules! impl_bsp_parse_vector {
	($ty:ty : [$element:ty; $count:expr]) => {
		impl BspValue for $ty {
			fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
				Ok(<$ty>::from_array(reader.read::<[$element; $count]>()?))
			}
			fn bsp_struct_size(_ctx: &BspParseContext) -> usize {
				size_of::<$element>() * $count
			}
		}
	};
}

impl_bsp_parse_primitive!(u16);
impl_bsp_parse_primitive!(u32);

impl_bsp_parse_primitive!(i16);
impl_bsp_parse_primitive!(i32);

impl_bsp_parse_primitive!(f32);

impl BspValue for u8 {
	#[inline]
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		reader.read_bytes(1).map(|bytes| bytes[0])
	}
	#[inline]
	fn bsp_struct_size(_ctx: &BspParseContext) -> usize {
		1
	}
}

impl_bsp_parse_vector!(Vec3: [f32; 3]);
impl_bsp_parse_vector!(IVec3: [i32; 3]);
impl_bsp_parse_vector!(UVec3: [u32; 3]);
impl_bsp_parse_vector!(U16Vec3: [u16; 3]);

impl_bsp_parse_vector!(U16Vec2: [u16; 2]);

// We'd have to change this if we want to impl BspRead for u8
impl<T: BspValue + std::fmt::Debug, const N: usize> BspValue for [T; N] {
	#[inline]
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		// Look ma, no heap allocations!
		let mut out = [(); N].map(|_| mem::MaybeUninit::uninit());
		for out in out.iter_mut() {
			out.write(reader.read()?);
		}
		Ok(out.map(|v| unsafe { v.assume_init() }))
	}
	#[inline]
	fn bsp_struct_size(ctx: &BspParseContext) -> usize {
		T::bsp_struct_size(ctx) * N
	}
}

/// A value in a BSP file where its size differs between formats. Should be implemented for the "output type", that
/// will be the final type exposed in the `BspData`.
pub trait BspVariableValue: Sized {
	/// The type of this field for BSP v29 (Quake 1). See [Quake Wiki](https://quakewiki.org/wiki/Quake_BSP_Format).
	type Bsp29: BspValue + Into<Self>;
	/// The type of this field for BSP2 (BSP v29 with increased limits, originally for RemakeQuake). See [Quake Wiki](https://quakewiki.org/wiki/BSP2).
	type Bsp2: BspValue + Into<Self>;
	/// The type of this field for BSP30 (Goldsrc). See [Valve Developer Community](https://developer.valvesoftware.com/wiki/BSP_(GoldSrc)).
	type Bsp30: BspValue + Into<Self>;
	/// The type of this field for BSP38 (Quake 2). See [Flipcode](https://www.flipcode.com/archives/Quake_2_BSP_File_Format.shtml).
	type Bsp38: BspValue + Into<Self>;
}

impl<T> BspValue for T
where
	T: BspVariableValue,
{
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		match reader.ctx.format {
			BspFormat::BSP2 => T::Bsp2::bsp_parse(reader).map(Into::into),
			BspFormat::BSP29 => T::Bsp29::bsp_parse(reader).map(Into::into),
			BspFormat::BSP30 => T::Bsp30::bsp_parse(reader).map(Into::into),
			BspFormat::BSP38 => T::Bsp38::bsp_parse(reader).map(Into::into),
		}
	}

	fn bsp_struct_size(ctx: &BspParseContext) -> usize {
		match ctx.format {
			BspFormat::BSP2 => T::Bsp2::bsp_struct_size(ctx),
			BspFormat::BSP29 => T::Bsp29::bsp_struct_size(ctx),
			BspFormat::BSP30 => T::Bsp30::bsp_struct_size(ctx),
			BspFormat::BSP38 => T::Bsp38::bsp_struct_size(ctx),
		}
	}
}

/// An unsigned variable integer parsed from a BSP. u32 when parsing BSP2, u16 when parsing BSP29.
///
/// In almost all cases, BSP38 and BSP30 do not have increased limits, and so they still use 16-bit indices.
#[derive(BspVariableValue, Hash, Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[bsp29(u16)]
#[bsp2(u32)]
#[bsp30(u16)]
#[bsp38(u16)]
pub struct UBspValue(pub u32);

/// A signed variable integer parsed from a BSP. i32 when parsing BSP2, i16 when parsing BSP29.
///
/// In almost all cases, BSP38 and BSP30 do not have increased limits, and so they still use 16-bit indices.
#[derive(BspVariableValue, Hash, Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[bsp29(i16)]
#[bsp2(i32)]
#[bsp30(i16)]
#[bsp38(i16)]
pub struct IBspValue(pub i32);

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

/// A variable length array in the format of `N` (count) then `[T; N]` (elements).
#[derive(Debug, Clone, Default, derive_more::Deref, derive_more::DerefMut, derive_more::IntoIterator)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspVariableArray<T, N> {
	#[deref]
	#[deref_mut]
	#[into_iterator(owned, ref, ref_mut)]
	pub inner: Vec<T>,
	#[cfg_attr(feature = "bevy_reflect", reflect(ignore))]
	_marker: PhantomData<N>,
}
impl<T: BspValue, N: BspValue + TryInto<usize, Error: std::fmt::Debug>> BspValue for BspVariableArray<T, N> {
	#[track_caller] // Just in case this unwrap fails, almost certainly not needed.
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		let count: usize = reader.read::<N>().job("count")?.try_into().unwrap();
		let mut inner = Vec::with_capacity(count);

		for _ in 0..count {
			inner.push(reader.read().job(std::any::type_name::<T>())?);
		}

		Ok(Self { inner, _marker: PhantomData })
	}
	fn bsp_struct_size(_ctx: &BspParseContext) -> usize {
		unimplemented!("{} is of variable size", std::any::type_name::<Self>());
	}
}

/// Fixed-sized UTF-8 string. Zero-padded.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
pub struct FixedStr<const N: usize> {
	data: [u8; N],
}
impl<const N: usize> BspValue for FixedStr<N> {
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		let data = reader.read()?;
		Self::new(data).map_err(BspParseError::map_utf8_error(&data))
	}
	#[inline]
	fn bsp_struct_size(_ctx: &BspParseContext) -> usize {
		N
	}
}
impl<const N: usize> FixedStr<N> {
	pub fn new(mut data: [u8; N]) -> Result<Self, std::str::Utf8Error> {
		// Clear any garbage after the '\0' terminator.
		if let Some(index) = data.iter().position(|b| *b == 0) {
			data[index..].fill(0);
		}
		std::str::from_utf8(&data)?;
		Ok(Self { data })
	}

	pub fn as_str(&self) -> &str {
		// SAFETY: This is checked when a FixedStr is created
		unsafe { std::str::from_utf8_unchecked(&self.data) }.trim_end_matches('\0')
	}
}
impl<const N: usize> std::fmt::Debug for FixedStr<N> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		self.as_str().fmt(f)
	}
}
impl<const N: usize> std::fmt::Display for FixedStr<N> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		self.as_str().fmt(f)
	}
}
impl<const N: usize> FromStr for FixedStr<N> {
	type Err = ();

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		if s.len() > N {
			return Err(());
		}
		let mut data = [0; N];

		#[allow(clippy::manual_memcpy)]
		for i in 0..s.len() {
			data[i] = s.as_bytes()[i];
		}

		Ok(Self { data })
	}
}
#[cfg(feature = "serde")]
impl<const N: usize> Serialize for FixedStr<N> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		serializer.serialize_str(self.as_str())
	}
}
#[cfg(feature = "serde")]
impl<'de, const N: usize> Deserialize<'de> for FixedStr<N> {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		struct DataVisitor<const N: usize>;
		impl<const N: usize> de::Visitor<'_> for DataVisitor<N> {
			type Value = [u8; N];
			fn expecting(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
				write!(fmt, "Fixed string of len {N}")
			}

			fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
				v.as_bytes()
					.try_into()
					.map_err(|_| E::custom(format_args!("string was of len {}, when max len is {N}", v.len())))
			}
		}

		// `visit_str` ensures the string is valid
		Ok(Self {
			data: deserializer.deserialize_seq(DataVisitor::<N>)?,
		})
	}
}

/// If loading a BSP2, parses a float-based bounding box, else if BSP29, parses a short-based bounding box.
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

/// Annoyingly, Quake 2 is the only format that has a differing number of root hulls. This is because
/// it handles collision differently to all other IDTech2-based engines.
///
/// Note that in the Quake 1 engine, only the first 3 hulls are used. As this is a restriction of the
/// engine, not Quake 1's QuakeC game code, this means that BSP29 will have only 3 valid hulls in
/// almost all cases.
///
/// [The specification](https://www.gamers.org/dEngine/quake/spec/quake-spec34/qkspec_4.htm#BLE) notes
/// that the 4th hull is "usually zero" in Quake 1.
/// [The Valve Developer Wiki](https://developer.valvesoftware.com/wiki/BSP_(GoldSrc)) says "hull
/// 3 is unused in Quake" (meaning the 4th hull, as the 1st hull is hull 0).
#[derive(BspValue, Clone, PartialEq, Debug, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Hulls {
	pub bsp_root: BspNodeRef,
	/// If positive, index of clip node. If -2, the Front part is inside the model. If -1, the Front part is outside the model.
	pub clip_nodes: Option<[BspNodeRef; 3]>,
}

impl BspVariableValue for Option<[BspNodeRef; 3]> {
	type Bsp29 = [BspNodeRef; 3];
	type Bsp2 = [BspNodeRef; 3];
	type Bsp30 = [BspNodeRef; 3];
	type Bsp38 = NoField;
}

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

/// A `LumpEntry` that doesn't exist for BSP30 or BSP38.
#[derive(BspVariableValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[bsp2(LumpEntry)]
#[bsp29(LumpEntry)]
#[bsp30(NoField)]
#[bsp38(NoField)]
pub struct PreBsp30LumpEntry(Option<LumpEntry>);

/// A `LumpEntry` that only exists for BSP38.
#[derive(BspVariableValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[bsp2(NoField)]
#[bsp29(NoField)]
#[bsp30(NoField)]
#[bsp38(LumpEntry)]
pub struct Bsp38OnlyLumpEntry(Option<LumpEntry>);

/// A `LumpEntry` that only exists for BSP30 and BSP38.
#[derive(BspVariableValue, Debug, Clone, Copy)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[bsp2(NoField)]
#[bsp29(NoField)]
#[bsp30(LumpEntry)]
#[bsp38(LumpEntry)]
pub struct Bsp3xLumpEntry(Option<LumpEntry>);

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

	pub bspx: BspxDirectory,
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

			bspx: BspxDirectory::default(),
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

#[test]
fn fixed_str_from_str() {
	assert!(FixedStr::<8>::from_str("12345678").is_ok());
	assert!(FixedStr::<8>::from_str("123456789").is_err());
}

#[test]
fn fixed_str_from_null_garbage() {
	assert!(FixedStr::<8>::new([b'+', b's', b'k', b'y', 0, b'+', b'v', 189]).is_ok());
}
