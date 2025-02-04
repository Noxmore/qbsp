//! BSP file data parsing.

pub mod bsp;
pub mod bspx;

use std::{marker::PhantomData, str::FromStr};

use crate::*;

/// Like an [io::Cursor], but i don't have to constantly juggle buffers.
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

/// A value in a BSP file where its size differs between formats.
#[derive(Debug, Clone, Copy)]
pub enum BspVariableValue<BSP2, BSP29> {
	BSP2(BSP2),
	BSP29(BSP29),
}
impl<BSP2: BspValue, BSP29: BspValue> BspValue for BspVariableValue<BSP2, BSP29> {
	#[inline]
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		match reader.ctx.format {
			BspFormat::BSP2 => Ok(Self::BSP2(reader.read()?)),
			BspFormat::BSP29 => Ok(Self::BSP29(reader.read()?)),
		}
	}
	#[inline]
	fn bsp_struct_size(ctx: &BspParseContext) -> usize {
		match ctx.format {
			BspFormat::BSP2 => mem::size_of::<BSP2>(),
			BspFormat::BSP29 => mem::size_of::<BSP29>(),
		}
	}
}
impl<BSP2, BSP29: Into<BSP2>> BspVariableValue<BSP2, BSP29> {
	/// Converts the value stored within into the value expected by the BSP2 format. This is the function you want to get the value out of this.
	#[inline]
	pub fn bsp2(self) -> BSP2 {
		match self {
			Self::BSP2(v) => v,
			Self::BSP29(v) => v.into(),
		}
	}
}

/// An unsigned variable integer parsed from a BSP. u32 when parsing BSP2, u16 when parsing BSP29.
pub type UBspValue = BspVariableValue<u32, u16>;
/// A signed variable integer parsed from a BSP. i32 when parsing BSP2, i16 when parsing BSP29.
pub type IBspValue = BspVariableValue<i32, i16>;

/// A variable length array in the format of `N` (count) then `[T; N]` (elements).
#[derive(Debug, Clone, Default, derive_more::Deref, derive_more::DerefMut, derive_more::IntoIterator)]
pub struct BspVariableArray<T, N> {
	#[deref]
	#[deref_mut]
	#[into_iterator(owned, ref, ref_mut)]
	pub inner: Vec<T>,
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
	pub fn new(data: [u8; N]) -> Result<Self, std::str::Utf8Error> {
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

#[derive(BspValue, Debug, Clone, Copy)]
pub struct BoundingBox {
	pub min: Vec3,
	pub max: Vec3,
}
#[derive(BspValue, Debug, Clone, Copy)]
pub struct ShortBoundingBox {
	pub min: U16Vec3,
	pub max: U16Vec3,
}
impl From<ShortBoundingBox> for BoundingBox {
	fn from(value: ShortBoundingBox) -> Self {
		Self {
			min: value.min.as_vec3(),
			max: value.max.as_vec3(),
		}
	}
}

/// If loading a BSP2, parses a float-based bounding box, else if BSP29, parses a short-based bounding box.
pub type VariableBoundingBox = BspVariableValue<BoundingBox, ShortBoundingBox>;

/// Points to the chunk of data in the file a lump resides in.
#[derive(BspValue, Debug, Clone, Copy)]
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

/// Contains the list of lump entries
#[derive(Debug, Clone)]
pub struct LumpDirectory {
	pub entities: LumpEntry,
	pub planes: LumpEntry,
	pub textures: LumpEntry,
	pub vertices: LumpEntry,
	pub visibility: LumpEntry,
	pub nodes: LumpEntry,
	pub tex_info: LumpEntry,
	pub faces: LumpEntry,
	pub lighting: LumpEntry,
	pub clip_nodes: LumpEntry,
	pub leaves: LumpEntry,
	pub mark_surfaces: LumpEntry,
	pub edges: LumpEntry,
	pub surf_edges: LumpEntry,
	pub models: LumpEntry,

	pub bspx: BspxDirectory,
}
impl LumpDirectory {
	pub fn bsp_entries(&self) -> [LumpEntry; 15] {
		[
			self.entities,
			self.planes,
			self.textures,
			self.vertices,
			self.visibility,
			self.nodes,
			self.tex_info,
			self.faces,
			self.lighting,
			self.clip_nodes,
			self.leaves,
			self.mark_surfaces,
			self.edges,
			self.surf_edges,
			self.models,
		]
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
			edges: reader.read().job("Reading edges entry")?,
			surf_edges: reader.read().job("Reading surf_edges entry")?,
			models: reader.read().job("Reading models entry")?,

			bspx: BspxDirectory::default(),
		};

		// TODO why subtract 4??
		let bspx_offset = dir.bsp_entries().into_iter().map(|entry| entry.offset + entry.len).max().unwrap() - 4;
		match reader.with_pos(bspx_offset as usize).read() {
			Ok(bspx_dir) => dir.bspx = bspx_dir,
			Err(BspParseError::NoBspxDirectory) => {}
			Err(err) => return Err(BspParseError::DoingJob("Reading BSPX directory".to_string(), Box::new(err))),
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
