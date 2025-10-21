//! Module containing the core of reading a binary BSP file and interpreting it into structured data.

#[cfg(feature = "bevy_reflect")]
use bevy_reflect::Reflect;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use std::mem;

use crate::{prelude::*, BspFormat, BspParseError};

use glam::{IVec3, U16Vec2, U16Vec3, UVec3, Vec3};

/// Like a [`Cursor`](std::io::Cursor), but i don't have to constantly juggle buffers.
#[derive(Clone)]
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

	fn rest(&self) -> &[u8] {
		&self.bytes[self.pos..]
	}

	#[inline]
	pub fn len(&self) -> usize {
		self.rest().len()
	}

	#[inline]
	pub fn is_empty(&self) -> bool {
		self.rest().is_empty()
	}

	#[inline]
	pub fn read<T: BspValue>(&mut self) -> BspResult<T> {
		T::bsp_parse(self)
	}

	/// Consume the rest of the reader, returning all remaining bytes.
	#[inline]
	pub fn read_rest(&mut self) -> &[u8] {
		let pos = self.pos;
		self.pos = self.bytes.len();

		&self.bytes[pos..]
	}

	#[inline]
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

	#[inline]
	pub fn with_context(&'a self, ctx: &'a BspParseContext) -> Self {
		Self {
			ctx,
			bytes: self.bytes,
			pos: self.pos,
		}
	}

	#[inline]
	pub fn pos(&self) -> usize {
		self.pos
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
	/// The type of this field for BSP30 (GoldSrc). See [Valve Developer Community](https://developer.valvesoftware.com/wiki/BSP_(GoldSrc)).
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

#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspParseContext {
	pub format: BspFormat,
}
