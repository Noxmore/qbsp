//! Utilities for BSP data that don't warrant their own modules.

use std::{marker::PhantomData, str::FromStr};

#[cfg(feature = "bevy_reflect")]
use bevy_reflect::Reflect;
use qbsp_macros::BspVariableValue;
#[cfg(feature = "serde")]
use serde::{de, Deserialize, Deserializer, Serialize, Serializer};

use crate::{
	reader::{BspByteReader, BspParseContext, BspValue, BspVariableValue},
	BspParseError, BspParseResultDoingJobExt, BspResult,
};

/// [`BspValue`] implementor that always returns `Ok(NoField)`, and has 0 size, effectively a NO-OP.
///
/// This is used for [`BspVariableValue`] implementors, where certain BSP formats don't have the information at all.
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

/// Internal module to prevent the required imports from polluting the `util` module.
mod no_field_impls {
	use crate::{
		data::{
			models::PerSizeHulls,
			nodes::{BspAmbience, BspLeafBrushes},
			texture::{BspTexQ2Info, Palette},
		},
		LumpEntry,
	};

	use super::NoField;

	/// Workaround for `impl From<T> for Option<T>` being in the stdlib
	///
	/// Noxmore: I really do not like this, but I can't think of another solution right now.
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
	impl_from_no_field_for_option!(BspTexQ2Info);
	impl_from_no_field_for_option!(BspAmbience);
	impl_from_no_field_for_option!(BspLeafBrushes);
	impl_from_no_field_for_option!(LumpEntry);
	impl_from_no_field_for_option!(Palette);
	impl_from_no_field_for_option!(PerSizeHulls);
}

// We can do _some_ glob impls.
impl<T, N> From<NoField> for Option<BspVariableArray<T, N>> {
	fn from(_: NoField) -> Self {
		None
	}
}
impl<T, const N: usize> From<NoField> for Option<[T; N]> {
	fn from(_: NoField) -> Self {
		None
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

#[cfg(test)]
mod fixed_str_tests {
	use std::str::FromStr;

	use super::FixedStr;

	#[test]
	fn fixed_str_from_str() {
		assert!(FixedStr::<8>::from_str("12345678").is_ok());
		assert!(FixedStr::<8>::from_str("123456789").is_err());
	}

	#[test]
	fn fixed_str_from_null_garbage() {
		assert!(FixedStr::<8>::new([b'+', b's', b'k', b'y', 0, b'+', b'v', 189]).is_ok());
	}
}
