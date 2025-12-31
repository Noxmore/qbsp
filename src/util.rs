//! Various utilities that don't fit cleanly into other modules, but are too small to warrant their own.

use std::{ops::Range, slice::Iter};

use glam::Vec2;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

// Simple generic rectangle type.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Rect<T> {
	pub min: T,
	pub max: T,
}
impl<T> Rect<T> {
	pub const fn new(min: T, max: T) -> Self {
		Self { min, max }
	}
}

impl<T: std::ops::Sub<Output = T>> Rect<T> {
	#[inline]
	pub fn size(self) -> T {
		self.max - self.min
	}
}

impl Rect<Vec2> {
	pub const EMPTY: Self = Self {
		max: Vec2::NEG_INFINITY,
		min: Vec2::INFINITY,
	};

	/// Build a new rectangle formed of the union of this rectangle and a point.
	///
	/// The union is the smallest rectangle enclosing both the rectangle and the point. If the
	/// point is already inside the rectangle, this method returns a copy of the rectangle.
	#[inline]
	pub fn union_point(&self, other: Vec2) -> Self {
		Self {
			min: self.min.min(other),
			max: self.max.max(other),
		}
	}
}

/// Quake strings, like that used for the entity lump, aren't UTF-8. Instead, they're null-terminated ASCII with a second character set.
///
/// This is a convenience function that removes the extra data, converting it into valid UTF-8 in-place, then returns the resulting string.
pub fn quake_string_to_utf8_lossy(quake_str: &mut Vec<u8>) -> &str {
	for byte in quake_str.iter_mut() {
		if *byte > 127 {
			// Convert alternate glyph versions.
			*byte -= 128;
		}
		/* if *byte == 0 {
			// Truncate the null terminator. Probably doesn't need to run on every byte.
			quake_str.truncate(i);
			break;
		} */
	}

	// Remove null terminator.
	if quake_str.last().copied() == Some(0) {
		quake_str.pop();
	}

	// SAFETY: All characters are in the range of 0..=127.
	unsafe { str::from_utf8_unchecked(quake_str) }
}

/// Quake strings, like that used for the entity lump, aren't UTF-8. Instead, they're null-terminated ASCII with a second character set.
///
/// This function creates a new UTF-8 string that prefixes and suffixes the alternate character set.
pub fn quake_string_to_utf8(quake_str: &[u8], alt_prefix: &str, alt_suffix: &str) -> String {
	let mut s = String::with_capacity(quake_str.len());

	let mut was_alt = false;
	for mut byte in quake_str.iter().copied() {
		let is_alt = byte > 127;

		if is_alt && !was_alt {
			s.push_str(alt_prefix);
		} else if !is_alt && was_alt {
			s.push_str(alt_suffix);
		}

		if is_alt {
			byte -= 128;
		}
		// Handle null terminator.
		if byte == 0 {
			return s;
		}

		// SAFETY: If `char_number_range` passes, this will as well.
		s.push(unsafe { char::from_u32_unchecked(byte as u32) });

		was_alt = is_alt;
	}

	// Final suffix.
	if was_alt {
		s.push_str(alt_suffix);
	}

	s
}

/// Displays bytes in string form if they make up a string, else just displays them as bytes.
pub(crate) fn display_magic_number(bytes: &[u8]) -> String {
	std::str::from_utf8(bytes)
		.ok()
		.and_then(|s| s.is_ascii().then_some(s))
		.map(str::to_owned)
		.unwrap_or(format!("{bytes:?}"))
}

/// Iterate over the bits of a byte, from least- to most-significant.
#[derive(Debug)]
struct BitsIter {
	byte: u8,
	shift: u8,
}

impl BitsIter {
	/// Construct a new [`BitsIter`] to loop over the bits in the supplied byte.
	fn new(byte: u8) -> Self {
		Self { byte, shift: 0 }
	}
}

impl Iterator for BitsIter {
	type Item = bool;

	fn next(&mut self) -> Option<Self::Item> {
		if self.shift >= 8 {
			None
		} else {
			let out = (self.byte & 1 << self.shift) != 0;
			self.shift += 1;

			Some(out)
		}
	}
}

/// Loop over PVS indices, given run-length encoded visibility data. The logic is easier to
/// understand as a coroutine, this is the behavior we expect to implement:
///
/// ```ignore
/// std::iter::from_coroutine(
/// #[coroutine]
/// || {
///     let mut vis_leaf = 1;
///     let mut it = vis_data[vis_list..].iter();
///
///     while vis_leaf < num_leaves {
///         let byte = it.next().unwrap();
///         match *byte {
///             // a zero byte signals the start of an RLE sequence
///             0 => visleaf += 8 * *it.next().unwrap() as usize,
///
///             bits => {
///                 for shift in 0..8 {
///                     if bits & 1 << shift != 0 {
///                         yield vis_leaf;
///                     }
///
///                         vis_leaf += 1;
///                     }
///                 }
///             }
///         }
///     },
/// )
/// ```
///
/// Above code adapted from [Richter](https://github.com/cormac-obrien/richter/blob/506504d5f9f93dab807e61ba3cad1a27d6d5a707/src/common/bsp/mod.rs#L831-L866),
/// itself adapted from [code by David Etherton and Tony Myles](https://www.gamers.org/dEngine/quake/spec/quake-spec34/qkspec_4.htm#BL4).
pub struct VisdataIter<'a> {
	vis_leaves: Range<usize>,
	data_bytes: Iter<'a, u8>,
	/// If `Some`, we are currently iterating over the bits in a byte.
	cur_byte: Option<BitsIter>,
}

impl Iterator for VisdataIter<'_> {
	type Item = usize;

	fn next(&mut self) -> Option<Self::Item> {
		loop {
			if let Some(is_visible) = self.cur_byte.as_mut().and_then(|byte| byte.next()) {
				let value = self.vis_leaves.next();
				if is_visible {
					return value;
				}
			} else {
				let next_byte = *self.data_bytes.next()?;
				match next_byte {
					0 => {
						let advance_by = *self.data_bytes.next()?;
						self.vis_leaves.start += 8 * advance_by as usize;
					}
					bits => {
						self.cur_byte = Some(BitsIter::new(bits));
					}
				}
			}

			// Eventually we'll either run out of visleaves
			// or data bytes so this cannot loop infinitely.
		}
	}
}

/// Get an iterator of potentially-visible leaf indices (starting at 1), given a byte array of visdata.
/// The slice should be calculated using the `vis_list` field of `BspLeaf` - if this field is positive,
/// then it is the index to slice the `BspData`'s visdata from.
pub(crate) fn calculate_visdata_indices(vis_data: &[u8], num_leaves: usize) -> VisdataIter<'_> {
	VisdataIter {
		// Leaf index 0 is always invalid (used to represent leaves that are out-of-bounds), so Quake
		// doesn't even store a bit for it - counting always starts at 1
		vis_leaves: 1..num_leaves,
		data_bytes: vis_data.iter(),
		cur_byte: None,
	}
}

#[cfg(test)]
mod tests {
	use crate::util::{calculate_visdata_indices, quake_string_to_utf8};

	const TEST_VISDATA: &[u8] = &[0b1010_0111, 0, 5, 0b0000_0001, 0b0001_0000, 0, 12, 0b1000_0000];

	#[test]
	fn test_pvs_calculation() {
		assert_eq!(
			calculate_visdata_indices(TEST_VISDATA, 256).collect::<Vec<_>>(),
			&[1, 2, 3, 6, 8, 49, 61, 168]
		);
	}

	// Sanity check, as quake_string_to_utf8 functions assume this.
	#[test]
	fn char_number_range() {
		for i in 0..=127u8 {
			assert!(char::from_u32(i as u32).is_some());
		}
	}

	#[test]
	fn quake_string_alt_text_surrounding() {
		let quake_string = [b'h', b'i', b' ', b't' + 128, b'h' + 128, b'e', b'r' + 128, b'e' + 128];

		let out = quake_string_to_utf8(&quake_string, "<b>", "</b>");

		assert_eq!(out, "hi <b>th</b>e<b>re</b>");
	}
}
