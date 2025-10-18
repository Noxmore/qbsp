use std::{ops::Range, slice::Iter};

use crate::*;

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
struct VisdataIterator<'a> {
	vis_leaves: Range<usize>,
	data_bytes: Iter<'a, u8>,
	/// If `Some`, we are currently iterating over the bits in a byte.
	cur_byte: Option<BitsIter>,
}

impl Iterator for VisdataIterator<'_> {
	type Item = usize;

	fn next(&mut self) -> Option<Self::Item> {
		loop {
			if let Some(is_visible) = dbg!(self.cur_byte.as_mut()).and_then(|byte| dbg!(byte.next())) {
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
pub(crate) fn potentially_visible_leaf_indices(vis_data: &[u8], num_leaves: usize) -> impl Iterator<Item = usize> + '_ {
	VisdataIterator {
		// Leaf index 0 is always invalid (used to represent leaves that are out-of-bounds), so Quake
		// doesn't even store a bit for it - counting always starts at 1
		vis_leaves: 1..num_leaves,
		data_bytes: vis_data.iter(),
		cur_byte: None,
	}
}

#[cfg(test)]
mod test {
	use crate::potentially_visible_leaf_indices;

	const TEST_VISDATA: &[u8] = &[0b1010_0111, 0, 5, 0b0000_0001, 0b0001_0000, 0, 12, 0b1000_0000];

	#[test]
	fn test_pvs_calculation() {
		assert_eq!(
			potentially_visible_leaf_indices(TEST_VISDATA, 256).collect::<Vec<_>>(),
			&[1, 2, 3, 6, 8, 49, 61, 168]
		);
	}
}
