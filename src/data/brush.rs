use qbsp_macros::BspValue;

#[cfg(feature = "bevy_reflect")]
use bevy_reflect::Reflect;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::data::{nodes::BspLeafContentFlags, util::UBspValue};

/// A brush stored inside a Quake 2 BSP.
#[derive(BspValue, Debug, Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspBrush {
	pub first_side: u32,
	pub num_sides: u32,
	pub contents: BspLeafContentFlags,
}

/// The side of a brush stored inside a Quake 2 BSP.
#[derive(BspValue, Debug, Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspBrushSide {
	pub plane_idx: UBspValue,
	pub tex_info_idx: UBspValue,
}
