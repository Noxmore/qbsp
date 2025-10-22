//! Data definitions for visibility data, storing which parts of the map can be seen from any given location.

#[cfg(feature = "bevy_reflect")]
use bevy_reflect::Reflect;
use qbsp_macros::{BspValue, BspVariableValue};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::{
	data::util::{BspVariableArray, NoField},
	reader::{BspByteReader, BspParseContext, BspValue, BspVariableValue},
	BspParseResultDoingJobExt, BspResult,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum VisdataRef {
	/// Unlike all other IDTech2-derived formats, Quake 2 uses clusters, like Quake 3 and Source.
	/// See [`BspVisData`]. The `offsets` field can be used to get the actual
	/// offsets into the visdata array.
	Cluster(i16),
	/// A raw offset into the visdata.
	Offset(i32),
}

impl VisdataRef {
	/// If the inner value is `-1`, this leaf's visdata is invalid.
	pub fn is_empty(&self) -> bool {
		match *self {
			VisdataRef::Cluster(val) => val == -1,
			VisdataRef::Offset(val) => val == -1,
		}
	}
}

impl BspVariableValue for VisdataRef {
	type Bsp29 = i32;
	type Bsp2 = i32;
	type Bsp30 = i32;
	type Bsp38 = i16;
}

impl From<i16> for VisdataRef {
	fn from(value: i16) -> Self {
		Self::Cluster(value)
	}
}

impl From<i32> for VisdataRef {
	fn from(value: i32) -> Self {
		Self::Offset(value)
	}
}

/// Offsets for each of the potentially-visible set and potentially-audible set.
#[derive(BspValue, Debug, Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspClusterOffsets {
	/// Index into `BspVisData.visibility` where the potentially visible set starts.
	pub pvs: u32,
	/// Index into `BspVisData.visibility` where the potentially audible set starts.
	pub phs: u32,
}

#[derive(BspVariableValue, Default, Debug, Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[bsp2(NoField)]
#[bsp29(NoField)]
#[bsp30(NoField)]
#[bsp38(BspVariableArray<BspClusterOffsets, u32>)]
pub struct BspVisDataOffsets(pub Option<BspVariableArray<BspClusterOffsets, u32>>);

/// The visiblity lump - for pre-BSP38 files, this is just a flat byte vector. For BSP38,
/// this includes a header describing where in the bytes to find the PVS and PHS (see
/// `BspClusterOffsets`).
#[derive(Default, Debug, Clone)]
#[cfg_attr(feature = "bevy_reflect", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BspVisData {
	/// For BSP38, the offsets into the visibility bytes that the PVS and PHS can be found at.
	pub vis_data_offsets: BspVisDataOffsets,
	pub visdata: Vec<u8>,
}

impl BspVisData {
	pub fn pvs(&self, vis_ref: VisdataRef) -> Option<&[u8]> {
		match vis_ref {
			VisdataRef::Cluster(cluster) => {
				let offsets = self.vis_data_offsets.as_ref()?;

				let BspClusterOffsets { pvs, .. } = offsets.get(usize::try_from(cluster).ok()?)?;

				self.visdata.get(*pvs as usize..)
			}
			VisdataRef::Offset(vis_leaf) => {
				if self.vis_data_offsets.is_some() {
					return None;
				}

				self.visdata.get(usize::try_from(vis_leaf).ok()?..)
			}
		}
	}

	pub fn phs(&self, vis_ref: VisdataRef) -> Option<&[u8]> {
		match vis_ref {
			VisdataRef::Cluster(cluster) => {
				let offsets = self.vis_data_offsets.as_ref()?;

				let BspClusterOffsets { phs, .. } = offsets.get(usize::try_from(cluster).ok()?)?;

				self.visdata.get(*phs as usize..)
			}
			VisdataRef::Offset(vis_leaf) => {
				if self.vis_data_offsets.is_some() {
					return None;
				}

				self.visdata.get(usize::try_from(vis_leaf).ok()?..)
			}
		}
	}
}

impl BspValue for BspVisData {
	fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
		let vis_data_offsets: BspVisDataOffsets = reader.read()?;

		Ok(Self {
			vis_data_offsets,
			visdata: reader.read_rest().to_vec(),
		})
	}

	fn bsp_struct_size(ctx: &BspParseContext) -> usize {
		BspVisDataOffsets::bsp_struct_size(ctx)
	}
}
