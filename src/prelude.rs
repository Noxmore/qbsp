//! Module that re-exports common/necessary imports to make the user experience that much nicer.

pub use crate::{BspData, BspParseError, BspParseInput, BspParseSettings, BspResult, QUAKE_PALETTE};

pub use qbsp_macros::{BspValue, BspVariableValue};

#[cfg(feature = "meshing")]
pub use crate::mesh::{
	lightmap::{ComputeLightmapSettings, LightmapUvMap, PerSlotLightmapPacker, PerStyleLightmapPacker},
	ExportedMesh,
};
