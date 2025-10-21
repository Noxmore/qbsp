use std::collections::HashMap;

use glam::{uvec2, UVec2, Vec2};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use smallvec::{smallvec, SmallVec};
use thiserror::Error;

mod packer;

pub use packer::{DefaultLightmapPacker, LightmapPacker, LightmapPackerFaceView, PerSlotLightmapPacker, PerStyleLightmapPacker};

use crate::{
	data::{lighting::LightmapStyle, texture::BspTexFlags},
	mesh::FaceExtents,
	BspData, BspParseError,
};

#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ComputeLightmapSettings {
	/// The of a pixel is no lightmaps are stored there.
	pub default_color: [u8; 3],
	/// A single pixel of a lightmap atlas is reserved for faces which don't have a lightmap or `special` flag, this is the color of that pixel.
	pub no_lighting_color: [u8; 3],
	/// A single pixel of a lightmap atlas is reserved for faces which don't have a lightmap, but do have the `special` flag, this is the color of that pixel.
	pub special_lighting_color: [u8; 3],
	pub max_width: u32,
	pub max_height: u32,
	/// Number of pixels to pad around each island, stretches the sides of textures.
	pub extrusion: u32,
}
impl Default for ComputeLightmapSettings {
	fn default() -> Self {
		Self {
			default_color: [0; 3],
			no_lighting_color: [0; 3],
			special_lighting_color: [255; 3],
			max_width: 2048,
			max_height: u32::MAX,
			extrusion: 0,
		}
	}
}

#[derive(Error, Debug, Clone)]
pub enum ComputeLightmapAtlasError {
	#[error(
		"Failed to pack lightmap of size {lightmap_size}, {images_packed} lightmaps have already been packed. Max atlas size: {max_lightmap_size}"
	)]
	PackFailure {
		lightmap_size: UVec2,
		images_packed: usize,
		max_lightmap_size: UVec2,
	},
	#[error("No lightmaps")]
	NoLightmaps,
	#[error("DECOUPLED_LM BSPX lump is present, but failed to parse: {0}")]
	InvalidDecoupledLM(BspParseError),
}

struct ReservedLightmapPixel {
	position: Option<UVec2>,
	color: [u8; 3],
}
impl ReservedLightmapPixel {
	pub fn new(color: [u8; 3]) -> Self {
		Self { position: None, color }
	}

	pub fn get_uvs<P: LightmapPacker>(
		&mut self,
		lightmap_packer: &mut P,
		view: LightmapPackerFaceView,
	) -> Result<FaceUvs, ComputeLightmapAtlasError> {
		let position = match self.position {
			Some(v) => v,
			None => {
				// TODO: Is this handled by `texture_packer`?
				let rect = lightmap_packer.pack(
					view,
					P::create_single_color_input(UVec2::ONE + lightmap_packer.settings().extrusion * 2, self.color),
				)?;
				self.position = Some(rect.min);
				rect.min
			}
		};

		Ok(smallvec![position.as_vec2() + Vec2::splat(0.5); view.face.num_edges.0 as usize])
	}
}

impl BspData {
	/// Packs every face's lightmap together onto a single atlas for GPU rendering.
	pub fn compute_lightmap_atlas<P: LightmapPacker>(&self, mut packer: P) -> Result<LightmapAtlasOutput<P>, ComputeLightmapAtlasError> {
		let Some(lighting) = &self.lighting else { return Err(ComputeLightmapAtlasError::NoLightmaps) };
		let mut decoupled_lm = match self.bspx.parse_decoupled_lm(&self.parse_ctx) {
			Some(x) => Some(x.map_err(ComputeLightmapAtlasError::InvalidDecoupledLM)?),
			None => None,
		};

		// This is done in FTE quake's source code, each with a comment saying "sigh" after, not sure why.
		if let Some(lm_infos) = &mut decoupled_lm {
			for lm_info in lm_infos {
				lm_info.projection.u_offset += 0.5;
				lm_info.projection.v_offset += 0.5;
			}
		}

		let settings = packer.settings();

		let mut lightmap_uvs: HashMap<u32, FaceUvs> = HashMap::new();

		let mut empty_reserved_pixel = ReservedLightmapPixel::new(settings.no_lighting_color);
		let mut special_reserved_pixel = ReservedLightmapPixel::new(settings.special_lighting_color);

		for (face_idx, face) in self.faces.iter().enumerate() {
			let tex_info = &self.tex_info[face.texture_info_idx.0 as usize];

			let decoupled_lightmap = decoupled_lm.as_ref().map(|lm_infos| lm_infos[face_idx]);

			let lm_info = match &decoupled_lightmap {
				Some(lm_info) => {
					let uvs: FaceUvs = face.vertices(self).map(|pos| lm_info.projection.project(pos)).collect();
					let extents = FaceExtents::new_decoupled(uvs.iter().copied(), lm_info);

					LightmapInfo {
						uvs,
						extents,
						lightmap_offset: lm_info.offset,
					}
				}
				None => {
					let uvs: FaceUvs = face.vertices(self).map(|pos| tex_info.projection.project(pos)).collect();
					let extents = FaceExtents::new(uvs.iter().copied());

					LightmapInfo {
						uvs,
						extents,
						lightmap_offset: face.lightmap_offset.pixels,
					}
				}
			};

			let view = LightmapPackerFaceView {
				lm_info: &lm_info,

				bsp: self,

				face_idx,
				face,
				tex_info,
				lighting,
			};

			if lm_info.lightmap_offset.is_negative() || lm_info.extents.lightmap_size() == UVec2::ZERO {
				lightmap_uvs.insert(
					face_idx as u32,
					if tex_info.flags.texture_flags.unwrap_or_default() == BspTexFlags::Normal {
						// TODO: For BSP3x (Goldsrc/Quake 2), we should look at the texture name
						// to figure out the texture flags.
						empty_reserved_pixel.get_uvs(&mut packer, view)?
					} else {
						special_reserved_pixel.get_uvs(&mut packer, view)?
					},
				);
				continue;
			}

			let input = packer.read_from_face(view);

			let frame = packer.pack(view, input)?;

			lightmap_uvs.insert(
				face_idx as u32,
				lm_info
					.extents
					.compute_lightmap_uvs(lm_info.uvs, (frame.min + settings.extrusion).as_vec2())
					.collect(),
			);
		}

		let atlas = packer.export();

		// Normalize lightmap UVs from texture space
		for uvs in lightmap_uvs.values_mut() {
			for uv in uvs {
				*uv /= atlas.size().as_vec2();
			}
		}

		Ok(LightmapAtlasOutput {
			uvs: lightmap_uvs,
			data: atlas,
		})
	}
}

/// Computed information about the specifics of how a lightmap applies to a face.
#[derive(Debug, Clone)]
pub struct LightmapInfo {
	/// The vertices of the face projected onto it's texture or decoupled lightmap.
	pub uvs: FaceUvs,
	pub extents: FaceExtents,
	/// The offset into the lightmap lump in bytes to read the lightmap data or -1. Will need to be multiplied by 3 for colored lighting.
	pub lightmap_offset: i32,
}
impl LightmapInfo {
	/// Computes the index into [`BspLighting`] for the specific face specified. Assumes [`lightmap_offset`](Self::lightmap_offset) is positive.
	#[inline]
	pub fn compute_lighting_index(&self, light_style_idx: usize, x: u32, y: u32) -> usize {
		self.lightmap_offset as usize
			+ (self.extents.lightmap_pixels() as usize * light_style_idx)
			+ (y * self.extents.lightmap_size().x + x) as usize
	}
}

/// Trait for a resulting lightmap atlas from a [`LightmapPacker`].
pub trait LightmapAtlas {
	fn size(&self) -> UVec2;
}

pub struct PerSlotLightmapData {
	pub slots: [image::RgbImage; 4],
	pub styles: image::RgbaImage,
}
impl LightmapAtlas for PerSlotLightmapData {
	fn size(&self) -> UVec2 {
		self.styles.dimensions().into()
	}
}

/// Container for mapping lightmap styles to lightmap images (either atlas' or standalone) to later composite together to achieve animated lightmaps.
///
/// This is just a wrapper for a HashMap that ensures that all containing images are the same size.
#[derive(Debug, Clone)]
pub struct PerStyleLightmapData {
	size: UVec2,
	inner: HashMap<LightmapStyle, image::RgbImage>,
}
impl PerStyleLightmapData {
	#[inline]
	pub fn new(size: impl Into<UVec2>) -> Self {
		Self {
			size: size.into(),
			inner: HashMap::new(),
		}
	}

	#[inline]
	pub fn inner(&self) -> &HashMap<LightmapStyle, image::RgbImage> {
		&self.inner
	}

	#[inline]
	pub fn into_inner(self) -> HashMap<LightmapStyle, image::RgbImage> {
		self.inner
	}

	/// Modifies the internal map, checking to ensure all images are the same size after.
	pub fn modify_inner<O, F: FnOnce(&mut HashMap<LightmapStyle, image::RgbImage>) -> O>(
		&mut self,
		modifier: F,
	) -> Result<O, LightmapsInvalidSizeError> {
		let out = modifier(&mut self.inner);

		for (style, image) in &self.inner {
			let image_size = uvec2(image.width(), image.height());
			if self.size != image_size {
				return Err(LightmapsInvalidSizeError {
					style: *style,
					image_size,
					expected_size: self.size,
				});
			}
		}

		Ok(out)
	}

	/// Inserts a new image into the collection. Returns `Err` if the atlas' size doesn't match the collection's expected size.
	pub fn insert(&mut self, style: LightmapStyle, image: image::RgbImage) -> Result<Option<image::RgbImage>, LightmapsInvalidSizeError> {
		let image_size = uvec2(image.width(), image.height());
		if self.size != image_size {
			return Err(LightmapsInvalidSizeError {
				style,
				image_size,
				expected_size: self.size,
			});
		}

		Ok(self.inner.insert(style, image))
	}
}
impl LightmapAtlas for PerStyleLightmapData {
	fn size(&self) -> UVec2 {
		self.size
	}
}

#[derive(Debug, Error)]
#[error("Lightmap image of style {style} is size {image_size}, when the lightmap collection's expected size is {expected_size}")]
pub struct LightmapsInvalidSizeError {
	pub style: LightmapStyle,
	pub image_size: UVec2,
	pub expected_size: UVec2,
}

/// Contains a lightmap packers' output, and the UVs into said atlas' for each face.
pub struct LightmapAtlasOutput<P: LightmapPacker> {
	/// Map of face indexes to normalized UV coordinates into the atlas.
	pub uvs: LightmapUvMap,
	pub data: P::Output,
}

/// Maps face indexes to normalized UV coordinates into a lightmap atlas.
pub type LightmapUvMap = HashMap<u32, FaceUvs>;

/// The vast majority of faces have 5 or less vertices, so this is a pretty easy optimization.
pub type FaceUvs = SmallVec<[Vec2; 5]>;
