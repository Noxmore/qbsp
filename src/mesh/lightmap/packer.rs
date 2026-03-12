//! Module for packing a bunch of smaller lightmaps read from a BSP file together into a single lightmap atlas.
//!
//! Currently, this integrates with the [`texture_packer`] crate as the packing backend.

use std::collections::HashMap;

use glam::{UVec2, uvec2};
use image::{DynamicImage, GenericImage, ImageBuffer, Pixel};
use texture_packer::{TexturePacker, TexturePackerConfig, texture::Texture};

use super::ComputeLightmapSettings;

use crate::{
	data::lighting::LightmapStyle,
	mesh::lightmap::{ComputeLightmapAtlasError, LightmapAtlas, LightmapInfo, PerSlotLightmapData, PerStyleLightmapData},
	util::Rect,
};

/// A trait for packing lightmaps into texture atlas'. Specifically using image::RgbImage.
pub trait LightmapPacker {
	type Input;
	type Output: LightmapAtlas;

	fn create_single_color_input(size: impl Into<UVec2>, color: [u8; 3]) -> Self::Input;
	fn read_from_face<P>(&self, view: LightmapPackerFaceView<P::Subpixel>) -> Self::Input
	where
		P: Pixel,
		DynamicImage: From<ImageBuffer<P, Vec<P::Subpixel>>>;

	fn settings(&self) -> ComputeLightmapSettings;

	fn pack<P>(&mut self, view: LightmapPackerFaceView<P::Subpixel>, images: Self::Input) -> Result<Rect<UVec2>, ComputeLightmapAtlasError>
	where
		P: Pixel,
		DynamicImage: From<ImageBuffer<P, Vec<P::Subpixel>>>;
	fn export(&self) -> Self::Output;
}

/// Information provided to read a lightmap from a face.
#[derive(Clone, Copy)]
pub struct LightmapPackerFaceView<'a, P> {
	pub lm_info: &'a LightmapInfo,

	pub face_idx: usize,
	pub lightmap_styles: [LightmapStyle; 4],
	pub lighting_buffer: &'a [P],
}

/// Currently, we use texture_packer to create atlas' and have to do
/// this dummy texture and pixel stuff to get around the fact the packer
/// doesn't expose its textures.
#[derive(Clone, Copy)]
struct DummyTexture {
	width: u32,
	height: u32,
}
#[derive(Clone, Copy)]
struct DummyPixel;
impl texture_packer::texture::Pixel for DummyPixel {
	fn is_transparent(&self) -> bool {
		false
	}
	fn outline() -> Self {
		Self
	}
	fn transparency() -> Option<Self> {
		None
	}
}
impl Texture for DummyTexture {
	type Pixel = DummyPixel;
	fn width(&self) -> u32 {
		self.width
	}
	fn height(&self) -> u32 {
		self.height
	}
	fn get(&self, x: u32, y: u32) -> Option<Self::Pixel> {
		(x < self.width && y < self.height).then_some(DummyPixel)
	}
	#[allow(unused)]
	fn set(&mut self, x: u32, y: u32, val: Self::Pixel) {}
}

pub struct DefaultLightmapPacker<LM> {
	packer: TexturePacker<'static, DummyTexture, u32>,
	settings: ComputeLightmapSettings,
	// I have to store images separately, since TexturePacker doesn't give me access
	images: Vec<(Rect<UVec2>, LM)>,
}
impl<LM> DefaultLightmapPacker<LM> {
	pub fn new(settings: ComputeLightmapSettings) -> Self {
		Self {
			packer: TexturePacker::new_skyline(TexturePackerConfig {
				max_width: settings.max_width,
				max_height: settings.max_height,
				// Sizes are consistent enough that i don't think we need to support rotation
				allow_rotation: false,
				force_max_dimensions: false,
				texture_extrusion: settings.extrusion,
				texture_padding: 0, // This defaults to 1
				..Default::default()
			}),
			settings,
			images: Vec::new(),
		}
	}

	fn allocate_and_push<P>(
		&mut self,
		view: LightmapPackerFaceView<P::Subpixel>,
		width: u32,
		height: u32,
		lm: LM,
	) -> Result<Rect<UVec2>, ComputeLightmapAtlasError>
	where
		P: Pixel,
	{
		self.packer
			.pack_own(view.face_idx as u32, DummyTexture { width, height })
			.map_err(|_| ComputeLightmapAtlasError::PackFailure {
				lightmap_size: uvec2(width, height),
				images_packed: self.images.len(),
				max_lightmap_size: uvec2(self.settings.max_width, self.settings.max_height),
			})?;

		Ok(self
			.packer
			.get_frame(&(view.face_idx as u32))
			.map(|frame| {
				let min = uvec2(frame.frame.x, frame.frame.y);
				let rect = Rect {
					min,
					max: min + uvec2(frame.frame.w, frame.frame.h),
				};

				self.images.push((rect, lm));

				rect
			})
			.unwrap())
	}

	fn total_size(&self) -> UVec2 {
		// TODO the packer and this give different sizes
		let mut size = UVec2::ZERO;

		for (frame, _) in &self.images {
			size = size.max(frame.max);
		}

		size
	}
}

pub type PerStyleLightmapPackerRgb = DefaultLightmapPacker<PerStyleLightmapData>;
pub type PerSlotLightmapPackerRgb = DefaultLightmapPacker<[(image::RgbImage, LightmapStyle); 4]>;

impl<Image> LightmapPacker for DefaultLightmapPacker<PerStyleLightmapData<Image>>
where
	Image: GenericImage,
	DynamicImage: Into<Image>,
{
	type Input = PerStyleLightmapData<Image>;
	type Output = PerStyleLightmapData<Image>;

	fn create_single_color_input(size: impl Into<UVec2>, color: [u8; 3]) -> Self::Input {
		let size = size.into();

		let image = DynamicImage::from(image::RgbImage::from_pixel(size.x, size.y, image::Rgb(color))).into();

		PerStyleLightmapData {
			size,
			inner: HashMap::from([(LightmapStyle::NORMAL, image)]),
		}
	}

	fn read_from_face<P>(&self, view: LightmapPackerFaceView<P::Subpixel>) -> Self::Input
	where
		P: Pixel,
		DynamicImage: From<ImageBuffer<P, Vec<P::Subpixel>>>,
	{
		if view.lightmap_styles[0] == LightmapStyle::NONE {
			Self::create_single_color_input(view.lm_info.extents.lightmap_size(), [0; 3])
		} else {
			let mut lightmaps = PerStyleLightmapData::<Image>::new(view.lm_info.extents.lightmap_size());
			for (i, style) in view.lightmap_styles.into_iter().enumerate() {
				if style == LightmapStyle::NONE {
					break;
				}

				let lm_size = view.lm_info.extents.lightmap_size();

				let start = view.lm_info.compute_lighting_index(i, 0, 0);
				let end = view
					.lm_info
					.compute_lighting_index(i, lm_size.x.saturating_sub(1), lm_size.y.saturating_sub(1));

				let lightmap_data = image::ImageBuffer::<P, _>::from_raw(lm_size.x, lm_size.y, view.lighting_buffer[start..=end].to_vec())
					.expect("Failed to create lightmap image");
				let rgb_image = DynamicImage::from(lightmap_data).into();

				lightmaps.insert(style, rgb_image).unwrap();
			}

			lightmaps
		}
	}

	fn settings(&self) -> ComputeLightmapSettings {
		self.settings
	}

	fn pack<P>(&mut self, view: LightmapPackerFaceView<P::Subpixel>, lightmaps: Self::Input) -> Result<Rect<UVec2>, ComputeLightmapAtlasError>
	where
		P: Pixel,
		DynamicImage: From<ImageBuffer<P, Vec<P::Subpixel>>>,
	{
		self.allocate_and_push::<P>(view, lightmaps.size().x, lightmaps.size().y, lightmaps)
	}

	fn export(&self) -> Self::Output {
		let size = self.total_size();

		let mut atlas = PerStyleLightmapData::<Image>::new(size);
		let [atlas_width, atlas_height] = atlas.size().to_array();

		for (frame, lightmap_images) in &self.images {
			let [frame_width, frame_height] = frame.size().to_array();

			for (light_style, lightmap_image) in lightmap_images.inner() {
				atlas
					.modify_inner(|map| {
						let dst_image = map.entry(*light_style).or_insert_with(|| {
							DynamicImage::from(image::RgbImage::from_pixel(
								atlas_width,
								atlas_height,
								image::Rgb(self.settings.default_color),
							))
							.into()
						});

						for x in 0..frame_width + self.settings.extrusion * 2 {
							let global_x = frame.min.x + x;
							if global_x >= size.x {
								continue;
							}

							for y in 0..frame_height + self.settings.extrusion * 2 {
								let global_y = frame.min.y + y;
								if global_y >= size.y {
									continue;
								}

								dst_image.put_pixel(
									global_x,
									global_y,
									lightmap_image.get_pixel(
										x.saturating_sub(self.settings.extrusion).min(frame_width - 1),
										y.saturating_sub(self.settings.extrusion).min(frame_height - 1),
									),
								);
							}
						}
					})
					.unwrap();
			}
		}

		atlas
	}
}

impl LightmapPacker for PerSlotLightmapPackerRgb {
	type Input = [(image::RgbImage, LightmapStyle); 4];
	type Output = PerSlotLightmapData;

	fn create_single_color_input(size: impl Into<UVec2>, color: [u8; 3]) -> Self::Input {
		let size: UVec2 = size.into();
		[(); 4].map(|_| (image::RgbImage::from_pixel(size.x, size.y, image::Rgb(color)), LightmapStyle::NORMAL))
	}

	fn read_from_face<P>(&self, view: LightmapPackerFaceView<P::Subpixel>) -> Self::Input
	where
		P: Pixel,
		DynamicImage: From<ImageBuffer<P, Vec<P::Subpixel>>>,
	{
		let mut i = 0;
		view.lightmap_styles.map(|style| {
			let UVec2 { x: width, y: height } = view.lm_info.extents.lightmap_size();

			let image = if style == LightmapStyle::NONE {
				image::RgbImage::from_pixel(width, height, self.settings.default_color.into())
			} else {
				let start = view.lm_info.compute_lighting_index(i, 0, 0);
				let end = view.lm_info.compute_lighting_index(i, width.saturating_sub(1), height.saturating_sub(1));

				let lightmap_data = image::ImageBuffer::<P, _>::from_raw(width, height, view.lighting_buffer[start..=end].to_vec())
					.expect("Failed to create lightmap image");
				DynamicImage::from(lightmap_data).into_rgb8()
			};

			i += 1;

			(image, style)
		})
	}

	fn settings(&self) -> ComputeLightmapSettings {
		self.settings
	}

	fn pack<P>(&mut self, view: LightmapPackerFaceView<P::Subpixel>, images: Self::Input) -> Result<Rect<UVec2>, ComputeLightmapAtlasError>
	where
		P: Pixel,
	{
		let size = images
			.iter()
			.map(|(image, _)| uvec2(image.width(), image.height()))
			.reduce(UVec2::max)
			.unwrap();

		self.allocate_and_push::<P>(view, size.x, size.y, images)
	}

	fn export(&self) -> Self::Output {
		let size = self.total_size();

		let mut slots = [(); 4].map(|_| None);
		let mut styles = image::RgbaImage::from_pixel(size.x, size.y, image::Rgba([255; 4]));

		for (frame, src_slots) in &self.images {
			let [frame_width, frame_height] = frame.size().to_array();

			for (slot_idx, (slot_image, style)) in src_slots.iter().enumerate() {
				if *style == LightmapStyle::NONE {
					continue;
				}
				let dst_image =
					slots[slot_idx].get_or_insert_with(|| image::RgbImage::from_pixel(size.x, size.y, image::Rgb(self.settings.default_color)));

				for x in 0..frame_width + self.settings.extrusion * 2 {
					let global_x = frame.min.x + x;
					if global_x >= size.x {
						continue;
					}

					for y in 0..frame_height + self.settings.extrusion * 2 {
						let global_y = frame.min.y + y;
						if global_y >= size.y {
							continue;
						}

						styles.get_pixel_mut(global_x, global_y).0[slot_idx] = style.0;

						dst_image.put_pixel(
							global_x,
							global_y,
							*slot_image.get_pixel(
								x.saturating_sub(self.settings.extrusion).min(frame_width - 1),
								y.saturating_sub(self.settings.extrusion).min(frame_height - 1),
							),
						);
					}
				}
			}
		}

		PerSlotLightmapData {
			slots: slots.map(|image| image.unwrap_or_else(|| image::RgbImage::from_pixel(1, 1, image::Rgb(self.settings.default_color)))),
			styles,
		}
	}
}
