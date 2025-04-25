use super::*;
use crate::*;

use texture_packer::{texture::Texture, TexturePacker, TexturePackerConfig};

/// A trait for packing lightmaps into texture atlas'. Specifically using image::RgbImage.
pub trait LightmapPacker {
	type Input;
	type Output: LightmapAtlas;

	fn create_single_color_input(size: impl Into<UVec2>, color: [u8; 3]) -> Self::Input;
	fn read_from_face(&self, view: LightmapPackerFaceView) -> Self::Input;

	fn settings(&self) -> ComputeLightmapSettings;

	fn pack(&mut self, view: LightmapPackerFaceView, images: Self::Input) -> Result<Rect<UVec2>, ComputeLightmapAtlasError>;
	fn export(&self) -> Self::Output;
}

/// Information provided to read a lightmap from a face.
#[derive(Clone, Copy)]
pub struct LightmapPackerFaceView<'a> {
	pub lm_info: &'a LightmapInfo,

	pub bsp: &'a BspData,

	pub face_idx: usize,
	/// Shortcut for `bsp.faces[face_idx]`.
	pub face: &'a BspFace,
	/// Shortcut for `bsp.tex_info[bsp.faces[face_idx].texture_info_idx]`.
	pub tex_info: &'a BspTexInfo,
	/// Shortcut for `bsp.lighting` since it's guaranteed to be [`Some`].
	pub lighting: &'a BspLighting,
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
				texture_padding: 0, // This defaults to 1
				..Default::default()
			}),
			settings,
			images: Vec::new(),
		}
	}

	fn allocate_and_push(&mut self, view: LightmapPackerFaceView, width: u32, height: u32, lm: LM) -> Result<Rect<UVec2>, ComputeLightmapAtlasError> {
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

pub type PerStyleLightmapPacker = DefaultLightmapPacker<PerStyleLightmapData>;
pub type PerSlotLightmapPacker = DefaultLightmapPacker<[(image::RgbImage, LightmapStyle); 4]>;

impl LightmapPacker for PerStyleLightmapPacker {
	type Input = PerStyleLightmapData;
	type Output = PerStyleLightmapData;

	fn create_single_color_input(size: impl Into<UVec2>, color: [u8; 3]) -> Self::Input {
		let size = size.into();
		PerStyleLightmapData {
			size,
			inner: HashMap::from([(LightmapStyle::NORMAL, image::RgbImage::from_pixel(size.x, size.y, image::Rgb(color)))]),
		}
	}

	fn read_from_face(&self, view: LightmapPackerFaceView) -> Self::Input {
		if view.face.lightmap_offset.is_negative() || view.face.lightmap_styles[0] == LightmapStyle::NONE {
			Self::create_single_color_input(view.lm_info.extents.lightmap_size(), [0; 3])
		} else {
			let mut lightmaps = PerStyleLightmapData::new(view.lm_info.extents.lightmap_size());
			for (i, style) in view.face.lightmap_styles.into_iter().enumerate() {
				if style == LightmapStyle::NONE {
					break;
				}
				lightmaps
					.insert(
						style,
						image::RgbImage::from_fn(view.lm_info.extents.lightmap_size().x, view.lm_info.extents.lightmap_size().y, |x, y| {
							image::Rgb(view.lighting.get(view.lm_info.compute_lighting_index(i, x, y)).unwrap_or_default())
						}),
					)
					.unwrap();
			}
			lightmaps
		}
	}

	fn settings(&self) -> ComputeLightmapSettings {
		self.settings
	}

	fn pack(&mut self, view: LightmapPackerFaceView, lightmaps: Self::Input) -> Result<Rect<UVec2>, ComputeLightmapAtlasError> {
		self.allocate_and_push(view, lightmaps.size().x, lightmaps.size().y, lightmaps)
	}

	fn export(&self) -> Self::Output {
		let mut atlas = PerStyleLightmapData::new(self.total_size());
		let [atlas_width, atlas_height] = atlas.size().to_array();

		for (frame, lightmap_images) in &self.images {
			let [frame_width, frame_height] = frame.size().to_array();

			for (light_style, lightmap_image) in lightmap_images.inner() {
				atlas
					.modify_inner(|map| {
						let dst_image = map
							.entry(*light_style)
							.or_insert_with(|| image::RgbImage::from_pixel(atlas_width, atlas_height, image::Rgb(self.settings.default_color)));

						for x in 0..frame_width {
							for y in 0..frame_height {
								dst_image.put_pixel(frame.min.x + x, frame.min.y + y, *lightmap_image.get_pixel(x, y));
							}
						}
					})
					.unwrap();
			}
		}

		atlas
	}
}

impl LightmapPacker for PerSlotLightmapPacker {
	type Input = [(image::RgbImage, LightmapStyle); 4];
	type Output = PerSlotLightmapData;

	fn create_single_color_input(size: impl Into<UVec2>, color: [u8; 3]) -> Self::Input {
		let size: UVec2 = size.into();
		[(); 4].map(|_| (image::RgbImage::from_pixel(size.x, size.y, image::Rgb(color)), LightmapStyle::NORMAL))
	}

	fn read_from_face(&self, view: LightmapPackerFaceView) -> Self::Input {
		let mut i = 0;
		view.face.lightmap_styles.map(|style| {
			let image = image::RgbImage::from_fn(view.lm_info.extents.lightmap_size().x, view.lm_info.extents.lightmap_size().y, |x, y| {
				if style == LightmapStyle::NONE {
					image::Rgb(self.settings.default_color)
				} else {
					image::Rgb(view.lighting.get(view.lm_info.compute_lighting_index(i, x, y)).unwrap_or_default())
				}
			});

			i += 1;

			(image, style)
		})
	}

	fn settings(&self) -> ComputeLightmapSettings {
		self.settings
	}

	fn pack(&mut self, view: LightmapPackerFaceView, images: Self::Input) -> Result<Rect<UVec2>, ComputeLightmapAtlasError> {
		let size = images
			.iter()
			.map(|(image, _)| uvec2(image.width(), image.height()))
			.reduce(UVec2::max)
			.unwrap();

		self.allocate_and_push(view, size.x, size.y, images)
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

				for x in 0..frame_width {
					for y in 0..frame_height {
						styles.get_pixel_mut(frame.min.x + x, frame.min.y + y).0[slot_idx] = style.0;

						dst_image.put_pixel(frame.min.x + x, frame.min.y + y, *slot_image.get_pixel(x, y));
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

/// Maps face indexes to normalized UV coordinates into a lightmap atlas. The vast majority of faces have 5 or less vertices.
pub type LightmapUvMap = HashMap<u32, SmallVec<[Vec2; 5]>>;