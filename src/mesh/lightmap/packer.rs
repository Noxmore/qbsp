use super::*;
use crate::*;

/// A trait for packing lightmaps into texture atlas'.
pub trait LightmapPacker: Sized {
	type Input;
	type Output: LightmapAtlas;

	fn read_single_color_from_face(view: LightmapPackerFaceReadView, size: impl Into<UVec2>, color: [u8; 3]) -> Self::Input;
	fn read_from_face(&self, view: LightmapPackerFaceReadView) -> Self::Input;

	fn settings(&self) -> ComputeLightmapSettings;

	/// Returns the item index, a unique id for each pushed lightmap.
	fn push(&mut self, item: Self::Input) -> usize;
	fn export(self) -> Result<LightmapAtlasExport<Self>, ComputeLightmapAtlasError>;
}

/// Information provided to read a lightmap from a face.
#[derive(Clone, Copy)]
pub struct LightmapPackerFaceReadView<'a> {
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

#[derive(Clone)]
pub struct DefaultPackerItem<LM> {
	pub lm_info: LightmapInfo,
	pub face_idx: usize,
	pub lightmap: LM,
}

pub struct DefaultLightmapPacker<LM> {
	/// Stores a packer of lightmap offsets
	items: Vec<crunch::Item<DefaultPackerItem<LM>>>,
	settings: ComputeLightmapSettings,
}
impl<LM> DefaultLightmapPacker<LM> {
	pub fn new(settings: ComputeLightmapSettings) -> Self {
		Self {
			items: Vec::new(),
			settings,
		}
	}

	fn push_internal(&mut self, width: u32, height: u32, item: DefaultPackerItem<LM>) -> usize {
		let idx = self.items.len();
		self.items.push(crunch::Item {
			data: item,
			w: width as usize,
			h: height as usize,
			// TODO:
			rot: crunch::Rotation::None,
		});
		idx
	}
}

pub type PerStyleLightmapPacker = DefaultLightmapPacker<PerStyleLightmapData>;
pub type PerSlotLightmapPacker = DefaultLightmapPacker<[(image::RgbImage, LightmapStyle); 4]>;

impl LightmapPacker for PerStyleLightmapPacker {
	type Input = DefaultPackerItem<PerStyleLightmapData>;
	type Output = PerStyleLightmapData;

	fn read_single_color_from_face(view: LightmapPackerFaceReadView, size: impl Into<UVec2>, color: [u8; 3]) -> Self::Input {
		let size = size.into();
		DefaultPackerItem {
			lm_info: view.lm_info.clone(),
			face_idx: view.face_idx,
			lightmap: PerStyleLightmapData {
				size,
				inner: HashMap::from([(LightmapStyle::NORMAL, image::RgbImage::from_pixel(size.x, size.y, image::Rgb(color)))]),
			},
		}
	}

	fn read_from_face(&self, view: LightmapPackerFaceReadView) -> Self::Input {
		// TODO: we should probably get rid of this lightmap_offset check
		if view.face.lightmap_offset.is_negative() || view.face.lightmap_styles[0] == LightmapStyle::NONE {
			Self::read_single_color_from_face(view, view.lm_info.extents.lightmap_size(), [0; 3])
		} else {
			let mut lightmap = PerStyleLightmapData::new(view.lm_info.extents.lightmap_size());
			for (i, style) in view.face.lightmap_styles.into_iter().enumerate() {
				if style == LightmapStyle::NONE {
					break;
				}
				lightmap
					.insert(
						style,
						image::RgbImage::from_fn(view.lm_info.extents.lightmap_size().x, view.lm_info.extents.lightmap_size().y, |x, y| {
							image::Rgb(view.lighting.get(view.lm_info.compute_lighting_index(i, x, y)).unwrap_or_default())
						}),
					)
					.unwrap();
			}

			DefaultPackerItem {
				lm_info: view.lm_info.clone(),
				face_idx: view.face_idx,
				lightmap,
			}
		}
	}

	fn settings(&self) -> ComputeLightmapSettings {
		self.settings
	}

	fn push(&mut self, input: Self::Input) -> usize {
		self.push_internal(input.lightmap.size().x, input.lightmap.size().y, input)
	}

	fn export(self) -> Result<LightmapAtlasExport<Self>, ComputeLightmapAtlasError> {
		let rect = crunch::Rect::new(0, 0, self.settings.max_width as usize, self.settings.max_height as usize);
		
		let packed_items = match crunch::pack(rect, self.items) {
			Ok(x) => x,
			// TODO:
			Err(err) => return Err(ComputeLightmapAtlasError::PackFailure { lightmap_size: UVec2::ZERO, images_packed: 0, max_lightmap_size: UVec2::ZERO }),
		};

		let total_x = packed_items.iter().map(|item| item.rect.right()).max().unwrap_or_default() as u32;
		let total_y = packed_items.iter().map(|item| item.rect.bottom()).max().unwrap_or_default() as u32;
		
		let mut atlas = PerStyleLightmapData::new([total_x, total_y]);
		let [atlas_width, atlas_height] = atlas.size().to_array();

		for packed_item in packed_items {
			for (light_style, lightmap_image) in packed_item.data.lightmap.inner() {
				atlas
					.modify_inner(|map| {
						let dst_image = map
							.entry(*light_style)
							.or_insert_with(|| image::RgbImage::from_pixel(atlas_width, atlas_height, image::Rgb(self.settings.default_color)));

						for x in 0..packed_item.rect.w as u32 {
							for y in 0..packed_item.rect.h as u32 {
								dst_image.put_pixel(packed_item.rect.x as u32 + x, packed_item.rect.y as u32 + y, *lightmap_image.get_pixel(x, y));
							}
						}
					})
					.unwrap();
			}
		}

		/* Ok(LightmapAtlasExport {
			uvs,
			atlas,
		}) */
		todo!()
	}
}

impl LightmapPacker for PerSlotLightmapPacker {
	type Input = DefaultPackerItem<[(image::RgbImage, LightmapStyle); 4]>;
	type Output = PerSlotLightmapData;

	fn read_single_color_from_face(view: LightmapPackerFaceReadView, size: impl Into<UVec2>, color: [u8; 3]) -> Self::Input {
		let size: UVec2 = size.into();
		DefaultPackerItem {
			lm_info: view.lm_info.clone(),
			face_idx: view.face_idx,
			lightmap: [(); 4].map(|_| (image::RgbImage::from_pixel(size.x, size.y, image::Rgb(color)), LightmapStyle::NORMAL)),
		}
	}

	fn read_from_face(&self, view: LightmapPackerFaceReadView) -> Self::Input {
		let mut i = 0;
		DefaultPackerItem {
			lm_info: view.lm_info.clone(),
			face_idx: view.face_idx,
			lightmap: view.face.lightmap_styles.map(|style| {
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
	}

	fn settings(&self) -> ComputeLightmapSettings {
		self.settings
	}

	fn push(&mut self, input: Self::Input) -> usize {
		let size = input.lightmap
			.iter()
			.map(|(image, _)| uvec2(image.width(), image.height()))
			.reduce(UVec2::max)
			.unwrap();

		self.push_internal(size.x, size.y, input)
	}

	fn export(self) -> Result<LightmapAtlasExport<Self>, ComputeLightmapAtlasError> {
		let rect = crunch::Rect::new(0, 0, self.settings.max_width as usize, self.settings.max_height as usize);
		
		let packed_items = match crunch::pack(rect, self.items) {
			Ok(x) => x,
			// TODO:
			Err(err) => return Err(ComputeLightmapAtlasError::PackFailure { lightmap_size: UVec2::ZERO, images_packed: 0, max_lightmap_size: UVec2::ZERO }),
		};

		let total_x = packed_items.iter().map(|item| item.rect.right()).max().unwrap_or_default() as u32;
		let total_y = packed_items.iter().map(|item| item.rect.bottom()).max().unwrap_or_default() as u32;

		let mut slots = [(); 4].map(|_| None);
		let mut styles = image::RgbaImage::from_pixel(total_x, total_y, image::Rgba([255; 4]));

		let mut uvs = HashMap::new();
		
		for packed_item in packed_items {
			uvs.insert(packed_item.data.face_idx as u32, packed_item.data.lm_info.extents.compute_lightmap_uvs(packed_item.data.lm_info.uvs, vec2(packed_item.rect.x as f32, packed_item.rect.y as f32)).collect());
			
			for (slot_idx, (slot_image, style)) in packed_item.data.lightmap.iter().enumerate() {
				if *style == LightmapStyle::NONE {
					continue;
				}
				let dst_image =
					slots[slot_idx].get_or_insert_with(|| image::RgbImage::from_pixel(total_x, total_y, image::Rgb(self.settings.default_color)));

				for x in 0..packed_item.rect.w as u32 {
					for y in 0..packed_item.rect.h as u32 {
						styles.get_pixel_mut(packed_item.rect.x as u32 + x, packed_item.rect.y as u32 + y).0[slot_idx] = style.0;

						dst_image.put_pixel(packed_item.rect.x as u32 + x, packed_item.rect.y as u32 + y, *slot_image.get_pixel(x, y));
					}
				}
			}
		}

		Ok(LightmapAtlasExport {
			uvs,
			atlas: PerSlotLightmapData {
				slots: slots.map(|image| image.unwrap_or_else(|| image::RgbImage::from_pixel(1, 1, image::Rgb(self.settings.default_color)))),
				styles,
			},
		})
	}
}

/// Maps face indexes to normalized UV coordinates into a lightmap atlas. The vast majority of faces have 5 or less vertices.
pub type LightmapUvMap = HashMap<u32, SmallVec<[Vec2; 5]>>;

// /// Intermediate step 
pub struct LightmapAtlasExport<P: LightmapPacker> {
	/// Map of face indexes to normalized UV coordinates into the atlas.
	pub uvs: LightmapUvMap,
	pub atlas: P::Output,
}
