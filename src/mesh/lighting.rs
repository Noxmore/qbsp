use super::*;
use crate::*;

use texture_packer::{texture::Texture, TexturePacker, TexturePackerConfig};

#[derive(Debug, Clone, Copy)]
pub struct ComputeLightmapSettings {
	/// The of a pixel is no lightmaps are stored there.
	pub default_color: [u8; 3],
	/// A single pixel of a lightmap atlas is reserved for faces which don't have a lightmap or `special` flag, this is the color of that pixel.
	pub no_lighting_color: [u8; 3],
	/// A single pixel of a lightmap atlas is reserved for faces which don't have a lightmap, but do have the `special` flag, this is the color of that pixel.
	pub special_lighting_color: [u8; 3],
	pub max_width: u32,
	pub max_height: u32,
}
impl Default for ComputeLightmapSettings {
	fn default() -> Self {
		Self {
			default_color: [0; 3],
			no_lighting_color: [0; 3],
			special_lighting_color: [255; 3],
			max_width: 2048,
			max_height: u32::MAX,
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
}

#[derive(Default)]
struct ReservedLightmapPixel {
	position: Option<UVec2>,
}
impl ReservedLightmapPixel {
	pub fn get_uvs<LM>(
		&mut self,
		face: &BspFace,
		lightmap_packer: &mut impl LightmapPacker<Input = LM>,
		lightmap_provider: impl FnOnce() -> LM,
	) -> Result<SmallVec<[Vec2; 5]>, ComputeLightmapAtlasError> {
		let position = match self.position {
			Some(v) => v,
			None => {
				let rect = lightmap_packer.pack(face, lightmap_provider())?;
				self.position = Some(rect.min);
				rect.min
			}
		};

		Ok(smallvec![position.as_vec2() + Vec2::splat(0.5); face.num_edges.0 as usize])
	}
}

impl BspData {
	/// Packs every face's lightmap together onto a single atlas for GPU rendering.
	pub fn compute_lightmap_atlas(
		&self,
		settings: ComputeLightmapSettings,
		ty: LightmapAtlasType,
	) -> Result<LightmapAtlas, ComputeLightmapAtlasError> {
		let Some(lighting) = &self.lighting else { return Err(ComputeLightmapAtlasError::NoLightmaps) };

		let config = TexturePackerConfig {
			max_width: settings.max_width,
			max_height: settings.max_height,
			// Sizes are consistent enough that i don't think we need to support rotation
			allow_rotation: false,
			force_max_dimensions: false,
			texture_padding: 0, // This defaults to 1
			..Default::default()
		};

		let mut lightmap_uvs: HashMap<u32, SmallVec<[Vec2; 5]>> = HashMap::new();

		let mut empty_reserved_pixel = ReservedLightmapPixel::default();
		let mut special_reserved_pixel = ReservedLightmapPixel::default();

		let atlas = match ty {
			LightmapAtlasType::PerStyle => {
				let mut lightmap_packer = PerStyleLightmapPacker::new(config);

				for (face_idx, face) in self.faces.iter().enumerate() {
					if face.lightmap_offset.is_negative() {
						let tex_info = self.tex_info[face.texture_info_idx.0 as usize];

						lightmap_uvs.insert(
							face_idx as u32,
							if tex_info.flags != BspTexFlags::Normal {
								special_reserved_pixel.get_uvs(face, &mut lightmap_packer, || {
									Lightmaps::new_single_color([1, 1], settings.special_lighting_color)
								})?
							} else {
								empty_reserved_pixel.get_uvs(face, &mut lightmap_packer, || {
									Lightmaps::new_single_color([1, 1], settings.no_lighting_color)
								})?
							},
						);
						continue;
					}

					let tex_info = &self.tex_info[face.texture_info_idx.0 as usize];

					let uvs: Vec<Vec2> = face.vertices(self).map(|pos| world_uv(pos, tex_info)).collect();
					let extents = FaceExtents::new(uvs.iter().copied());

					let lightmaps = Lightmaps::read_from_face(face, &extents, lighting);

					let frame = lightmap_packer.pack(face, lightmaps)?;

					lightmap_uvs.insert(face_idx as u32, extents.compute_lightmap_uvs(uvs, frame.min.as_vec2()).collect());
				}

				lightmap_packer.export(settings.default_color)
			}

			LightmapAtlasType::PerSlot => {
				let mut lightmap_packer = PerSlotLightmapPacker::new(config);

				for (face_idx, face) in self.faces.iter().enumerate() {
					if face.lightmap_offset.is_negative() {
						let tex_info = self.tex_info[face.texture_info_idx.0 as usize];

						lightmap_uvs.insert(
							face_idx as u32,
							if tex_info.flags != BspTexFlags::Normal {
								special_reserved_pixel.get_uvs(face, &mut lightmap_packer, || {
									[(); 4].map(|_| {
										(
											image::RgbImage::from_pixel(1, 1, image::Rgb(settings.special_lighting_color)),
											LightmapStyle::NORMAL,
										)
									})
								})?
							} else {
								empty_reserved_pixel.get_uvs(face, &mut lightmap_packer, || {
									[(); 4].map(|_| {
										(
											image::RgbImage::from_pixel(1, 1, image::Rgb(settings.no_lighting_color)),
											LightmapStyle::NORMAL,
										)
									})
								})?
							},
						);
						continue;
					}

					let tex_info = &self.tex_info[face.texture_info_idx.0 as usize];

					let uvs: Vec<Vec2> = face.vertices(self).map(|pos| world_uv(pos, tex_info)).collect();
					let extents = FaceExtents::new(uvs.iter().copied());

					let mut i = 0;
					let lightmaps = face.lightmap_styles.map(|style| {
						let image = image::RgbImage::from_fn(extents.lightmap_size().x, extents.lightmap_size().y, |x, y| {
							if style == LightmapStyle::NONE {
								image::Rgb(settings.default_color)
							} else {
								image::Rgb(lighting.get(compute_lighting_index(face, &extents, i, x, y)).unwrap_or_default())
							}
						});

						i += 1;

						(image, style)
					});

					let frame = lightmap_packer.pack(face, lightmaps)?;

					lightmap_uvs.insert(face_idx as u32, extents.compute_lightmap_uvs(uvs, frame.min.as_vec2()).collect());
				}

				lightmap_packer.export(settings.default_color)
			}
		};

		// Normalize lightmap UVs from texture space
		for uvs in lightmap_uvs.values_mut() {
			for uv in uvs {
				*uv /= atlas.size().as_vec2();
			}
		}

		Ok(LightmapAtlas {
			uvs: lightmap_uvs,
			data: atlas,
		})
	}
}

/// A trait for packing lightmaps into texture atlas'. Specifically using image::RgbImage.
trait LightmapPacker {
	type Input;
	fn pack(&mut self, face: &BspFace, images: Self::Input) -> Result<Rect<UVec2>, ComputeLightmapAtlasError>;
	fn export(&self, default: [u8; 3]) -> LightmapAtlasData;
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

struct DefaultLightmapPacker<LM> {
	packer: TexturePacker<'static, DummyTexture, u32>,
	config: TexturePackerConfig,
	// I have to store images separately, since TexturePacker doesn't give me access
	images: Vec<(Rect<UVec2>, LM)>,
}
impl<LM> DefaultLightmapPacker<LM> {
	pub fn new(config: TexturePackerConfig) -> Self {
		Self {
			packer: TexturePacker::new_skyline(config),
			config,
			images: Vec::new(),
		}
	}

	fn allocate_and_push(&mut self, face: &BspFace, width: u32, height: u32, lm: LM) -> Result<Rect<UVec2>, ComputeLightmapAtlasError> {
		self.packer
			.pack_own(face.lightmap_offset as u32, DummyTexture { width, height })
			.map_err(|_| ComputeLightmapAtlasError::PackFailure {
				lightmap_size: uvec2(width, height),
				images_packed: self.images.len(),
				max_lightmap_size: uvec2(self.config.max_width, self.config.max_height),
			})?;

		Ok(self
			.packer
			.get_frame(&(face.lightmap_offset as u32))
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

type PerStyleLightmapPacker = DefaultLightmapPacker<Lightmaps>;
type PerSlotLightmapPacker = DefaultLightmapPacker<[(image::RgbImage, LightmapStyle); 4]>;

impl LightmapPacker for PerStyleLightmapPacker {
	type Input = Lightmaps;

	fn pack(&mut self, face: &BspFace, lightmaps: Self::Input) -> Result<Rect<UVec2>, ComputeLightmapAtlasError> {
		self.allocate_and_push(face, lightmaps.size().x, lightmaps.size().y, lightmaps)
	}
	fn export(&self, default: [u8; 3]) -> LightmapAtlasData {
		let mut atlas = Lightmaps::new(self.total_size());
		let [atlas_width, atlas_height] = atlas.size().to_array();

		for (frame, lightmap_images) in &self.images {
			let [frame_width, frame_height] = frame.size().to_array();

			for (light_style, lightmap_image) in lightmap_images.inner() {
				atlas
					.modify_inner(|map| {
						let dst_image = map
							.entry(*light_style)
							.or_insert_with(|| image::RgbImage::from_pixel(atlas_width, atlas_height, image::Rgb(default)));

						for x in 0..frame_width {
							for y in 0..frame_height {
								dst_image.put_pixel(frame.min.x + x, frame.min.y + y, *lightmap_image.get_pixel(x, y));
							}
						}
					})
					.unwrap();
			}
		}

		LightmapAtlasData::PerStyle(atlas)
	}
}

impl LightmapPacker for PerSlotLightmapPacker {
	type Input = [(image::RgbImage, LightmapStyle); 4];

	fn pack(&mut self, face: &BspFace, images: Self::Input) -> Result<Rect<UVec2>, ComputeLightmapAtlasError> {
		let size = images
			.iter()
			.map(|(image, _)| uvec2(image.width(), image.height()))
			.reduce(UVec2::max)
			.unwrap();

		self.allocate_and_push(face, size.x, size.y, images)
	}

	fn export(&self, default: [u8; 3]) -> LightmapAtlasData {
		let size = self.total_size();

		let mut slots = [(); 4].map(|_| None);
		let mut styles = image::RgbaImage::from_pixel(size.x, size.y, image::Rgba([255; 4]));

		for (frame, src_slots) in &self.images {
			let [frame_width, frame_height] = frame.size().to_array();

			for (slot_idx, (slot_image, style)) in src_slots.iter().enumerate() {
				if *style == LightmapStyle::NONE {
					continue;
				}
				let dst_image = slots[slot_idx].get_or_insert_with(|| image::RgbImage::from_pixel(size.x, size.y, image::Rgb(default)));

				for x in 0..frame_width {
					for y in 0..frame_height {
						styles.get_pixel_mut(frame.min.x + x, frame.min.y + y).0[slot_idx] = style.0;

						dst_image.put_pixel(frame.min.x + x, frame.min.y + y, *slot_image.get_pixel(x, y));
					}
				}
			}
		}

		LightmapAtlasData::PerSlot {
			slots: slots.map(|image| image.unwrap_or_else(|| image::RgbImage::from_pixel(1, 1, image::Rgb(default)))),
			styles,
		}
	}
}

/// Container for mapping lightmap styles to lightmap images (either atlas' or standalone) to later composite together to achieve animated lightmaps.
///
/// This is just a wrapper for a HashMap that ensures that all containing images are the same size.
#[derive(Debug, Clone)]
pub struct Lightmaps {
	size: UVec2,
	inner: HashMap<LightmapStyle, image::RgbImage>,
}
impl Lightmaps {
	#[inline]
	pub fn new(size: impl Into<UVec2>) -> Self {
		Self {
			size: size.into(),
			inner: HashMap::new(),
		}
	}

	/// Constructs a Lightmaps collection with a single lightmap of the specified `size` filled with a single `color`.
	pub fn new_single_color(size: impl Into<UVec2>, color: [u8; 3]) -> Self {
		let size = size.into();
		Self {
			size,
			inner: HashMap::from([(LightmapStyle::NORMAL, image::RgbImage::from_pixel(size.x, size.y, image::Rgb(color)))]),
		}
	}

	pub fn read_from_face(face: &BspFace, extents: &FaceExtents, lighting: &BspLighting) -> Self {
		if face.lightmap_offset.is_negative() || face.lightmap_styles[0] == LightmapStyle::NONE {
			Lightmaps::new_single_color(extents.lightmap_size(), [0; 3])
		} else {
			let mut lightmaps = Lightmaps::new(extents.lightmap_size());
			for (i, style) in face.lightmap_styles.into_iter().enumerate() {
				if style == LightmapStyle::NONE {
					break;
				}
				lightmaps
					.insert(
						style,
						image::RgbImage::from_fn(extents.lightmap_size().x, extents.lightmap_size().y, |x, y| {
							image::Rgb(lighting.get(compute_lighting_index(face, extents, i, x, y)).unwrap_or_default())
						}),
					)
					.unwrap();
			}
			lightmaps
		}
	}

	#[inline]
	pub fn size(&self) -> UVec2 {
		self.size
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

#[derive(Debug, Error)]
#[error("Lightmap image of style {style} is size {image_size}, when the lightmap collection's expected size is {expected_size}")]
pub struct LightmapsInvalidSizeError {
	pub style: LightmapStyle,
	pub image_size: UVec2,
	pub expected_size: UVec2,
}

/// Contains lightmap atlas' mapped with lightmap styles, and the UVs into said atlas' for each face.
pub struct LightmapAtlas {
	/// Map of face indexes to normalized UV coordinates into the atlas.
	pub uvs: LightmapUvMap, // Vast majority of faces have 5 or less vertices.
	pub data: LightmapAtlasData,
}

/// Maps dace indexes to normalized UV coordinates into a lightmap atlas.
pub type LightmapUvMap = HashMap<u32, SmallVec<[Vec2; 5]>>;

/// The different ways a lightmap atlas can be stored.
#[derive(strum::EnumDiscriminants)]
#[strum_discriminants(name(LightmapAtlasType))]
pub enum LightmapAtlasData {
	/// One image is generated for each [LightmapStyle] with lights using it, which then can be simply summed to generate the final lightmap.
	PerStyle(Lightmaps),

	/// Without using the `LMSTYLE16` BSPX lump, each face can only have up to 4 lightmap styles specified. This technique takes advantage of that fact, having an atlas for each slot for all faces rather than each style.
	///
	/// It then uses an RGBA image, with each color channel's byte being a [LightmapStyle].
	///
	/// This is less simple than [PerStyle](LightmapAtlasType::PerStyle), but usually uses much less memory and computation at runtime.
	///
	/// NOTE: Not all the images are guaranteed to be the same size, if a slot is never used the image will be 1x1 to save on memory.
	PerSlot { slots: [image::RgbImage; 4], styles: image::RgbaImage },
}

impl LightmapAtlasData {
	/// Computes the resolution of the lightmap atlas.
	pub fn size(&self) -> UVec2 {
		match self {
			Self::PerStyle(lightmaps) => lightmaps.size(),
			Self::PerSlot { slots, styles: _ } => slots.iter().map(|image| uvec2(image.width(), image.height())).reduce(UVec2::max).unwrap(),
		}
	}
}
