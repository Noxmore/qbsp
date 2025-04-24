use super::*;
use crate::*;

use texture_packer::{texture::Texture, TexturePacker, TexturePackerConfig};

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

	pub fn get_uvs<P: LightmapPacker>(&mut self, lightmap_packer: &mut P, face: &BspFace) -> Result<SmallVec<[Vec2; 5]>, ComputeLightmapAtlasError> {
		let position = match self.position {
			Some(v) => v,
			None => {
				// TODO: Padding for bicubic filtering
				let rect = lightmap_packer.pack(face, P::create_single_color_input([1, 1], self.color))?;
				self.position = Some(rect.min);
				rect.min
			}
		};

		Ok(smallvec![position.as_vec2() + Vec2::splat(0.5); face.num_edges.0 as usize])
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

		let mut lightmap_uvs: HashMap<u32, SmallVec<[Vec2; 5]>> = HashMap::new();

		let mut empty_reserved_pixel = ReservedLightmapPixel::new(settings.no_lighting_color);
		let mut special_reserved_pixel = ReservedLightmapPixel::new(settings.special_lighting_color);

		for (face_idx, face) in self.faces.iter().enumerate() {
			let tex_info = &self.tex_info[face.texture_info_idx.0 as usize];

			if decoupled_lm.is_none() && face.lightmap_offset.is_negative() {
				lightmap_uvs.insert(
					face_idx as u32,
					if tex_info.flags != BspTexFlags::Normal {
						special_reserved_pixel.get_uvs(&mut packer, face)?
					} else {
						empty_reserved_pixel.get_uvs(&mut packer, face)?
					},
				);
				continue;
			}

			let decoupled_lightmap = decoupled_lm.as_ref().map(|lm_infos| lm_infos[face_idx]);

			let lm_info = match &decoupled_lightmap {
				Some(lm_info) => {
					let uvs: Vec<Vec2> = face.vertices(self).map(|pos| lm_info.projection.project(pos)).collect();
					let extents = FaceExtents::new_decoupled(uvs.iter().copied(), lm_info);

					LightmapInfo {
						uvs,
						extents,
						lightmap_offset: lm_info.offset as usize,
					}
				}
				None => {
					let uvs: Vec<Vec2> = face.vertices(self).map(|pos| tex_info.projection.project(pos)).collect();
					let extents = FaceExtents::new(uvs.iter().copied());

					LightmapInfo {
						uvs,
						extents,
						lightmap_offset: face.lightmap_offset as usize,
					}
				}
			};

			let input = packer.read_from_face(LightmapPackerFaceReaderView {
				lm_info: &lm_info,

				bsp: self,

				face_idx,
				face,
				tex_info,
				lighting,
			});

			let frame = packer.pack(face, input)?;

			lightmap_uvs.insert(
				face_idx as u32,
				lm_info.extents.compute_lightmap_uvs(lm_info.uvs, frame.min.as_vec2()).collect(),
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

/// A trait for packing lightmaps into texture atlas'. Specifically using image::RgbImage.
pub trait LightmapPacker {
	type Input;
	type Output: LightmapAtlas;

	fn create_single_color_input(size: impl Into<UVec2>, color: [u8; 3]) -> Self::Input;
	fn read_from_face(&self, view: LightmapPackerFaceReaderView) -> Self::Input;

	fn settings(&self) -> ComputeLightmapSettings;

	fn pack(&mut self, face: &BspFace, images: Self::Input) -> Result<Rect<UVec2>, ComputeLightmapAtlasError>;
	fn export(&self) -> Self::Output;
}

/// Information provided to read a lightmap from a face.
#[derive(Clone, Copy)]
pub struct LightmapPackerFaceReaderView<'a> {
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

/// Computed information about the specifics of how a lightmap applies to a face.
#[derive(Debug, Clone)]
pub struct LightmapInfo {
	/// The vertices of the face projected onto it's texture or decoupled lightmap.
	pub uvs: Vec<Vec2>,
	pub extents: FaceExtents,
	/// The offset into the lightmap lump in bytes to read the lightmap data. Will be x3 for an `.lit` lump.
	pub lightmap_offset: usize,
}
impl LightmapInfo {
	/// Computes the index into [`BspLighting`] for the specific face specified.
	#[inline]
	pub fn compute_lighting_index(&self, light_style_idx: usize, x: u32, y: u32) -> usize {
		self.lightmap_offset + (self.extents.lightmap_pixels() as usize * light_style_idx) + (y * self.extents.lightmap_size().x + x) as usize
	}
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

	fn allocate_and_push(&mut self, face: &BspFace, width: u32, height: u32, lm: LM) -> Result<Rect<UVec2>, ComputeLightmapAtlasError> {
		self.packer
			.pack_own(face.lightmap_offset as u32, DummyTexture { width, height })
			.map_err(|_| ComputeLightmapAtlasError::PackFailure {
				lightmap_size: uvec2(width, height),
				images_packed: self.images.len(),
				max_lightmap_size: uvec2(self.settings.max_width, self.settings.max_height),
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

	fn read_from_face(&self, view: LightmapPackerFaceReaderView) -> Self::Input {
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

	fn pack(&mut self, face: &BspFace, lightmaps: Self::Input) -> Result<Rect<UVec2>, ComputeLightmapAtlasError> {
		self.allocate_and_push(face, lightmaps.size().x, lightmaps.size().y, lightmaps)
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

	fn read_from_face(&self, view: LightmapPackerFaceReaderView) -> Self::Input {
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

	fn pack(&mut self, face: &BspFace, images: Self::Input) -> Result<Rect<UVec2>, ComputeLightmapAtlasError> {
		let size = images
			.iter()
			.map(|(image, _)| uvec2(image.width(), image.height()))
			.reduce(UVec2::max)
			.unwrap();

		self.allocate_and_push(face, size.x, size.y, images)
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

/// Maps face indexes to normalized UV coordinates into a lightmap atlas. The vast majority of faces have 5 or less vertices.
pub type LightmapUvMap = HashMap<u32, SmallVec<[Vec2; 5]>>;

// TODO:
pub trait LightmapPackerBackend {}

pub struct SkylinePackerBackend;
pub struct CrunchPackerBackend;
