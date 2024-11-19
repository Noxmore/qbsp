//! Turning [BspData] into a renderable mesh.

use crate::*;

use texture_packer::{
    texture::Texture, TexturePacker, TexturePackerConfig
};

/// A mesh exported from a BSP file for rendering.
#[derive(Debug, Clone, Default)]
pub struct ExportedMesh {
    /// Positions of vertices in this mesh. NOTE: These are in Z-up coordinate space.
    pub positions: Vec<Vec3>,
    /// Normal vectors of vertices in this mesh. NOTE: These are in Z-up coordinate space.
    pub normals: Vec<Vec3>,
    /// Normalized texture coordinates. (0..1)
    pub uvs: Vec<Vec2>,
    /// Optional uvs for the lightmap atlas.
    pub lightmap_uvs: Option<Vec<Vec2>>,
    /// Triangle list.
    pub indices: Vec<[u32; 3]>,

    pub tex_flags: BspTexFlags,

    /// All faces in the bsp data used to create this mesh.
    pub faces: Vec<u32>,

    pub texture: String,
}

/// The output of [BspData::mesh_model]. Contains one mesh for each texture used in the model.
pub struct MeshModelOutput {
    pub meshes: Vec<ExportedMesh>,
}

#[derive(Error, Debug, Clone)]
pub enum ComputeLightmapAtlasError {
    #[error("Failed to pack lightmap of size {lightmap_size}, {images_packed} lightmaps have already been packed")]
    PackFailure {
        lightmap_size: UVec2,
        images_packed: usize,
    },
    #[error("No lightmaps")]
    NoLightmaps,
}

impl BspData {
    /// Packs every face's lightmap together onto a single atlas for GPU rendering.
    pub fn compute_lightmap_atlas(&self, default_lightmap_color: [u8; 3]) -> Result<LightmapAtlas, ComputeLightmapAtlasError> {
        let Some(lighting) = &self.lighting else { return Err(ComputeLightmapAtlasError::NoLightmaps) };

        let mut lightmap_packer = DefaultLightmapPacker::new(TexturePackerConfig {
            max_height: u32::MAX,
            // Sizes are consistent enough that i don't think we need to support rotation
            allow_rotation: false,
            force_max_dimensions: false,
            texture_padding: 0, // This defaults to 1
            ..Default::default()
        });

        let mut lightmap_uvs: HashMap<u32, SmallVec<[Vec2; 5]>> = HashMap::new();
        
        for (face_idx, face) in self.faces.iter().enumerate() {
            let tex_info = &self.tex_info[face.texture_info_idx.bsp2() as usize];
            
            let uvs: Vec<Vec2> = face.vertices(self).map(|pos| world_uv(pos, tex_info)).collect();
            let extents = FaceExtents::new(uvs.iter().copied());

            let lightmaps = read_lightmaps_from_face(face, &extents, lighting);

            let frame = lightmap_packer.pack(face, lightmaps)
                .ok_or_else(|| ComputeLightmapAtlasError::PackFailure { lightmap_size: extents.lightmap_size(), images_packed: lightmap_packer.images.len() })?;
            
            lightmap_uvs.insert(face_idx as u32, extents.compute_lightmap_uvs(uvs, frame.min.as_vec2()).collect());
        }

        let atlas = lightmap_packer.export(default_lightmap_color);

        // Normalize lightmap UVs from texture space
        for (_, uvs) in &mut lightmap_uvs {
            for uv in uvs {
                *uv /= atlas.size().as_vec2();
            }
        }

        Ok(LightmapAtlas { uvs: lightmap_uvs, images: atlas })
    }
    
    /// Meshes a model at the specified index. Returns one mesh for each texture used in the model.
    pub fn mesh_model(&self, model_idx: usize, lightmap_atlas: Option<&LightmapAtlas>) -> MeshModelOutput {
        let model = &self.models[model_idx];

        // Group faces by texture, also storing index for packing use
        let mut grouped_faces: HashMap<(&str, BspTexFlags), Vec<(u32, &BspFace)>> = Default::default();

        for i in model.first_face..model.first_face + model.num_faces {
            let face = &self.faces[i as usize];
            let tex_info = &self.tex_info[face.texture_info_idx.bsp2() as usize];
            let Some(texture) = &self.textures[tex_info.texture_idx as usize] else { continue };

            grouped_faces.entry((texture.header.name.as_str(), tex_info.flags)).or_default().push((i, face));
        }

        let mut meshes = Vec::with_capacity(grouped_faces.len());

        for ((texture, tex_flags), faces) in grouped_faces {
            let mut mesh = ExportedMesh::default();
            mesh.texture = texture.to_string();
            mesh.tex_flags = tex_flags;

            for (face_idx, face) in faces {
                mesh.faces.push(face_idx);
                
                let plane = &self.planes[face.plane_idx.bsp2() as usize];
                let tex_info = &self.tex_info[face.texture_info_idx.bsp2() as usize];
                let texture_size = self.textures[tex_info.texture_idx as usize].as_ref()
                    .map(|tex| vec2(tex.header.width as f32, tex.header.height as f32))
                    .unwrap_or(Vec2::ONE);


                // The uv coordinates of the face's lightmap in the world, rather than on a lightmap atlas
                let mut lightmap_world_uvs: Vec<Vec2> = Vec::with_capacity(face.num_edges.bsp2() as usize);

                let first_index = mesh.positions.len() as u32;
                for pos in face.vertices(self) {
                    mesh.positions.push(pos);
                    mesh.normals.push(if face.plane_side.bsp2() == 0 { plane.normal } else { -plane.normal });

                    let uv = world_uv(pos, tex_info);

                    mesh.uvs.push(uv / texture_size);
                    // Lightmap uvs have a constant scale of 16-units to 1 texel
                    lightmap_world_uvs.push(uv);
                }

                // Calculate indices
                for i in 1..face.num_edges.bsp2() - 1 {
                    mesh.indices.push([0, i + 1, i].map(|x| first_index + x));
                }

                if let Some(atlas) = lightmap_atlas {
                    if let Some(uvs) = atlas.uvs.get(&face_idx) {
                        assert_eq!(uvs.len(), face.num_edges.bsp2() as usize);
                        mesh.lightmap_uvs.get_or_insert_with(Vec::new).extend(uvs);
                    }
                }
            }
            
            meshes.push(mesh);
        }
        
        MeshModelOutput { meshes }
    }
}

/// Calculates a world UV coordinate from a position and texture info.
/// 
/// Converts to double for calculation to minimise floating-point imprecision as demonstrated [here](https://github.com/Novum/vkQuake/blob/b6eb0cf5812c09c661d51e3b95fc08d88da2288a/Quake/gl_model.c#L1315).
#[inline]
pub fn world_uv(pos: Vec3, tex_info: &BspTexInfo) -> Vec2 {
    dvec2(
        pos.as_dvec3().dot(tex_info.u_axis.as_dvec3()) + tex_info.u_offset as f64,
        pos.as_dvec3().dot(tex_info.v_axis.as_dvec3()) + tex_info.v_offset as f64,
    ).as_vec2()
}

/// Computes the index into [BspLighting] for the specific face specified.
#[inline]
pub fn compute_lighting_index(face: &BspFace, extents: &FaceExtents, light_style_idx: usize, x: u32, y: u32) -> usize {
    face.lightmap_offset as usize + (extents.lightmap_pixels() as usize * light_style_idx) + (y * extents.lightmap_size().x + x) as usize
}

pub fn read_lightmaps_from_face(face: &BspFace, extents: &FaceExtents, lighting: &BspLighting) -> Lightmaps {
    if face.lightmap_offset.is_negative() || face.lightmap_styles[0] == LightmapStyle::NONE {
        Lightmaps::new_single_color(extents.lightmap_size(), [0; 3])
    } else {
        let mut lightmaps = Lightmaps::new(extents.lightmap_size());
        for (i, style) in face.lightmap_styles.into_iter().enumerate() {
            if style == LightmapStyle::NONE { break }
            lightmaps.insert(style, image::RgbImage::from_fn(extents.lightmap_size().x, extents.lightmap_size().y, |x, y| {
                image::Rgb(lighting.get(compute_lighting_index(face, &extents, i, x, y)).unwrap_or_default())
            })).unwrap();
        }
        lightmaps
    }
}

/// Computed extents of a face for various calculations, mainly involving lightmaps.
#[derive(Debug, Clone, Copy, Default)]
pub struct FaceExtents {
    face_rect: Rect<Vec2>,
    
    lightmap_rect: Rect<IVec2>,
    lightmap_size: UVec2,
    lightmap_pixels: u32,
}
impl FaceExtents {
    /// Calculates face extents from unscaled UVs.
    pub fn new(uvs: impl IntoIterator<Item = Vec2>) -> Self {
        let mut extents = Self::default();
        
        extents.face_rect = Rect::EMPTY;
        for uv in uvs {
            extents.face_rect = extents.face_rect.union_point(uv);
        }

        // Calculation referenced from vkQuake
        extents.lightmap_rect = Rect::new(
            (extents.face_rect.min / 16.).floor().as_ivec2(),
            (extents.face_rect.max / 16.).ceil().as_ivec2(),
        );

        extents.lightmap_size = extents.lightmap_rect.size().as_uvec2() + 1;

        extents.lightmap_pixels = extents.lightmap_size.element_product();

        extents
    }

    /// Bounding box rectangle covering all supplied uvs.
    #[inline]
    pub fn face_rect(&self) -> Rect<Vec2> {
        self.face_rect
    }

    #[inline]
    /// World-space rectangle that the lightmap takes up.
    pub fn lightmap_rect(&self) -> Rect<IVec2> {
        self.lightmap_rect
    }

    /// The size of the lightmap rectangle.
    #[inline]
    pub fn lightmap_size(&self) -> UVec2 {
        self.lightmap_size
    }

    /// The total number of pixels in the lightmap.
    #[inline]
    pub fn lightmap_pixels(&self) -> u32 {
        self.lightmap_pixels
    }

    /// Computes texture-space lightmap UVs, provide the same set of face UVs supplied to [FaceExtents::new], and the position of the lightmap on the atlas.
    pub fn compute_lightmap_uvs<'a>(&'a self, uvs: impl IntoIterator<Item = Vec2> + 'a, lightmap_position: Vec2) -> impl Iterator<Item = Vec2> + 'a {
        uvs.into_iter().map(move |mut uv| {
            // Move from world space into top left corner
            uv -= (self.lightmap_rect.min * 16).as_vec2();
            // Offset by half a texel to remove bleeding artifacts
            uv += 8.;
            // 16 Units per texel
            uv /= 16.;
            // Finally, move to where the lightmap starts
            uv += lightmap_position;

            uv
        })
    }
}


/// A trait for packing lightmaps into texture atlas'. Specifically using image::RgbImage.
trait LightmapPacker {
    fn pack(&mut self, face: &BspFace, images: Lightmaps) -> Option<Rect<UVec2>>;
    fn export(&self, default: [u8; 3]) -> Lightmaps;
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

struct DefaultLightmapPacker {
    packer: TexturePacker<'static, DummyTexture, u32>,
    // I have to store images separately, since TexturePacker doesn't give me access
    images: Vec<(Rect<UVec2>, Lightmaps)>,
}
impl DefaultLightmapPacker {
    pub fn new(config: TexturePackerConfig) -> Self {
        Self { packer: TexturePacker::new_skyline(config), images: Vec::new() }
    }
}
impl LightmapPacker for DefaultLightmapPacker {
    fn pack(&mut self, face: &BspFace, lightmaps: Lightmaps) -> Option<Rect<UVec2>> {
        self.packer.pack_own(face.lightmap_offset as u32, DummyTexture { width: lightmaps.size().x, height: lightmaps.size().y }).ok()?;
        self.packer.get_frame(&(face.lightmap_offset as u32)).map(|frame| {
            let min = uvec2(frame.frame.x, frame.frame.y);
            let rect = Rect { min, max: min + uvec2(frame.frame.w, frame.frame.h) };

            self.images.push((rect, lightmaps));
            
            rect
        })
    }
    fn export(&self, default: [u8; 3]) -> Lightmaps {
        // TODO the packer and this give different sizes
        let mut size = UVec2::ZERO;

        for (frame, _) in &self.images {
            size = size.max(frame.max);
        }

        let mut atlas = Lightmaps::new(size);
        let [atlas_width, atlas_height] = atlas.size().to_array();

        for (frame, lightmap_images) in &self.images {
            let [frame_width, frame_height] = frame.size().to_array();
            
            for (light_style, lightmap_image) in lightmap_images.inner() {
                atlas.modify_inner(|map| {
                    let dst_image = map.entry(*light_style).or_insert_with(|| image::RgbImage::from_pixel(atlas_width, atlas_height, image::Rgb(default)));

                    for x in 0..frame_width {
                        for y in 0..frame_height {
                            dst_image.put_pixel(frame.min.x + x, frame.min.y + y, *lightmap_image.get_pixel(x, y));
                        }
                    }
                }).unwrap();
            }
        }

        atlas
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
        Self { size: size.into(), inner: HashMap::new() }
    }

    /// Constructs a Lightmaps collection with a single lightmap of the specified `size` filled with a single `color`.
    pub fn new_single_color(size: impl Into<UVec2>, color: [u8; 3]) -> Self {
        let size = size.into();
        Self { size, inner: HashMap::from([(LightmapStyle::NORMAL, image::RgbImage::from_pixel(size.x, size.y, image::Rgb(color)))]) }
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
    pub fn modify_inner<O, F: FnOnce(&mut HashMap<LightmapStyle, image::RgbImage>) -> O>(&mut self, modifier: F) -> Result<O, LightmapsInvalidSizeError> {
        let out = modifier(&mut self.inner);

        for (style, image) in &self.inner {
            let image_size = uvec2(image.width(), image.height());
            if self.size != image_size {
                return Err(LightmapsInvalidSizeError { style: *style, image_size, expected_size: self.size });
            }
        }

        Ok(out)
    }

    /// Inserts a new image into the collection. Returns `Err` if the atlas' size doesn't match the collection's expected size.
    pub fn insert(&mut self, style: LightmapStyle, image: image::RgbImage) -> Result<Option<image::RgbImage>, LightmapsInvalidSizeError> {
        let image_size = uvec2(image.width(), image.height());
        if self.size != image_size {
            return Err(LightmapsInvalidSizeError { style, image_size, expected_size: self.size });
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
    pub uvs: HashMap<u32, SmallVec<[Vec2; 5]>>, // Vast majority of faces have 5 or less vertices.
    pub images: Lightmaps,
}