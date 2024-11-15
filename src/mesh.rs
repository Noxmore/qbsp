//! Turning [BspData] into a renderable mesh.

use crate::*;

use image::GenericImage;
use texture_packer::{
    texture::Texture, TexturePacker, TexturePackerConfig
};

// TODO Currently, we use a rather rigid system where meshing produces one lightmap atlas, and a mesh for each texture in the model.
//      We probably want to give the programmer more control, especially for vis support? (Not sure yet what that would look like)
pub struct BspMeshOptions {
    pub lightmap_separation: LightmapSeparation,
    pub mesh_separation: MeshSeparation,
    pub default_lightmap_color: [u8; 3],
    pub texture_padding: u32,
}
pub enum LightmapSeparation {
    Bsp,
    Model,
    Material,
}
pub enum MeshSeparation {
    Model,
    Material,
}
// ^^^ This is just temporary for now, a concept of what the api might look like

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

pub struct MeshExportOutput {
    pub meshes: Vec<ExportedMesh>,
    /// Map of lightmap atlas' for each lightmap style, which you can then composite together to achieve animated lightmaps.
    pub lightmap_atlas: Option<Lightmaps>,
}

impl BspData {
    /// Meshes a model at the specified index. Returns one mesh for each texture used in the model.
    pub fn mesh_model(&self, model_idx: usize) -> MeshExportOutput {
        let model = &self.models[model_idx];

        let mut lightmap_packer = DefaultLightmapPacker::new(TexturePackerConfig {
            max_height: u32::MAX,
            // Sizes are consistent enough that i don't think we need to support rotation
            allow_rotation: false,
            force_max_dimensions: false,
            texture_padding: 0, // This defaults to 1
            ..Default::default()
        });

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
                for i in face.first_edge..face.first_edge + face.num_edges.bsp2() {
                    let surf_edge = self.surface_edges[i as usize];
                    let edge = self.edges[surf_edge.abs() as usize];
                    let vert_idx = if surf_edge.is_negative() { (edge.b, edge.a) } else { (edge.a, edge.b) };

                    let pos = self.vertices[vert_idx.0.bsp2() as usize];

                    mesh.positions.push(pos);
                    mesh.normals.push(if face.plane_side.bsp2() == 0 { plane.normal } else { -plane.normal });

                    // Converting to double for calculation to minimise floating-point imprecision as demonstrated here: https://github.com/Novum/vkQuake/blob/b6eb0cf5812c09c661d51e3b95fc08d88da2288a/Quake/gl_model.c#L1315
                    let uv = dvec2(
                        pos.as_dvec3().dot(tex_info.u_axis.as_dvec3()) + tex_info.u_offset as f64,
                        pos.as_dvec3().dot(tex_info.v_axis.as_dvec3()) + tex_info.v_offset as f64,
                    ).as_vec2();

                    mesh.uvs.push(uv / texture_size);
                    // Lightmap uvs have a constant scale of 16-units to 1 texel
                    lightmap_world_uvs.push(uv);
                }

                // Calculate indices
                for i in 1..face.num_edges.bsp2() - 1 {
                    mesh.indices.push([0, i + 1, i].map(|x| first_index + x));
                }

                //////////////////////////////////////////////////////////////////////////////////
                //// LIGHTMAP
                //////////////////////////////////////////////////////////////////////////////////

                let Some(lighting) = &self.lighting else { continue };
                let lightmap_uvs = mesh.lightmap_uvs.get_or_insert_with(Vec::new);
                
                let mut world_lightmap_rect = Rect::EMPTY;
                for uv in &lightmap_world_uvs {
                    world_lightmap_rect = world_lightmap_rect.union_point(*uv);
                }

                // Face extent calculation referenced from vkQuake
                let i_min = (world_lightmap_rect.min / 16.).floor().as_ivec2();
                let i_max = (world_lightmap_rect.max / 16.).ceil().as_ivec2();
                let face_lightmap_size = (i_max - i_min).as_uvec2() + 1;

                // Stores in terms of pixels instead of bytes because we index into the lighting per pixel, even if it's stored in RGB.
                let face_lightmap_pixels = face_lightmap_size.x * face_lightmap_size.y;

                let lightmaps = if face.lightmap_offset.is_negative() || face.lightmap_styles[0] == LightmapStyle::NONE {
                    // HashMap::from([(LightmapStyle::NORMAL, image::RgbImage::from_pixel(face_lightmap_size.x, face_lightmap_size.y, image::Rgb([0; 3])))])
                    Lightmaps::new_single_color(face_lightmap_size, [0; 3])
                } else {
                    let mut lightmaps = Lightmaps::new(face_lightmap_size);
                    for (i, style) in face.lightmap_styles.into_iter().enumerate() {
                        if style == LightmapStyle::NONE { break }
                        lightmaps.insert(style, image::RgbImage::from_fn(face_lightmap_size.x, face_lightmap_size.y, |x, y| {
                            // if face_idcx == 56 {
                                image::Rgb(lighting.get(face.lightmap_offset as usize + (face_lightmap_pixels as usize * i) + (y * face_lightmap_size.x + x) as usize).unwrap_or_default())
                            // } else {
                                // image::Rgb([fastrand::u8(..), fastrand::u8(..), fastrand::u8(..)])
                            // }
                        })).unwrap();
                    }
                    lightmaps
                };
                let Some(frame) = lightmap_packer.pack(face_idx, lightmaps) else {
                    // TODO Should this return Err() instead? 
                    panic!("Failed to pack lightmap of size {face_lightmap_size}, {} lightmaps have already been packed.", lightmap_packer.images.len());
                };
                // println!("{frame:?}");
                // if frame.max - frame.min == Vec2::ZERO {
                //     println!("bad");
                // }
                
                // Append lightmap uvs, since lightmap face size is calculated from the uvs bounds, we don't need to resize it, just move it into place
                // Atlas uvs will be in texture space until converted later
                lightmap_uvs.extend(lightmap_world_uvs.into_iter().map(|mut uv| {
                    // Move from world space into top left corner
                    uv -= (i_min * 16).as_vec2();
                    // Offset by half a texel to remove bleeding artifacts
                    uv += 8.;
                    // 16 Units per texel
                    uv /= 16.;
                    // Finally, move to there the lightmap starts
                    uv += frame.min;

                    uv
                }));
            }
            
            meshes.push(mesh);
        }

        // Finalize lightmap atlas
        let lightmap_atlas = if self.lighting.is_some() {
            let atlas = lightmap_packer.export([0; 3]); // TODO make default color customizable

            if atlas.size() == UVec2::ZERO {
                None
            } else {
                // Normalize lightmap UVs from texture space
                for mesh in &mut meshes {
                    let Some(lightmap_uvs) = &mut mesh.lightmap_uvs else { continue };
    
                    for uv in lightmap_uvs {
                        *uv /= atlas.size().as_vec2();
                    }
                }
    
                Some(atlas)
            }
        } else {
            None
        };
        
        MeshExportOutput {
            meshes,
            lightmap_atlas,
        }
    }
}

/// A trait for packing lightmaps into texture atlas'. Specifically using image::RgbImage.
trait LightmapPacker {
    fn pack(&mut self, face_idx: u32, images: Lightmaps) -> Option<Rect>;
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
    images: Vec<(texture_packer::Frame<u32>, Lightmaps)>,
}
impl DefaultLightmapPacker {
    pub fn new(config: TexturePackerConfig) -> Self {
        Self { packer: TexturePacker::new_skyline(config), images: Vec::new() }
    }
}
impl LightmapPacker for DefaultLightmapPacker {
    fn pack(&mut self, face_idx: u32, lightmaps: Lightmaps) -> Option<Rect> {
        self.packer.pack_own(face_idx, DummyTexture { width: lightmaps.size().x, height: lightmaps.size().y }).ok()?;
        self.packer.get_frame(&face_idx).map(|frame| {
            self.images.push((frame.clone(), lightmaps));
            let min = vec2(frame.frame.x as f32, frame.frame.y as f32);
            Rect { min, max: min + vec2(frame.frame.w as f32, frame.frame.h as f32) }
        })
    }
    fn export(&self, default: [u8; 3]) -> Lightmaps {
        let mut images = Lightmaps::new([self.packer.width(), self.packer.height()]);
        let [atlas_width, atlas_height] = images.size().to_array();

        for (frame, lightmap_images) in &self.images {
            for (light_style, lightmap_image) in lightmap_images.map() {
                images.modify_map(|map| {
                    let atlas = map.entry(*light_style).or_insert_with(|| image::RgbImage::from_pixel(atlas_width, atlas_height, image::Rgb(default)));

                    // image.copy_from(lightmap_image, frame.frame.x, frame.frame.y); // TODO ?
                    for x in 0..frame.frame.w {
                        for y in 0..frame.frame.h {
                            // TODO ?? This should be impossible
                            // if frame.frame.x + x >= atlas.width() || frame.frame.y + y >= atlas.height() || x >= lightmap_image.width() || y >= lightmap_image.height() {
                            //     continue;
                            // }
                            *atlas.get_pixel_mut(frame.frame.x + x, frame.frame.y + y) = *lightmap_image.get_pixel(x, y);
                        }
                    }
                }).unwrap();
            }
        }

        images
    }
}