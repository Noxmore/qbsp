//! Turning [BspData] into a renderable mesh.

use crate::*;

use texture_packer::{
    texture::Texture, TexturePacker, TexturePackerConfig
};
// use open_texture_packer::*;

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

    pub texture: String,
}

pub struct MeshExportOutput {
    pub meshes: Vec<ExportedMesh>,
    // TODO Lightmap animation?
    pub lightmap_atlas: Option<image::RgbImage>,
}

impl BspData {
    /// Meshes a model at the specified index. Returns one mesh for each texture used in the model.
    pub fn mesh_model(&self, model_idx: usize) -> MeshExportOutput {
        let model = &self.models[model_idx];

        // let mut lightmap_atlas: Grid<Option<[u8; 3]>> = Grid::new(1, 1);
        let mut lightmap_packer = DefaultLightmapPacker::new(TexturePackerConfig {
            // max_width: u32::MAX, // TODO do we want a max size?
            // max_height: u32::MAX,
            max_height: u32::MAX,
            allow_rotation: false, // TODO support? frame will store if it was rotated
            force_max_dimensions: false,
            texture_padding: 0, // TODO
            // texture_outlines: true,
            // texture_extrusion: 1,
            ..Default::default()
        });
        // let mut lightmap_packer = ShelfPacker::default();

        
        
        let mut grouped_faces: HashMap<&str, Vec<(u32, &BspFace)>> = Default::default();

        for i in model.first_face..model.first_face + model.num_faces {
            let face = &self.faces[i as usize];
            let tex_info = &self.tex_info[face.texture_info_idx as usize];
            let Some(texture) = &self.textures[tex_info.texture_idx as usize] else { continue };

            grouped_faces.entry(texture.header.name.as_str()).or_default().push((i, face));
        }

        let mut meshes = Vec::with_capacity(grouped_faces.len());

        // TODO Here's where the code gets ugly, big WIP zone.

        

        for (texture, faces) in grouped_faces {
            let mut mesh = ExportedMesh::default();
            mesh.texture = texture.to_string();

            for (face_idx, face) in faces {
                let plane = &self.planes[face.plane_idx as usize];
                let tex_info = &self.tex_info[face.texture_info_idx as usize];
                // let face_extents = BspFaceExtents::calculate(self, face);
                let texture_size = self.textures[tex_info.texture_idx as usize].as_ref()
                    .map(|tex| vec2(tex.header.width as f32, tex.header.height as f32))
                    .unwrap_or(Vec2::ONE);


                // The uv coordinates of the face's lightmap in the world, rather than on a lightmap atlas
                let mut lightmap_world_uvs: Vec<Vec2> = Vec::with_capacity(face.num_edges as usize);

                let first_index = mesh.positions.len() as u32;
                for i in face.first_edge..face.first_edge + face.num_edges {
                    let surf_edge = self.surface_edges[i as usize];
                    let edge = self.edges[surf_edge.abs() as usize];
                    let vert_idx = if surf_edge.is_negative() { (edge.b, edge.a) } else { (edge.a, edge.b) };

                    let pos = self.vertices[vert_idx.0 as usize];

                    mesh.positions.push(pos);
                    mesh.normals.push(if face.plane_side == 0 { plane.normal } else { -plane.normal });

                    // Converting to double for calculation to minimise floating-point imprecision as demonstrated here: https://github.com/Novum/vkQuake/blob/b6eb0cf5812c09c661d51e3b95fc08d88da2288a/Quake/gl_model.c#L1315
                    let uv = dvec2(
                        pos.as_dvec3().dot(tex_info.u_axis.as_dvec3()) + tex_info.u_offset as f64,
                        pos.as_dvec3().dot(tex_info.v_axis.as_dvec3()) + tex_info.v_offset as f64,
                    ).as_vec2();
                    // let uv = vec2(
                    //     pos.dot(tex_info.u_axis) + tex_info.u_offset,
                    //     pos.dot(tex_info.v_axis) + tex_info.v_offset,
                    // );

                    mesh.uvs.push(uv / texture_size);
                    // Lightmap uvs have a constant scale of 16-units to 1 texel
                    lightmap_world_uvs.push(uv);
                }

                // Calculate indices
                for i in 1..face.num_edges - 1 {
                    mesh.indices.push([0, i + 1, i].map(|x| first_index + x));
                }

                //////////////////////////////////////////////////////////////////////////////////
                //// LIGHTMAP
                //////////////////////////////////////////////////////////////////////////////////

                let Some(lighting) = &self.lighting else { continue };
                let lightmap_uvs = mesh.lightmap_uvs.get_or_insert_with(Vec::new);
                /* if face.lightmap_offset.is_negative() {
                    // Just in case only some faces are negative (Not sure why this happens)
                    lightmap_uvs.extend(repeat_n(Vec2::ZERO, face.num_edges as usize));
                    println!("no lightmap on {face_idx} with {} edges:\n{lightmap_world_uvs:#?}", face.num_edges);
                    continue;
                } */
                
                let mut world_lightmap_rect = Rect::EMPTY;
                for uv in &lightmap_world_uvs {
                    world_lightmap_rect = world_lightmap_rect.union_point(*uv);
                }

                // Face extent calculation referenced from vkQuake
                let bmin = (world_lightmap_rect.min / 16.).floor().as_ivec2();
                let bmax = (world_lightmap_rect.max / 16.).ceil().as_ivec2();
                let face_lightmap_size = (bmax - bmin).as_uvec2() + 1;
                // let face_lightmap_size_other = (world_lightmap_rect.size() / 16.).ceil().as_uvec2() + 1;
                // let face_lightmap_size = face_extents.extents.as_uvec2() / 16 + 1;
                // println!("{face_idx} | rect: {world_lightmap_rect:?}, min: {bmin}, max: {bmax}, out: {face_lightmap_size}, other: {face_lightmap_size_other}");

                /* lightmap_packer.pack_own(face_idx, image::RgbImage::from_fn(face_lightmap_size.x, face_lightmap_size.y, |x, y| {
                    // if x == 0 && y == 0 { return image::Rgb([0, 255, 0]) } 
                    // image::Rgb(lighting.get(face.lightmap_offset as usize + (y * face_lightmap_size.x + x) as usize).unwrap_or_default())
                    // let normal = ((if face.plane_side == 0 { plane.normal } else { -plane.normal }).normalize() * 255.).abs();
                    // [normal.x as u8, normal.y as u8, normal.z as u8].into()
                    [255; 3].into()
                })).unwrap();
                let frame = lightmap_packer.get_frame(&face_idx).unwrap().frame; */
                let lightmap_image = if face.lightmap_offset.is_negative() {
                    image::RgbImage::from_pixel(face_lightmap_size.x, face_lightmap_size.y, image::Rgb([0; 3]))
                } else {
                    image::RgbImage::from_fn(face_lightmap_size.x, face_lightmap_size.y, |x, y| {
                        // if x == 0 && y == 0 { return image::Rgb([0, 255, 0]) } 
                        image::Rgb(lighting.get(face.lightmap_offset as usize + (y * face_lightmap_size.x + x) as usize).unwrap_or_default())
                        // let normal = ((if face.plane_side == 0 { plane.normal } else { -plane.normal }).normalize() * 255.).abs();
                        // [normal.x as u8, normal.y as u8, normal.z as u8].into()
                        // [255; 3].into()
                    })
                };
                let Some(frame) = lightmap_packer.pack(face_idx, lightmap_image) else {
                    panic!("Failed to pack image of size {face_lightmap_size}");
                };
                
                // println!("{face_idx}: {}", frame.min);
                // println!("{}", world_lightmap_rect.min - face_extents.texture_mins.as_vec2());

                // Append lightmap uvs, since lightmap face size is calculated from the uvs bounds, we don't need to resize it, just move it into place
                // Atlas uvs will be in texture space until converted later
                lightmap_uvs.extend(lightmap_world_uvs.into_iter().map(|mut uv| {
                    // uv -= face_extents.texture_mins.as_vec2();
                    uv -= (bmin * 16).as_vec2();
                    // Offset by half a texel to remove bleeding artifacts
                    uv += 8.;
                    // 16 Units per texel
                    uv /= 16.;
                    uv += frame.min;
                    // uv += vec2(frame.x as f32, frame.y as f32);

                    uv
                }));
            }

            // TODO why do i do this, do i need to do this?
            mesh.indices.dedup();
            
            meshes.push(mesh);
        }

        // Finalize lightmap atlas
        let lightmap_atlas = if self.lighting.is_some() {
            let image = lightmap_packer.export([0; 3]); // TODO make customizable

            // Normalize lightmap UVs from texture space
            let atlas_size = vec2(image.width() as f32, image.height() as f32);
            for mesh in &mut meshes {
                let Some(lightmap_uvs) = &mut mesh.lightmap_uvs else { continue };

                for uv in lightmap_uvs {
                    *uv /= atlas_size; // * 16
                    // assert!(uv.x <= 1.); // TODO tmp
                    // assert!(uv.y <= 1.);
                }
            }

            Some(image)
        } else {
            None
        };
        
        MeshExportOutput {
            meshes,
            lightmap_atlas,
        }
    }
}

/// A trait for packing textures into a texture atlas. Specifically using image::RgbImage.
trait AtlasPacker {
    fn pack(&mut self, face_idx: u32, image: image::RgbImage) -> Option<Rect>;
    fn export(&self, default: [u8; 3]) -> image::RgbImage;
}

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
    images: Vec<(texture_packer::Frame<u32>, image::RgbImage)>,
}
impl DefaultLightmapPacker {
    pub fn new(config: TexturePackerConfig) -> Self {
        Self { packer: TexturePacker::new_skyline(config), images: Vec::new() }
    }
}
impl AtlasPacker for DefaultLightmapPacker {
    fn pack(&mut self, face_idx: u32, image: image::RgbImage) -> Option<Rect> {
        self.packer.pack_own(face_idx, DummyTexture { width: image.width(), height: image.height() }).ok()?;
        self.packer.get_frame(&face_idx).map(|frame| {
            self.images.push((frame.clone(), image));
            let min = vec2(frame.frame.x as f32, frame.frame.y as f32);
            Rect { min, max: min + vec2(frame.frame.w as f32, frame.frame.h as f32) }
        })
    }
    fn export(&self, default: [u8; 3]) -> image::RgbImage {
        let mut image = image::RgbImage::from_pixel(self.packer.width(), self.packer.height(), image::Rgb(default));
        for (frame, lightmap_image) in &self.images {
            // let lightmap_image = &self.images[*face_idx as usize];
            for x in 0..frame.frame.w {
                for y in 0..frame.frame.h {
                    if frame.frame.x + x >= image.width() || frame.frame.y + y >= image.height() || x >= lightmap_image.width() || y >= lightmap_image.height() {
                        continue;
                    }
                    *image.get_pixel_mut(frame.frame.x + x, frame.frame.y + y) = *lightmap_image.get_pixel(x, y);
                }
            }
        }
        image
    }
}
/* impl AtlasPacker for TexturePacker<'_, image::RgbImage, u32> {
    fn pack(&mut self, face_idx: u32, image: image::RgbImage) -> Option<Rect> {
        self.pack_own(face_idx, image).ok()?;
        self.get_frame(&face_idx).map(|frame| {
            let min = vec2(frame.frame.x as f32, frame.frame.y as f32);
            Rect { min, max: min + vec2(frame.frame.w as f32, frame.frame.h as f32) }
        })
    }
    fn export(&self, default: [u8; 3]) -> image::RgbImage {
        image::RgbImage::from_fn(
            self.width(),
            self.height(),
            // The time complexity of get() looks REALLY BAD, replace ASAP
            |x, y| self.get(x, y).unwrap_or(image::Rgb(default)),
        )
    }
} */

// struct ShelfPacker {
//     allocator: AtlasAllocator,
//     images: Vec<image::RgbImage>,
// }
// impl Default for ShelfPacker {
//     fn default() -> Self {
//         Self {
//             allocator: AtlasAllocator::new(size2(1024, 1024)),
//             images: Vec::new(),
//         }
//     }
// }
// impl AtlasPacker for ShelfPacker {
//     fn pack(&mut self, image: image::RgbImage) -> Option<Rect> {
//         self.allocator.allocate(size2(image.width(), image.height()).to_i32()).map(|alloc| {
//             self.images.push(image);
//             let rect = alloc.rectangle.to_f32();
//             Rect { min: Vec2::from_array(rect.min.to_array()), max: Vec2::from_array(rect.max.to_array()) }
//         })
//     }
//     fn export(&self) -> image::RgbImage {
//         let mut image = image::RgbImage::new(self.allocator.size().width as u32, self.allocator.size().height as u32);
//         for allocation in self.allocator.iter() {
//             let sub_image = self.allocator.get_index(allocation.id);
//             let min = allocation.rectangle.min;
//             let size = allocation.rectangle.size();
//             // image.view
//             image.sub_image(min.x, min.y, size.width, size.height).
//         }
//         image
//     }
// }



/* 
for (texture, faces) in grouped_faces {
            let mut mesh = ExportedMesh::default();
            mesh.texture = texture.to_string();

            for face in faces {
                let plane = &self.planes[face.plane_idx as usize];
                let tex_info = &self.tex_info[face.texture_info_idx as usize];
                // let face_extents = BspFaceExtents::calculate(self, face);
                let texture_size = self.textures[tex_info.texture_idx as usize].as_ref()
                    .map(|tex| vec2(tex.header.width as f32, tex.header.height as f32))
                    .unwrap_or(Vec2::ONE);


                // The uv coordinates of the face's lightmap in the world, rather than on a lightmap atlas
                let mut lightmap_world_uvs: Vec<Vec2> = Default::default();

                let first_index = mesh.positions.len() as u32;
                for i in face.first_edge..face.first_edge + face.num_edges {
                    let surf_edge = self.surface_edges[i as usize];
                    let edge = self.edges[surf_edge.abs() as usize];
                    let vert_idx = if surf_edge.is_negative() { (edge.b, edge.a) } else { (edge.a, edge.b) };

                    let pos = self.vertices[vert_idx.0 as usize];

                    mesh.positions.push(pos);
                    mesh.normals.push(if face.plane_side == 0 { plane.normal } else { -plane.normal });

                    let uv = vec2(
                        // Counteract the trenchbroom_to_bevy_space conversion by multiplying by scale twice
                        // TODO is there a more elegant way of fixing this?
                        pos.dot(tex_info.u_axis) + tex_info.u_offset,
                        pos.dot(tex_info.v_axis) + tex_info.v_offset,
                    );

                    mesh.uvs.push(uv / texture_size);
                    // Lightmap uvs have a constant scale of 16-units to 1 texel
                    lightmap_world_uvs.push(uv);
                }

                // Calculate indices
                for i in 1..face.num_edges - 1 {
                    mesh.indices.push([0, i + 1, i].map(|x| first_index + x));
                }

                //////////////////////////////////////////////////////////////////////////////////
                //// LIGHTMAP
                //////////////////////////////////////////////////////////////////////////////////

                let Some(lighting) = &self.lighting else { continue };
                let uvs_light = mesh.lightmap_uvs.get_or_insert_with(Vec::new);
                // TODO does this happen?
                /* if face.lightmap_offset.is_negative() {
                    // Just in case only some faces are negative (Not sure why this happens)
                    uvs_light.extend(repeat_n(Vec2::ZERO, face.num_edges as usize));
                    continue;
                } */
                // match lighting {
                //     BspLighting::White(_) => println!("white"),
                //     &BspLighting::Colored(_) => println!("colored"),
                // }

                let mut world_lightmap_rect = Rect::EMPTY;
                for uv in &lightmap_world_uvs {
                    world_lightmap_rect = world_lightmap_rect.union_point(*uv);
                }
                let face_lightmap_size = world_lightmap_rect.size().ceil().as_uvec2() / 16;
                // let face_lightmap_size = face_extents.extents.as_uvec2() / 16 + 1; // why +1??
                // println!("{size}");
                // println!("rect: {lightmap_rect:?}, size: {size}");
                // println!("rect: {:?}", Rect::EMPTY.union_point(vec2(13., 15.)));

                let mut target_pos: Option<UVec2> = None;
                // Brute force search for free space
                if lightmap_atlas.cols() as u32 >= face_lightmap_size.x && lightmap_atlas.rows() as u32 >= face_lightmap_size.y {
                    'find_loop: for x in 0..lightmap_atlas.cols().saturating_sub(face_lightmap_size.x as usize) {
                        'next_position: for y in 0..lightmap_atlas.rows().saturating_sub(face_lightmap_size.y as usize) {
                            // Check the rect against this position
                            for local_x in 0..face_lightmap_size.x {
                                for local_y in 0..face_lightmap_size.y {
                                    // If this cell has already been taken, continue
                                    if lightmap_atlas[(y + local_y as usize, x + local_x as usize)].is_some() {
                                        continue 'next_position;
                                    }
                                }
                            }
                            // If we get here, this spot is good.
                            target_pos = Some(uvec2(x as u32, y as u32));

                            break 'find_loop;
                        }
                    }
                }
                let target_pos = match target_pos {
                    Some(pos) => pos,
                    None => {
                        // println!("Made room: {face_lightmap_size}");
                        // We couldn't find a spot for this lightmap, let's make some room!
                        let prev_cols = lightmap_atlas.cols();

                        // println!("From ({}, {})", lightmap_atlas.cols(), lightmap_atlas.rows());
                        for _ in 0..face_lightmap_size.x {
                            lightmap_atlas.push_col(vec![None; lightmap_atlas.rows()]);
                        }
                        for _ in 0..face_lightmap_size.y {
                            lightmap_atlas.push_row(vec![None; lightmap_atlas.cols()]);
                        }
                        // println!("To ({}, {})", lightmap_atlas.cols(), lightmap_atlas.rows());
                        // println!();

                        uvec2(prev_cols as u32, 0)
                    }
                };

                // let lighting_value = Some([fastrand::u8(..), fastrand::u8(..), fastrand::u8(..)]);
                for y in 0..face_lightmap_size.y {
                    for x in 0..face_lightmap_size.x {
                        let pos = target_pos + uvec2(x, y);
                        // println!("pos: {pos}, size: {}, {}", lightmap_atlas.cols(), lightmap_atlas.rows());
                        let lighting_idx = face.lightmap_offset as usize + (y * face_lightmap_size.x + x) as usize;
                        // let mut lighting_value = lighting.get(lighting_idx);
                        // if lighting_idx == 0 {
                        //     lighting_value = Some([0, 255, 0]);
                        // }
                        let normal = ((if face.plane_side == 0 { plane.normal } else { -plane.normal }).normalize() * 255.).abs();
                        // println!("{normal}");
                        let mut lighting_value = Some([normal.x as u8, normal.y as u8, normal.z as u8]);
                        // let lighting_value = Some([255; 3]);

                        if lighting_value.is_none() { // TODO remove probably
                            eprintln!("Lighting index {lighting_idx} is out of bounds of lighting of length {}, this is a bug!!!", lighting.len());
                        }

                        lightmap_atlas[(pos.y as usize, pos.x as usize)] = lighting_value;
                    }
                }

                // Append lightmap uvs, since lightmap face size is calculated from the uvs bounds, we don't need to resize it, just move it into place
                // Atlas uvs will be in texture space until converted later
                uvs_light.extend(lightmap_world_uvs.into_iter().map(|mut uv| {
                    let normalized = (uv - world_lightmap_rect.min) / world_lightmap_rect.size();
                    // let normalized = (uv - face_extents.texture_mins.as_vec2()) / world_lightmap_rect.size();
                    // dbg!(normalized);
                    // let scaled = normalized * face_lightmap_size.as_vec2() + target_pos.as_vec2();
                    let scaled = normalized + target_pos.as_vec2();
                    // println!();
                    scaled
                    // uv -= face_extents.texture_mins.as_vec2();
                    // uv -= world_lightmap_rect.min;
                    // uv /= vec2(lightmap_atlas.cols() as f32, lightmap_atlas.rows() as f32);
                    // uv += target_pos.as_vec2();
                    // uv += 8.;
                    // uv
                }));
                // uvs_light.extend([
                //     target_pos,
                //     uvec2(target_pos.x + face_lightmap_size.x, target_pos.y),
                //     uvec2(target_pos.x + face_lightmap_size.x, target_pos.y + face_lightmap_size.y),
                //     uvec2(target_pos.x, target_pos.y + face_lightmap_size.y),
                // ].map(|v| v.as_vec2() / vec2(lightmap_atlas.cols() as f32, lightmap_atlas.rows() as f32)));
            }
            // for uv in &uvs_light {
            //     assert!(uv.x <= lightmap_atlas.cols() as f32, "uv x out of bounds: {} > {}", uv.x, lightmap_atlas.cols());
            //     assert!(uv.y <= lightmap_atlas.rows() as f32, "uv y out of bounds: {} > {}", uv.y, lightmap_atlas.rows());
            // }
            // assert!(uvs_light.iter().all(|uv| uv.x <= lightmap_atlas.cols() as f32 && uv.y <= lightmap_atlas.rows() as f32));

            // TODO why do i do this, do i need to do this?
            mesh.indices.dedup();

            meshes.push(mesh);
        }

        // Finalize lightmap atlas
        let lightmap_atlas = if self.lighting.is_some() {
            let image = image::RgbImage::from_fn(
                lightmap_atlas.cols() as u32,
                lightmap_atlas.rows() as u32,
                |x, y| image::Rgb(lightmap_atlas.get(x, y).copied().flatten().unwrap_or([0; 3])),
            );

            // Normalize lightmap UVs from texture space
            let atlas_size = vec2(lightmap_atlas.cols() as f32, lightmap_atlas.rows() as f32);
            for mesh in &mut meshes {
                let Some(lightmap_uvs) = &mut mesh.lightmap_uvs else { continue };

                for uv in lightmap_uvs {
                    *uv /= atlas_size;
                    assert!(uv.x <= 1.); // TODO tmp
                    assert!(uv.y <= 1.);
                }
            }

            Some(image)
        } else {
            None
        };
*/