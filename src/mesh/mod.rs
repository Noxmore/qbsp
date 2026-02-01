//! Module for turning [`BspData`] into a renderable mesh.

use std::collections::HashMap;

use glam::{IVec2, UVec2, Vec2, Vec3, Vec4, vec2};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::{
	BspData,
	data::{
		bspx::DecoupledLightmap,
		models::BspFace,
		texture::{BspTexFlags, TextureName},
	},
	util::Rect,
};

pub mod lightmap;

/// A mesh exported from a BSP file for rendering, collision, etc.
/// This is an in-between format that you should convert into your engine's mesh structure.
#[derive(Debug, Clone, Default)]
pub struct ExportedMesh {
	/// Positions of vertices in this mesh. NOTE: These are in Z-up coordinate space.
	pub positions: Vec<Vec3>,

	/// Normal vectors of vertices in this mesh. NOTE: These are in Z-up coordinate space.
	pub normals: Vec<Vec3>,

	/// Tangent vectors of vertices in this mesh. NOTE: These are in Z-up coordinate space.
	/// 
	/// The fourth element is the bitangent sign so that
	/// ```ignore
	/// bitangent = cross(normal, tangent.xyz) * tangent.w
	/// ```
	///
	/// These are only set if the BSP contains the `FACENORMALS` BSPX lump.
	pub tangents: Option<Vec<Vec4>>,

	/// Normalized texture coordinates. (0..1)
	pub uvs: Vec<Vec2>,

	/// `true` if the uvs have been scaled based on texture size, otherwise `false`.
	///
	/// Scaling will not occur if the texture's size isn't stored in the BSP, such as in BSP38 (Quake 2) files.
	/// For those, it is up to you to divide all elements of [`ExportedMesh::uvs`] by the size of the texture by the name of [`ExportedMesh::texture`].
	pub prescaled_uvs: bool,

	/// Optional uvs for the lightmap atlas.
	pub lightmap_uvs: Option<Vec<Vec2>>,

	/// Triangle list.
	pub indices: Vec<[u32; 3]>,

	pub tex_flags: BspTexFlags,

	/// All faces in the bsp data used to create this mesh.
	pub faces: Vec<u32>,

	pub texture: Option<TextureName>,
}

/// The output of [`BspData::mesh_model`]. Contains one mesh for each texture used in the model.
pub struct MeshModelOutput {
	pub meshes: Vec<ExportedMesh>,
}

impl BspData {
	// TODO I would like this to be more powerful, being able to change things like where meshes split and such would be nice, but i can't think of a good API for it.
	//      Also, support PVS data.

	/// Meshes a model at the specified index. Returns one mesh for each texture used in the model.
	pub fn mesh_model(&self, model_idx: usize, lightmap_uvs: Option<&lightmap::LightmapUvMap>) -> MeshModelOutput {
		let model = &self.models[model_idx];

		// Group faces by texture, also storing index for packing use
		type MaterialKey = (Option<TextureName>, BspTexFlags);
		type FacesWithIndex<'a> = Vec<(u32, &'a BspFace)>;
		let mut grouped_faces: HashMap<MaterialKey, FacesWithIndex> = Default::default();

		for i in model.first_face..model.first_face + model.num_faces {
			let face = &self.faces[i as usize];
			let tex_info = &self.tex_info[face.texture_info_idx.0 as usize];
			let name = self.get_texture_name(tex_info);

			grouped_faces
				.entry((name, tex_info.flags.texture_flags.unwrap_or_default()))
				.or_default()
				.push((i, face));
		}

		let mut meshes = Vec::with_capacity(grouped_faces.len());

		for ((texture, tex_flags), faces) in grouped_faces {
			let mut mesh = ExportedMesh {
				texture,
				tex_flags,
				..Default::default()
			};

			// If this is empty, the mesh will have `None`.
			let mut tangents = Vec::new();

			for (face_idx, face) in faces {
				mesh.faces.push(face_idx);

				let plane = &self.planes[face.plane_idx.0 as usize];
				let tex_info = &self.tex_info[face.texture_info_idx.0 as usize];
				let texture_size = tex_info
					.texture_idx
					.and_then(|idx| self.textures.get(idx as usize))
					.and_then(|tex| tex.as_ref())
					.map(|tex| vec2(tex.header.width as f32, tex.header.height as f32));

				// The uv coordinates of the face's lightmap in the world, rather than on a lightmap atlas
				let mut lightmap_world_uvs: Vec<Vec2> = Vec::with_capacity(face.num_edges.0 as usize);

				let first_index = mesh.positions.len() as u32;
				for (vertex_idx, pos) in face.vertices(self).enumerate() {
					mesh.positions.push(pos);
					if let Some(face_normals) = &self.bspx.face_normals {
						let vertex_normal_idx = face_normals.faces[face_idx as usize].vertex_start as usize + vertex_idx;
						let vertex_normal_info = face_normals.face_vertices[vertex_normal_idx];

						let normal = face_normals.unique_vecs[vertex_normal_info.normal_idx as usize];
						let tangent = face_normals.unique_vecs[vertex_normal_info.tangent_idx as usize];
						let bi_tangent = face_normals.unique_vecs[vertex_normal_info.bi_tangent_idx as usize];

						mesh.normals.push(normal);

						let cross_bi_tangent = normal.cross(tangent);
						let bi_tangent_sign = if cross_bi_tangent.dot(bi_tangent) < 0. { -1. } else { 1. };
						
						tangents.push(tangent.extend(bi_tangent_sign));
					} else {
						mesh.normals.push(if face.plane_side.0 == 0 { plane.normal } else { -plane.normal });
					}

					let uv = tex_info.projection.project(pos);

					mesh.prescaled_uvs = texture_size.is_some();
					mesh.uvs.push(if let Some(texture_size) = texture_size { uv / texture_size } else { uv });
					lightmap_world_uvs.push(uv);
				}

				// Calculate indices
				for i in 1..face.num_edges.0 - 1 {
					mesh.indices.push([0, i + 1, i].map(|x| first_index + x));
				}

				// Insert lightmap uvs
				if let Some(uv_map) = lightmap_uvs
					&& let Some(uvs) = uv_map.get(&face_idx)
				{
					assert_eq!(uvs.len(), face.num_edges.0 as usize);
					mesh.lightmap_uvs.get_or_insert_with(Vec::new).extend(uvs);
				}
			}

			if !tangents.is_empty() {
				mesh.tangents = Some(tangents);
			}

			meshes.push(mesh);
		}

		MeshModelOutput { meshes }
	}
}

/// Computed extents of a face for various calculations, mainly involving lightmaps.
#[derive(Debug, Clone, Copy, Default)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct FaceExtents {
	face_rect: Rect<Vec2>,

	lightmap_rect: Rect<IVec2>,
	lightmap_size: UVec2,
	lightmap_pixels: u32,

	precomputed_uv_snap: bool,
}

impl FaceExtents {
	/// Calculates face extents from unscaled UVs.
	pub fn new(uvs: impl IntoIterator<Item = Vec2>) -> Self {
		let mut extents = Self {
			face_rect: Rect::EMPTY,
			..Default::default()
		};

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

	pub fn new_decoupled(uvs: impl IntoIterator<Item = Vec2>, lm_info: &DecoupledLightmap) -> Self {
		let mut extents = Self {
			face_rect: Rect::EMPTY,
			precomputed_uv_snap: true,
			..Default::default()
		};

		for uv in uvs {
			extents.face_rect = extents.face_rect.union_point(uv);
		}

		extents.lightmap_size = lm_info.size.as_uvec2();

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

	/// `true` if the projection data implicitly puts the uvs at around 0, 0
	#[inline]
	pub fn precomputed_uv_snap(&self) -> bool {
		self.precomputed_uv_snap
	}

	/// Computes texture-space lightmap UVs, provide the same set of face UVs supplied to [`FaceExtents::new`], and the position of the lightmap on the atlas.
	pub fn compute_lightmap_uvs<'a>(&'a self, uvs: impl IntoIterator<Item = Vec2> + 'a, lightmap_position: Vec2) -> impl Iterator<Item = Vec2> + 'a {
		uvs.into_iter().map(move |mut uv| {
			if !self.precomputed_uv_snap {
				// Move from world space into top left corner
				uv -= (self.lightmap_rect.min * 16).as_vec2();
				// Offset by half a texel to remove bleeding artifacts
				uv += 8.;
				// 16 Units per texel
				uv /= 16.;
			}
			// Finally, move to where the lightmap starts
			uv += lightmap_position;

			uv
		})
	}
}
