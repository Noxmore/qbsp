//! Turning [`BspData`] into a renderable mesh.

pub mod lightmap;

use crate::*;

/// A mesh exported from a BSP file for rendering.
#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
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

/// The output of [`BspData::mesh_model`]. Contains one mesh for each texture used in the model.
pub struct MeshModelOutput {
	pub meshes: Vec<ExportedMesh>,
}

impl BspData {
	// TODO I would like this to be more powerful, being able to change things like where meshes split and such would be nice, but i can't think of a good API for it.
	//      Also, support PVS data.

	/// Meshes a model at the specified index. Returns one mesh for each texture used in the model.
	pub fn mesh_model(&self, model_idx: usize, lightmap_uvs: Option<&LightmapUvMap>) -> MeshModelOutput {
		let model = &self.models[model_idx];

		// Group faces by texture, also storing index for packing use
		let mut grouped_faces: HashMap<(&str, BspTexFlags), Vec<(u32, &BspFace)>> = Default::default();

		for i in model.first_face..model.first_face + model.num_faces {
			let face = &self.faces[i as usize];
			let tex_info = &self.tex_info[face.texture_info_idx.0 as usize];
			let Some(texture) = &self.textures[tex_info.texture_idx as usize] else { continue };

			grouped_faces
				.entry((texture.header.name.as_str(), tex_info.flags))
				.or_default()
				.push((i, face));
		}

		let mut meshes = Vec::with_capacity(grouped_faces.len());

		for ((texture, tex_flags), faces) in grouped_faces {
			let mut mesh = ExportedMesh { ..Default::default() };
			mesh.texture = texture.to_string();
			mesh.tex_flags = tex_flags;

			for (face_idx, face) in faces {
				mesh.faces.push(face_idx);

				let plane = &self.planes[face.plane_idx.0 as usize];
				let tex_info = &self.tex_info[face.texture_info_idx.0 as usize];
				let texture_size = self.textures[tex_info.texture_idx as usize]
					.as_ref()
					.map(|tex| vec2(tex.header.width as f32, tex.header.height as f32))
					.unwrap_or(Vec2::ONE);

				// The uv coordinates of the face's lightmap in the world, rather than on a lightmap atlas
				let mut lightmap_world_uvs: Vec<Vec2> = Vec::with_capacity(face.num_edges.0 as usize);

				let first_index = mesh.positions.len() as u32;
				for pos in face.vertices(self) {
					mesh.positions.push(pos);
					mesh.normals.push(if face.plane_side.0 == 0 { plane.normal } else { -plane.normal });

					let uv = tex_info.projection.project(pos);

					mesh.uvs.push(uv / texture_size);
					lightmap_world_uvs.push(uv);
				}

				// Calculate indices
				for i in 1..face.num_edges.0 - 1 {
					mesh.indices.push([0, i + 1, i].map(|x| first_index + x));
				}

				// Insert lightmap uvs
				if let Some(uv_map) = lightmap_uvs {
					if let Some(uvs) = uv_map.get(&face_idx) {
						assert_eq!(uvs.len(), face.num_edges.0 as usize);
						mesh.lightmap_uvs.get_or_insert_with(Vec::new).extend(uvs);
					}
				}
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
