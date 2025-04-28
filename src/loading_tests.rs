use crate::*;

#[derive(Clone, Copy)]
pub struct TestingBsp {
	pub name: &'static str,
	pub bsp: &'static [u8],
	pub lit: Option<&'static [u8]>,
}

macro_rules! testing_bsp {
	($name:literal) => {
		TestingBsp {
			name: $name,
			bsp: include_bytes!(concat!("../assets/", $name)),
			lit: None,
		}
	};
	($name:literal, $lit:literal) => {
		TestingBsp {
			name: $name,
			bsp: include_bytes!(concat!("../assets/", $name)),
			lit: Some(include_bytes!(concat!("../assets/", $lit))),
		}
	};
}

pub static EXAMPLE_BSP: TestingBsp = testing_bsp!("example.bsp");

pub static TESTING_BSPS: &[TestingBsp] = &[
	testing_bsp!("ad_crucial.bsp", "ad_crucial.lit"),
	testing_bsp!("ad_end.bsp", "ad_end.lit"),
	testing_bsp!("ad_tears.bsp", "ad_tears.lit"),
	EXAMPLE_BSP,
];

#[test]
fn use_bspx_rgb_lighting() {
	let with_usage = BspData::parse(BspParseInput {
		bsp: EXAMPLE_BSP.bsp,
		lit: None,
		settings: BspParseSettings { use_bspx_rgb_lighting: true },
	})
	.unwrap();

	let without_usage = BspData::parse(BspParseInput {
		bsp: EXAMPLE_BSP.bsp,
		lit: None,
		settings: BspParseSettings {
			use_bspx_rgb_lighting: false,
		},
	})
	.unwrap();

	assert!(matches!(with_usage.lighting, Some(BspLighting::Colored(_))));
	assert!(without_usage.lighting.is_none());
}

#[test]
fn lit_loading() {
	for TestingBsp { name, bsp, lit } in TESTING_BSPS.iter().copied() {
		println!("{name}");
		if lit.is_none() {
			continue;
		};

		let data = BspData::parse(BspParseInput {
			bsp,
			lit,
			settings: BspParseSettings::default(),
		})
		.unwrap();

		assert!(matches!(data.lighting, Some(BspLighting::Colored(_))));
	}
}

#[test]
fn validate_bounds() {
	#[track_caller]
	fn validate_node_ref(node_ref: &BspNodeRef, data: &BspData) {
		match node_ref {
			BspNodeRef::Node(node_idx) => assert!(*node_idx < data.nodes.len().max(1) as u32),
			BspNodeRef::Leaf(leaf_idx) => assert!(*leaf_idx < data.leaves.len().max(1) as u32),
		}
	}

	#[track_caller]
	fn validate_range(start: u32, num: u32, len: usize) {
		if num == 0 {
			return;
		}
		assert!((start as usize + num.saturating_sub(1) as usize) < len)
	}

	for TestingBsp { name, bsp, lit } in TESTING_BSPS.iter().copied() {
		println!("{name}");

		let data = BspData::parse(BspParseInput {
			bsp,
			lit,
			settings: BspParseSettings::default(),
		})
		.unwrap();

		// We max(0) the lengths here to make index 0 a valid index for a length of 0

		for node in &data.nodes {
			assert!(node.plane_idx < data.planes.len().max(1) as u32);
			validate_range(node.face_idx.0, node.face_num.0, data.faces.len());
			validate_node_ref(&node.front.0, &data);
			validate_node_ref(&node.back.0, &data);
		}

		for tex_info in &data.tex_info {
			assert!(tex_info.texture_idx < data.textures.len().max(1) as u32);
		}

		for face in &data.faces {
			validate_range(face.first_edge, face.num_edges.0, data.surface_edges.len());
			assert!(face.texture_info_idx.0 < data.tex_info.len().max(1) as u32);
			if let Some(lighting) = &data.lighting {
				assert!((face.lightmap_offset as i64) < lighting.len().max(1) as i64);
			}
		}

		for clip_node in &data.clip_nodes {
			assert!(clip_node.plane_idx < data.planes.len().max(1) as u32);
			assert!((clip_node.front.0 as i64) < data.clip_nodes.len().max(1) as i64);
			assert!((clip_node.back.0 as i64) < data.clip_nodes.len().max(1) as i64);
		}

		for leaf in &data.leaves {
			// TODO PVS support
			// assert!((leaf.vis_list as i64) < data.visibility.len() as i64);
			validate_range(leaf.face_idx.0, leaf.face_num.0, data.mark_surfaces.len());
		}

		for surface_idx in &data.mark_surfaces {
			assert!(surface_idx.0 < data.faces.len().max(1) as u32);
		}

		for edge in &data.edges {
			assert!(edge.a.0 < data.vertices.len().max(1) as u32);
			assert!(edge.b.0 < data.vertices.len().max(1) as u32);
		}

		for surface_edge in &data.surface_edges {
			assert!((surface_edge.unsigned_abs()) < data.edges.len().max(1) as u32);
		}

		for model in &data.models {
			validate_node_ref(&model.head_bsp_node, &data);
			assert!((model.first_clip_node as i64) < data.clip_nodes.len().max(1) as i64);
			assert!((model.second_clip_node as i64) < data.clip_nodes.len().max(1) as i64);

			validate_range(model.first_face, model.num_faces, data.faces.len());
		}
	}
}
