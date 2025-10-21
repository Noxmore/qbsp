use crate::{idtech2::BspNodeRef, *};

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
	testing_bsp!("librequake/lq_e0m1.bsp"),
	testing_bsp!("librequake/lq_e0m2.bsp"),
	testing_bsp!("librequake/lq_e0m3.bsp"),
	testing_bsp!("librequake/lq_e0m4.bsp"),
	testing_bsp!("librequake/lq_e0m1-quake2.bsp"),
	testing_bsp!("librequake/lq_e0m2-quake2.bsp"),
	testing_bsp!("librequake/lq_e0m3-quake2.bsp"),
	testing_bsp!("librequake/lq_e0m4-quake2.bsp"),
	// I couldn't find any FOSS Goldsrc .bsp files and couldn't
	// work out how to compile my own in a way that didn't end
	// up compiling proprietary HL1 textures into the .bsp,
	// but if you put your own copy of some HL1 maps into the
	// `assets/halflife` directory you can test this code
	// against them.
	// testing_bsp!("halflife/c0a0.bsp"),
	// testing_bsp!("halflife/c0a0a.bsp"),
	// testing_bsp!("halflife/c0a0b.bsp"),
	// testing_bsp!("halflife/c0a0c.bsp"),
	// testing_bsp!("halflife/c0a0d.bsp"),
	// testing_bsp!("halflife/c0a0e.bsp"),
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
		let data = BspData::parse(BspParseInput {
			bsp,
			lit,
			settings: BspParseSettings::default(),
		})
		.unwrap();

		if name.ends_with("-quake2.bsp") {
			assert_eq!(
				data.format,
				BspFormat::BSP38,
				"{name} was of wrong format (expecting BSP38, found {})",
				data.format
			);
			assert!(data.version.is_some());
		}

		if name.starts_with("halflife") {
			assert_eq!(
				data.format,
				BspFormat::BSP30,
				"{name} was of wrong format (expecting BSP30, found {})",
				data.format
			);
			assert!(data.version.is_none());
		}

		// We max(0) the lengths here to make index 0 a valid index for a length of 0

		for node in &data.nodes {
			assert!(node.plane_idx < data.planes.len().max(1) as u32);
			validate_range(node.face_idx.0, node.face_num.0, data.faces.len());
			validate_node_ref(&node.front, &data);
			validate_node_ref(&node.back, &data);
		}

		for tex_info in &data.tex_info {
			if let Some(texture_idx) = *tex_info.texture_idx {
				assert!(texture_idx < data.textures.len().max(1) as u32);
			} else {
				assert!(tex_info.extra_info.is_some());
			}
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
			assert!(clip_node.front.leaf().is_some() || (clip_node.front.node().unwrap() as usize) < data.clip_nodes.len().max(1));
			assert!(clip_node.back.leaf().is_some() || (clip_node.back.node().unwrap() as usize) < data.clip_nodes.len().max(1));
		}

		for leaf in &data.leaves {
			match leaf.vis_list {
				VisdataRef::VisLeaves(vis_list) => {
					assert!((vis_list as i64) < data.visibility.len() as i64);
					validate_range(leaf.face_idx.0, leaf.face_num.0, data.mark_surfaces.len());
				}
				VisdataRef::Cluster(cluster) => {
					assert!((cluster as i64) < data.visibility.len() as i64);
				}
			}
			// TODO PVS support
		}

		for surface_idx in &data.mark_surfaces {
			assert!(dbg!(surface_idx.0) < dbg!(data.faces.len().max(1)) as u32, "Failed for {name}");
		}

		for edge in &data.edges {
			assert!(edge.a.0 < data.vertices.len().max(1) as u32);
			assert!(edge.b.0 < data.vertices.len().max(1) as u32);
		}

		for surface_edge in &data.surface_edges {
			assert!((surface_edge.unsigned_abs()) < data.edges.len().max(1) as u32);
		}

		for model in &data.models {
			validate_node_ref(&model.root_hulls.bsp_root, &data);
			if let Some(clip_nodes) = model.root_hulls.clip_nodes {
				assert!((clip_nodes[0].node().unwrap() as usize) < data.clip_nodes.len().max(1));
				assert!((clip_nodes[1].node().unwrap() as usize) < data.clip_nodes.len().max(1));
			}

			validate_range(model.first_face, model.num_faces, data.faces.len());
		}
	}
}
