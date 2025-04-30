use qbsp::prelude::*;
use std::{env, fs};

fn main() {
	let path = env::args()
		.nth(1)
		.expect("Supply 1 argument corresponding to a map in the assets folder.");

	eprintln!("Reading BSP {path}.bsp");
	let data = BspData::parse(BspParseInput {
		bsp: &fs::read(format!("assets/{path}.bsp")).expect("Failed to read bsp file"),
		lit: fs::read(format!("assets/{path}.lit")).ok().as_deref(),
		settings: BspParseSettings::default(),
	})
	.unwrap();

	let lightmap_settings = ComputeLightmapSettings {
		extrusion: 1,
		..Default::default()
	};

	fs::remove_dir_all("target/lightmaps").ok();
	fs::create_dir("target/lightmaps").unwrap();

	fs::create_dir("target/lightmaps/per-slot").unwrap();

	eprintln!("Computing per slot lightmap atlas");
	let atlas = data.compute_lightmap_atlas(PerSlotLightmapPacker::new(lightmap_settings)).unwrap();
	for (slot_idx, slot) in atlas.data.slots.into_iter().enumerate() {
		slot.save_with_format(format!("target/lightmaps/per-slot/slot_{slot_idx}.png"), image::ImageFormat::Png)
			.unwrap();
	}
	atlas
		.data
		.styles
		.save_with_format("target/lightmaps/per-slot/styles.png", image::ImageFormat::Png)
		.unwrap();

	fs::create_dir("target/lightmaps/per-style").unwrap();
	eprintln!("Computing per style lightmap atlas");
	let atlas = data.compute_lightmap_atlas(PerStyleLightmapPacker::new(lightmap_settings)).unwrap();
	for (style, image) in atlas.data.inner() {
		image
			.save_with_format(format!("target/lightmaps/per-style/{}.png", style.0), image::ImageFormat::Png)
			.unwrap();
	}
}
