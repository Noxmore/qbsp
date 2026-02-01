# QBSP

[![crates.io](https://img.shields.io/crates/v/qbsp)](https://crates.io/crates/qbsp)
[![docs.rs](https://docs.rs/qbsp/badge.svg)](https://docs.rs/qbsp)

Rust crate for parsing, and operating with Quake 1, 2, and GoldSrc BSP files.

## Features
- Parsing `.bsp` files with the BSP29 BSP2, BSP30 BSP38, and Qbism formats.
- Structured easy access to the bsp data.
- BSP raycasting.
- Mesh generation.
- Lightmap atlas generation either per-style or per-slot (`.lit` supported).
- BSPX support, including built-in structures for the `RGBLIGHTING`, `LIGHTGRID_OCTREE`, `BRUSHLIST`, `DECOUPLED_LM`, and `FACENORMALS` lumps.

## Quickstart
```rust
use qbsp::prelude::*;

let _ = BspData::parse(BspParseInput {
    bsp: &[], // Data of the bsp file.
    lit: None, // Optional lit file for colored lighting if no `RGBLIGHTING` BSPX lump is present.
    settings: BspParseSettings::default(),
});
```

# Feature wishlist
I might work on these at a later date, but if anyone wants to help out or just give some ideas, they're more than welcome to!
- More flexible meshing API
- BSP writing

# Version support table
I wish these used whatever was in your project, but at the time of writing, wildcards just makes Cargo just choose the latest version rather what what is best for you.

| QBSP      | bevy_reflect | glam |
|-----------|--------------|------|
| 0.13-0.14 | 0.18         | 0.30 |
