# QBSP

Rust crate for parsing, and operating with Quake 1 BSP files.

## Features
- Parsing `.bsp` files with the BSP29 and BSP2 formats.
- Structured easy access to the bsp data.
- BSP raycasting.
- Mesh generation.
- Lightmap atlas generation either per-style or per-slot (`.lit` supported).
- BSPX support, including built-in structures for the `RGBLIGHTING`, `LIGHTGRID_OCTREE`, and `BRUSHLIST` lumps.

## How to use
```rust
use qbsp::prelude::*;

let _ = BspData::parse(BspParseInput {
    bsp: &[], // Data of the bsp file.
    lit: None, // Optional lit file for colored lighting if no `RGBLIGHTING` BSPX lump is present.
    settings: BspParseSettings::default(),
});
```

# Future plans
- More flexible meshing API
- PVS data support
- BSP writing