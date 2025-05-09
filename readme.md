# QBSP

[![crates.io](https://img.shields.io/crates/v/qbsp)](https://crates.io/crates/qbsp)
[![docs.rs](https://docs.rs/qbsp/badge.svg)](https://docs.rs/qbsp)

Rust crate for parsing, and operating with Quake 1 BSP files.

## Features
- Parsing `.bsp` files with the BSP29 and BSP2 formats.
- Structured easy access to the bsp data.
- BSP raycasting.
- Mesh generation.
- Lightmap atlas generation either per-style or per-slot (`.lit` supported).
- BSPX support, including built-in structures/support for the `RGBLIGHTING`, `LIGHTGRID_OCTREE`, `BRUSHLIST`, and `DECOUPLED_LM` lumps.

## How to use
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
- PVS data support
- BSP writing