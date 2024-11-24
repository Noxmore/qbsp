pub mod prelude;
pub(crate) use prelude::*;

pub mod data;
pub(crate) use data::*;

#[cfg(feature = "meshing")]
pub mod mesh;

pub mod util;

// Re-exports
pub use glam;
pub use image;
pub use smallvec;

/// The default quake palette.
pub static QUAKE_PALETTE: Palette = unsafe { mem::transmute_copy(include_bytes!("../palette.lmp")) };

pub struct BspParseInput<'a> {
    /// The data for the BSP file itself.
    pub bsp: &'a [u8],
    /// The optional .lit file for external colored lighting.
    pub lit: Option<&'a [u8]>,
}

#[derive(Debug, Clone, Error)]
pub enum BspParseError {
    #[error("Palette byte length {0} instead of 768.")]
    InvalidPaletteLength(usize),
    #[error("Lump ({0:?}) out of bounds of data! Malformed/corrupted BSP?")]
    LumpOutOfBounds(LumpEntry),
    #[error("Tried to read bytes from {from} to {to} from buffer of size {size}")]
    BufferOutOfBounds {
        from: usize,
        to: usize,
        size: usize,
    },
    #[error("Failed to parse string at index {index}, invalid utf-8 sequence: {sequence:?}")]
    InvalidString {
        index: usize,
        sequence: Vec<u8>,
    },
    #[error("Wrong magic number! Expected {expected}, found \"{}\"", display_magic_number(found))]
    WrongMagicNumber {
        found: [u8; 4],
        expected: &'static str,
    },
    #[error("Invalid color data, size {0} is not devisable by 3!")]
    ColorDataSizeNotDevisableBy3(usize),
    #[error("Invalid value: {value}, acceptable:\n{acceptable}")]
    InvalidVariant { value: i32, acceptable: &'static str },

    /// For telling the user exactly where the error occurred in the process.
    #[error("{0} - {1}")]
    DoingJob(String, Box<BspParseError>),
}
impl BspParseError {
    /// The error error behind any [BspParseError::DoingJob].
    pub fn root(&self) -> &BspParseError {
        let mut err = self;
        loop {
            match err {
                Self::DoingJob(_, child) => err = &child,
                _ => return err,
            }
        }
    }

    #[inline]
    pub fn map_utf8_error<'a>(data: &'a [u8]) -> impl FnOnce(std::str::Utf8Error) -> Self + 'a {
        |err| BspParseError::InvalidString { index: err.valid_up_to(), sequence: data[err.valid_up_to()..err.valid_up_to() + err.error_len().unwrap_or(1)].to_vec() }
    }
}

pub type BspResult<T> = Result<T, BspParseError>;

pub trait BspParseResultDoingJobExt {
    /// Like `map_err`, but specifically for adding messages to BSP errors to tell the user exactly what was going on when the error occurred.
    fn job(self, job: impl ToString) -> Self;
}
impl<T> BspParseResultDoingJobExt for BspResult<T> {
    fn job(self, job: impl ToString) -> Self {
        match self {
            Ok(v) => Ok(v),
            Err(err) => Err(BspParseError::DoingJob(job.to_string(), Box::new(err))),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BspFormat {
    /// Modern BSP format with expanded limits
    BSP2,
    /// Original quake format, in most cases, you should use BSP2 over this.
    BSP29,
}
impl BspFormat {
    pub fn from_magic_number(data: [u8; 4]) -> Result<Self, BspParseError> {
        match &data {
            b"BSP2" => Ok(Self::BSP2),
            [0x1D, 0x00, 0x00, 0x00] => Ok(Self::BSP29),
            _ => Err(BspParseError::WrongMagicNumber { found: data, expected: "BSP2 or 0x1D for BSP29" }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BspParseContext {
    pub format: BspFormat,
}

/// An Id Tech 1 palette to use for embedded images.
#[repr(C)] // Because we transmute data with QUAKE_PALETTE, don't want Rust to pull any shenanigans
#[derive(Debug, Clone)]
pub struct Palette {
    pub colors: [[u8; 3]; 256],
}
impl Default for Palette {
    fn default() -> Self {
        QUAKE_PALETTE.clone()
    }
}
impl Palette {
    /// Parses a palette from data. Palettes must be 768 bytes in length exactly.
    pub fn parse(data: &[u8]) -> BspResult<Self> {
        if data.len() != 768 {
            return Err(BspParseError::InvalidPaletteLength(data.len()));
        }

        Ok(Self { colors: data.chunks_exact(3).map(|col| [col[0], col[1], col[2]]).collect::<Vec<_>>().try_into().unwrap() })
    }
}

/// Points to the chunk of data in the file a lump resides in.
#[derive(Debug, Clone, Copy)]
pub struct LumpEntry {
    pub offset: u32,
    pub len: u32,
}
// BspRead implemented in lump_data
impl LumpEntry {
    /// Returns the slice of `data` (BSP file input) that this entry points to.
    pub fn get<'a>(&self, data: &'a [u8]) -> BspResult<&'a [u8]> {
        let (from, to) = (self.offset as usize, self.offset as usize + self.len as usize);
        if to > data.len() {
            Err(BspParseError::LumpOutOfBounds(*self))
        } else {
            Ok(&data[from..to])
        }
    }
}

/// Helper function to read an array of data of type `T` from a lump. Takes in the BSP file data, the lump directory, and the lump to read from.
pub fn read_lump<T: BspParse>(data: &[u8], entry: LumpEntry, lump_name: &'static str, ctx: &BspParseContext) -> BspResult<Vec<T>> {
    // let entry = lump_dir.get(lump);
    let lump_data = entry.get(data)?;
    let lump_entries = entry.len as usize / T::bsp_struct_size(ctx);

    let mut reader = BspByteReader::new(lump_data, ctx);
    let mut out = Vec::with_capacity(lump_entries);

    for i in 0..lump_entries {
        out.push(reader.read().job(format!("Parsing {lump_name} lump entry {i}"))?);
    }

    Ok(out)
}

/// Contains the list of lump entries
#[derive(Debug, Clone, Copy)]
pub struct LumpDirectory {
    pub entities: LumpEntry,
    pub planes: LumpEntry,
    pub textures: LumpEntry,
    pub vertices: LumpEntry,
    pub visibility: LumpEntry,
    pub nodes: LumpEntry,
    pub tex_info: LumpEntry,
    pub faces: LumpEntry,
    pub lighting: LumpEntry,
    pub clip_nodes: LumpEntry,
    pub leaves: LumpEntry,
    pub mark_surfaces: LumpEntry,
    pub edges: LumpEntry,
    pub surf_edges: LumpEntry,
    pub models: LumpEntry,
}

/// The data parsed from a BSP file.
#[derive(Debug, Clone)]
pub struct BspData {
    /// Essentially an embedded .map file, the differences being:
    /// - Brush data has been stripped.
    /// - Brush entities have a `model` property indexing into the `models` field of this struct.
    pub entities: String,
    /// All vertex positions.
    pub vertices: Vec<Vec3>,
    pub planes: Vec<BspPlane>,
    pub edges: Vec<BspEdge>,
    pub surface_edges: Vec<i32>,
    pub faces: Vec<BspFace>,
    pub tex_info: Vec<BspTexInfo>,
    pub models: Vec<BspModel>,
    pub nodes: Vec<BspNode>,
    pub textures: Vec<Option<BspTexture>>,
    pub lighting: Option<BspLighting>,

    // TODO support BSPX
}
impl BspData {
    /// Parses the data from BSP input.
    pub fn parse(input: BspParseInput) -> BspResult<Self> {
        let BspParseInput { bsp, lit } = input;
        if bsp.len() < 4 {
            return Err(BspParseError::BufferOutOfBounds { from: 0, to: 4, size: bsp.len() });
        }
        
        let ctx = BspParseContext {
            format: BspFormat::from_magic_number(bsp[0..4].try_into().unwrap())?,
        };
        let mut reader = BspByteReader::new(&bsp[4..], &ctx);
        
        let lump_dir: LumpDirectory = reader.read()?;
        
        let mut entities_bytes = lump_dir.entities.get(bsp)?.to_vec();
        for (i, byte) in entities_bytes.iter_mut().enumerate() {
            if *byte > 127 {
                // For some reason some characters in the entities lump are offset to the second half of byte values, no idea why.
                *byte -= 128;
            } else if *byte == 0 {
                // Also, sometimes the entity lump ends early, so this just truncates it if that is the case.
                entities_bytes.truncate(i);
                break;
            }
        }
        
        let data = Self {
            entities: std::str::from_utf8(&entities_bytes).map_err(BspParseError::map_utf8_error(&entities_bytes)).job("Reading entities lump")?.to_string(),
            vertices: read_lump(bsp, lump_dir.vertices, "vertices", &ctx)?,
            planes: read_lump(bsp, lump_dir.planes, "planes", &ctx)?,
            edges: read_lump(bsp, lump_dir.edges, "edges", &ctx)?,
            surface_edges: read_lump(bsp, lump_dir.surf_edges, "surface edges", &ctx)?,
            faces: read_lump(bsp, lump_dir.faces, "faces", &ctx)?,
            tex_info: read_lump(bsp, lump_dir.tex_info, "texture infos", &ctx)?,
            models: read_lump(bsp, lump_dir.models, "models", &ctx)?,
            nodes: read_lump(bsp, lump_dir.nodes, "nodes", &ctx)?,
            textures: read_texture_lump(&mut BspByteReader::new(lump_dir.textures.get(bsp)?, &ctx)).job("Reading texture lump")?,
            lighting: if let Some(lit) = lit {
                Some(BspLighting::read_lit(lit, &ctx).job("Parsing .lit file")?)
                // TODO BSPX (DECOUPLED_LM && RGBLIGHTING)
            } else {
                let lighting = lump_dir.lighting.get(bsp)?;

                if lighting.is_empty() {
                    None
                } else {
                    Some(BspLighting::White(lighting.to_vec()))
                }
            },
        };

        Ok(data)
    }

    /// Parses embedded textures using the provided palette. Use [QUAKE_PALETTE] for the default Quake palette.
    pub fn parse_embedded_textures(&self, palette: &Palette) -> HashMap<String, image::RgbImage> {
        let mut map = HashMap::new();

        for texture in self.textures.iter().flatten() {
            let Some(data) = &texture.data else { continue };
            // TODO Do proper error handling if this returns None
            let image = image::RgbImage::from_fn(texture.header.width, texture.header.height, |x, y| {
                image::Rgb(palette.colors[data[(y * texture.header.width + x) as usize] as usize])
            });
            
            map.insert(texture.header.name.to_string(), image);
        }
        
        map
    }
}