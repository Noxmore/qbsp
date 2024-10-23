pub mod prelude;
pub(crate) use prelude::*;

pub mod data;
pub(crate) use data::*;

pub mod mesh;

pub mod util;

// Re-exports
pub use glam;
pub use image;

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
    #[error("Failed to parse string: {0}")]
    InvalidString(std::str::Utf8Error),
    #[error("Wrong magic number! Expected {expected}, found {}", display_magic_number(found))]
    WrongMagicNumber {
        found: [u8; 4],
        expected: &'static str,
    },
    #[error("Invalid color data, size {0} is not devisable by 3!")]
    ColorDataSizeNotDevisableBy3(usize),

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

/// An Id Tech 1 palette to use for embedded images.
#[repr(C)] // Because we transmute data with QUAKE_PALETTE, don't want Rust to pull any shenanigans
pub struct Palette {
    pub colors: [[u8; 3]; 256],
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

/// The different lumps in the bsp file in order. Used as indexes into the lump entry directory.
#[derive(Debug, Clone, Copy)]
pub enum LumpSection {
    Entities = 0,
    Planes,
    Textures,
    Vertices,
    Visibility,
    Nodes,
    TexInfo,
    Faces,
    Lighting,
    ClipNodes,
    Leafs,
    MarkSurfaces,
    Edges,
    SurfEdges,
    Models,
}
pub const LUMP_COUNT: usize = 15; // I don't want to bring in strum just for this

/// Points to the chunk of data in the file a lump resides in.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
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
/// 
/// NOTE: The amount of data to read is based on the memory size of `T`r
pub fn read_lump<T: BspParse>(data: &[u8], lump_dir: &LumpDirectory, lump: LumpSection) -> BspResult<Vec<T>> {
    let entry = lump_dir.get(lump);
    let lump_data = entry.get(data)?;
    let lump_entries = entry.len as usize / mem::size_of::<T>();

    let mut reader = BspByteReader::new(lump_data);
    let mut out = Vec::with_capacity(lump_entries);

    for i in 0..lump_entries {
        out.push(reader.read().job(format!("Parsing lump \"{lump:?}\" entry {i}"))?);
    }

    Ok(out)
}

/// Contains the list of lump entries
#[derive(Debug, Clone)]
pub struct LumpDirectory {
    pub entries: [LumpEntry; LUMP_COUNT],
}
impl LumpDirectory {
    /// Returns the entry of the specified lump in this directory.
    #[inline]
    pub fn get(&self, lump: LumpSection) -> LumpEntry {
        self.entries[lump as usize]
    }
}

/// The data parsed from a BSP file.
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
    pub textures: Vec<Option<BspTexture>>,
    pub lighting: Option<BspLighting>,
}
impl BspData {
    /// Parses the data from BSP input.
    pub fn parse(input: BspParseInput) -> BspResult<Self> {
        let BspParseInput { bsp, lit } = input;
        let mut reader = BspByteReader::new(bsp);

        let magic: [u8; 4] = reader.read()?;
        if &magic != b"BSP2" {
            return Err(BspParseError::WrongMagicNumber { found: magic, expected: "BSP2" });
        }
        
        let lump_dir: LumpDirectory = reader.read()?;
        
        // println!("entities lump size: {}", lump_dir.get(LumpSection::Entities).get(bsp)?.len());
        let data = Self {
            entities: std::str::from_utf8(
                lump_dir.get(LumpSection::Entities)
                    .get(bsp)?
                    // We split off the null byte here since this is a C string. TODO do we have to?
                    .split_last()
                    .map(|(_, v)| v)
                    .unwrap_or(&[])
            ).map_err(BspParseError::InvalidString).job("Reading entities lump")?.to_string(),
            vertices: read_lump(bsp, &lump_dir, LumpSection::Vertices)?,
            planes: read_lump(bsp, &lump_dir, LumpSection::Planes)?,
            edges: read_lump(bsp, &lump_dir, LumpSection::Edges)?,
            surface_edges: read_lump(bsp, &lump_dir, LumpSection::SurfEdges)?,
            faces: read_lump(bsp, &lump_dir, LumpSection::Faces)?,
            tex_info: read_lump(bsp, &lump_dir, LumpSection::TexInfo)?,
            models: read_lump(bsp, &lump_dir, LumpSection::Models)?,
            textures: read_texture_lump(&mut BspByteReader::new(lump_dir.get(LumpSection::Textures).get(bsp)?)).job("Reading texture lump")?,
            lighting: if let Some(lit) = lit {
                Some(BspLighting::read_lit(lit).job("Parsing .lit file")?)
                // TODO BSPX (DECOUPLED_LM && RGBLIGHTING)
            } else {
                let lighting = lump_dir.get(LumpSection::Lighting).get(bsp)?;

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

/// Displays bytes in string form if they make up a string, else just displays them as bytes.
pub(crate) fn display_magic_number(bytes: &[u8]) -> String {
    std::str::from_utf8(bytes).map(str::to_owned).unwrap_or(format!("{bytes:?}"))
}