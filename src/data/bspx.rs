//! [BSPX](https://developer.valvesoftware.com/wiki/BSPX) data definitions.

use crate::*;
use super::*;

pub const BSPX_ENTRY_NAME_LEN: usize = 24;

#[derive(Debug, Clone, Copy)]
pub struct BspxLumpEntry {
    pub name: FixedStr<BSPX_ENTRY_NAME_LEN>,
    pub entry: LumpEntry,
}
impl_bsp_parse_simple!(BspxLumpEntry, name, entry);

#[derive(Debug, Clone, Default)]
pub struct BspxDirectory {
    pub inner: HashMap<FixedStr<BSPX_ENTRY_NAME_LEN>, LumpEntry>,
}
impl BspParse for BspxDirectory {
    fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
        match reader.read().and_then(|magic| {
            if &magic != b"BSPX" {
                Err(BspParseError::WrongMagicNumber { found: magic, expected: "BSPX" })
            } else {
                Ok(())
            }
        }) {
            Ok(()) => {}
            Err(BspParseError::BufferOutOfBounds { .. }) => return Err(BspParseError::NoBspxDirectory),
            Err(err) => return Err(err),
        }
        

        let num_lumps: u32 = reader.read().job("lump count")?;

        let mut inner = HashMap::new();

        for i in 0..num_lumps {
            let entry: BspxLumpEntry = reader.read().job(format!("lump entry {i}/{num_lumps}"))?;

            inner.insert(entry.name, entry.entry);
        }
        
        Ok(Self { inner })
    }
    fn bsp_struct_size(_ctx: &BspParseContext) -> usize {
        unimplemented!("BspxDirectory is of variable size")
    }
}

/// Owned version of [BspxDirectory]. Convert via [BspxData::new].
#[derive(Debug, Clone, Default)]
pub struct BspxData {
    pub inner: HashMap<FixedStr<BSPX_ENTRY_NAME_LEN>, Box<[u8]>>,
}
impl BspxData {
    pub fn new(bsp: &[u8], dir: &BspxDirectory) -> BspResult<Self> {
        let mut data = Self::default();

        for (name, entry) in &dir.inner {
            data.inner.insert(*name, entry.get(bsp)?.into());
        }

        Ok(data)
    }
    
    /// Retrieves a lump entry from the directory, returns `None` if the entry does not exist.
    #[inline]
    pub fn get(&self, s: &str) -> Option<&[u8]> {
        self.inner.get(&FixedStr::from_str(s)?).map(|v| &**v)
    }

    /// Returns `None` if the lump does not exist, else returns `Some` with the parse result.
    pub fn parse_rgb_lighting(&self, ctx: &BspParseContext) -> Option<BspResult<BspLighting>> {
        Some(BspLighting::read_lit(self.get("RGBLIGHTING")?, ctx, true).job("Parsing RGBLIGHTING BSPX lump"))
    }
}