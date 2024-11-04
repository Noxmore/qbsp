//! BSP data parsing and definitions.

use crate::*;

/// Like an [io::Cursor], but i don't have to constantly juggle buffers.
pub struct BspByteReader<'a> { // TODO make something like "BspByteReader"
    pub ctx: &'a BspParseContext,
    bytes: &'a [u8],
    pos: usize,
}
impl<'a> BspByteReader<'a> {
    #[inline]
    pub fn new(bytes: &'a [u8], ctx: &'a BspParseContext) -> Self {
        Self { ctx, bytes, pos: 0 }
    }

    #[inline]
    pub fn read<T: BspParse>(&mut self) -> BspResult<T> {
        T::bsp_parse(self)
    }

    pub fn read_bytes(&mut self, count: usize) -> BspResult<&[u8]> {
        let (from, to) = (self.pos, self.pos + count);
        if to > self.bytes.len() {
            return Err(BspParseError::BufferOutOfBounds { from, to, size: self.bytes.len() });
        }
        let bytes = &self.bytes[from..to];
        self.pos += count;
        Ok(bytes)
    }

    #[inline]
    pub fn with_pos(&self, pos: usize) -> Self {
        Self { ctx: self.ctx, bytes: self.bytes, pos }
    }
}

// TODO Support other BSP versions, mainly BSP29

/// Defines how a type should be read from a BSP file.
pub trait BspParse: Sized {
    fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self>;
    fn bsp_struct_size(ctx: &BspParseContext) -> usize;
}
#[inline]
fn bsp_struct_size<T: BspParse>(_: &T, ctx: &BspParseContext) -> usize {
    T::bsp_struct_size(ctx)
}
macro_rules! impl_bsp_read_primitive {($ty:ty) => {
    impl BspParse for $ty {
        fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
            Ok(<$ty>::from_le_bytes(reader.read()?))
        }
        fn bsp_struct_size(_ctx: &BspParseContext) -> usize {
            mem::size_of::<$ty>()
        }
    }
};}
/// It would be nicer to do this with a proc macro, but i'd rather keep this to one crate if possible
macro_rules! impl_bsp_parse_simple {($ty:ty, $($field:ident),+ $(,)?) => {
    impl BspParse for $ty {
        fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
            Ok(Self { $($field: reader.read().job(concat!("Reading field \"", stringify!($field), "\" on type ", stringify!($ty)))?),+ })
        }
        fn bsp_struct_size(ctx: &BspParseContext) -> usize {
            // TODO this is annoying, there should be a better way of doing this
            let tmp: Self = unsafe { mem::zeroed() };
            $(bsp_struct_size(&tmp.$field, ctx) +)+ 0
        }
    }
};}
macro_rules! bsp_parsed_unit_enum {
    {
        $(#[$outer:meta])*
        $vis:vis enum $name:ident: $repr:ty {
            $($(#[$inner:meta $($args:tt)*])* $variant:ident = $num:literal),+ $(,)?
        }
    } => {
        $(#[$outer])*
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        #[repr($repr)]
        $vis enum $name {
            $($(#[$inner $($args)*])* $variant = $num),+
        }
        impl BspParse for $name {
            fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
                match reader.read::<$repr>()? {
                    $($num => Ok(Self::$variant)),+,
                    n => Err(BspParseError::InvalidVariant { value: n as i32, acceptable: concat!($(stringify!($num), " - ", stringify!($variant), "\n"),+) }),
                }
            }
            #[inline]
            fn bsp_struct_size(_ctx: &BspParseContext) -> usize {
                mem::size_of::<$repr>()
            }
        }
    };
}
impl_bsp_read_primitive!(u16);
impl_bsp_read_primitive!(u32);

impl_bsp_read_primitive!(i16);
impl_bsp_read_primitive!(i32);

impl_bsp_read_primitive!(f32);

impl BspParse for u8 {
    fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
        reader.read_bytes(1).map(|bytes| bytes[0])
    }
    #[inline]
    fn bsp_struct_size(_ctx: &BspParseContext) -> usize {
        1
    }
}


impl_bsp_parse_simple!(LumpEntry, offset, len);
impl_bsp_parse_simple!(LumpDirectory, entities, planes, textures, vertices, visibility, nodes, tex_info, faces, lighting, clip_nodes, leaves, mark_surfaces, edges, surf_edges, models);

impl_bsp_parse_simple!(Vec3, x, y, z);
impl_bsp_parse_simple!(U16Vec3, x, y, z);

// We'd have to change this if we want to impl BspRead for u8
impl<T: BspParse + std::fmt::Debug, const N: usize> BspParse for [T; N] {
    fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
        // Look ma, no heap allocations!
        let mut out = [(); N].map(|_| mem::MaybeUninit::uninit());
        for i in 0..N {
            out[i].write(reader.read()?);
        }
        Ok(out.map(|v| unsafe { v.assume_init() }))
    }
    #[inline]
    fn bsp_struct_size(ctx: &BspParseContext) -> usize {
        T::bsp_struct_size(ctx) * N
    }
}

/// A value in a BSP file where its size differs between formats.
#[derive(Debug, Clone, Copy)]
pub enum BspVariableValue<BSP2, QBSP> {
    BSP2(BSP2),
    QBSP(QBSP),
}
impl<BSP2: BspParse, QBSP: BspParse> BspParse for BspVariableValue<BSP2, QBSP> {
    #[inline]
    fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
        match reader.ctx.format {
            BspFormat::BSP2 => Ok(Self::BSP2(reader.read()?)),
            BspFormat::QBSP => Ok(Self::QBSP(reader.read()?)),
        }
    }
    #[inline]
    fn bsp_struct_size(ctx: &BspParseContext) -> usize {
        match ctx.format {
            BspFormat::BSP2 => mem::size_of::<BSP2>(),
            BspFormat::QBSP => mem::size_of::<QBSP>(),
        }
    }
}
impl<BSP2, QBSP: Into<BSP2>> BspVariableValue<BSP2, QBSP> {
    #[inline]
    pub fn value(self) -> BSP2 {
        match self {
            Self::BSP2(v) => v,
            Self::QBSP(v) => v.into(),
        }
    }
}

pub type UBspValue = BspVariableValue<u32, u16>;
pub type IBspValue = BspVariableValue<i32, i16>;


/// Fixed-sized ascii string.
#[derive(Clone)]
pub struct FixedStr<const N: usize> {
    data: [u8; N],
}
impl<const N: usize> BspParse for FixedStr<N> {
    fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
        Self::new(reader.read()?).map_err(BspParseError::InvalidString)
    }
    #[inline]
    fn bsp_struct_size(_ctx: &BspParseContext) -> usize {
        N
    }
}
impl<const N: usize> FixedStr<N> {
    pub fn new(data: [u8; N]) -> Result<Self, std::str::Utf8Error> {
        std::str::from_utf8(&data)?;
        Ok(Self { data })
    }
    
    pub fn as_str(&self) -> &str {
        // SAFETY: This is checked when a FixedStr is created
        unsafe { std::str::from_utf8_unchecked(&self.data) }.trim_end_matches('\0')
    }
}
impl<const N: usize> std::fmt::Debug for FixedStr<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_str().fmt(f)
    }
}
impl<const N: usize> std::fmt::Display for FixedStr<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_str().fmt(f)
    }
}






#[derive(Debug, Clone, Copy)]
pub struct BoundingBox {
    pub min: Vec3,
    pub max: Vec3,
}
impl_bsp_parse_simple!(BoundingBox, min, max);
#[derive(Debug, Clone, Copy)]
pub struct ShortBoundingBox {
    pub min: U16Vec3,
    pub max: U16Vec3,
}
impl_bsp_parse_simple!(ShortBoundingBox, min, max);
impl From<ShortBoundingBox> for BoundingBox {
    fn from(value: ShortBoundingBox) -> Self {
        Self { min: value.min.as_vec3(), max: value.max.as_vec3() }
    }
}

pub type VariableBoundingBox = BspVariableValue<BoundingBox, ShortBoundingBox>;

#[derive(Debug, Clone, Copy)]
pub struct BspEdge {
    /// The index to the first vertex this edge connects
    pub a: UBspValue,
    /// The index to the second vertex this edge connects
    pub b: UBspValue,
}
impl_bsp_parse_simple!(BspEdge, a, b);

#[derive(Debug, Clone)]
pub struct BspFace {
    /// Index of the plane the face is parallel to
    pub plane_idx: UBspValue,
    /// If not zero, seems to indicate that the normal should be inverted when creating meshes
    pub plane_side: UBspValue,

    /// Index of the first edge (in the face edge array)
    pub first_edge: u32,
    /// Number of consecutive edges (in the face edge array)
    pub num_edges: UBspValue,

    /// Index of the texture info structure
    pub texture_info_idx: UBspValue,

    /// Styles (bit flags) for the lightmaps
    pub lightmap_styles: [u8; 4],

    /// Offset of the lightmap (in bytes) in the lightmap lump, or -1 if no lightmap
    pub lightmap_offset: i32,
}
impl_bsp_parse_simple!(BspFace, plane_idx, plane_side, first_edge, num_edges, texture_info_idx, lightmap_styles, lightmap_offset);
impl BspFace {
    /// The kind of lighting that should be applied to the face.
    /// - value 0 is the normal value, to be used with a light map.
    /// - value 0xFF is to be used when there is no light map.
    /// - value 1 produces a fast pulsating light
    /// - value 2 produces a slow pulsating light
    /// - value 3 to 10 produce various other lighting effects. (TODO implement these lighting effects somehow?)
    #[inline]
    pub fn light_type(&self) -> u8 {
        self.lightmap_styles[0]
    }

    /// Gives the base light level for the face, that is the minimum light level for the light map, or the constant light level in the absence of light map.
    /// Curiously, value 0xFF codes for minimum light, and value 0 codes for maximum light.
    #[inline]
    pub fn base_light(&self) -> u8 {
        self.lightmap_styles[1]
    }
}

#[derive(Debug, Clone)]
pub struct BspTexInfo {
    pub u_axis: Vec3,
    pub u_offset: f32,

    pub v_axis: Vec3,
    pub v_offset: f32,

    pub texture_idx: u32,
    pub flags: BspTexFlags,
}
impl_bsp_parse_simple!(BspTexInfo, u_axis, u_offset, v_axis, v_offset, texture_idx, flags);

bsp_parsed_unit_enum! {
    #[derive(Default)]
    pub enum BspTexFlags: u32 {
        #[default]
        /// Normal lightmapped surface.
        Normal = 0,
        /// No lighting or 256 subdivision.
        Special = 1,
        /// Texture cannot be found.
        Missing = 2,
    }
}

/* #[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum BspTexFlags {
    /// Normal lightmapped surface.
    #[default]
    Normal = 0,
    /// No lighting or 256 subdivision.
    Special = 1,
    /// Texture cannot be found.
    Missing = 2,
}
impl BspParse for BspTexFlags {
    fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
        Self::try_from(reader.read::<u32>()?)
            .map_err(|err| BspParseError::InvalidVariant(err.number))
    }
    fn bsp_struct_size(_ctx: &BspParseContext) -> usize {
        mem::size_of::<u32>()
    }
} */

#[derive(Debug, Clone)]
pub struct BspModel {
    pub bound: BoundingBox,
    /// Origin of model, usually (0,0,0)
    pub origin: Vec3,

    pub head_node: [u32; 4],

    /// Number of visleafs not including the solid leaf 0
    pub visleafs: u32,
    pub first_face: u32,
    pub num_faces: u32,
}
impl_bsp_parse_simple!(BspModel, bound, origin, head_node, visleafs, first_face, num_faces);

#[derive(Debug, Clone)]
pub struct BspPlane {
    pub normal: Vec3,
    pub dist: f32,
    /// Not really sure what this is, not used anywhere
    pub ty: u32,
}
impl_bsp_parse_simple!(BspPlane, normal, dist, ty);

/// The texture lump is more complex than just a vector of the same type of item, so it needs its own function.
pub fn read_texture_lump(reader: &mut BspByteReader) -> BspResult<Vec<Option<BspTexture>>> {
    let mut textures = Vec::new();
    let num_mip_textures: u32 = reader.read()?;

    for _ in 0..num_mip_textures {
        let offset: i32 = reader.read()?;
        if offset.is_negative() {
            textures.push(None);
            continue;
        }
        textures.push(Some(BspTexture::bsp_parse(&mut reader.with_pos(offset as usize))?));
    }

    Ok(textures)
}

#[derive(Debug, Clone)]
pub struct BspNode {
    /// Index of the [BspPlane] that splits the node.
    pub plane_idx: u32,

    pub front: IBspValue,
    pub back: IBspValue,
    
    /// Bounding box of the node and all its children.
    pub bound: VariableBoundingBox,
    /// Index of the first [BspFace] the node contains.
    pub face_idx: UBspValue,
    /// Number of faces this node contains.
    pub face_num: UBspValue,
}
impl_bsp_parse_simple!(BspNode, plane_idx, front, back, bound, face_idx, face_num);

bsp_parsed_unit_enum! {
    pub enum BspTreeLeafContents: i32 {
        Empty = -1,
        Solid = -2,
        Water = -3,
        Slime = -4,
        Lava  = -5,
        Sky   = -6,
        // Origin = -7, removed at csg time
        // Clip = -8, changed to contents_solid
        Current0 = -9,
        Current90 = -10,
        Current180 = -11,
        Current270 = -12,
        CurrentUp = -13,
        CurrentDown = -14,
    }
}

#[derive(Debug, Clone)]
pub struct BspTreeLeaf {
    pub contents: BspTreeLeafContents,
    pub vis_list: u32,

    pub bound: VariableBoundingBox,

    pub face_idx: UBspValue,
    pub face_num: UBspValue,

    pub ambience_water: u8,
    pub ambience_sky: u8,
    pub ambience_slime: u8,
    pub ambience_lava: u8,
}
impl_bsp_parse_simple!(BspTreeLeaf, contents, vis_list, bound, face_idx, face_num, ambience_water, ambience_sky, ambience_slime, ambience_lava);

#[derive(Clone)]
pub struct BspTexture {
    pub header: BspTextureHeader,
    pub data: Option<Vec<u8>>,
}
impl BspParse for BspTexture {
    fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
        // TODO animated textures and the like
        let start_pos = reader.pos;
        let header: BspTextureHeader = reader.read()?;
        
        // From my testing, it seems the data starts at the end of the header, but this is just making sure
        reader.pos = start_pos + header.offset_full as usize;

        let data = if header.offset_full == 0 { None } else {
            Some(reader.read_bytes(header.width as usize * header.height as usize).job(format!("Reading texture with header {header:#?}"))?.to_vec())
        };

        Ok(Self { header, data })
    }
    fn bsp_struct_size(ctx: &BspParseContext) -> usize {
        BspTextureHeader::bsp_struct_size(ctx)
    }
}
impl std::fmt::Debug for BspTexture {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.header.fmt(f)
    }
}

#[derive(Debug, Clone)]
pub struct BspTextureHeader {
    pub name: FixedStr<16>,

    pub width: u32,
    pub height: u32,

    pub offset_full: u32,
    #[allow(unused)] pub offset_half: u32,
    #[allow(unused)] pub offset_quarter: u32,
    #[allow(unused)] pub offset_eighth: u32,
}
impl_bsp_parse_simple!(BspTextureHeader, name, width, height, offset_full, offset_half, offset_quarter, offset_eighth);

/// Lighting data stored in a BSP file or a neighboring LIT file.
#[derive(Clone)]
pub enum BspLighting {
    White(Vec<u8>),
    Colored(Vec<[u8; 3]>),
}
impl BspLighting {
    /// Parses colored lighting from a LIT file.
    pub fn read_lit(data: &[u8], ctx: &BspParseContext) -> BspResult<Self> {
        let mut reader = BspByteReader::new(data, ctx);
        
        let magic: [u8; 4] = reader.read()?;
        if &magic != b"QLIT" {
            return Err(BspParseError::WrongMagicNumber { found: magic, expected: "QLIT" });
        }

        let _version: i32 = reader.read()?;

        if data[reader.pos..].len() % 3 != 0 {
            return Err(BspParseError::ColorDataSizeNotDevisableBy3(data[reader.pos..].len()));
        }

        Ok(Self::Colored(data[reader.pos..].chunks_exact(3).map(|v| [v[0], v[1], v[2]]).collect()))
    }

    /// Convince function to get a location as an RGB color.
    pub fn get(&self, i: usize) -> Option<[u8; 3]> {
        match self {
            Self::White(v) => {
                let v = *v.get(i)?;
                Some([v, v, v])
            },
            Self::Colored(v) => v.get(i).copied(),
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        match self {
            Self::White(vec) => vec.len(),
            Self::Colored(vec) => vec.len(),
        }
    }
}
impl std::fmt::Debug for BspLighting {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::White(vec) => write!(f, "White(...) (len: {})", vec.len()),
            Self::Colored(vec) => write!(f, "Colored(...) (len: {})", vec.len()),
        }
    }
}