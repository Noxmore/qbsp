use crate::*;

// Simple generic rectangle type.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Rect<T> {
    pub min: T,
    pub max: T,
}
impl<T> Rect<T> {
    pub const fn new(min: T, max: T) -> Self {
        Self { min, max }
    }
}

impl<T: std::ops::Sub<Output = T>> Rect<T> {
    #[inline]
    pub fn size(self) -> T {
        self.max - self.min
    }
}

impl Rect<Vec2> {
    pub const EMPTY: Self = Self {
        max: Vec2::NEG_INFINITY,
        min: Vec2::INFINITY,
    };

    /// Build a new rectangle formed of the union of this rectangle and a point.
    ///
    /// The union is the smallest rectangle enclosing both the rectangle and the point. If the
    /// point is already inside the rectangle, this method returns a copy of the rectangle.
    #[inline]
    pub fn union_point(&self, other: Vec2) -> Self {
        Self {
            min: self.min.min(other),
            max: self.max.max(other),
        }
    }
}

/// Displays bytes in string form if they make up a string, else just displays them as bytes.
pub(crate) fn display_magic_number(bytes: &[u8]) -> String {
    std::str::from_utf8(bytes).ok().and_then(|s| s.is_ascii().then_some(s)).map(str::to_owned).unwrap_or(format!("{bytes:?}"))
}