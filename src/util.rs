use crate::*;

// Simple rectangle type partially copied from Bevy.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Rect {
    pub min: Vec2,
    pub max: Vec2,
}
impl Rect {
    pub const EMPTY: Self = Self {
        max: Vec2::NEG_INFINITY,
        min: Vec2::INFINITY,
    };

    #[inline]
    pub fn size(&self) -> Vec2 {
        self.max - self.min
    }

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
    std::str::from_utf8(bytes).map(str::to_owned).unwrap_or(format!("{bytes:?}"))
}