use std::{
    cmp::Ordering,
    fmt::Debug,
    ops::{Bound, Range, RangeBounds},
};

/// a half-open interval in ℕ, [start, end)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TextRange {
    pub start: usize,
    pub end: usize,
}

impl Ord for TextRange {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.start.cmp(&other.start) {
            Ordering::Equal => self.end.cmp(&other.end),
            x => x,
        }
    }
}

impl PartialOrd for TextRange {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl RangeBounds<usize> for TextRange {
    fn start_bound(&self) -> Bound<&usize> {
        Bound::Included(&self.start)
    }

    fn end_bound(&self) -> Bound<&usize> {
        Bound::Excluded(&self.start)
    }
}

impl<T: Into<usize>> From<Range<T>> for TextRange {
    fn from(range: Range<T>) -> Self {
        Self { start: range.start.into(), end: range.end.into() }
    }
}
impl<T: Into<usize>> From<(T, T)> for TextRange {
    fn from((start, end): (T, T)) -> Self {
        Self { start: start.into(), end: end.into() }
    }
}

impl TextRange {
    /// caller should check that start < end
    pub fn new(start: usize, end: usize) -> Self {
        if start <= end {
            Self { start, end }
        } else {
            panic!("invalid TextRange")
        }
    }

    /// Creates a new `TextRange` only if start < end (non-empty interval).
    /// Returns None if start >= end.
    pub fn new_valid(start: usize, end: usize) -> Option<Self> {
        (start < end).then_some(Self { start, end })
    }

    pub fn as_range(&self) -> Range<usize> {
        self.start..self.end
    }

    pub fn empty(&self) -> bool {
        self.start >= self.end
    }

    pub fn contains(&self, pos: usize) -> bool {
        self.start <= pos && pos < self.end
    }

    /// Determines the strict ordering relationship between two ranges.
    ///
    /// Returns `Some(Ordering::Less)` if this range is completely before the other range.
    /// Returns `Some(Ordering::Greater)` if this range is completely after the other range.
    /// Returns `None` if the ranges overlap.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::cmp::Ordering;
    /// use interval_tree::TextRange;
    ///
    /// let a = TextRange::new(0, 5);
    /// let b = TextRange::new(6, 10);
    /// assert_eq!(a.strict_order(&b), Some(Ordering::Less));
    ///
    /// let c = TextRange::new(7, 8);
    /// assert_eq!(c.strict_order(&b), None);
    /// ```
    pub fn strict_order(&self, other: &Self) -> Option<Ordering> {
        if self.end <= other.start {
            return Some(Ordering::Less);
        }
        if self.start >= other.end {
            return Some(Ordering::Greater);
        }
        None
    }
    /// split self, return the split out interval.
    /// if `left` is true, the left part is split out and returned.
    /// This function does not check whether `position` is valid.
    pub fn split_at(&mut self, position: usize, left: bool) -> Self {
        if left {
            let start = self.start;
            self.start = position;
            Self::new(start, position)
        } else {
            let end = self.end;
            self.end = position;
            Self::new(position, end)
        }
    }

    pub fn includes(&self, other: Self) -> bool {
        self.end >= other.end && self.start <= other.start
    }

    pub fn intersects(&self, other: Self) -> bool {
        self.end > other.start && self.start < other.end
    }

    fn intersection_uncheck(&self, other: Self) -> Self {
        Self::new(self.start.max(other.start), self.end.min(other.end))
    }

    pub fn intersection(&self, other: impl Into<Self>) -> Option<Self> {
        let other = other.into();
        self.intersects(other).then(|| self.intersection_uncheck(other))
    }

    pub fn advance(&mut self, offset: usize) {
        self.start += offset;
        self.end += offset;
    }

    pub fn move_back(&self, offset: usize) -> Self {
        Self::new(self.start + offset, self.end + offset)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cmp::Ordering;

    #[test]
    #[allow(clippy::many_single_char_names)]
    fn test_strict_order() {
        // Test ranges that are completely before
        let a = TextRange::new(0, 5);
        let b = TextRange::new(6, 10);
        assert_eq!(a.strict_order(&b), Some(Ordering::Less));
        assert_eq!(b.strict_order(&a), Some(Ordering::Greater));

        // Test ranges that are completely after
        let c = TextRange::new(15, 20);
        assert_eq!(b.strict_order(&c), Some(Ordering::Less));
        assert_eq!(c.strict_order(&b), Some(Ordering::Greater));

        // Test overlapping ranges
        let d = TextRange::new(7, 8);
        assert_eq!(b.strict_order(&d), None);
        assert_eq!(d.strict_order(&b), None);

        // Test adjacent ranges
        let e = TextRange::new(10, 15);
        assert_eq!(b.strict_order(&e), Some(Ordering::Less));
        assert_eq!(e.strict_order(&b), Some(Ordering::Greater));

        // Test equal ranges
        let f = TextRange::new(6, 10);
        assert_eq!(b.strict_order(&f), None);
        assert_eq!(f.strict_order(&b), None);
    }
}
