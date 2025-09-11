use interval_tree::TextRange;

/// Simple reference implementation for testing against `IntervalTree`
/// Uses a Vec<Option<T>> where each index represents a position
/// This makes it trivial to understand and verify behavior
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct SimpleIntervalMap<T> {
    /// Vector where index = position, value = Some(T) if position has a value
    data: Vec<Option<T>>,
}

pub(crate) struct RefMapIterator<'a, T> {
    pos: usize,
    map: &'a SimpleIntervalMap<T>,
}

impl<'a, T> RefMapIterator<'a, T> {
    pub(crate) fn new(map: &'a SimpleIntervalMap<T>) -> Self {
        Self { pos: 0, map }
    }
}

impl<'a, T: PartialEq> Iterator for RefMapIterator<'a, T> {
    type Item = (TextRange, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        let empty_len = self.map.data[self.pos..].iter().take_while(|x| x.is_none()).count();
        let start = self.pos + empty_len;
        if let Some(val) = self.map.data.get(start) {
            let len = self.map.data[start..].iter().take_while(|x| *x == val).count();
            assert!(len >= 1);
            self.pos = start + len;
            Some((TextRange::new(start, start + len), val.as_ref().unwrap()))
        } else {
            None
        }
    }
}

impl<'a, T: PartialEq> IntoIterator for &'a SimpleIntervalMap<T> {
    type Item = (TextRange, &'a T);
    type IntoIter = RefMapIterator<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        RefMapIterator::new(self)
    }
}

#[allow(dead_code)]
impl<T: Clone + PartialEq> SimpleIntervalMap<T> {
    pub(crate) fn new() -> Self {
        Self { data: Vec::new() }
    }

    /// Ensure the vector is large enough to hold the given position
    fn ensure_capacity(&mut self, pos: usize) {
        if pos >= self.data.len() {
            self.data.resize(pos + 1, None);
        }
    }

    /// Insert an interval with a value, using `merge_fn` to combine with existing values
    pub(crate) fn insert<F: Fn(T, T) -> T>(&mut self, range: TextRange, val: T, merge_fn: F) {
        if range.start >= range.end {
            return; // Empty intervals not allowed
        }

        self.ensure_capacity(range.end - 1);

        for pos in range.start..range.end {
            match &self.data[pos] {
                Some(existing) => {
                    let merged = merge_fn(val.clone(), existing.clone());
                    self.data[pos] = Some(merged);
                }
                None => {
                    self.data[pos] = Some(val.clone());
                }
            }
        }
    }

    /// Get the value at a specific position
    pub(crate) fn get(&self, pos: usize) -> Option<T> {
        self.data.get(pos).and_then(Clone::clone)
    }

    /// Delete values in a range
    /// Only removes the intersecting portions of intervals
    pub(crate) fn delete(&mut self, range: TextRange) {
        if range.start >= range.end {
            return;
        }

        let end_pos = range.end.min(self.data.len());
        for pos in range.start..end_pos {
            self.data[pos] = None;
        }
    }

    /// Advance all positions >= position by length
    pub(crate) fn advance(&mut self, position: usize, length: usize) {
        // Create new vector with expanded size
        let mut new_data = vec![None; self.data.len() + length];

        // Copy elements before insertion point unchanged
        let min_pos = position.min(self.data.len());
        new_data[..min_pos].clone_from_slice(&self.data[..min_pos]);

        // Copy elements at/after insertion point, shifted by length
        for pos in position..self.data.len() {
            if let Some(val) = &self.data[pos] {
                new_data[pos + length] = Some(val.clone());
            }
        }

        self.data = new_data;
    }

    /// Find the value at a specific position (same as get)
    pub(crate) fn find(&self, position: usize) -> Option<T> {
        self.get(position)
    }

    /// Apply a transformation to all positions that intersect with range
    pub(crate) fn apply_with_split<F: Fn(T) -> Option<T>>(&mut self, f: F, range: TextRange) {
        if range.start >= range.end {
            return;
        }

        let end_pos = range.end.min(self.data.len());
        for pos in range.start..end_pos {
            if let Some(val) = self.data[pos].take() {
                self.data[pos] = f(val);
            }
        }
    }

    /// Get the total number of positions that have values
    pub(crate) fn size(&self) -> usize {
        self.data.iter().filter(|opt| opt.is_some()).count()
    }

    /// Get all positions that have values, sorted
    pub(crate) fn positions(&self) -> Vec<usize> {
        self.data
            .iter()
            .enumerate()
            .filter_map(|(pos, opt)| opt.as_ref().map(|_| pos))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_insert_and_get() {
        let mut map = SimpleIntervalMap::new();
        map.insert(TextRange::new(1, 4), 5, |new, _| new);

        assert_eq!(map.get(0), None);
        assert_eq!(map.get(1), Some(5));
        assert_eq!(map.get(2), Some(5));
        assert_eq!(map.get(3), Some(5));
        assert_eq!(map.get(4), None);
    }

    #[test]
    fn test_simple_merge() {
        let mut map = SimpleIntervalMap::new();
        map.insert(TextRange::new(1, 3), 5, |new, _| new);
        map.insert(TextRange::new(2, 4), 3, |new, old| new + old);

        assert_eq!(map.get(1), Some(5));
        assert_eq!(map.get(2), Some(8)); // 3 + 5
        assert_eq!(map.get(3), Some(3));
    }

    #[test]
    fn test_simple_delete() {
        let mut map = SimpleIntervalMap::new();
        map.insert(TextRange::new(0, 5), 1, |new, _| new);

        map.delete(TextRange::new(2, 4));

        assert_eq!(map.get(1), Some(1));
        assert_eq!(map.get(2), None);
        assert_eq!(map.get(3), None);
        assert_eq!(map.get(4), Some(1));
    }

    #[test]
    fn test_simple_advance() {
        let mut map = SimpleIntervalMap::new();
        map.insert(TextRange::new(0, 3), 1, |new, _| new);
        map.insert(TextRange::new(5, 7), 2, |new, _| new);

        map.advance(3, 2); // Insert 2 positions at position 3

        assert_eq!(map.get(0), Some(1)); // Before insertion point - unchanged
        assert_eq!(map.get(1), Some(1));
        assert_eq!(map.get(2), Some(1));
        assert_eq!(map.get(3), None); // Gap
        assert_eq!(map.get(4), None); // Gap
        assert_eq!(map.get(5), None); // Old position 5 moved
        assert_eq!(map.get(6), None); // Old position 6 moved
        assert_eq!(map.get(7), Some(2)); // Old position 5 moved here
        assert_eq!(map.get(8), Some(2)); // Old position 6 moved here
    }

    #[test]
    fn test_iter() {
        let mut map = SimpleIntervalMap::new();
        map.insert(TextRange::new(0, 100), 0, |x, _| x);
        map.insert(TextRange::new(1, 2), 1, |x, _| x);

        let mut iter = map.into_iter();

        assert_eq!(iter.next().unwrap(), (TextRange::new(0, 1), &0));
        assert_eq!(iter.next().unwrap(), (TextRange::new(1, 2), &1));
        assert_eq!(iter.next().unwrap(), (TextRange::new(2, 100), &0));
    }

    #[test]
    fn test_simple_vector_structure() {
        let mut map = SimpleIntervalMap::new();
        map.insert(TextRange::new(1, 4), 5, |new, _| new);

        // The beauty of vector-based: you can directly inspect the structure
        assert_eq!(map.data, vec![None, Some(5), Some(5), Some(5)]);

        // Add another interval
        map.insert(TextRange::new(2, 5), 3, |new, old| new + old);

        // Now positions 2 and 3 should have merged values
        assert_eq!(map.data, vec![None, Some(5), Some(8), Some(8), Some(3)]);

        // This makes it trivial to verify correctness!
        assert_eq!(map.get(0), None);
        assert_eq!(map.get(1), Some(5));
        assert_eq!(map.get(2), Some(8)); // 3 + 5
        assert_eq!(map.get(3), Some(8)); // 3 + 5
        assert_eq!(map.get(4), Some(3));
    }
}
