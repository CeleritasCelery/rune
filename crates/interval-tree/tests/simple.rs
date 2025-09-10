use interval_tree::TextRange;

/// Simple reference implementation for testing against `IntervalTree`
/// Uses a Vec<Option<T>> where each index represents a position
/// This makes it trivial to understand and verify behavior
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct SimpleIntervalMap<T: Clone> {
    /// Vector where index = position, value = Some(T) if position has a value
    data: Vec<Option<T>>,
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

    /// Find all positions that intersect with the given range and have values
    pub(crate) fn find_intersects(&self, range: TextRange) -> Vec<(TextRange, T)> {
        if range.start >= range.end {
            return Vec::new();
        }

        let mut result = Vec::new();
        let mut current_start: Option<usize> = None;
        let mut current_val: Option<T> = None;

        let end_pos = range.end.min(self.data.len());

        // Scan through the range looking for consecutive positions with the same value
        for pos in range.start..end_pos {
            if let Some(val) = &self.data[pos] {
                match (&current_start, &current_val) {
                    (Some(_start), Some(prev_val)) if prev_val == val => {
                        // Continue current interval
                    }
                    (Some(start), Some(prev_val)) => {
                        // End previous interval, start new one
                        result.push((TextRange::new(*start, pos), prev_val.clone()));
                        current_start = Some(pos);
                        current_val = Some(val.clone());
                    }
                    (None, None) => {
                        // Start first interval
                        current_start = Some(pos);
                        current_val = Some(val.clone());
                    }
                    _ => unreachable!(),
                }
            } else if let (Some(start), Some(val)) = (&current_start, &current_val) {
                // End current interval due to gap
                result.push((TextRange::new(*start, pos), val.clone()));
                current_start = None;
                current_val = None;
            }
        }

        // Close final interval if needed
        if let (Some(start), Some(val)) = (&current_start, &current_val) {
            // Find the actual end of the interval, not just the search boundary
            let mut end = end_pos;
            while end < self.data.len() && self.data[end].as_ref() == Some(val) {
                end += 1;
            }
            result.push((TextRange::new(*start, end), val.clone()));
        }

        result
    }

    /// Delete values in a range
    /// If `del_extend` is true, removes entire intervals that intersect
    /// If `del_extend` is false, only removes the intersecting portions
    pub(crate) fn delete(&mut self, range: TextRange, del_extend: bool) {
        if range.start >= range.end {
            return;
        }

        if del_extend {
            // Find all intervals that intersect and remove them entirely
            let intersecting_intervals = self.find_intersects(range);
            for (interval_range, _) in intersecting_intervals {
                for pos in interval_range.start..interval_range.end.min(self.data.len()) {
                    if pos < self.data.len() {
                        self.data[pos] = None;
                    }
                }
            }
        } else {
            // Only remove positions within the specified range
            let end_pos = range.end.min(self.data.len());
            for pos in range.start..end_pos {
                self.data[pos] = None;
            }
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

    /// Clean the map by merging adjacent positions with equal values and removing empty values
    pub(crate) fn clean<F: Fn(&T, &T) -> bool, G: Fn(&T) -> bool>(&mut self, _eq: F, empty: G) {
        // Remove empty values
        for val in &mut self.data {
            if let Some(v) = val
                && empty(v)
            {
                *val = None;
            }
        }

        // Note: Since this is a simple reference implementation and the real IntervalTree
        // maintains non-overlapping intervals, the "merging" here doesn't change the
        // fundamental structure. The cleaning is mainly about removing empty values.
        // The IntervalTree's clean operation is more complex due to its tree structure.
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

    /// Convert to a compact representation showing ranges and their values
    pub(crate) fn to_ranges(&self) -> Vec<(TextRange, T)> {
        let positions = self.positions();
        if positions.is_empty() {
            return Vec::new();
        }

        let mut result = Vec::new();
        let mut current_start = positions[0];
        let mut current_val = self.data[positions[0]].as_ref().unwrap().clone();

        for i in 1..positions.len() {
            let pos = positions[i];
            let val = self.data[pos].as_ref().unwrap();

            // Check if this position continues the current range
            if pos == positions[i - 1] + 1 && val == &current_val {
                // Continue current range
            } else {
                // End current range and start new one
                let end_pos = positions[i - 1] + 1;
                result.push((TextRange::new(current_start, end_pos), current_val.clone()));
                current_start = pos;
                current_val = val.clone();
            }
        }

        // Add final range
        let final_end = positions.last().unwrap() + 1;
        result.push((TextRange::new(current_start, final_end), current_val));

        result
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
    fn test_simple_find_intersects() {
        let mut map = SimpleIntervalMap::new();
        map.insert(TextRange::new(0, 3), 1, |new, _| new);
        map.insert(TextRange::new(5, 8), 2, |new, _| new);

        let intersects = map.find_intersects(TextRange::new(2, 6));
        assert_eq!(intersects.len(), 2);
        assert_eq!(intersects[0].0, TextRange::new(2, 3));
        assert_eq!(intersects[0].1, 1);
        assert_eq!(intersects[1].0, TextRange::new(5, 6));
        assert_eq!(intersects[1].1, 2);
    }

    #[test]
    fn test_simple_delete() {
        let mut map = SimpleIntervalMap::new();
        map.insert(TextRange::new(0, 5), 1, |new, _| new);

        map.delete(TextRange::new(2, 4), false);

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
    fn test_to_ranges() {
        let mut map = SimpleIntervalMap::new();
        map.insert(TextRange::new(0, 3), 1, |new, _| new);
        map.insert(TextRange::new(5, 7), 2, |new, _| new);
        map.insert(TextRange::new(10, 12), 1, |new, _| new);

        let ranges = map.to_ranges();
        assert_eq!(ranges.len(), 3);
        assert_eq!(ranges[0], (TextRange::new(0, 3), 1));
        assert_eq!(ranges[1], (TextRange::new(5, 7), 2));
        assert_eq!(ranges[2], (TextRange::new(10, 12), 1));
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
