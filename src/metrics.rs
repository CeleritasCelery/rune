#![allow(dead_code)]
use std::ops::{Add, AddAssign, Sub, SubAssign};

use bytecount::num_chars;
use str_indices::chars;

#[derive(Debug, Default, Clone, Copy)]
struct Node {
    bytes: usize,
    chars: usize,
}

impl Node {
    fn all_ascii(&self) -> bool {
        self.bytes == self.chars
    }
}

impl Add for Node {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Node {
            bytes: self.bytes + rhs.bytes,
            chars: self.chars + rhs.chars,
        }
    }
}

impl Sub for Node {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Node {
            bytes: self.bytes - rhs.bytes,
            chars: self.chars - rhs.chars,
        }
    }
}

impl AddAssign for Node {
    fn add_assign(&mut self, other: Node) {
        self.bytes += other.bytes;
        self.chars += other.chars;
    }
}

impl SubAssign for Node {
    fn sub_assign(&mut self, other: Node) {
        self.bytes -= other.bytes;
        self.chars -= other.chars;
    }
}

#[derive(Debug)]
struct RopeBuilder(Vec<Node>);

impl RopeBuilder {
    fn new(parents: usize, leafs: usize) -> Self {
        let mut data = Vec::with_capacity(parents + leafs);
        data.resize_with(parents, Node::default);
        RopeBuilder(data)
    }

    fn push(&mut self, node: Node) {
        let end = self.0.len();
        self.0.push(node);
        Rope::propagate(&mut self.0, end);
    }
}

#[derive(Debug, Default)]
pub(crate) struct Rope {
    data: Box<[Node]>,
    gap_idx: usize,
    leaf_start: usize,
}

// const NODE_SIZE: usize = 2^11;
const NODE_SIZE: usize = 5;
// the tree field of the Rope struct is a binary tree of Metrics implemented as
// a binary heap
impl Rope {
    pub(crate) fn new(data: &[u8], gap_start: usize, gap_end: usize) -> Self {
        let pre = data[..gap_start].chunks(NODE_SIZE);
        let pre_len = pre.len().max(1);
        let post = data[gap_end..].chunks(NODE_SIZE);
        let size = pre_len + post.len() + 1;

        let parent_nodes = size.next_power_of_two() - 1;
        let mut builder = RopeBuilder::new(parent_nodes, size);
        // add nodes before the gap
        if pre_len == 0 {
            // if the gap is at the start of the file, we need to add a dummy
            // node so that inserts can go here
            builder.push(Node::default());
        } else {
            for chunk in pre {
                let chars = num_chars(chunk);
                let bytes = chunk.len();
                builder.push(Node { bytes, chars })
            }
        }
        // add node for gap
        builder.push(Node {
            bytes: gap_end - gap_start,
            chars: 0,
        });
        // add nodes after the gap
        for chunk in post {
            let chars = num_chars(chunk);
            let bytes = chunk.len();
            builder.push(Node { bytes, chars });
        }
        assert_eq!(builder.0.len(), size + parent_nodes);
        Self {
            data: builder.0.into_boxed_slice(),
            gap_idx: parent_nodes + pre_len,
            leaf_start: parent_nodes,
        }
    }

    fn insert(&mut self, chars: usize, bytes: usize) {
        let idx = self.gap_idx;
        assert!((1..self.data.len()).contains(&idx));
        self.update(idx, |n| n.bytes -= bytes);
        self.update(idx - 1, |n| *n += Node { chars, bytes });
    }

    // Assumes that gap is at the start of the deleted region and then deletes
    // forward
    fn delete(&mut self, bytes: usize, chars: usize) {
        let delete = Node { bytes, chars };
        let (delete_node_idx, delete_node_offset) = self.node_at_byte(bytes);
        assert!(delete_node_idx > self.gap_idx);
        let mut total_size = 0;
        for i in self.gap_idx + 1..delete_node_idx {
            let node = self.data[i];
            total_size += node.bytes;
            self.update(i, |n| *n -= node);
        }
        let before = delete - delete_node_offset;
        total_size += before.bytes;
        self.update(delete_node_idx, |n| *n -= before);
        self.update(self.gap_idx, |n| n.bytes += total_size);
    }

    fn move_gap(&mut self, bytes: usize, chars: usize) {
        let new_gap = Node { bytes, chars };
        let (new_gap_idx, gap_node_start) = self.node_at_byte(bytes);
        if self.gap_idx == new_gap_idx {
            return;
        }
        if self.gap_idx - 1 == new_gap_idx {
            // split the node at the new gap position and update before and after the gap
            //
            // | A^B | Gap | C  | ...
            // | A   | Gap | BC | ...
            let after_gap = gap_node_start + self.data[new_gap_idx] - new_gap;
            self.update(new_gap_idx, |n| *n -= after_gap);
            self.update(new_gap_idx + 2, |n| *n += after_gap);
        } else if self.gap_idx + 1 == new_gap_idx {
            // same as above but with the new gap position after the gap
            //
            // | A  | Gap | B^C | ...
            // | AB | Gap | C   | ...
            let before_gap = new_gap - gap_node_start;
            self.update(new_gap_idx, |n| *n -= before_gap);
            self.update(new_gap_idx - 2, |n| *n += before_gap);
        } else if new_gap_idx == self.leaf_start {
            // if the new gap is the first node, then move it to the second so
            // we have a node for insertion
            //
            // | A^B | C   | D   | ...
            // | A   | Gap | BCD | ...
            let before_gap = new_gap - gap_node_start;
            let after_gap = self.data[new_gap_idx] - before_gap;
            self.update(new_gap_idx, |n| *n -= after_gap);
            let second_node = self.data[new_gap_idx + 1];
            self.update(new_gap_idx + 2, |n| *n += second_node + after_gap);
            let old_gap_node = self.data[self.gap_idx];
            self.update(new_gap_idx + 1, |n| *n = *n + old_gap_node - second_node);
            self.gap_idx = new_gap_idx + 1;
        } else {
            // | A | Gap | C | ... | D  | E^F | G | ...
            // | A | 0   | C | ... | DE | Gap | FG | ...
            {
                // Take the metrics from the new gap and split it before and after
                // the gap
                let before_gap = new_gap - gap_node_start;
                let after_gap = self.data[new_gap_idx] - before_gap;
                self.update(new_gap_idx - 1, |n| *n += before_gap);
                self.update(new_gap_idx + 1, |n| *n += after_gap);
            }
            {
                // Update the new gap and clear the old gap
                let gap_node = self.data[new_gap_idx];
                let old_gap_node = self.data[self.gap_idx];
                self.update(new_gap_idx, |n| *n = *n + old_gap_node - gap_node);
                // TODO: if one of the neighbours is large we can split it into the
                // old gap
                self.update(self.gap_idx, |n| *n -= old_gap_node);
            }
            self.gap_idx = new_gap_idx;
        }
    }

    fn node_at_byte(&self, bytes: usize) -> (usize, Node) {
        let mut bytes = bytes;
        let mut closet = Node::default();
        let mut i = 0;
        while let Some(node) = self.data.get(i) {
            if node.bytes > bytes {
                i = Self::left_child(i);
            } else {
                bytes -= node.bytes;
                closet += *node;
                i = Self::right_child(i);
            }
        }
        (Self::parent(i).unwrap(), closet)
    }

    fn byte_to_char(&self, bytes: usize, data: &[u8]) -> usize {
        let mut bytes = prev_char_boundary(data, bytes);
        let mut closet = Node::default();
        let mut i = 0;
        while let Some(node) = self.data.get(i) {
            if node.bytes > bytes {
                i = Self::left_child(i);
            } else {
                bytes -= node.bytes;
                closet += *node;
                i = Self::right_child(i);
            }
        }
        let parent = Self::parent(i).unwrap();
        if parent >= self.leaf_start {
            let node = self.data[parent];
            assert!(bytes <= node.bytes);
            // if the current node is ascii, we can just index
            if node.all_ascii() {
                closet.chars + bytes
            } else {
                let start = next_char_boundary(data, closet.bytes);
                let end = prev_char_boundary(data, closet.bytes + node.bytes);
                // TODO: make this unchecked
                let slice = std::str::from_utf8(&data[start..end]).unwrap();
                closet.chars + chars::from_byte_idx(slice, bytes)
            }
        } else {
            closet.chars
        }
    }

    fn char_to_byte(&self, chars: usize, data: &[u8]) -> usize {
        let mut chars = chars;
        let mut bytes = 0;
        let mut i = 0;
        while let Some(node) = self.data.get(i) {
            if node.chars > chars {
                i = Self::left_child(i);
            } else {
                chars -= node.chars;
                bytes += node.bytes;
                i = Self::right_child(i);
            }
        }
        let parent = Self::parent(i).unwrap();
        if parent >= self.leaf_start {
            let node = self.data[parent];
            assert!(chars <= node.chars);
            if node.all_ascii() {
                bytes + chars
            } else {
                // TODO: make this checked
                let slice = unsafe { std::str::from_utf8_unchecked(&data[bytes..]) };
                bytes + chars::to_byte_idx(slice, chars)
            }
        } else {
            bytes
        }
    }

    fn propagate(data: &mut [Node], idx: usize) {
        let mut i = idx;
        while i > 0 {
            // is odd
            let is_left_child = i & 1 == 1;
            i = Self::parent(i).unwrap();
            if is_left_child {
                data[i] += data[idx];
            }
        }
    }

    fn update(&mut self, idx: usize, f: impl Fn(&mut Node)) {
        let mut i = idx;
        let mut is_left_child = true;
        loop {
            if is_left_child {
                f(&mut self.data[i]);
            }
            is_left_child = i & 1 == 1; // is odd
            i = match Self::parent(i) {
                Some(p) => p,
                None => break,
            };
        }
    }

    fn parent(idx: usize) -> Option<usize> {
        if idx == 0 {
            None
        } else {
            Some((idx - 1) / 2)
        }
    }

    fn left_child(idx: usize) -> usize {
        2 * idx + 1
    }

    fn right_child(idx: usize) -> usize {
        2 * idx + 2
    }
}

const fn is_char_boundary(byte: &u8) -> bool {
    // This is bit magic equivalent to: b < 128 || b >= 192
    (*byte as i8) >= -0x40
}

fn next_char_boundary(data: &[u8], i: usize) -> usize {
    i + data[i..].iter().position(is_char_boundary).unwrap()
}

fn prev_char_boundary(data: &[u8], i: usize) -> usize {
    let mut i = i;
    while Some(true) == data.get(i).map(|b| !is_char_boundary(b)) {
        i -= 1;
    }
    i
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_rope() {
        let data = b"hello world";
        let rope = Rope::new(data, 5, 6);
        assert_eq!(rope.data.len(), 6);
        // parents
        assert_eq!(rope.data[0].bytes, 6);
        assert_eq!(rope.data[1].bytes, 5);
        assert_eq!(rope.data[2].bytes, 5);
        // leafs
        assert_eq!(rope.data[3].bytes, 5);
        assert_eq!(rope.data[4].chars, 0);
        assert_eq!(rope.data[4].bytes, 1);
        assert_eq!(rope.data[5].bytes, 5);
    }

    #[test]
    fn test_insert() {
        let data = b"hello   world";
        let mut rope = Rope::new(data, 5, 8);
        let idx = rope.gap_idx;
        assert_eq!(rope.data[idx].bytes, 3);
        assert_eq!(rope.data[0].chars, 5);
        rope.insert(1, 2);
        assert_eq!(rope.data[idx].bytes, 1);
        assert_eq!(rope.data[idx - 1].bytes, 7);
        assert_eq!(rope.data[0].chars, 6);
    }

    #[test]
    fn test_move_gap() {
        let data = b"hello world";
        let mut rope = Rope::new(data, 5, 6);
        rope.move_gap(4, 4);
        assert_eq!(rope.data[rope.gap_idx - 1].bytes, 4);
        assert_eq!(rope.data[rope.gap_idx + 1].bytes, 6);
        assert_eq!(rope.data[rope.gap_idx].bytes, 1);
        assert_eq!(rope.data[rope.gap_idx].chars, 0);

        rope.move_gap(rope.char_to_byte(6, data), 6);
        assert_eq!(rope.data[rope.gap_idx - 1].bytes, 6);
        assert_eq!(rope.data[rope.gap_idx + 1].bytes, 4);
        assert_eq!(rope.data[rope.gap_idx].bytes, 1);
        assert_eq!(rope.data[rope.gap_idx].chars, 0);

        let data = b"hello world this date";
        let mut rope = Rope::new(data, 5, 6);
        rope.move_gap(rope.char_to_byte(14, data), 14);
        assert_eq!(rope.data[rope.gap_idx].bytes, 1);
        assert_eq!(rope.data[rope.gap_idx].chars, 0);

        let data = b"hello world this date";
        let mut rope = Rope::new(data, 10, 11);
        rope.move_gap(0, 0);
        assert_eq!(rope.gap_idx, rope.leaf_start + 1);
        assert_eq!(rope.data[rope.leaf_start].bytes, 0);
        assert_eq!(rope.data[rope.leaf_start].chars, 0);
    }

    #[test]
    fn test_delete() {
        let data = b"hello world";
        let mut rope = Rope::new(data, 5, 6);
        rope.delete(rope.char_to_byte(7, data), 7);
        assert_eq!(rope.data[rope.gap_idx].bytes, 3);
        assert_eq!(rope.data[rope.gap_idx + 1].chars, 3);
    }

    #[test]
    fn test_indexing() {
        let data = b"hello world";
        let rope = Rope::new(data, 5, 6);
        assert_eq!(rope.byte_to_char(0, data), 0);
        assert_eq!(rope.byte_to_char(3, data), 3);
        assert_eq!(rope.byte_to_char(5, data), 5);
        assert_eq!(rope.byte_to_char(6, data), 5);
        assert_eq!(rope.byte_to_char(10, data), 9);

        assert_eq!(rope.char_to_byte(0, data), 0);
        assert_eq!(rope.char_to_byte(3, data), 3);
        assert_eq!(rope.char_to_byte(4, data), 4);
        assert_eq!(rope.char_to_byte(5, data), 6);
        assert_eq!(rope.char_to_byte(6, data), 7);
        assert_eq!(rope.char_to_byte(9, data), 10);

        let data = b"hello world. This is a test string that \
                     contains a lot of ascii characters.";
        let rope = Rope::new(data, 40, 50);
        assert_eq!(rope.char_to_byte(1, data), 1);
        assert_eq!(rope.char_to_byte(11, data), 11);
        assert_eq!(rope.char_to_byte(23, data), 23);
        assert_eq!(rope.char_to_byte(23, data), 23);
        assert_eq!(rope.char_to_byte(39, data), 39);
        assert_eq!(rope.char_to_byte(40, data), 50);
        assert_eq!(rope.char_to_byte(51, data), 61);
        assert_eq!(rope.char_to_byte(65, data), 75);
        assert_eq!(rope.char_to_byte(100, data), 75);
    }

    #[test]
    fn test_indexing_unicode() {
        let data_str = "せかい せかい";
        let gap_start = 9;
        let gap_end = 10;
        let gap_len = gap_end - gap_start;
        assert!(data_str.is_char_boundary(gap_start));
        assert!(data_str.is_char_boundary(gap_end));
        let data = data_str.as_bytes();
        let rope = Rope::new(data, gap_start, gap_end);
        for i in 0..=gap_start {
            assert_eq!(
                rope.byte_to_char(i, data),
                chars::from_byte_idx(data_str, i),
                "mismatch on idx {i}"
            );
        }
        for i in gap_end..data.len() + 5 {
            assert_eq!(
                rope.byte_to_char(i, data),
                // remove gap_len from the result to account for gap
                chars::from_byte_idx(data_str, i) - gap_len,
                "mismatch on idx {i}"
            );
        }

        let chars_before = data_str[..gap_start].chars().count();

        for i in 0..chars_before {
            assert_eq!(
                rope.char_to_byte(i, data),
                chars::to_byte_idx(data_str, i),
                "mismatch on idx {i}"
            );
        }
        for i in chars_before..data.len() {
            assert_eq!(
                rope.char_to_byte(i, data),
                // add gap_len to i to account for gap
                chars::to_byte_idx(data_str, i + gap_len),
                "mismatch on idx {i}"
            );
        }
    }
}
