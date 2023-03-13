#![allow(dead_code)]
use std::ops::AddAssign;

use bytecount::num_chars;

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

impl AddAssign for Node {
    fn add_assign(&mut self, other: Node) {
        self.bytes += other.bytes;
        self.chars += other.chars;
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
        }
    }

    fn insert(&mut self, chars: usize, bytes: usize) {
        let idx = self.gap_idx;
        assert!((1..self.data.len()).contains(&idx));
        self.update(idx, |n| n.bytes -= bytes);
        self.update(idx - 1, |n| *n += Node { chars, bytes });
    }

    fn byte_to_char(&self, bytes: usize) -> Node {
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
        // if the current node is ascii, we can just index
        if let Some(node) = self.data.get(Self::parent(i).unwrap()) {
            if node.all_ascii() {
                let idx = bytes.min(node.bytes);
                closet.bytes += idx;
                closet.chars += idx;
            }
        }
        closet
    }

    fn char_to_byte(&self, chars: usize) -> Node {
        let mut chars = chars;
        let mut closet = Node::default();
        let mut i = 0;
        while let Some(node) = self.data.get(i) {
            if node.chars > chars {
                i = Self::left_child(i);
            } else {
                chars -= node.chars;
                closet += *node;
                i = Self::right_child(i);
            }
        }
        // if the current node is ascii we can index
        if let Some(node) = self.data.get(Self::parent(i).unwrap()) {
            if node.all_ascii() {
                let idx = chars.min(node.chars);
                closet.bytes += idx;
                closet.chars += idx;
            }
        }
        closet
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
    fn test_indexing() {
        let data = b"hello world";
        let rope = Rope::new(data, 5, 6);
        assert_eq!(rope.byte_to_char(0).chars, 0);
        assert_eq!(rope.byte_to_char(3).chars, 3);
        assert_eq!(rope.byte_to_char(5).chars, 5);
        assert_eq!(rope.byte_to_char(6).chars, 5);
        assert_eq!(rope.byte_to_char(10).chars, 9);

        assert_eq!(rope.char_to_byte(0).bytes, 0);
        assert_eq!(rope.char_to_byte(3).bytes, 3);
        assert_eq!(rope.char_to_byte(4).bytes, 4);
        assert_eq!(rope.char_to_byte(5).bytes, 6);
        assert_eq!(rope.char_to_byte(6).bytes, 7);
        assert_eq!(rope.char_to_byte(9).bytes, 10);

        let data = b"hello world. This is a test string that contains a lot of ascii characters.";
        let rope = Rope::new(data, 40, 50);
        assert_eq!(rope.char_to_byte(1).bytes, 1);
        assert_eq!(rope.char_to_byte(11).bytes, 11);
        assert_eq!(rope.char_to_byte(23).bytes, 23);
        assert_eq!(rope.char_to_byte(23).bytes, 23);
        assert_eq!(rope.char_to_byte(39).bytes, 39);
        assert_eq!(rope.char_to_byte(40).bytes, 50);
        assert_eq!(rope.char_to_byte(51).bytes, 61);
        assert_eq!(rope.char_to_byte(65).bytes, 75);
        assert_eq!(rope.char_to_byte(100).bytes, 75);
    }
}
