#![allow(dead_code)]

use smallvec::{smallvec, SmallVec};
use std::{
    fmt,
    iter::Sum,
    mem,
    ops::{Add, AddAssign, RangeBounds, Sub, SubAssign},
};

const MAX: usize = 4;
const MIN: usize = MAX / 2;

type Metrics = SmallVec<[Metric; MAX]>;

#[derive(Debug, Default)]
struct Internal {
    metrics: Metrics,
    children: SmallVec<[Box<Node>; MAX]>,
}

impl Internal {
    fn len(&self) -> usize {
        assert_eq!(self.metrics.len(), self.children.len());
        self.metrics.len()
    }

    fn pop(&mut self) -> Option<(Metric, Box<Node>)> {
        match self.len() {
            0 => None,
            _ => Some((self.metrics.pop().unwrap(), self.children.pop().unwrap())),
        }
    }

    fn push(&mut self, child: Box<Node>) {
        let metric = child.metrics();
        self.children.push(child);
        self.metrics.push(metric);
    }

    fn insert(&mut self, idx: usize, child: Box<Node>) {
        let metric = child.metrics();
        self.children.insert(idx, child);
        self.metrics.insert(idx, metric);
    }

    fn take(&mut self, other: &mut Self, range: impl RangeBounds<usize> + Clone) {
        self.metrics.extend(other.metrics.drain(range.clone()));
        self.children.extend(other.children.drain(range.clone()));
    }

    fn search_char_pos(&self, char_pos: usize) -> (usize, Metric) {
        let mut acc = Metric::default();
        for (i, metric) in self.metrics.iter().enumerate() {
            if char_pos <= acc.chars + metric.chars {
                return (i, acc);
            } else {
                acc += *metric;
            }
        }
        unreachable!("char index {} out of bounds", char_pos);
    }

    fn insert_node(&mut self, idx: usize, new_child: Box<Node>) -> Option<Box<Node>> {
        // update the metrics for the current child
        self.metrics[idx] = self.children[idx].metrics();
        // shift idx to the right
        let idx = idx + 1;
        if self.len() < MAX {
            // If there is room in this node then insert the
            // node before the current one
            self.insert(idx, new_child);
            None
        } else {
            assert_eq!(self.len(), MAX);
            // split this node into two and return the left one
            let middle = MAX / 2;

            let mut right = Internal {
                metrics: self.metrics.drain(middle..).collect(),
                children: self.children.drain(middle..).collect(),
            };
            if idx < middle {
                self.insert(idx, new_child);
            } else {
                right.insert(idx - middle, new_child);
            }
            Some(Box::new(Node::Internal(right)))
        }
    }

    /// Balance the node by either stealing from it's siblings or merging with
    ///
    /// Return true if the node still underfull
    fn balance_node(&mut self, idx: usize) -> bool {
        let missing = MIN.saturating_sub(self.children[idx].len());
        if missing == 0 {
            return false;
        }

        let free_nodes = |x: &Box<Node>| x.len().saturating_sub(MIN);

        let left_free = if idx == 0 { 0 } else { free_nodes(&self.children[idx - 1]) };
        let right_free = self.children.get(idx + 1).map(free_nodes).unwrap_or(0);
        if left_free + right_free >= missing {
            // short circuit
            let failed = self.try_steal_left(idx) && self.try_steal_right(idx);
            assert!(!failed);
            false
        } else {
            self.merge_children(idx)
        }
    }

    /// Merge this node with it's siblings.
    ///
    /// Returns true if the merged node is still is underfull
    fn merge_children(&mut self, idx: usize) -> bool {
        if self.len() <= 1 {
            // no siblings to merge
            return true;
        }
        let right_idx = if idx != 0 { idx } else { idx + 1 };
        let left_idx = right_idx - 1;
        let (left, right) = self.children.split_at_mut(right_idx);
        let underfull = left[left_idx].merge_sibling(&mut right[0]);
        self.children.remove(right_idx);
        let right_metric = self.metrics.remove(right_idx);
        self.metrics[left_idx] += right_metric;
        underfull
    }

    fn try_steal_left(&mut self, idx: usize) -> bool {
        assert!(idx < self.children.len());
        assert!(idx < self.metrics.len());
        let Some(left_idx) = idx.checked_sub(1) else { return true };

        while self.children[idx].len() < MIN {
            let left_node = self.children[left_idx].steal(false);
            if let Some((node, node_metric)) = left_node {
                self.children[idx].merge_node(node, node_metric, 0);
                self.metrics[idx] += node_metric;
                self.metrics[left_idx] -= node_metric;
            } else {
                return true;
            }
        }
        return false;
    }

    fn try_steal_right(&mut self, idx: usize) -> bool {
        assert_eq!(self.children.len(), self.metrics.len());
        let right_idx = idx + 1;
        if right_idx >= self.children.len() {
            return true;
        }

        while self.children[idx].len() < MIN {
            let right_node = self.children[right_idx].steal(true);
            if let Some((node, node_metric)) = right_node {
                let underfull_child = &mut self.children[idx];
                let len = underfull_child.len();
                underfull_child.merge_node(node, node_metric, len);
                self.metrics[idx] += node_metric;
                self.metrics[right_idx] -= node_metric;
            } else {
                return true;
            }
        }
        return false;
    }
}

#[derive(Debug, Default)]
struct Leaf {
    metrics: Metrics,
}

impl Leaf {
    fn len(&self) -> usize {
        self.metrics.len()
    }

    fn pop(&mut self) -> Option<Metric> {
        self.metrics.pop()
    }

    fn search_char_pos(&self, char_pos: usize) -> (usize, Metric) {
        let mut acc = Metric::default();
        let last = self.len() - 1;
        for (i, metric) in self.metrics[..last].iter().enumerate() {
            if char_pos < acc.chars + metric.chars {
                return (i, acc);
            } else {
                acc += *metric;
            }
        }
        (last, acc)
    }

    fn insert_at(&mut self, idx: usize, pos: Metric, data: Metric) -> Option<Box<Node>> {
        if (self.metrics[idx].bytes + data.bytes) < 20 {
            self.metrics[idx] += data;
            return None;
        }
        let left_metric = pos;
        let right_metric = self.metrics[idx] - left_metric;

        let new = if left_metric.bytes <= right_metric.bytes {
            self.metrics[idx] = left_metric + data;
            right_metric
        } else {
            self.metrics[idx] = left_metric;
            right_metric + data
        };
        // shift idx to the right
        let idx = idx + 1;
        if self.len() < MAX {
            // If there is room in this node then insert the
            // leaf before the current one, splitting the
            // size
            self.metrics.insert(idx, new);
            None
        } else {
            assert_eq!(self.len(), MAX);
            // split this node into two and return the left one
            let middle = MAX / 2;
            let mut right_metrics: Metrics = self.metrics.drain(middle..).collect();
            if idx < middle {
                self.metrics.insert(idx, new);
            } else {
                right_metrics.insert(idx - middle, new);
            }
            let right = Node::Leaf(Leaf {
                metrics: right_metrics,
            });
            Some(Box::new(right))
        }
    }

    fn insert_node(&mut self, idx: usize, needle: Metric) -> Option<Box<Node>> {
        let new_metric = self.metrics[idx] - needle;
        self.metrics[idx] = needle;
        // shift idx to the right
        let idx = idx + 1;
        if self.len() < MAX {
            // If there is room in this node then insert the
            // leaf before the current one, splitting the
            // size
            self.metrics.insert(idx, new_metric);
            None
        } else {
            assert_eq!(self.len(), MAX);
            // split this node into two and return the left one
            let middle = MAX / 2;
            let mut right_metrics: Metrics = self.metrics.drain(middle..).collect();
            if idx < middle {
                self.metrics.insert(idx, new_metric);
            } else {
                right_metrics.insert(idx - middle, new_metric);
            }
            let right = Node::Leaf(Leaf {
                metrics: right_metrics,
            });
            Some(Box::new(right))
        }
    }

    fn push(&mut self, metric: Metric) -> Option<Box<Node>> {
        if self.len() < MAX {
            // If there is room in this node then insert the
            // leaf before the current one, splitting the
            // size
            self.metrics.push(metric);
            None
        } else {
            assert_eq!(self.len(), MAX);
            // split this node into two and return the left one
            let right = Node::Leaf(Leaf {
                metrics: smallvec![metric],
            });
            Some(Box::new(right))
        }
    }
}

#[derive(Debug, Default)]
pub(crate) struct BufferMetrics {
    root: Node,
}

impl BufferMetrics {
    pub(crate) fn search_byte(&self, bytes: usize) -> (Metric, usize) {
        self.root.search_impl(bytes, |x| x.bytes)
    }

    pub(crate) fn search_char(&self, chars: usize) -> (Metric, usize) {
        self.root.search_impl(chars, |x| x.chars)
    }

    pub(crate) fn build(each: impl Iterator<Item = Metric>) -> Self {
        // build the base layer of leaf nodes
        let cap = (each.size_hint().0 / MAX) + 1;
        let mut nodes = Vec::with_capacity(cap);
        let mut leaf = Leaf::default();
        for metric in each {
            leaf.push(metric);
            if leaf.len() == MAX {
                nodes.push(Box::new(Node::Leaf(leaf)));
                leaf = Leaf::default();
            }
        }
        if leaf.len() > 0 {
            nodes.push(Box::new(Node::Leaf(leaf)));
        }
        // build each layer of internal nodes from the bottom up
        let mut next_level = Vec::with_capacity((nodes.len() / MAX) + 1);
        while nodes.len() > 1 {
            let len = nodes.len();
            let parent_count = len / MAX;
            let remainder = len % MAX;
            let split_idx = if remainder != 0 && remainder != len && remainder < MIN {
                // If that last node is too small then merge it with the
                // previous one by splitting it early
                len - MIN - 1
            } else {
                // index will never equal len
                len
            };
            let mut int = Internal::default();
            for (idx, node) in nodes.drain(..).enumerate() {
                int.metrics.push(node.metrics());
                int.children.push(node);
                if int.len() == MAX || idx == split_idx {
                    next_level.push(Box::new(Node::Internal(int)));
                    int = Internal::default();
                }
            }
            assert_eq!(next_level.len(), parent_count);
            mem::swap(&mut nodes, &mut next_level);

            if int.len() > 0 {
                nodes.push(Box::new(Node::Internal(int)));
            }
        }
        let root = *nodes.pop().unwrap_or_default();
        let built = Self { root };
        built.assert_invariants();
        built
    }

    pub(crate) fn insert(&mut self, pos: Metric, data: impl Iterator<Item = Metric>) {
        let size = data.size_hint().0;
        let len = self.root.metrics();
        assert!(pos.bytes <= len.bytes);

        if size == 0 {
            return;
        }
        if len.bytes == 0 {
            // empty tree
            let _ = mem::replace(self, Self::build(data));
            return;
        }

        if size < 6 {
            let mut pos = pos;
            for metric in data {
                let offset = metric;
                self.root.insert_at(pos, metric);
                pos += offset;
            }
        } else {
            // build a new tree and splice it in
            let new = Self::build(data);
            if len.bytes == pos.bytes {
                // append
                self.root.append(new.root);
            } else {
                let new_chars = new.root.metrics().chars;
                let right = self.root.split(pos);
                self.root.append(new.root);
                self.root.append(right);
                self.root.fix_seam(pos.chars);
                self.root.fix_seam(pos.chars + new_chars);
            }
        }
        self.assert_invariants();
    }

    pub(crate) fn delete(&mut self, start: Metric, end: Metric) {
        assert!(start.bytes <= end.bytes);
        assert!(start.chars <= end.chars);
        let fix_seam = self.root.delete_impl(start, end);
        if fix_seam {
            self.root.fix_seam(start.chars);
        }
        while self.root.len() == 1 {
            match &mut self.root {
                Node::Internal(int) => {
                    // collapse the root
                    let child = int.children.pop().unwrap();
                    let _ = mem::replace(&mut self.root, *child);
                }
                Node::Leaf(_) => break,
            }
        }
        self.assert_invariants();
    }

    fn assert_invariants(&self) {
        self.root.assert_integrity();
        self.root.assert_balance();
        self.root.assert_node_size(true);
    }
}

#[repr(u8)]
enum SearchNeedle {
    Char = 0,
    Byte = 1,
}

#[derive(Debug)]
enum Node {
    Leaf(Leaf),
    Internal(Internal),
}

impl Node {
    fn new() -> Self {
        Self::default()
    }

    fn metric_slice(&self) -> &[Metric] {
        match self {
            Self::Internal(x) => &x.metrics,
            Self::Leaf(x) => &x.metrics,
        }
    }

    fn is_underfull(&self) -> bool {
        match self {
            Node::Leaf(leaf) => leaf.len() < 1,
            Node::Internal(int) => int.len() < MIN,
        }
    }

    fn metrics(&self) -> Metric {
        let metrics = match self {
            Self::Leaf(x) => &x.metrics,
            Self::Internal(x) => &x.metrics,
        };
        metrics.iter().copied().sum()
    }

    fn len(&self) -> usize {
        match self {
            Self::Leaf(x) => x.len(),
            Self::Internal(x) => x.len(),
        }
    }

    fn depth(&self) -> usize {
        match self {
            Self::Leaf(_) => 0,
            Self::Internal(x) => 1 + x.children[0].depth(),
        }
    }

    fn search_char_pos(&self, char_pos: usize) -> (usize, Metric) {
        let metrics = self.metric_slice();
        let mut acc = Metric::default();
        let last = metrics.len() - 1;
        for (i, metric) in metrics[..last].iter().enumerate() {
            if char_pos < acc.chars + metric.chars {
                return (i, acc);
            } else {
                acc += *metric;
            }
        }
        (last, acc)
    }

    fn insert_at(&mut self, pos: Metric, data: Metric) {
        let len = self.metrics();
        assert!(pos.bytes <= len.bytes);
        if self.len() == 0 {
            assert!(pos.bytes == 0);
            let Node::Leaf(leaf) = self else { unreachable!() };
            leaf.metrics.push(data);
            return;
        }
        let new = self.insert_impl(pos, data);
        if let Some(right) = new {
            // split the root, making the old root the left child
            let left = mem::replace(self, Node::Internal(Internal::default()));
            let Node::Internal(int) = self else { unreachable!() };
            int.metrics = smallvec![left.metrics(), right.metrics()];
            int.children = smallvec![Box::new(left), right];
        }
    }

    fn insert_impl(&mut self, pos: Metric, data: Metric) -> Option<Box<Node>> {
        self.assert_node_integrity();
        let (idx, metric) = self.search_char_pos(pos.chars);
        let offset = pos - metric;
        match self {
            Node::Leaf(leaf) => leaf.insert_at(idx, offset, data),
            Node::Internal(int) => match int.children[idx].insert_impl(offset, data) {
                Some(new) => int.insert_node(idx, new),
                None => {
                    int.metrics[idx] += data;
                    None
                }
            },
        }
    }

    fn delete_impl(&mut self, mut start: Metric, mut end: Metric) -> bool {
        self.assert_node_integrity();
        assert!(start.chars <= end.chars);
        let (start_idx, end_idx) = self.get_delete_indices(&mut start, &mut end);

        match self {
            Node::Internal(int) => {
                if start_idx == end_idx {
                    // delete range is in a single child
                    let idx = start_idx;
                    let metrics = &mut int.metrics;
                    let fix_seam = int.children[idx].delete_impl(start, end);
                    metrics[idx] -= end - start;
                    if int.children[idx].is_underfull() {
                        let fix = int.balance_node(idx);
                        assert!(!fix);
                    }
                    fix_seam
                } else {
                    // if the byte index covers the entire node, delete the
                    // whole thing
                    let start_delete = if start.bytes == 0 { start_idx } else { start_idx + 1 };
                    let end_size = int.metrics[end_idx].bytes;
                    let end_delete = if end.bytes == end_size { end_idx + 1 } else { end_idx };
                    // Delete nodes in the middle
                    if start_delete < end_delete {
                        int.children.drain(start_delete..end_delete);
                        int.metrics.drain(start_delete..end_delete);
                    }
                    // since we might have deleted nodes in the middle, the
                    // index is now 1 more then start.
                    let end_idx = start_idx + 1;
                    let mut fix_seam = false;
                    let mut merge_left = false;
                    let mut merge_right = false;
                    // has a left child
                    if start_delete > start_idx {
                        fix_seam |=
                            int.children[start_idx].delete_impl(start, int.metrics[start_idx]);
                        int.metrics[start_idx] = start;
                        if int.children[start_idx].is_underfull() {
                            merge_left = true;
                        }
                    }
                    // has a right child
                    if end_delete <= end_idx {
                        assert_eq!(
                            end_idx,
                            start_idx + 1 + end_delete.saturating_sub(start_delete)
                        );
                        fix_seam |= int.children[end_idx].delete_impl(Metric::default(), end);
                        int.metrics[end_idx] -= end;
                        if int.children[end_idx].is_underfull() {
                            merge_right = true;
                        }
                    }
                    // merge right child first so that the index of left is not changed
                    if merge_right {
                        fix_seam |= int.balance_node(end_idx);
                    }
                    if merge_left {
                        fix_seam |= int.balance_node(start_idx);
                    }
                    fix_seam
                }
            }
            Node::Leaf(leaf) => {
                if start_idx == end_idx {
                    // TODO: handle below min bytes
                    leaf.metrics[start_idx] -= end - start;
                } else {
                    let start_delete = if start.bytes == 0 { start_idx } else { start_idx + 1 };
                    let end_size = leaf.metrics[end_idx].bytes;
                    let end_delete = if end_size == end.bytes { end_idx + 1 } else { end_idx };

                    leaf.metrics[end_idx] -= end;
                    leaf.metrics[start_idx] = start;
                    if start_delete < end_delete {
                        leaf.metrics.drain(start_delete..end_delete);
                    }
                }
                false
            }
        }
    }

    fn get_delete_indices(&self, start: &mut Metric, end: &mut Metric) -> (usize, usize) {
        let mut start_idx = None;
        let mut end_idx = None;
        for idx in 0..self.len() {
            let metric = self.metric_slice()[idx];
            if start_idx.is_none() && (start.chars < metric.chars || start.chars == 0) {
                start_idx = Some(idx);
            }
            if end.chars <= metric.chars {
                end_idx = Some(idx);
                break;
            }
            if start_idx.is_none() {
                *start -= metric;
            }
            *end -= metric;
        }
        (start_idx.unwrap(), end_idx.unwrap())
    }

    fn merge_node(&mut self, node: Option<Box<Node>>, metric: Metric, idx: usize) {
        match (self, node) {
            (Node::Internal(int), Some(node)) => int.insert(idx, node),
            (Node::Leaf(leaf), None) => {
                // TODO remove this once the other delete is gone
                match leaf.len() {
                    0 => leaf.metrics.push(metric),
                    1 => leaf.metrics[0] += metric,
                    _ => unreachable!(),
                }
            }
            _ => unreachable!("cannot merge internal and leaf nodes"),
        }
    }

    fn merge_sibling(&mut self, right: &mut Self) -> bool {
        match (self, right) {
            (Node::Internal(left), Node::Internal(right)) => {
                left.metrics.append(&mut right.metrics);
                left.children.append(&mut right.children);
                left.children.len() < MIN
            }
            (Node::Leaf(left), Node::Leaf(right)) => {
                assert_eq!(left.len(), 1);
                assert_eq!(right.len(), 1);
                left.metrics[0] += right.metrics[0];
                // TODO: fix this when we add a min size to the leaf node
                false
            }
            _ => unreachable!("cannot merge internal and leaf nodes"),
        }
    }

    fn steal(&mut self, first: bool) -> Option<(Option<Box<Node>>, Metric)> {
        let idx = if first { 0 } else { self.len() - 1 };
        match self {
            Node::Internal(int) if int.len() > MIN => {
                let metric = int.metrics.remove(idx);
                let child = int.children.remove(idx);
                Some((Some(child), metric))
            }
            Node::Leaf(leaf) if leaf.len() > 1 => {
                let metric = leaf.metrics.remove(idx);
                Some((None, metric))
            }
            _ => None,
        }
    }

    fn fix_seam(&mut self, char_pos: usize) -> bool {
        if let Node::Internal(int) = self {
            let prev_len = int.len();
            loop {
                let (idx, metric) = int.search_char_pos(char_pos);
                let on_seam = metric.chars == char_pos;
                if on_seam && int.children.get(idx + 1).is_some_and(|x| x.is_underfull()) {
                    // we are on a seam and there is a right sibling
                    int.balance_node(idx + 1);
                }

                if int.children[idx].is_underfull() {
                    int.balance_node(idx);
                }

                // recalculate because position might have changed
                let (idx, metric) = int.search_char_pos(char_pos);
                let mut retry = false;

                // recurse into children
                if let Some(child) = int.children.get_mut(idx + 1) {
                    let on_seam = metric.chars == char_pos;
                    if on_seam {
                        retry |= child.fix_seam(0);
                    }
                }

                let new_pos = char_pos - metric.chars;
                retry |= int.children[idx].fix_seam(new_pos);
                // If one of the children was underfull we need to retry the
                // loop to merge it again
                if !retry {
                    break;
                }
            }
            let len = int.len();
            len < prev_len && len < MIN
        } else {
            false
        }
    }

    /// Split the tree at the given point. Returns the right side of the split.
    /// Note that the resulting trees may have underfull nodes and will need to
    /// be fixed later.
    fn split(&mut self, pos: Metric) -> Node {
        let (idx, metric) = self.search_char_pos(pos.chars);
        match self {
            Node::Leaf(leaf) => {
                let offset = pos - metric;
                let mut right;
                if offset.bytes == 0 {
                    right = leaf.metrics.drain(idx..).collect();
                } else {
                    let right_node = leaf.metrics[idx] - offset;
                    leaf.metrics[idx] = offset;
                    right = smallvec![right_node];
                    right.extend(leaf.metrics.drain(idx + 1..));
                }
                Node::Leaf(Leaf { metrics: right })
            }
            Node::Internal(int) => {
                let offset = pos - metric;
                let mut right;
                if offset.bytes == 0 {
                    right = Internal {
                        metrics: int.metrics.drain(idx..).collect(),
                        children: int.children.drain(idx..).collect(),
                    };
                } else {
                    let right_node = int.children[idx].split(offset);
                    let right_metric = int.metrics[idx] - offset;
                    int.metrics[idx] = offset;
                    right = Internal {
                        metrics: smallvec![right_metric],
                        children: smallvec![Box::new(right_node)],
                    };
                    right.take(int, idx + 1..);
                }
                Node::Internal(right)
            }
        }
    }

    fn append(&mut self, other: Self) {
        let self_depth = self.depth();
        let other_depth = other.depth();
        let new = if other_depth <= self_depth {
            self.append_at_depth(other, self_depth - other_depth)
        } else {
            let left = mem::replace(self, other);
            self.prepend_at_depth(left, other_depth - self_depth)
        };
        if let Some(right) = new {
            // split the root, making the old root the left child
            let left = mem::replace(self, Node::Internal(Internal::default()));
            let Node::Internal(int) = self else { unreachable!() };
            int.metrics = smallvec![left.metrics(), right.metrics()];
            int.children = smallvec![Box::new(left), right];
        }
    }

    fn append_at_depth(&mut self, other: Self, depth: usize) -> Option<Box<Node>> {
        if depth == 0 {
            match (self, other) {
                (Node::Leaf(_), Node::Leaf(right)) => Some(Box::new(Node::Leaf(right))),
                (Node::Internal(left), Node::Internal(mut right)) => {
                    if left.len() + right.len() <= MAX {
                        left.take(&mut right, ..);
                        None
                    } else {
                        Some(Box::new(Node::Internal(right)))
                    }
                }
                _ => unreachable!("siblings have different types"),
            }
        } else if let Node::Internal(int) = self {
            match int.children.last_mut().unwrap().append_at_depth(other, depth - 1) {
                Some(new) if int.len() < MAX => {
                    int.push(new);
                    None
                }
                Some(new) => Some(Box::new(Node::Internal(Internal {
                    metrics: smallvec![new.metrics()],
                    children: smallvec![new],
                }))),
                None => None,
            }
        } else {
            unreachable!("reached leaf node while depth was non-zero");
        }
    }

    fn prepend_at_depth(&mut self, other: Self, depth: usize) -> Option<Box<Node>> {
        if depth == 0 {
            match (other, self) {
                (Node::Leaf(left), Node::Leaf(_)) => Some(Box::new(Node::Leaf(left))),
                (Node::Internal(mut left), Node::Internal(right)) => {
                    if left.len() + right.len() <= MAX {
                        left.take(right, ..);
                        *right = left;
                        None
                    } else {
                        Some(Box::new(Node::Internal(left)))
                    }
                }
                _ => unreachable!("siblings have different types"),
            }
        } else if let Node::Internal(int) = self {
            match int.children.first_mut().unwrap().prepend_at_depth(other, depth - 1) {
                Some(new) if int.len() < MAX => {
                    int.insert(0, new);
                    None
                }
                Some(new) => Some(Box::new(Node::Internal(Internal {
                    metrics: smallvec![new.metrics()],
                    children: smallvec![new],
                }))),
                None => None,
            }
        } else {
            unreachable!("reached leaf node while depth was non-zero");
        }
    }

    pub(crate) fn search_byte(&self, bytes: usize) -> (Metric, usize) {
        self.search_impl(bytes, |x| x.bytes)
    }

    pub(crate) fn search_char(&self, chars: usize) -> (Metric, usize) {
        self.search_impl(chars, |x| x.chars)
    }

    fn search_impl(&self, needle: usize, getter: impl Fn(&Metric) -> usize) -> (Metric, usize) {
        self.assert_node_integrity();
        let mut needle = needle;
        let mut sum = Metric::default();
        for (idx, metric) in self.metric_slice().iter().enumerate() {
            // fast path if we happen get the exact position in the node
            if needle == 0 {
                break;
            }
            let pos = getter(metric);
            if needle < pos {
                let child_sum = match &self {
                    Node::Internal(int) => {
                        let (metric, offset) = int.children[idx].search_impl(needle, getter);
                        (sum + metric, offset)
                    }
                    Node::Leaf(_) => (sum, needle),
                };
                return child_sum;
            }
            sum += *metric;
            needle -= pos;
        }
        // we are beyond total size of the tree
        (sum, needle)
    }

    fn assert_node_integrity(&self) {
        match self {
            Node::Internal(int) => {
                assert!(int.metrics.len() > 0);
                assert!(int.metrics.len() <= MAX);
                assert_eq!(int.metrics.len(), int.children.len());
                for i in 0..int.children.len() {
                    assert_eq!(int.children[i].metrics(), int.metrics[i]);
                }
            }
            Node::Leaf(leaf) => {
                assert!(leaf.metrics.len() > 0);
                assert!(leaf.metrics.len() <= MAX);
            }
        };
    }

    fn assert_integrity(&self) {
        match self {
            Node::Leaf(_) => {}
            Node::Internal(int) => {
                assert_eq!(int.metrics.len(), int.children.len());
                for i in 0..int.children.len() {
                    assert_eq!(int.children[i].metrics(), int.metrics[i]);
                    int.children[i].assert_integrity();
                }
            }
        }
    }

    fn assert_balance(&self) -> usize {
        match self {
            Node::Leaf(_) => 1,
            Node::Internal(int) => {
                let first_depth = int.children[0].assert_balance();
                for node in &int.children[1..] {
                    assert_eq!(node.assert_balance(), first_depth);
                }
                first_depth + 1
            }
        }
    }

    fn assert_node_size(&self, is_root: bool) {
        match self {
            Node::Leaf(leaf) => {
                assert!(leaf.len() <= MAX);
                if !is_root {
                    assert!(leaf.len() > 0);
                }
            }
            Node::Internal(int) => {
                assert!(int.len() <= MAX);
                if is_root {
                    assert!(int.len() > 1);
                } else {
                    assert!(int.len() >= MIN);
                }

                for node in &int.children {
                    node.assert_node_size(false);
                }
            }
        }
    }
}

impl Default for Node {
    fn default() -> Self {
        Self::Leaf(Leaf::default())
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // print the children level by level by adding them to a pair of
        // alternating arrays for each level
        let mut current = Vec::new();
        let mut next: Vec<&Self> = Vec::new();
        current.push(self);
        let mut level = 0;
        while !current.is_empty() {
            next.clear();
            write!(f, "level {level}:")?;
            for node in &current {
                write!(f, " [")?;
                match node {
                    Node::Internal(int) => {
                        for metric in &int.metrics {
                            write!(f, "({metric}) ")?;
                        }
                        for child in &int.children {
                            next.push(child);
                        }
                    }
                    Node::Leaf(leaf) => {
                        for metric in &leaf.metrics {
                            write!(f, "({metric}) ")?;
                        }
                    }
                }
                write!(f, "]")?;
            }
            writeln!(f)?;
            level += 1;
            mem::swap(&mut current, &mut next);
        }
        Ok(())
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub(crate) struct Metric {
    bytes: usize,
    chars: usize,
}

impl fmt::Display for Metric {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "b:{}, c:{}", self.bytes, self.chars)
    }
}

impl Sum for Metric {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Self::default(), |a, b| Self {
            bytes: a.bytes + b.bytes,
            chars: a.chars + b.chars,
        })
    }
}

impl Add for Metric {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            bytes: self.bytes + rhs.bytes,
            chars: self.chars + rhs.chars,
        }
    }
}

impl Sub for Metric {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            bytes: self.bytes - rhs.bytes,
            chars: self.chars - rhs.chars,
        }
    }
}

impl AddAssign for Metric {
    fn add_assign(&mut self, rhs: Self) {
        self.bytes += rhs.bytes;
        self.chars += rhs.chars;
    }
}

impl SubAssign for Metric {
    fn sub_assign(&mut self, rhs: Self) {
        self.bytes -= rhs.bytes;
        self.chars -= rhs.chars;
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn metric(x: usize) -> Metric {
        Metric {
            bytes: x * 2,
            chars: x,
        }
    }

    fn mock_search_byte(root: &Node, needle: usize) -> Metric {
        let (metric, offset) = root.search_byte(needle);
        Metric {
            bytes: metric.bytes + offset,
            chars: metric.chars + offset / 2,
        }
    }

    fn mock_search_char(root: &Node, needle: usize) -> Metric {
        let (metric, offset) = root.search_char(needle);
        Metric {
            bytes: metric.bytes + offset * 2,
            chars: metric.chars + offset,
        }
    }

    struct TreeBuilderBasic {
        count: usize,
        step: usize,
    }

    impl Iterator for TreeBuilderBasic {
        type Item = Metric;

        fn next(&mut self) -> Option<Self::Item> {
            if self.count == 0 {
                None
            } else {
                self.count -= 1;
                Some(metric(self.step))
            }
        }

        fn size_hint(&self) -> (usize, Option<usize>) {
            (self.count, Some(self.count))
        }
    }

    #[test]
    fn test_insert_empty() {
        let mut buffer = BufferMetrics::build(&mut TreeBuilderBasic { count: 1, step: 5 });
        let builder = &mut TreeBuilderBasic { count: 4, step: 1 };
        buffer.insert(metric(0), builder);
        for i in 0..10 {
            println!("searching for {i}");
            let cmp = mock_search_char(&buffer.root, i);
            assert_eq!(cmp, metric(i));
        }
    }

    #[test]
    fn test_insert() {
        let mut buffer = BufferMetrics::default();
        let builder = &mut TreeBuilderBasic { count: 10, step: 1 };
        buffer.insert(metric(0), builder);
        buffer.root.insert_at(metric(5), metric(5));
        println!("{}", buffer.root);
        for i in 0..15 {
            println!("searching for {i}");
            let cmp = mock_search_char(&buffer.root, i);
            assert_eq!(cmp, metric(i));
        }
    }

    #[test]
    fn test_search() {
        let builder = &mut TreeBuilderBasic { count: 20, step: 1 };
        let root = BufferMetrics::build(builder);
        for i in 0..20 {
            println!("searching for {i}");
            let cmp = mock_search_byte(&root.root, i * 2);
            assert_eq!(cmp, metric(i));
        }
    }

    #[test]
    fn test_search_chars() {
        let builder = &mut TreeBuilderBasic { count: 20, step: 1 };
        let root = BufferMetrics::build(builder);
        for i in 0..20 {
            println!("searching for {i}");
            let cmp = mock_search_char(&root.root, i);
            assert_eq!(cmp, metric(i));
        }
    }

    #[test]
    fn test_delete_range_leaf() {
        // shouldn't need more then a single leaf node
        let builder = &mut TreeBuilderBasic { count: 3, step: 4 };
        let mut buffer = BufferMetrics::build(builder);
        assert_eq!(buffer.root.metrics(), metric(12));
        println!("init: {}", buffer.root);
        buffer.delete(metric(1), metric(3));
        assert_eq!(buffer.root.metrics(), metric(10));
        println!("after: {}", buffer.root);
        buffer.delete(metric(2), metric(6));
        assert_eq!(buffer.root.metrics(), metric(6));
        println!("after: {}", buffer.root);
        buffer.delete(metric(1), metric(4));
        assert_eq!(buffer.root.metrics(), metric(3));
        println!("after: {}", buffer.root);
        buffer.delete(metric(0), metric(1));
        assert_eq!(buffer.root.metrics(), metric(2));
        println!("after: {}", buffer.root);
    }

    #[test]
    fn test_delete_range_internal() {
        let builder = &mut TreeBuilderBasic { count: 6, step: 4 };
        let mut buffer = BufferMetrics::build(builder);
        println!("init: {}", buffer.root);
        buffer.delete(metric(0), metric(12));
        assert_eq!(buffer.root.metrics(), metric(12));
        println!("after: {}", buffer.root);

        let builder = &mut TreeBuilderBasic { count: 6, step: 4 };
        let mut buffer = BufferMetrics::build(builder);
        println!("init: {}", buffer.root);
        buffer.delete(metric(12), metric(24));
        assert_eq!(buffer.root.metrics(), metric(12));
        println!("after: {}", buffer.root);
    }

    #[test]
    fn test_split() {
        let builder = &mut TreeBuilderBasic { count: 20, step: 1 };
        let mut buffer = BufferMetrics::build(builder);
        println!("init: {}", buffer.root);
        let right = buffer.root.split(metric(10));
        println!("left: {}", buffer.root);
        println!("right: {}", buffer.root);
        assert_eq!(buffer.root.metrics(), right.metrics());
        for i in 0..10 {
            println!("searching for {i}");
            let cmp = mock_search_char(&buffer.root, i);
            assert_eq!(cmp, metric(i));
            let cmp = mock_search_char(&right, i);
            assert_eq!(cmp, metric(i));
        }
    }

    #[test]
    fn test_append() {
        let builder = &mut TreeBuilderBasic { count: 10, step: 1 };
        let mut buffer = BufferMetrics::build(builder);
        println!("init: {}", buffer.root);
        let builder = &mut TreeBuilderBasic { count: 10, step: 1 };
        let right = BufferMetrics::build(builder);
        println!("right: {}", right.root);
        buffer.root.append(right.root);
        println!("after: {}", buffer.root);
        for i in 0..20 {
            println!("searching for {i}");
            let cmp = mock_search_char(&buffer.root, i);
            assert_eq!(cmp, metric(i));
        }
    }

    #[test]
    fn test_build() {
        {
            let builder = &mut TreeBuilderBasic { count: 0, step: 0 };
            let buffer = BufferMetrics::build(builder);
            assert_eq!(buffer.root.len(), 0);
        }
        {
            let builder = &mut TreeBuilderBasic { count: 1, step: 1 };
            let buffer = BufferMetrics::build(builder);
            assert_eq!(buffer.root.len(), 1);
        }
        let builder = &mut TreeBuilderBasic { count: 20, step: 1 };
        let buffer = BufferMetrics::build(builder);
        println!("{}", buffer.root);
        for i in 0..20 {
            println!("searching for {i}");
            let cmp = mock_search_char(&buffer.root, i);
            assert_eq!(cmp, metric(i));
        }
    }
}
