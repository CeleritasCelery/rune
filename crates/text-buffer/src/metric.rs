#![allow(dead_code)]

use smallvec::{smallvec, SmallVec};
use std::{
    fmt,
    iter::Sum,
    mem,
    ops::{Add, AddAssign, Sub, SubAssign},
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

    fn insert(&mut self, idx: usize, child: Box<Node>) {
        let metric = child.metrics();
        self.children.insert(idx, child);
        self.metrics.insert(idx, metric);
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

#[derive(Debug)]
enum Node {
    Leaf(Leaf),
    Internal(Internal),
}

impl Node {
    fn new() -> Self {
        Self::Leaf(Leaf::default())
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

    fn search_char_pos(&self, char_pos: usize) -> (usize, Metric) {
        let metrics = self.metric_slice();
        let mut acc = Metric::default();
        for (i, metric) in metrics.iter().enumerate() {
            if char_pos <= acc.chars + metric.chars {
                return (i, acc);
            } else {
                acc += *metric;
            }
        }
        unreachable!("char index {} out of bounds", char_pos);
    }

    /// May return one past the end
    fn search_char_pos_le(&self, char_pos: usize) -> (usize, Metric) {
        let metrics = self.metric_slice();
        let mut acc = Metric::default();
        for (i, metric) in metrics.iter().enumerate() {
            if char_pos < acc.chars + metric.chars {
                return (i, acc);
            } else {
                acc += *metric;
            }
        }
        (metrics.len(), acc)
    }

    pub(crate) fn insert(&mut self, needle: Metric) {
        let size = self.metrics();
        let new = if self.len() == 0 || size.chars < needle.chars {
            self.append_impl(needle - size)
        } else {
            self.insert_impl(needle)
        };

        if let Some(right) = new {
            // split the root, making the old root the left child
            let left = mem::replace(self, Node::Internal(Internal::default()));
            match self {
                Node::Internal(int) => {
                    int.metrics = smallvec![left.metrics(), right.metrics()];
                    int.children = smallvec![Box::new(left), right];
                }
                Node::Leaf(_) => unreachable!(),
            }
        }
    }

    fn append_impl(&mut self, metric: Metric) -> Option<Box<Node>> {
        self.assert_integrity();
        match self {
            Node::Leaf(leaf) => leaf.push(metric),
            Node::Internal(int) => {
                let last = int.len() - 1;
                match int.children[last].append_impl(metric) {
                    Some(new) => int.insert_node(last, new),
                    None => {
                        int.metrics[last] += metric;
                        None
                    }
                }
            }
        }
    }

    fn insert_impl(&mut self, needle: Metric) -> Option<Box<Node>> {
        self.assert_integrity();
        let (idx, metric) = self.search_char_pos(needle.chars);
        match self {
            Node::Internal(int) => {
                let new = match int.children[idx].insert_impl(needle - metric) {
                    Some(new) => int.insert_node(idx, new),
                    None => None,
                };
                return new;
            }
            Node::Leaf(leaf) => leaf.insert_node(idx, needle - metric),
        }
    }

    fn delete(&mut self, start: Metric, end: Metric) {
        assert!(start.bytes <= end.bytes);
        assert!(start.chars <= end.chars);
        let fix_seam = self.delete_impl(start, end);
        if fix_seam {
            self.fix_seam(start.chars);
        }
        while self.len() == 1 {
            match self {
                Node::Internal(int) => {
                    // collapse the root
                    let child = int.children.pop().unwrap();
                    let _ = mem::replace(self, *child);
                }
                Node::Leaf(_) => break,
            }
        }
    }

    fn delete_impl(&mut self, mut start: Metric, mut end: Metric) -> bool {
        self.assert_integrity();
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
        let (idx, metric) = self.search_char_pos_le(pos.chars);
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
                    right.metrics.extend(int.metrics.drain(idx + 1..));
                    right.children.extend(int.children.drain(idx + 1..));
                }
                Node::Internal(right)
            }
        }
    }

    pub(crate) fn search_byte(&self, bytes: usize) -> (Metric, usize) {
        self.search_impl::<{ Self::BYTE }>(bytes)
    }

    pub(crate) fn search_char(&self, chars: usize) -> (Metric, usize) {
        self.search_impl::<{ Self::CHAR }>(chars)
    }

    const BYTE: u8 = 0;
    const CHAR: u8 = 1;

    fn search_impl<const TYPE: u8>(&self, needle: usize) -> (Metric, usize) {
        self.assert_integrity();
        let mut needle = needle;
        let mut sum = Metric::default();
        for (idx, metric) in self.metric_slice().iter().enumerate() {
            // fast path if we happen get the exact position in the node
            if needle == 0 {
                break;
            }
            let pos = match TYPE {
                Self::BYTE => metric.bytes,
                Self::CHAR => metric.chars,
                _ => unreachable!(),
            };
            if needle < pos {
                let child_sum = match &self {
                    Node::Internal(int) => {
                        let (metric, offset) = int.children[idx].search_impl::<TYPE>(needle);
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

    // Go to a the correct node and then add the value of new to the metric there
    fn add(&mut self, char_pos: usize, new: Metric) {
        self.assert_integrity();
        let (idx, metric) = self.search_char_pos(char_pos);

        match self {
            Node::Leaf(leaf) => leaf.metrics[idx] += new,
            Node::Internal(int) => {
                int.metrics[idx] += new;
                int.children[idx].add(char_pos - metric.chars, new)
            }
        };
    }

    fn assert_integrity(&self) {
        match self {
            Node::Internal(int) => {
                assert!(int.metrics.len() <= MAX);
                assert_eq!(int.metrics.len(), int.children.len());
                for i in 0..int.children.len() {
                    assert_eq!(int.children[i].metrics(), int.metrics[i]);
                }
            }
            Node::Leaf(leaf) => {
                assert!(leaf.metrics.len() <= MAX);
            }
        };
    }

    fn assert_invariants(&self) {
        match self {
            Node::Internal(int) => {
                assert!(int.metrics.len() <= MAX);
                assert_eq!(int.metrics.len(), int.children.len());
                assert!(int.metrics.len() >= MIN);
                for i in 0..int.children.len() {
                    assert_eq!(int.children[i].metrics(), int.metrics[i]);
                }
            }
            Node::Leaf(leaf) => {
                assert!(leaf.metrics.len() <= MAX);
                assert!(!leaf.metrics.is_empty());
            }
        };
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
struct Metric {
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

    #[test]
    fn test_insert() {
        let mut root = Node::new();
        root.insert(metric(10));
        println!("{}", root);
        root.insert(metric(5));
        for i in 0..10 {
            println!("pushing {i}");
            root.insert(metric(i));
            println!("{}", root);
        }
    }

    #[test]
    fn test_push() {
        let mut root = Node::new();
        println!("{}", root);
        for i in 1..20 {
            println!("pushing {i}");
            root.insert(metric(i));
            println!("{}", root);
        }
    }

    #[test]
    fn test_search() {
        let mut root = Node::new();
        for i in 1..20 {
            root.insert(metric(i));
        }
        for i in 0..20 {
            println!("searching for {i}");
            let cmp = mock_search_byte(&root, i * 2);
            assert_eq!(cmp, metric(i));
        }
    }

    #[test]
    fn test_search_chars() {
        let mut root = Node::new();
        for i in 1..20 {
            root.insert(metric(i));
        }
        for i in 0..20 {
            println!("searching for {i}");
            let cmp = mock_search_char(&root, i);
            assert_eq!(cmp, metric(i));
        }
    }

    #[test]
    fn test_add() {
        let mut root = Node::new();
        for i in 1..20 {
            root.insert(metric(i));
        }
        for i in 0..20 {
            let cmp = mock_search_char(&root, i);
            assert_eq!(cmp, metric(i));
        }
        println!("init {root}");
        for i in (1..20).rev() {
            println!("adding {i}");
            root.add(i, metric(1));
            println!("{}", root);
        }
        for i in (0..20).step_by(2) {
            println!("searching for {i}");
            let cmp = mock_search_char(&root, i);
            assert_eq!(cmp, metric(i));
        }
    }

    #[test]
    fn test_delete_range_leaf() {
        let mut root = Node::new();
        // shouldn't need more then a single leaf node
        root.insert(metric(12));
        root.insert(metric(4));
        root.insert(metric(8));
        assert_eq!(root.metrics(), metric(12));
        println!("init: {root}");
        root.delete(metric(1), metric(3));
        assert_eq!(root.metrics(), metric(10));
        println!("after: {root}");
        root.delete(metric(2), metric(6));
        assert_eq!(root.metrics(), metric(6));
        println!("after: {root}");
        root.delete(metric(1), metric(4));
        assert_eq!(root.metrics(), metric(3));
        println!("after: {root}");
        root.delete(metric(0), metric(1));
        assert_eq!(root.metrics(), metric(2));
        println!("after: {root}");
    }

    #[test]
    fn test_delete_range_internal() {
        let mut root = Node::new();
        root.insert(metric(24));
        root.insert(metric(20));
        root.insert(metric(16));
        root.insert(metric(12));
        root.insert(metric(8));
        println!("init: {root}");
        root.delete(metric(0), metric(12));
        assert_eq!(root.metrics(), metric(12));
        println!("after: {root}");

        let mut root = Node::new();
        root.insert(metric(24));
        root.insert(metric(20));
        root.insert(metric(16));
        root.insert(metric(12));
        root.insert(metric(8));
        println!("init: {root}");
        root.delete(metric(12), metric(24));
        assert_eq!(root.metrics(), metric(12));
        println!("after: {root}");
    }

    #[test]
    fn test_split() {
        let mut root = Node::new();
        for i in 1..=20 {
            root.insert(metric(i));
        }
        println!("init: {root}");
        let right = root.split(metric(10));
        println!("left: {root}");
        println!("right: {root}");
        assert_eq!(root.metrics(), right.metrics());
        for i in 0..10 {
            println!("searching for {i}");
            let cmp = mock_search_char(&root, i);
            assert_eq!(cmp, metric(i));
            let cmp = mock_search_char(&right, i);
            assert_eq!(cmp, metric(i));
        }
    }
}
