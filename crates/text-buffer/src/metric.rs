use get_size2::GetSize;
use smallvec::{smallvec, SmallVec};
use std::{
    fmt,
    iter::Sum,
    mem,
    ops::{Add, AddAssign, RangeBounds, Sub, SubAssign},
};

const MAX: usize = 6;
const MIN: usize = MAX / 2;
#[cfg(test)]
pub(crate) const MAX_LEAF: usize = 18;
#[cfg(not(test))]
pub(crate) const MAX_LEAF: usize = 8000;

type Metrics = SmallVec<[Metric; MAX]>;

#[derive(Debug, Default, GetSize)]
struct Internal {
    #[get_size(size_fn = smallvec_size_helper)]
    metrics: Metrics,
    #[get_size(size_fn = smallvec_size_helper)]
    children: SmallVec<[Box<Node>; MAX]>,
}

fn smallvec_size_helper<T>(slice: &[T]) -> usize
where
    T: GetSize,
{
    slice.iter().map(GetSize::get_heap_size).sum()
}

impl Internal {
    fn len(&self) -> usize {
        debug_assert_eq!(self.metrics.len(), self.children.len());
        self.metrics.len()
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
        let last = self.metrics.len() - 1;
        for (i, metric) in self.metrics[..last].iter().enumerate() {
            if char_pos < acc.chars + metric.chars {
                return (i, acc);
            }
            acc += *metric;
        }
        (last, acc)
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

        #[expect(clippy::borrowed_box)]
        let free_nodes = |x: &Box<Node>| x.len().saturating_sub(MIN);

        let left_free = if idx == 0 { 0 } else { free_nodes(&self.children[idx - 1]) };
        let right_free = self.children.get(idx + 1).map_or(0, free_nodes);
        if left_free + right_free >= missing {
            // short circuit
            let failed = self.try_steal_left(idx) && self.try_steal_right(idx);
            debug_assert!(!failed);
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
        let right_idx = if idx == 0 { idx + 1 } else { idx };
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
        false
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
        false
    }
}

#[derive(Debug, Default, GetSize)]
struct Leaf {
    #[get_size(size_fn = smallvec_size_helper)]
    metrics: Metrics,
}

impl Leaf {
    fn len(&self) -> usize {
        self.metrics.len()
    }

    fn insert_at(&mut self, idx: usize, pos: Metric, data: Metric) -> Option<Box<Node>> {
        if (self.metrics[idx].bytes + data.bytes) < MAX_LEAF {
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
            let right = Node::Leaf(Leaf { metrics: right_metrics });
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
            let right = Node::Leaf(Leaf { metrics: smallvec![metric] });
            Some(Box::new(right))
        }
    }
}

#[derive(Debug, Default, GetSize)]
pub(crate) struct BufferMetrics {
    root: Node,
}

impl BufferMetrics {
    pub(crate) fn search_char(&self, chars: usize) -> (Metric, usize) {
        self.root.search_char(chars)
    }

    pub(crate) fn search_byte(&self, bytes: usize) -> (Metric, usize) {
        self.root.search_byte(bytes)
    }

    pub(crate) fn len(&self) -> Metric {
        self.root.metrics()
    }

    pub(crate) fn build(metrics: impl Iterator<Item = Metric>) -> Self {
        // build the base layer of leaf nodes
        let len = metrics.size_hint().0;
        let remainder = len % MAX;
        let split_idx = if remainder != 0 && remainder != len && remainder < MIN {
            // If that last node is too small then merge it with the
            // previous one by splitting it early
            len - MIN - 1
        } else {
            // index will never equal len
            len
        };
        let mut leaf = Leaf::default();
        let cap = (len / MAX) + 1;
        let mut nodes = Vec::with_capacity(cap);
        for (idx, metric) in metrics.enumerate() {
            leaf.push(metric);
            if leaf.len() == MAX || idx == split_idx {
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
            debug_assert_eq!(next_level.len(), parent_count);
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
            if pos.bytes == 0 {
                // append at the start by swapping self and new
                let new_pos = new.root.metrics().chars;
                let right = mem::replace(self, new);
                self.root.append(right.root);
                self.root.fix_seam(new_pos);
            } else if len.bytes == pos.bytes {
                // append at the end
                self.root.append(new.root);
                self.root.fix_seam(pos.chars);
            } else {
                // splice in the middle
                let right_metric = self.root.metrics() - pos;
                let new_metric = new.root.metrics();
                let right = self.root.split(pos);
                debug_assert_eq!(self.root.metrics(), pos);
                debug_assert_eq!(right.metrics(), right_metric);
                self.root.append(new.root);
                self.root.append(right);
                debug_assert_eq!(self.root.metrics(), pos + right_metric + new_metric);
                self.root.fix_seam(pos.chars);
                self.root.fix_seam(pos.chars + new_metric.chars);
            }
            self.root.collapse();
        }
        self.assert_invariants();
    }

    pub(crate) fn delete(&mut self, start: Metric, end: Metric) {
        debug_assert!(start.bytes <= end.bytes);
        debug_assert!(start.chars <= end.chars);
        if start.bytes == end.bytes {
            return;
        }
        if start.bytes == 0 && end.bytes == self.root.metrics().bytes {
            // delete the whole tree
            self.root = Node::default();
            return;
        }

        let fix_seam = self.root.delete_impl(start, end);
        if fix_seam {
            self.root.fix_seam(start.chars);
        }
        self.root.collapse();
        self.assert_invariants();
    }

    fn assert_invariants(&self) {
        if cfg!(debug_assertions) {
            self.root.assert_integrity();
            self.root.assert_node_size(true);
            self.root.assert_balance();
        }
    }
}

#[derive(Debug, GetSize)]
enum Node {
    Leaf(Leaf),
    Internal(Internal),
}

impl Node {
    fn metric_slice(&self) -> &[Metric] {
        match self {
            Self::Internal(x) => &x.metrics,
            Self::Leaf(x) => &x.metrics,
        }
    }

    fn is_underfull(&self) -> bool {
        match self {
            Node::Leaf(leaf) => leaf.len() < MIN,
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
            }
            acc += *metric;
        }
        (last, acc)
    }

    /// If only a single child remains, collapse the node into that child.
    fn collapse(&mut self) {
        while self.len() == 1 {
            match self {
                Node::Internal(int) => {
                    let child = int.children.pop().unwrap();
                    let _ = mem::replace(self, *child);
                }
                Node::Leaf(_) => break,
            }
        }
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
            Node::Internal(int) => {
                if let Some(new) = int.children[idx].insert_impl(offset, data) {
                    int.insert_node(idx, new)
                } else {
                    int.metrics[idx] += data;
                    None
                }
            }
        }
    }

    fn delete_impl(&mut self, start: Metric, end: Metric) -> bool {
        self.assert_node_integrity();
        assert!(start.chars <= end.chars);
        let ((start_idx, start), (end_idx, end)) = self.get_delete_indices(start, end);

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
                        debug_assert!(!fix);
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
                    let mut fix_seam = false;
                    let mut merge_left = false;
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
                        let end_idx = if start_idx == start_delete {
                            start_idx
                        } else {
                            debug_assert_eq!(
                                end_idx,
                                start_idx + 1 + end_delete.saturating_sub(start_delete)
                            );
                            start_idx + 1
                        };
                        fix_seam |= int.children[end_idx].delete_impl(Metric::default(), end);
                        int.metrics[end_idx] -= end;
                        // merge right child first so that the index of left is not changed
                        if int.children[end_idx].is_underfull() {
                            fix_seam |= int.balance_node(end_idx);
                        }
                    }
                    if merge_left {
                        fix_seam |= int.balance_node(start_idx);
                    }
                    fix_seam
                }
            }
            Node::Leaf(leaf) => {
                if start_idx == end_idx {
                    let chunk = end - start;
                    if chunk == leaf.metrics[start_idx] {
                        leaf.metrics.remove(start_idx);
                    } else {
                        leaf.metrics[start_idx] -= chunk;
                    }
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

    fn get_delete_indices(&self, start: Metric, end: Metric) -> ((usize, Metric), (usize, Metric)) {
        let (mut start, mut end) = (start, end);
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
                start -= metric;
            }
            end -= metric;
        }
        ((start_idx.unwrap(), start), (end_idx.unwrap(), end))
    }

    fn merge_node(&mut self, node: Option<Box<Node>>, metric: Metric, idx: usize) {
        match (self, node) {
            // TODO don't recalculate the metric
            (Node::Internal(int), Some(node)) => int.insert(idx, node),
            (Node::Leaf(leaf), None) => leaf.metrics.insert(idx, metric),
            _ => unreachable!("cannot merge internal and leaf nodes"),
        }
    }

    fn merge_sibling(&mut self, right: &mut Self) -> bool {
        assert!(self.len() + right.len() <= MAX);
        match (self, right) {
            (Node::Internal(left), Node::Internal(right)) => {
                left.metrics.append(&mut right.metrics);
                left.children.append(&mut right.children);
                left.len() < MIN
            }
            (Node::Leaf(left), Node::Leaf(right)) => {
                left.metrics.append(&mut right.metrics);
                left.len() < MIN
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
            Node::Leaf(leaf) if leaf.len() > MIN => {
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

                if int.children[idx].is_underfull() {
                    int.balance_node(idx);
                }
                let on_seam = metric.chars == char_pos && idx > 0;
                if on_seam && int.children[idx - 1].is_underfull() {
                    // we are on a seam and there is a left sibling
                    int.balance_node(idx - 1);
                }

                // recalculate because position might have changed
                let (idx, metric) = int.search_char_pos(char_pos);

                let mut retry = false;
                // recurse into children
                let on_seam = metric.chars == char_pos && idx > 0;
                if on_seam {
                    let new_pos = int.metrics[idx - 1].chars;
                    retry |= int.children[idx - 1].fix_seam(new_pos);
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
        if other_depth <= self_depth {
            let new = self.append_at_depth(other, self_depth - other_depth);

            if let Some(right) = new {
                // split the root, making the old root the left child
                let left = mem::replace(self, Node::Internal(Internal::default()));
                let Node::Internal(int) = self else { unreachable!() };
                int.metrics = smallvec![left.metrics(), right.metrics()];
                int.children = smallvec![Box::new(left), right];
            }
        } else {
            let left = mem::replace(self, other);
            let new = self.prepend_at_depth(left, other_depth - self_depth);

            if let Some(left) = new {
                // split the root, making the old root the right child
                let right = mem::replace(self, Node::Internal(Internal::default()));
                let Node::Internal(int) = self else { unreachable!() };
                int.metrics = smallvec![left.metrics(), right.metrics()];
                int.children = smallvec![left, Box::new(right)];
            }
        };
    }

    fn append_at_depth(&mut self, other: Self, depth: usize) -> Option<Box<Node>> {
        if depth == 0 {
            match (self, other) {
                (Node::Leaf(left), Node::Leaf(mut right)) => {
                    if left.len() + right.len() <= MAX {
                        left.metrics.extend(right.metrics.drain(..));
                        None
                    } else {
                        Some(Box::new(Node::Leaf(right)))
                    }
                }
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
                None => {
                    let update = int.children.last().unwrap().metrics();
                    *int.metrics.last_mut().unwrap() = update;
                    None
                }
            }
        } else {
            unreachable!("reached leaf node while depth was non-zero");
        }
    }

    fn prepend_at_depth(&mut self, other: Self, depth: usize) -> Option<Box<Node>> {
        if depth == 0 {
            match (other, self) {
                (Node::Leaf(mut left), Node::Leaf(right)) => {
                    if left.len() + right.len() <= MAX {
                        left.metrics.extend(right.metrics.drain(..));
                        *right = left;
                        None
                    } else {
                        Some(Box::new(Node::Leaf(left)))
                    }
                }
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
                None => {
                    let update = int.children[0].metrics();
                    int.metrics[0] = update;
                    None
                }
            }
        } else {
            unreachable!("reached leaf node while depth was non-zero");
        }
    }

    fn search_char(&self, chars: usize) -> (Metric, usize) {
        self.search_impl(chars, |x| x.chars)
    }

    fn search_byte(&self, bytes: usize) -> (Metric, usize) {
        self.search_impl(bytes, |x| x.bytes)
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
                // if it is ascii then we can just calculate the offset
                if metric.is_ascii() {
                    let offset = Metric { bytes: needle, chars: needle };
                    return (sum + offset, 0);
                }
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
        if cfg!(debug_assertions) {
            match self {
                Node::Internal(int) => {
                    assert!(!int.metrics.is_empty());
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
                    assert!(leaf.len() >= MIN);
                }
            }
            Node::Internal(int) => {
                assert!(int.len() <= MAX);
                assert!(int.len() >= 2);
                if !is_root {
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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

#[derive(Debug, Default, Copy, Clone, Eq, GetSize)]
pub(crate) struct Metric {
    pub(crate) bytes: usize,
    pub(crate) chars: usize,
}

impl PartialEq for Metric {
    fn eq(&self, other: &Self) -> bool {
        let eq = self.bytes == other.bytes;
        if eq {
            debug_assert_eq!(self.chars, other.chars);
        } else {
            debug_assert_ne!(self.chars, other.chars);
        }
        eq
    }
}

impl Metric {
    fn is_ascii(&self) -> bool {
        self.bytes == self.chars
    }
}

impl fmt::Display for Metric {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
        Self { bytes: self.bytes + rhs.bytes, chars: self.chars + rhs.chars }
    }
}

impl Sub for Metric {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self { bytes: self.bytes - rhs.bytes, chars: self.chars - rhs.chars }
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
        Metric { bytes: x * 2, chars: x }
    }

    fn mock_search_char(root: &Node, needle: usize) -> Metric {
        let (metric, offset) = root.search_char(needle);
        Metric { bytes: metric.bytes + offset * 2, chars: metric.chars + offset }
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
            let cmp = mock_search_char(&root.root, i);
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
