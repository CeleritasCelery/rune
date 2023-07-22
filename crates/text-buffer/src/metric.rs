#![allow(dead_code)]

use smallvec::{smallvec, SmallVec};
use std::{
    fmt,
    iter::Sum,
    mem,
    ops::{Add, AddAssign, Sub, SubAssign},
    ptr::NonNull,
};

const MAX: usize = 4;
const MIN: usize = MAX / 2;

type Metrics = SmallVec<[Metric; MAX]>;
type IntChildren = SmallVec<[Box<Internal>; MAX]>;
type LeafChildren = SmallVec<[Box<Leaf>; MAX]>;

fn new_root() -> Box<Internal> {
    let mut root = Box::new(Internal::new_leaf());
    root.metrics.push(Metric::default());
    let root_ptr = NonNull::from(&*root);
    if let Children::Leaf(children) = &mut root.children {
        children.push(Box::new(Leaf::new(root_ptr)));
    }
    root
}

#[derive(Debug)]
enum Children {
    Internal(IntChildren),
    Leaf(LeafChildren),
}

#[derive(Debug)]
enum Node {
    Internal(Box<Internal>),
    Leaf(Box<Leaf>),
}

impl Node {
    fn set_parent(&mut self, parent: &Internal) {
        let parent_ptr = NonNull::from(parent);
        match self {
            Node::Internal(x) => x.parent = Some(parent_ptr),
            Node::Leaf(x) => x.parent = Some(parent_ptr),
        }
    }
}

impl Children {
    fn len(&self) -> usize {
        match self {
            Children::Internal(x) => x.len(),
            Children::Leaf(x) => x.len(),
        }
    }

    fn set_parent(&mut self, parent: NonNull<Internal>) {
        match self {
            Children::Internal(x) => {
                for child in x.iter_mut() {
                    child.parent = Some(parent);
                }
            }
            Children::Leaf(x) => {
                for child in x.iter_mut() {
                    child.parent = Some(parent);
                }
            }
        }
    }

    fn assert_parent(&self, parent: NonNull<Internal>) {
        match self {
            Children::Internal(x) => {
                for child in x.iter() {
                    assert_eq!(child.parent, Some(parent));
                }
            }
            Children::Leaf(x) => {
                for child in x.iter() {
                    assert_eq!(child.parent, Some(parent));
                }
            }
        }
    }
}

#[derive(Debug)]
struct Internal {
    metrics: Metrics,
    children: Children,
    parent: Option<NonNull<Internal>>,
}

impl Internal {
    fn new_leaf() -> Self {
        Self {
            metrics: SmallVec::new(),
            children: Children::Leaf(SmallVec::new()),
            parent: None,
        }
    }

    fn new_internal() -> Self {
        Self {
            metrics: SmallVec::new(),
            children: Children::Internal(SmallVec::new()),
            parent: None,
        }
    }

    fn clear_parent(mut self: Box<Self>) -> Box<Self> {
        self.parent = None;
        self
    }

    fn metrics(&self) -> Metric {
        self.metrics.iter().copied().sum()
    }

    fn len(&self) -> usize {
        self.children.len()
    }

    fn insert_internal(
        children: &mut IntChildren,
        metrics: &mut Metrics,
        idx: usize,
        new_child: Box<Internal>,
    ) -> Option<Box<Internal>> {
        let len = children.len();
        // update the metrics for the current child
        metrics[idx] = children[idx].metrics();
        // shift idx to the right
        let idx = idx + 1;
        if len < MAX {
            // If there is room in this node then insert the
            // node before the current one
            metrics.insert(idx, new_child.metrics());
            children.insert(idx, new_child);
            None
        } else {
            assert_eq!(len, MAX);
            // split this node into two and return the left one
            let middle = MAX / 2;

            let mut right_metrics: Metrics = metrics.drain(middle..).collect();
            let mut right_children: IntChildren =
                children.drain(middle..).map(|x| x.clear_parent()).collect();
            if idx < middle {
                metrics.insert(idx, new_child.metrics());
                children.insert(idx, new_child);
            } else {
                right_metrics.insert(idx - middle, new_child.metrics());
                right_children.insert(idx - middle, new_child);
            }
            let right = Internal {
                metrics: right_metrics,
                children: Children::Internal(right_children),
                parent: None,
            };
            // Box it so it has a stable address
            let mut boxed = Box::new(right);
            let child_parent = NonNull::from(&*boxed);
            boxed.children.set_parent(child_parent);
            Some(boxed)
        }
    }

    fn insert_leaf(
        children: &mut LeafChildren,
        metrics: &mut Metrics,
        self_ptr: NonNull<Internal>,
        idx: usize,
        needle: Metric,
    ) -> Option<Box<Internal>> {
        let len = children.len();
        let new_metric = metrics[idx] - needle;
        metrics[idx] = needle;
        // shift idx to the right
        let idx = idx + 1;
        if len < MAX {
            // If there is room in this node then insert the
            // leaf before the current one, splitting the
            // size
            metrics.insert(idx, new_metric);
            children.insert(idx, Box::new(Leaf::new(self_ptr)));
            None
        } else {
            assert_eq!(len, MAX);
            // split this node into two and return the left one
            let middle = MAX / 2;
            let mut right_metrics: Metrics = metrics.drain(middle..).collect();
            let mut right_children: LeafChildren =
                children.drain(middle..).map(|x| x.clear_parent()).collect();
            let mut new = Leaf::default();
            if idx < middle {
                new.parent = Some(self_ptr);
                metrics.insert(idx, new_metric);
                children.insert(idx, Box::new(new));
            } else {
                right_metrics.insert(idx - middle, new_metric);
                right_children.insert(idx - middle, Box::new(new));
            }
            let right = Internal {
                metrics: right_metrics,
                children: Children::Leaf(right_children),
                parent: None,
            };
            let mut boxed = Box::new(right);
            // update the children's parent pointer
            let child_parent = NonNull::from(&*boxed);
            boxed.children.set_parent(child_parent);
            Some(boxed)
        }
    }

    fn push_leaf(
        children: &mut LeafChildren,
        metrics: &mut Metrics,
        self_ptr: NonNull<Internal>,
        metric: Metric,
    ) -> Option<Box<Internal>> {
        let len = children.len();
        if len < MAX {
            // If there is room in this node then insert the
            // leaf before the current one, splitting the
            // size
            metrics.push(metric);
            children.push(Box::new(Leaf::new(self_ptr)));
            None
        } else {
            assert_eq!(len, MAX);
            // split this node into two and return the left one
            let new = Leaf::default();
            let right_metrics: Metrics = smallvec![metric];
            let right_children: LeafChildren = smallvec![Box::new(new)];
            let right = Internal {
                metrics: right_metrics,
                children: Children::Leaf(right_children),
                parent: None,
            };
            let mut boxed = Box::new(right);
            // update the children's parent pointer
            let child_parent = NonNull::from(&*boxed);
            boxed.children.set_parent(child_parent);
            Some(boxed)
        }
    }

    pub(crate) fn insert(self: &mut Box<Self>, needle: Metric) {
        match self.insert_impl(needle) {
            None => {}
            Some(right) => {
                // split the root, making the old root the left child
                let left = mem::replace(self, Box::new(Internal::new_internal()));
                self.metrics = smallvec![left.metrics(), right.metrics()];
                self.children = Children::Internal(smallvec![left, right]);
                let this = NonNull::from(&**self);
                self.children.set_parent(this);
            }
        }
    }

    fn insert_impl(&mut self, mut needle: Metric) -> Option<Box<Internal>> {
        self.assert_invariants();
        let self_ptr = NonNull::from(&*self);
        let last = self.metrics.len() - 1;
        for (idx, metric) in self.metrics.iter_mut().enumerate() {
            let in_range = needle.chars < metric.chars;
            if idx == last || in_range {
                let mut new = match &mut self.children {
                    // call recursively and insert the new node
                    Children::Internal(children) => match children[idx].insert_impl(needle) {
                        Some(new) => Self::insert_internal(children, &mut self.metrics, idx, new),
                        None => {
                            // update the metric of the current node because we
                            // increased the max size
                            if !in_range {
                                assert_eq!(idx, last);
                                *metric = children.last().unwrap().metrics();
                            }
                            None
                        }
                    },
                    Children::Leaf(children) => {
                        if in_range {
                            Self::insert_leaf(children, &mut self.metrics, self_ptr, idx, needle)
                        } else {
                            assert_eq!(idx, last);
                            needle -= *metric;
                            Self::push_leaf(children, &mut self.metrics, self_ptr, needle)
                        }
                    }
                };
                // set the parent pointer of the new node
                if let Some(new) = &mut new {
                    new.parent = self.parent;
                }
                return new;
            } else {
                needle -= *metric;
            }
        }
        unreachable!("we should always recurse into a child node");
    }

    fn insert_child(
        &mut self,
        idx: usize,
        this: NonNull<Self>,
        needle: Metric,
    ) -> Option<Box<Self>> {
        let metrics = &mut self.metrics;
        let mut new = match &mut self.children {
            // call recursively and insert the new node
            Children::Internal(children) => match children[idx].insert_impl(needle) {
                Some(new) => Self::insert_internal(children, metrics, idx, new),
                None => None,
            },
            Children::Leaf(children) => Self::insert_leaf(children, metrics, this, idx, needle),
        };
        // set the parent pointer of the new node
        if let Some(new) = &mut new {
            new.parent = self.parent;
        }
        new
    }

    fn delete_range(&mut self, mut start: Metric, mut end: Metric) -> bool {
        self.assert_invariants();
        assert!(start.chars <= end.chars);
        let (start_idx, end_idx) = self.get_delete_indices(&mut start, &mut end);

        match &mut self.children {
            Children::Internal(_) => {}
            Children::Leaf(children) => {
                let end_metric = self.metrics[end_idx];
                if start.bytes == 0 {
                    assert_eq!(start_idx, 0);
                }
                let start_delete = if start.bytes == 0 { start_idx } else { start_idx + 1 };
                let end_delete = if end.bytes != end_metric.bytes { end_idx } else { end_idx + 1 };

                if start_delete < end_delete {
                    // There is a range of children to delete in the middle
                    self.metrics[end_idx] -= end;
                    self.metrics[start_idx] = start;
                    children.drain(start_delete..end_delete);
                    self.metrics.drain(start_delete..end_delete);
                } else {
                    if start_idx == end_idx {
                        self.metrics[start_idx] -= end - start;
                    } else {
                        self.metrics[end_idx] -= end;
                        self.metrics[start_idx] = start;
                    }
                }
                return children.len() == 0;
            }
        }
        unreachable!("we should always recurse into a child node");
    }

    fn get_delete_indices(&self, start: &mut Metric, end: &mut Metric) -> (usize, usize) {
        let len = self.metrics.len();
        let mut start_idx = None;
        let mut end_idx = None;
        for idx in 0..len {
            let metric = self.metrics[idx];
            if start_idx.is_none() && start.chars <= metric.chars {
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

    fn delete(self: &mut Box<Self>, pos: Metric) {
        if self.delete_impl(pos) {
            // shrink the height of the tree
            assert_eq!(self.metrics.len(), 1);
            match &mut self.children {
                Children::Internal(children) => {
                    assert_eq!(children.len(), 1);
                    let child = children.pop().unwrap();
                    let _ = mem::replace(self, child);
                    self.parent = None;
                }
                Children::Leaf(children) => {
                    assert_eq!(children.len(), 1);
                    todo!("delete final node")
                }
            }
        }
    }

    fn delete_impl(&mut self, mut pos: Metric) -> bool {
        self.assert_invariants();
        let len = self.metrics.len();
        for idx in 0..len {
            let metric = self.metrics[idx];
            if pos.chars < metric.chars {
                match &mut self.children {
                    Children::Internal(children) => {
                        let needs_merge = children[idx].delete_impl(pos);
                        if !needs_merge {
                            return false;
                        }
                        // see if we can steal from the left or right child
                        let left_sibling = idx.checked_sub(1).map(|i| &mut children[i]);
                        let left_node = left_sibling.and_then(|x| x.steal_greatest());
                        if let Some((node, node_metric)) = left_node {
                            let underfull_child = &mut children[idx];
                            underfull_child.merge_node(node, node_metric, 0);
                            self.metrics[idx] += node_metric;
                            self.metrics[idx - 1] -= node_metric;
                            return false;
                        }

                        let right_sibling = children.get_mut(idx + 1);
                        let right_node = right_sibling.and_then(|x| x.steal_least());
                        if let Some((node, node_metric)) = right_node {
                            let underfull_child = &mut children[idx];
                            let len = underfull_child.metrics.len();
                            underfull_child.merge_node(node, node_metric, len);
                            self.metrics[idx] += node_metric;
                            self.metrics[idx + 1] -= node_metric;
                            return false;
                        }

                        // merge with sibling
                        let right_idx = if idx != 0 { idx } else { idx + 1 };
                        let (left, right) = children.split_at_mut(right_idx);
                        let left = &mut left[right_idx - 1];
                        let right = &mut right[0];
                        left.merge_sibling(right);
                        children.remove(right_idx);
                        let right_metric = self.metrics.remove(right_idx);
                        self.metrics[right_idx - 1] += right_metric;
                        return children.len() < MIN;
                    }
                    Children::Leaf(children) => {
                        if children.len() > 1 {
                            if idx == 0 {
                                self.metrics[idx + 1] += metric;
                            } else {
                                self.metrics[idx - 1] += metric;
                            }
                            children.remove(idx);
                            self.metrics.remove(idx);
                            return false;
                        } else {
                            // needs to be merged
                            return true;
                        }
                    }
                }
            } else {
                pos -= metric;
            }
        }
        unreachable!("we should always recurse into a child node");
    }

    fn merge_node(&mut self, mut node: Node, metric: Metric, idx: usize) {
        node.set_parent(self);
        match (&mut self.children, node) {
            (Children::Internal(children), Node::Internal(node)) => {
                self.metrics.insert(idx, metric);
                children.insert(idx, node);
            }
            (Children::Leaf(children), Node::Leaf(node)) => {
                assert_eq!(children.len(), 1);
                self.metrics[0] += metric;
                children[0] = node;
            }
            _ => unreachable!("cannot merge internal and leaf nodes"),
        }
    }

    fn merge_sibling(&mut self, right: &mut Self) {
        let rf: &Self = self;
        let this = NonNull::from(rf);
        match (&mut self.children, &mut right.children) {
            (Children::Internal(left_children), Children::Internal(right_children)) => {
                for child in &mut *right_children {
                    child.parent = Some(this);
                }
                self.metrics.append(&mut right.metrics);
                left_children.append(right_children);
            }
            (Children::Leaf(children), Children::Leaf(right_children)) => {
                assert_eq!(children.len(), 1);
                assert_eq!(right_children.len(), 1);
                assert_eq!(self.metrics.len(), 1);
                assert_eq!(right.metrics.len(), 1);
                self.metrics[0] += right.metrics[0];
                right_children[0].parent = Some(this);
                children[0] = right_children.pop().unwrap();
            }
            _ => unreachable!("cannot merge internal and leaf nodes"),
        }
    }

    fn steal_greatest(&mut self) -> Option<(Node, Metric)> {
        match &mut self.children {
            Children::Internal(children) => match children.len() {
                0 | 1 => unreachable!("node should never be below MIN"),
                MIN => None,
                _ => {
                    let metric = self.metrics.pop().unwrap();
                    let child = children.pop().unwrap();
                    Some((Node::Internal(child), metric))
                }
            },
            Children::Leaf(children) => match children.len() {
                0 => unreachable!("leaf node should never be empty"),
                1 => None,
                _ => {
                    let metric = self.metrics.pop().unwrap();
                    let child = children.pop().unwrap();
                    Some((Node::Leaf(child), metric))
                }
            },
        }
    }

    fn steal_least(&mut self) -> Option<(Node, Metric)> {
        match &mut self.children {
            Children::Internal(children) => match children.len() {
                0 | 1 => unreachable!("node should never be below MIN"),
                MIN => None,
                _ => {
                    let metric = self.metrics.remove(0);
                    let child = children.remove(0);
                    Some((Node::Internal(child), metric))
                }
            },
            Children::Leaf(children) => match children.len() {
                0 => unreachable!("leaf node should never be empty"),
                1 => None,
                _ => {
                    let metric = self.metrics.remove(0);
                    let child = children.remove(0);
                    Some((Node::Leaf(child), metric))
                }
            },
        }
    }

    pub(crate) fn search_byte(&self, bytes: usize) -> Metric {
        self.search_impl::<{ Self::BYTE }>(bytes)
    }

    pub(crate) fn search_char(&self, chars: usize) -> Metric {
        self.search_impl::<{ Self::CHAR }>(chars)
    }

    const BYTE: u8 = 0;
    const CHAR: u8 = 1;

    fn search_impl<const TYPE: u8>(&self, needle: usize) -> Metric {
        self.assert_invariants();
        let mut needle = needle;
        let mut sum = Metric::default();
        for (idx, metric) in self.metrics.iter().enumerate() {
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
                let child_sum = match &self.children {
                    Children::Internal(children) => sum + children[idx].search_impl::<TYPE>(needle),
                    Children::Leaf(_) => sum,
                };
                return child_sum;
            }
            sum += *metric;
            needle -= pos;
        }
        sum
    }

    // Go to a the correct node and then add the value of new to the metric there
    fn add(&mut self, char_pos: usize, new: Metric) {
        self.assert_invariants();
        let mut char_pos = char_pos;
        for (idx, metric) in self.metrics.iter().enumerate() {
            let pos = metric.chars;
            // <= because we need to handle the last node correctly
            if char_pos <= pos {
                match &mut self.children {
                    Children::Internal(children) => {
                        self.metrics[idx] += new;
                        children[idx].add(char_pos, new)
                    }
                    Children::Leaf(_) => {
                        let metric = &mut self.metrics[idx];
                        *metric += new;
                    }
                };
                return;
            }
            char_pos -= pos;
        }
        unreachable!("we should always recurse into a child node");
    }

    fn remove(&mut self, char_pos: usize, update: Metric) {
        self.assert_invariants();
        let mut char_pos = char_pos;
        for (idx, metric) in self.metrics.iter().enumerate() {
            let pos = metric.chars;
            // <= because we need to handle the last node correctly
            if char_pos <= pos {
                match &mut self.children {
                    Children::Internal(children) => {
                        self.metrics[idx] -= update;
                        children[idx].remove(char_pos, update)
                    }
                    Children::Leaf(_) => {
                        let metric = &mut self.metrics[idx];
                        *metric -= update;
                    }
                };
                return;
            }
            char_pos -= pos;
        }
        unreachable!("we should always recurse into a child node");
    }

    fn assert_invariants(&self) {
        assert_eq!(self.metrics.len(), self.children.len());
        assert!(self.metrics.len() <= MAX);
        let this = NonNull::from(self);
        match &self.children {
            Children::Internal(children) => {
                assert!(self.metrics.len() >= MIN);
                for i in 0..children.len() {
                    assert_eq!(children[i].metrics(), self.metrics[i]);
                    assert_eq!(children[i].parent, Some(this));
                }
            }
            Children::Leaf(children) => {
                assert!(!self.metrics.is_empty());
                for i in 0..children.len() {
                    assert_eq!(children[i].parent, Some(this));
                }
            }
        };
        self.children.assert_parent(this);
    }
}

impl fmt::Display for Internal {
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
                for metric in &node.metrics {
                    write!(f, "({metric}) ")?;
                }
                write!(f, "]")?;
                if let Children::Internal(children) = &node.children {
                    for child in children {
                        next.push(child);
                    }
                }
            }
            writeln!(f)?;
            level += 1;
            mem::swap(&mut current, &mut next);
        }
        Ok(())
    }
}

#[derive(Debug, Default)]
struct Leaf {
    parent: Option<NonNull<Internal>>,
}

impl Leaf {
    fn new(parent: NonNull<Internal>) -> Self {
        Self {
            parent: Some(parent),
        }
    }

    fn clear_parent(mut self: Box<Self>) -> Box<Self> {
        self.parent = None;
        self
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

impl Sub for Metric {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            bytes: self.bytes - rhs.bytes,
            chars: self.chars - rhs.chars,
        }
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

    #[test]
    fn test_insert() {
        let mut root = new_root();
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
        let mut root = new_root();
        println!("{}", root);
        for i in 1..20 {
            println!("pushing {i}");
            root.insert(metric(i));
            println!("{}", root);
        }
    }

    #[test]
    fn test_search() {
        let mut root = new_root();
        for i in 1..20 {
            root.insert(metric(i));
        }
        for i in 0..20 {
            println!("searching for {i}");
            let metric = root.search_byte(i * 2);
            assert_eq!(metric.chars, i);
        }
    }

    #[test]
    fn test_search_chars() {
        let mut root = new_root();
        for i in 1..20 {
            root.insert(metric(i));
        }
        for i in 0..20 {
            println!("searching for {i}");
            let metric = root.search_char(i);
            assert_eq!(metric.bytes, i * 2);
        }
    }

    #[test]
    fn test_add() {
        let mut root = new_root();
        for i in 1..20 {
            root.insert(metric(i));
        }
        for i in 0..20 {
            let metric = root.search_char(i);
            assert_eq!(metric.bytes, i * 2);
        }
        println!("init {root}");
        for i in (1..20).rev() {
            println!("adding {i}");
            root.add(i, metric(1));
            println!("{}", root);
        }
        for i in (0..20).step_by(2) {
            println!("searching for {i}");
            let metric = root.search_char(i);
            assert_eq!(metric.bytes, i * 2);
        }
    }

    #[test]
    fn test_remove() {
        let mut root = new_root();
        for i in 1..20 {
            root.insert(metric(i));
        }
        for i in 0..20 {
            let metric = root.search_char(i);
            assert_eq!(metric.bytes, i * 2);
        }

        for i in (1..20).rev() {
            root.add(i, metric(1));
        }

        for i in (0..20).step_by(2) {
            let metric = root.search_char(i);
            assert_eq!(metric.bytes, i * 2);
        }

        println!("init: {root}");
        for i in (1..20).rev() {
            println!("removing {i}");
            root.remove(i * 2, metric(1));
            println!("{}", root);
        }

        for i in 0..20 {
            println!("searching for {i}");
            let metric = root.search_char(i);
            assert_eq!(metric.bytes, i * 2);
        }
    }

    #[test]
    fn test_delete() {
        let mut root = new_root();
        for i in 1..20 {
            root.insert(metric(i));
        }
        for i in 0..20 {
            let metric = root.search_char(i);
            assert_eq!(metric.bytes, i * 2);
        }
        let metrics = root.metrics();

        println!("init: {root}");
        for i in 0..19 {
            root.delete(metric(12));
            println!("after {i} iteration: {root}");
        }
        let metrics_after = root.metrics();
        assert_eq!(metrics, metrics_after);
    }

    #[test]
    fn test_delete_range_leaf() {
        let mut root = new_root();
        // shouldn't need more then a single leaf node
        root.insert(metric(12));
        root.insert(metric(4));
        root.insert(metric(8));
        assert_eq!(root.metrics(), metric(12));
        println!("init: {root}");
        root.delete_range(metric(1), metric(3));
        assert_eq!(root.metrics(), metric(10));
        println!("after: {root}");
        root.delete_range(metric(2), metric(6));
        assert_eq!(root.metrics(), metric(6));
        println!("after: {root}");
        root.delete_range(metric(1), metric(4));
        assert_eq!(root.metrics(), metric(3));
        println!("after: {root}");
        root.delete_range(metric(0), metric(1));
        assert_eq!(root.metrics(), metric(2));
        println!("after: {root}");
    }
}
