#![allow(dead_code)]

use smallvec::{smallvec, SmallVec};
use std::{
    fmt::Display,
    iter::Sum,
    ops::{AddAssign, Sub, SubAssign},
    ptr::NonNull,
};

const MAX: usize = 2;

type Metrics = SmallVec<[Metric; MAX]>;
type IntChildren = SmallVec<[Box<Internal>; MAX]>;
type LeafChildren = SmallVec<[Box<Leaf>; MAX]>;

#[derive(Debug)]
enum Node {
    Internal(IntChildren),
    Leaf(LeafChildren),
}

impl Node {
    fn len(&self) -> usize {
        match self {
            Node::Internal(x) => x.len(),
            Node::Leaf(x) => x.len(),
        }
    }

    fn set_parent(&mut self, children: NonNull<Internal>) {
        match self {
            Node::Internal(x) => {
                for child in x.iter_mut() {
                    child.parent = Some(children);
                }
            }
            Node::Leaf(x) => {
                for child in x.iter_mut() {
                    child.parent = Some(children);
                }
            }
        }
    }

    fn assert_parent(&self, parent: NonNull<Internal>) {
        match self {
            Node::Internal(x) => {
                for child in x.iter() {
                    assert_eq!(child.parent, Some(parent));
                }
            }
            Node::Leaf(x) => {
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
    children: Node,
    parent: Option<NonNull<Internal>>,
}

impl Internal {
    fn new_leaf() -> Self {
        Self {
            metrics: SmallVec::new(),
            children: Node::Leaf(SmallVec::new()),
            parent: None,
        }
    }

    fn new_internal() -> Self {
        Self {
            metrics: SmallVec::new(),
            children: Node::Internal(SmallVec::new()),
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

            let mut left_metrics: Metrics = metrics.drain(..middle).collect();
            let mut left_children: IntChildren =
                children.drain(..middle).map(|x| x.clear_parent()).collect();
            if idx < middle {
                left_metrics.insert(idx, new_child.metrics());
                left_children.insert(idx, new_child);
            } else {
                metrics.insert(idx - middle, new_child.metrics());
                children.insert(idx - middle, new_child);
            }
            let left = Internal {
                metrics: left_metrics,
                children: Node::Internal(left_children),
                parent: None,
            };
            // Box it so it has a stable address
            let mut boxed = Box::new(left);
            let child_parent = NonNull::from(&mut *boxed);
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
        children[idx].metric -= needle;
        metrics[idx] -= needle;
        if len < MAX {
            // If there is room in this node then insert the
            // leaf before the current one, splitting the
            // size
            let mut new = Leaf::new(needle);
            new.parent = Some(self_ptr);
            metrics.insert(idx, needle);
            children.insert(idx, Box::new(new));
            None
        } else {
            assert_eq!(len, MAX);
            // split this node into two and return the left one
            let middle = MAX / 2;
            let mut left_metrics: Metrics = metrics.drain(..middle).collect();
            let mut left_children: LeafChildren =
                children.drain(..middle).map(|x| x.clear_parent()).collect();
            let mut new = Leaf::new(needle);
            if idx < middle {
                left_metrics.insert(idx, needle);
                left_children.insert(idx, Box::new(new));
            } else {
                new.parent = Some(self_ptr);
                metrics.insert(idx - middle, needle);
                children.insert(idx - middle, Box::new(new));
            }
            let left = Internal {
                metrics: left_metrics,
                children: Node::Leaf(left_children),
                parent: None,
            };
            let mut boxed = Box::new(left);
            // update the children's parent pointer
            let child_parent = NonNull::from(&mut *boxed);
            boxed.children.set_parent(child_parent);
            Some(boxed)
        }
    }

    fn insert(self: &mut Box<Self>, needle: Metric) {
        match self.insert_impl(needle) {
            None => {}
            Some(left) => {
                // split the root, making the old root the right child
                let right = std::mem::replace(self, Box::new(Internal::new_internal()));
                self.metrics = smallvec![left.metrics(), right.metrics()];
                self.children = Node::Internal(smallvec![left, right]);
                let this = NonNull::from(&mut **self);
                self.children.set_parent(this);
            }
        }
    }

    fn insert_impl(&mut self, mut needle: Metric) -> Option<Box<Internal>> {
        // debug checks
        {
            let children_metrics = match &self.children {
                Node::Internal(x) => x.iter().map(|x| x.metrics()).sum(),
                Node::Leaf(x) => x.iter().map(|x| x.metric).sum(),
            };
            assert_eq!(self.metrics(), children_metrics);
            let this = NonNull::from(&mut *self);
            self.children.assert_parent(this);
        }
        let self_ptr = NonNull::from(&mut *self);
        for (idx, metric) in self.metrics.iter().enumerate() {
            if needle.bytes < metric.bytes {
                let metrics = &mut self.metrics;
                let mut new = match &mut self.children {
                    // call recursively and insert the new node
                    Node::Internal(children) => match children[idx].insert_impl(needle) {
                        Some(new) => Self::insert_internal(children, metrics, idx, new),
                        None => None,
                    },
                    Node::Leaf(children) => {
                        Self::insert_leaf(children, metrics, self_ptr, idx, needle)
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
        todo!("push");
    }
}

impl Display for Internal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // print the children level by level by adding them to a pair of
        // alternating queues for each level
        let mut current = Vec::new();
        let mut next: Vec<&Self> = Vec::new();
        current.push(self);
        let mut level = 0;
        loop {
            write!(f, "level {level}: ")?;
            for node in &current {
                for metric in &node.metrics {
                    write!(f, "({}) ", metric)?;
                }
                if let Node::Internal(children) = &node.children {
                    for child in children {
                        next.push(child);
                    }
                }
            }
            writeln!(f)?;
            level += 1;
            // next will be empty when all the children are leafs
            if next.is_empty() {
                break;
            }
            std::mem::swap(&mut current, &mut next);
            next.clear();
        }
        // we are at the level just before the leafs, so we can print them now
        write!(f, "leafs {level}: ")?;
        for node in current {
            let Node::Leaf(children) = &node.children else {unreachable!()};
            for child in children {
                write!(f, "({}) ", child.metric)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Default)]
struct Leaf {
    metric: Metric,
    parent: Option<NonNull<Internal>>,
}

impl Leaf {
    fn new(metric: Metric) -> Self {
        Self {
            metric,
            parent: None,
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

impl Display for Metric {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "b: {}, c: {}", self.bytes, self.chars)
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

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod test {
    use super::*;

    fn metric(x: usize) -> Metric {
        Metric { bytes: x, chars: x }
    }

    #[test]
    fn test_insert() {
        let mut root = Box::new(Internal::new_leaf());
        root.metrics.push(metric(10));
        let root_ptr = NonNull::from(&mut *root);
        if let Node::Leaf(children) = &mut root.children {
            let mut leaf = Leaf::new(metric(10));
            leaf.parent = Some(root_ptr);
            children.push(Box::new(leaf));
        }
        println!("{}", root);
        root.insert(metric(5));
        for i in 0..10 {
            println!("pushing {i}");
            root.insert(metric(i));
        }
        println!("{}", root);
    }
}
