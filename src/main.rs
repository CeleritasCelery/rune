#![allow(dead_code)]

mod node {
    use smallvec::SmallVec;
    use std::{
        iter::Sum,
        ops::{AddAssign, Sub, SubAssign},
        ptr::NonNull,
    };

    const MAX: usize = 2;

    pub(super) enum Node {
        Internal(Box<Internal>),
        Leaf(Box<Leaf>),
    }

    impl Node {
        fn parent(&self) -> Option<NonNull<Internal>> {
            match self {
                Node::Internal(x) => x.parent,
                Node::Leaf(x) => x.parent,
            }
        }

        fn parent_mut(&mut self) -> &mut Option<NonNull<Internal>> {
            match self {
                Node::Internal(x) => &mut x.parent,
                Node::Leaf(x) => &mut x.parent,
            }
        }

        fn metrics(&self) -> Metric {
            match self {
                Node::Internal(x) => x.metrics.iter().copied().sum(),
                Node::Leaf(x) => x.metric,
            }
        }
    }

    pub(super) struct Internal {
        metrics: SmallVec<[Metric; MAX]>,
        children: SmallVec<[Node; MAX]>,
        parent: Option<NonNull<Internal>>,
    }

    impl Internal {
        pub(super) fn new() -> Self {
            Self {
                metrics: SmallVec::new(),
                children: SmallVec::new(),
                parent: None,
            }
        }

        fn metrics(&self) -> Metric {
            self.metrics.iter().copied().sum()
        }

        pub(super) fn len(&self) -> usize {
            self.children.len()
        }

        pub(super) fn push(&mut self, node: Node) {
            if self.len() < MAX {
                let mut node = node;
                *node.parent_mut() = Some(NonNull::from(&mut *self));
                self.metrics.push(node.metrics());
                self.children.push(node);
            } else {
                // split the self in two
                let mut right = Internal::new();
                let middle = (MAX - 1) / 2;
                right.metrics = self.metrics.drain(middle + 1..).collect();
                right.children = self.children.drain(middle + 1..).collect();
                self.parent = Some(NonNull::from(&mut right));
                todo!();
            }
        }

        pub(super) fn search_bytes(&self, mut bytes: usize) -> Option<&Leaf> {
            for (idx, metric) in self.metrics.iter().enumerate() {
                if bytes < metric.bytes {
                    // go to this node and search it if it internal, return if a leaf
                    return match self.children[idx] {
                        Node::Leaf(ref leaf) => Some(leaf),
                        Node::Internal(ref internal) => internal.search_bytes(bytes),
                    };
                }
                bytes -= metric.bytes;
            }
            None
        }

        pub(super) fn insert(&mut self, mut needle: Metric) -> Option<Box<Internal>> {
            for (idx, metric) in self.metrics.iter().enumerate() {
                if needle.bytes < metric.bytes {
                    let len = self.len();
                    return match &mut self.children[idx] {
                        Node::Leaf(leaf) => {
                            assert_eq!(*metric, leaf.metric);
                            if len < MAX {
                                // If there is room in this node then insert the
                                // leaf before the current one, splitting the
                                // size
                                leaf.metric -= needle;
                                let mut new = Leaf::new(needle);
                                new.parent = Some(NonNull::from(&mut *self));
                                self.metrics.insert(idx, needle);
                                self.children.insert(idx, Node::Leaf(Box::new(new)));
                                None
                            } else {
                                assert_eq!(len, MAX);
                                // split this node into two and return the left one
                                let middle = MAX / 2;
                                let mut left = self.split();
                                let mut new = Leaf::new(needle);
                                if idx < middle {
                                    left.metrics.insert(idx, needle);
                                    left.children.insert(idx, Node::Leaf(Box::new(new)));
                                } else {
                                    new.parent = Some(NonNull::from(&mut *self));
                                    self.metrics.insert(idx - middle, needle);
                                    self.children
                                        .insert(idx - middle, Node::Leaf(Box::new(new)));
                                }
                                left.parent = self.parent;
                                let mut boxed = Box::new(left);
                                // update the children's parent pointer
                                let child_parent = Some(NonNull::from(&mut *boxed));
                                for child in boxed.children.iter_mut() {
                                    *child.parent_mut() = child_parent;
                                }
                                Some(boxed)
                            }
                        }
                        Node::Internal(internal) => {
                            match internal.insert(needle) {
                                Some(new_child) => {
                                    if len < MAX {
                                        // If there is room in this node then insert the
                                        // node before the current one
                                        self.metrics.insert(idx, new_child.metrics());
                                        self.children.insert(idx, Node::Internal(new_child));
                                        None
                                    } else {
                                        assert_eq!(len, MAX);
                                        // split this node into two and return the left one
                                        let middle = MAX / 2;
                                        let mut left = self.split();
                                        if idx < middle {
                                            left.metrics.insert(idx, new_child.metrics());
                                            left.children.insert(idx, Node::Internal(new_child));
                                        } else {
                                            self.metrics.insert(idx - middle, needle);
                                            self.children
                                                .insert(idx - middle, Node::Internal(new_child));
                                        }
                                        left.parent = self.parent;
                                        let mut boxed = Box::new(left);
                                        let child_parent = Some(NonNull::from(&mut *boxed));
                                        for child in boxed.children.iter_mut() {
                                            *child.parent_mut() = child_parent;
                                        }
                                        Some(boxed)
                                    }
                                }
                                None => None,
                            }
                        }
                    };
                }
                needle -= *metric;
            }
            todo!("push");
        }

        fn split(&mut self) -> Self {
            assert_eq!(self.len(), MAX);
            let middle = MAX / 2;
            let mut left = Internal::new();
            left.metrics = self.metrics.drain(..middle).collect();
            left.children = self.children.drain(..middle).collect();
            for child in &mut left.children {
                *child.parent_mut() = None;
            }
            left
        }

        pub(super) fn pop(&mut self) -> Option<Node> {
            let mut node = self.children.pop()?;
            *node.parent_mut() = None;
            Some(node)
        }
    }

    #[derive(Debug, Default)]
    pub(super) struct Leaf {
        metric: Metric,
        parent: Option<NonNull<Internal>>,
    }

    impl Leaf {
        pub(super) fn new(metric: Metric) -> Self {
            Self {
                metric,
                parent: None,
            }
        }
    }

    #[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
    pub(super) struct Metric {
        pub(super) bytes: usize,
        pub(super) chars: usize,
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
}

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod test {
    use super::node::*;

    #[test]
    fn test_new() {
        let mut node = Internal::new();
        node.push(Node::Leaf(Box::new(Leaf::new(Metric::default()))));
        node.push(Node::Leaf(Box::new(Leaf::new(Metric::default()))));
        node.push(Node::Leaf(Box::new(Leaf::new(Metric::default()))));
        node.push(Node::Leaf(Box::new(Leaf::new(Metric::default()))));
    }
}
