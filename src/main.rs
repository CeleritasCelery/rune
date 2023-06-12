#![allow(dead_code)]

mod node {
    use smallvec::SmallVec;
    use std::{
        iter::Sum,
        ops::{AddAssign, Sub, SubAssign},
        ptr::NonNull,
    };

    const MAX: usize = 2;

    #[derive(Debug)]
    pub(super) enum Node {
        Internal(SmallVec<[Box<Internal>; MAX]>),
        Leaf(SmallVec<[Box<Leaf>; MAX]>),
    }

    impl Node {
        fn len(&self) -> usize {
            match self {
                Node::Internal(x) => x.len(),
                Node::Leaf(x) => x.len(),
            }
        }

        fn set_children(&mut self, children: NonNull<Internal>) {
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

        fn insert_internal(&mut self, idx: usize, x: Box<Internal>) {
            let Node::Internal(children) = self else { unreachable!() };
            children.insert(idx, x);
        }

        fn insert_leaf(&mut self, idx: usize, x: Box<Leaf>) {
            let Node::Leaf(children) = self else { unreachable!() };
            children.insert(idx, x);
        }
    }

    #[derive(Debug)]
    pub(super) struct Internal {
        metrics: SmallVec<[Metric; MAX]>,
        children: Node,
        parent: Option<NonNull<Internal>>,
    }

    impl Internal {
        pub(super) fn new_leaf() -> Self {
            Self {
                metrics: SmallVec::new(),
                children: Node::Leaf(SmallVec::new()),
                parent: None,
            }
        }

        pub(super) fn new_internal() -> Self {
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

        pub(super) fn len(&self) -> usize {
            self.children.len()
        }

        fn insert_internal(
            children: &mut SmallVec<[Box<Internal>; MAX]>,
            metrics: &mut SmallVec<[Metric; MAX]>,
            parent: Option<NonNull<Internal>>,
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

                let mut left = Internal {
                    metrics: metrics.drain(..middle).collect(),
                    children: Node::Internal(
                        children.drain(..middle).map(|x| x.clear_parent()).collect(),
                    ),
                    parent,
                };
                if idx < middle {
                    left.metrics.insert(idx, new_child.metrics());
                    left.children.insert_internal(idx, new_child);
                } else {
                    metrics.insert(idx - middle, new_child.metrics());
                    children.insert(idx - middle, new_child);
                }
                // Box it so it has a stable address
                let mut boxed = Box::new(left);
                let child_parent = NonNull::from(&mut *boxed);
                boxed.children.set_children(child_parent);
                Some(boxed)
            }
        }

        fn insert_leaf(
            children: &mut SmallVec<[Box<Leaf>; MAX]>,
            metrics: &mut SmallVec<[Metric; MAX]>,
            parent: Option<NonNull<Internal>>,
            self_ptr: NonNull<Internal>,
            idx: usize,
            needle: Metric,
        ) -> Option<Box<Internal>> {
            let len = children.len();
            let leaf = &mut children[idx];
            leaf.metric -= needle;
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
                let mut left = Internal {
                    metrics: metrics.drain(..middle).collect(),
                    children: Node::Leaf(
                        children.drain(..middle).map(|x| x.clear_parent()).collect(),
                    ),
                    parent,
                };
                let mut new = Leaf::new(needle);
                if idx < middle {
                    left.metrics.insert(idx, needle);
                    left.children.insert_leaf(idx, Box::new(new));
                } else {
                    new.parent = Some(self_ptr);
                    metrics.insert(idx - middle, needle);
                    children.insert(idx - middle, Box::new(new));
                }
                let mut boxed = Box::new(left);
                // update the children's parent pointer
                let child_parent = NonNull::from(&mut *boxed);
                boxed.children.set_children(child_parent);
                Some(boxed)
            }
        }

        pub(super) fn insert(&mut self, mut needle: Metric) -> Option<Box<Internal>> {
            let self_ptr = NonNull::from(&mut *self);
            for (idx, metric) in self.metrics.iter().enumerate() {
                if needle.bytes < metric.bytes {
                    let metrics = &mut self.metrics;
                    let parent = self.parent;
                    return match &mut self.children {
                        Node::Internal(children) => match children[idx].insert(needle) {
                            Some(new_child) => {
                                Self::insert_internal(children, metrics, parent, idx, new_child)
                            }
                            None => None,
                        },
                        Node::Leaf(children) => {
                            Self::insert_leaf(children, metrics, parent, self_ptr, idx, needle)
                        }
                    };
                }
                needle -= *metric;
            }
            todo!("push");
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

        fn clear_parent(mut self: Box<Self>) -> Box<Self> {
            self.parent = None;
            self
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
    // use super::node::*;

    #[test]
    fn test_new() {
        // let mut node = Internal::new_internal();
        // node.push(Node::Leaf(Box::new(Leaf::new(Metric::default()))));
        // node.push(Node::Leaf(Box::new(Leaf::new(Metric::default()))));
        // node.push(Node::Leaf(Box::new(Leaf::new(Metric::default()))));
        // node.push(Node::Leaf(Box::new(Leaf::new(Metric::default()))));
    }
}
