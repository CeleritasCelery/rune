#![allow(dead_code)]
use std::{cmp::Ordering, ops::Range};

use crate::{
    core::object::{Gc, ListType, Object, NIL},
    fns::plist_get,
};

// NOTE test only
// type Object<'ob> = &'ob usize;

/// a half-open interval in ℕ, [start, end)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd)]
pub struct TextRange {
    pub start: usize,
    pub end: usize,
}
impl Ord for TextRange {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.start.cmp(&other.start) {
            Ordering::Less => Ordering::Less,
            Ordering::Equal => self.end.cmp(&other.end),
            Ordering::Greater => Ordering::Greater,
        }
    }
}

impl TextRange {
    /// caller should check that start < end
    pub fn new(start: usize, end: usize) -> Self {
        if start <= end {
            Self { start, end }
        } else {
            Self { start: end, end: start }
        }
    }

    pub fn as_range(&self) -> Range<usize> {
        self.start..self.end
    }

    pub fn contains(&self, pos: usize) -> bool {
        self.start <= pos && pos < self.end
    }

    pub fn strict_order(&self, other: &Self) -> Option<Ordering> {
        if self.end <= other.start {
            return Some(Ordering::Less);
        }
        if self.start >= other.end {
            return Some(Ordering::Greater);
        }
        None
    }
    /// split self, return the splitted out interval. This function does not check
    /// whether position is valid.
    pub fn split_at(&mut self, position: usize, left: bool) -> Self {
        if left {
            let start = self.start;
            self.start = position;
            Self::new(start, position)
        } else {
            let end = self.end;
            self.end = position;
            Self::new(position, end)
        }
    }

    pub fn intersects(&self, other: Self) -> bool {
        self.end > other.start && self.start < other.end
    }
    pub fn intersection_uncheck(&self, other: Self) -> Self {
        Self::new(self.start.max(other.start), self.end.min(other.end))
    }
    pub fn intersection(&self, other: Self) -> Option<Self> {
        self.intersects(other).then(|| self.intersection_uncheck(other))
    }

    pub fn advance(&mut self, offset: usize) {
        self.start += offset;
        self.end += offset;
    }
    pub fn move_back(&self, offset: usize) -> Self {
        Self::new(self.start + offset, self.end + offset)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Color {
    Red = 0,
    Black,
}

#[derive(Debug, Clone)]
pub struct Node<'ob> {
    key: TextRange,
    val: Object<'ob>,
    left: MaybeNode<'ob>,
    right: MaybeNode<'ob>,
    color: Color,
    n: usize,
}
pub type BoxedNode<'ob> = Box<Node<'ob>>;
pub type MaybeNode<'ob> = Option<BoxedNode<'ob>>;

impl<'ob> Node<'ob> {
    #[inline]
    pub fn red(node: &MaybeNode) -> bool {
        node.as_ref().map(|n| n.color == Color::Red).unwrap_or(false)
    }

    pub fn new(key: TextRange, val: Object<'ob>) -> Self {
        Self { key, val, left: None, right: None, color: Color::Red, n: 1 }
    }
    pub fn new_boxed(key: TextRange, val: Object<'ob>) -> BoxedNode<'ob> {
        Box::new(Self::new(key, val))
    }

    /// perform the following operation, \\ is the red link:
    ///      |            |
    ///      n            x
    ///     / \\        // \
    ///        x    =>  n
    ///       / \      / \
    ///      c            c
    pub fn rotate_left<'a>(node: &'a mut BoxedNode<'ob>) -> Option<&'a mut BoxedNode<'ob>> {
        let mut x = node.right.take()?;
        let es = x.left.take();
        node.right = es;
        x.color = node.color;
        x.n = node.n;
        node.color = Color::Red;
        node.n = node.n();
        // node and x swapped content
        std::mem::swap(node, &mut x);
        node.left.replace(x);
        Some(node)
    }

    /// perform the following operation, \\ is the red link:
    ///      |            |
    ///      n            x
    ///    // \          / \\
    ///    x       =>       n
    ///   / \              / \
    ///      c            c
    pub fn rotate_right<'a>(node: &'a mut BoxedNode<'ob>) -> Option<&'a mut BoxedNode<'ob>> {
        let mut x = node.left.take()?;
        let temp = x.right.take();
        node.left = temp;
        x.color = node.color;
        node.color = Color::Red;
        x.n = node.n;
        node.n = node.n();

        std::mem::swap(node, &mut x);
        node.right.replace(x);
        Some(node)
    }

    /// total number of items in this sub-tree
    pub fn n(&self) -> usize {
        let l = match self.left {
            Some(ref n) => n.n,
            None => 0,
        };
        let r = match self.right {
            Some(ref n) => n.n,
            None => 0,
        };
        1 + l + r
    }

    fn min(&self) -> &Node {
        if let Some(ref l) = self.left {
            l.min()
        } else {
            self
        }
    }

    fn min_mut<'a>(&'a mut self) -> &'a mut Node<'ob> {
        if let Some(ref mut l) = self.left {
            l.min_mut()
        } else {
            self
        }
    }

    fn get(&self, key: TextRange) -> Option<Object<'ob>> {
        match key.cmp(&self.key) {
            std::cmp::Ordering::Equal => Some(self.val),
            std::cmp::Ordering::Less => self.left.as_ref().and_then(|n| n.get(key)),
            std::cmp::Ordering::Greater => self.right.as_ref().and_then(|n| n.get(key)),
        }
    }
}

impl<'ob> Node<'ob> {
    pub fn insert_at<'a>(
        node: &'a mut MaybeNode<'ob>,
        key: TextRange,
        val: Object<'ob>,
    ) -> Option<&'a mut BoxedNode<'ob>> {
        match node {
            Some(ref mut n) => Node::insert_at_inner(n, key, val),
            None => {
                node.replace(Node::new_boxed(key, val));
                node.as_mut()
            }
        }
    }
    pub fn insert_at_inner<'a>(
        node: &'a mut BoxedNode<'ob>,
        mut key: TextRange,
        val: Object<'ob>,
    ) -> Option<&'a mut BoxedNode<'ob>> {
        let cmp = key.cmp(&node.key);
        let intersect = key.intersects(node.key);
        // TODO too taunting, also, plist should be cloned?
        if intersect {
            if key.start < node.key.start {
                if key.end < node.key.end {
                    let key_left = key.split_at(node.key.start, true);
                    let key_right = node.key.split_at(key.end, false);
                    Node::insert_at(&mut node.left, key_left, val);
                    Node::insert_at(&mut node.right, key_right, node.val);
                }
                if key.end > node.key.end {
                    let key_left = key.split_at(node.key.start, true);
                    let key_right = key.split_at(node.key.end, false);
                    Node::insert_at(&mut node.left, key_left, val);
                    Node::insert_at(&mut node.right, key_right, val);
                }
                if key.end == node.key.end {
                    let key_left = key.split_at(node.key.start, true);
                    Node::insert_at(&mut node.left, key_left, val);
                }
            } else {
                if key.end < node.key.end {
                    let key_left = node.key.split_at(key.start, true);
                    let key_right = node.key.split_at(key.end, false);
                    Node::insert_at(&mut node.left, key_left, node.val);
                    Node::insert_at(&mut node.right, key_right, node.val);
                }
                if key.end > node.key.end {
                    let key_left = node.key.split_at(key.start, true);
                    let key_right = key.split_at(node.key.end, false);
                    Node::insert_at(&mut node.left, key_left, node.val);
                    Node::insert_at(&mut node.right, key_right, val);
                }
                if key.end == node.key.end {
                    let key_left = node.key.split_at(key.start, true);
                    Node::insert_at(&mut node.left, key_left, node.val);
                }
            }
        }
        match cmp {
            Ordering::Less => {
                Node::insert_at(&mut node.left, key, val)?;
            }
            Ordering::Equal => {
                || -> anyhow::Result<Object<'ob>> {
                    let Ok(new_props) = Gc::<ListType>::try_from(val) else { return Ok(NIL) };
                    let mut iter = new_props.elements();
                    while let Some(cur_prop) = iter.next() {
                        let Some(new_val) = iter.next() else { return Ok(NIL) };
                        if let Ok(old_val) = plist_get(node.val, cur_prop?) {
                            todo!()
                        }
                    }
                    Ok(NIL)
                }().unwrap();

                // TODO add merging plist
            }
            Ordering::Greater => {
                Node::insert_at(&mut node.right, key, val)?;
            }
        };

        // cond1: r_red && !l_red
        if Node::red(&node.right) && !Node::red(&node.left) {
            Node::rotate_left(node)?;
        }

        // cond2: l_red && ll_red
        let cond2 = match node.left {
            Some(ref nl) => nl.color == Color::Red && Node::red(&nl.left),
            None => false,
        };
        if cond2 {
            Node::rotate_right(node)?;
        }

        // cond3: l_red && r_red
        if let (Some(l), Some(r)) = (&mut node.left, &mut node.right) {
            if l.color == Color::Red && r.color == Color::Red {
                l.color = Color::Black;
                r.color = Color::Black;
                node.color = Color::Red;
            }
        }
        // update node's size
        node.n = node.n();
        Some(node)
    }
    pub fn split_at<'a>(
        node: &'a mut BoxedNode<'ob>,
        position: usize,
        left: bool,
    ) -> Option<&'a mut BoxedNode<'ob>> {
        let splitted = node.key.split_at(position, left);
        Node::insert_at_inner(node, splitted, node.val)
    }
}

// deletion
impl<'ob> Node<'ob> {
    /// if node.left and node.right are both red, mark them black turn node to red.
    fn flip_colors(node: &mut MaybeNode) {
        if let Some(ref mut n) = node {
            Node::flip_colors_inner(n)
        };
    }
    /// if node.left and node.right are both red, mark them black turn node to red.
    fn flip_colors_inner(n: &mut BoxedNode<'ob>) {
        if Node::red(&n.right) && Node::red(&n.left) {
            if let Some(ref mut l) = n.left {
                l.color = Color::Black;
            }
            if let Some(ref mut r) = n.right {
                r.color = Color::Black;
            }
            n.color = Color::Red;
        }
    }

    fn balance(node: &mut BoxedNode<'ob>) -> Option<()> {
        // let n = node.as_mut()?;
        if Node::red(&node.right) {
            Node::rotate_left(node)?;
        }
        Some(())
    }

    fn move_red_left<'a>(node: &'a mut BoxedNode<'ob>) -> Option<()> {
        Node::flip_colors_inner(node);
        // let n = node.as_mut()?;
        let nr = node.right.as_mut()?;
        if Node::red(&nr.left) {
            Node::rotate_right(nr)?;
            Node::rotate_left(node)?;
        }
        Some(())
    }

    fn move_red_right<'a>(node: &'a mut BoxedNode<'ob>) -> Option<()> {
        Node::flip_colors_inner(node);
        let nl = node.left.as_mut()?;
        if !Node::red(&nl.left) {
            Node::rotate_right(node)?;
        }
        Some(())
    }

    fn delete_min<'a>(node: &'a mut MaybeNode<'ob>) -> MaybeNode<'ob> {
        let n = node.as_mut()?;
        match n.left {
            Some(ref mut l) => {
                if l.color == Color::Black && !Node::red(&l.left) {
                    Node::move_red_left(n)?;
                }
            }
            None => {
                return node.take();
            }
        }
        let result = Node::delete_min(&mut n.left);
        Node::balance(n)?;
        result
    }

    fn delete_max<'a>(node: &'a mut MaybeNode<'ob>) -> MaybeNode<'ob> {
        let n = node.as_mut()?;
        if Node::red(&n.left) {
            Node::rotate_right(n);
        }
        let n = node.as_mut()?;
        match n.right {
            Some(ref mut r) => {
                if r.color == Color::Black && !Node::red(&r.left) {
                    Node::move_red_right(n)?;
                }
            }
            None => {
                return node.take();
            }
        }
        let result = Node::delete_max(&mut n.right);
        Node::balance(n)?;
        result
    }

    fn delete(node: &mut MaybeNode<'ob>, key: TextRange) -> MaybeNode<'ob> {
        let n = node.as_mut()?;
        let result = if key < n.key {
            if let Some(ref mut l) = n.left {
                if l.color == Color::Black && !Node::red(&l.left) {
                    Node::move_red_left(n).unwrap();
                }
            }
            Node::delete(&mut n.left, key)
        } else {
            if Node::red(&n.left) {
                Node::rotate_right(n).unwrap();
            }
            if key == n.key && n.right.is_none() {
                return None;
            }
            // NOTE n.right will not be none
            let r = n.right.as_mut().unwrap();
            if r.color == Color::Black && !Node::red(&r.left) {
                Node::move_red_right(n).unwrap();
            }

            let r = n.right.as_mut().unwrap();
            if key == n.key {
                // n.val = r.get(r.min().key).unwrap();
                let r_min = r.min_mut();
                std::mem::swap(&mut n.val, &mut r_min.val);
                // we can just memswap, since all nodes on right is bigger than
                // node, that is, n.key is still r.min() after swap
                std::mem::swap(&mut n.key, &mut r_min.key);
                // n.key = r_min.key; // NOTE this need copy
                Node::delete_min(&mut n.right)
            } else {
                Node::delete(&mut n.right, key)
            }
        };

        Node::balance(n)?;
        result
    }
}

// intersection
impl<'ob> Node<'ob> {
    pub fn find_intersects<'a, 'b>(
        &'a self,
        range: TextRange,
        results: &'b mut Vec<&'a Node<'ob>>,
    ) {
        let ord = range.strict_order(&self.key);
        match ord {
            Some(Ordering::Less) => {
                if let Some(ref l) = self.left {
                    l.find_intersects(range, results);
                }
            }
            Some(Ordering::Greater) => {
                if let Some(ref r) = self.right {
                    r.find_intersects(range, results);
                }
            }
            _ => {
                if let Some(ref l) = self.left {
                    l.find_intersects(range, results);
                }
                results.push(self);
                if let Some(ref r) = self.right {
                    r.find_intersects(range, results);
                }
            }
        }
    }

    // /// a non-lazy iterator
    // pub fn iter(&self, result: &mut Vec<*const Node<TextRange, V>>) {
    //     if let Some(ref l) = self.left {
    //         l.iter(result);
    //     }
    //     result.push(self);
    //     if let Some(ref r) = self.right {
    //         r.iter(result);
    //     }
    // }
    // /// a non-lazy iterator
    // pub fn satisfy<F>(&self, f: &F, result: &mut Vec<*const Node<TextRange, V>>)
    // where
    //     F: Fn(&Node<TextRange, V>) -> bool,
    // {
    //     if let Some(ref l) = self.left {
    //         l.satisfy(f, result);
    //     }
    //     if f(self) {
    //         result.push(self);
    //     }
    //     if let Some(ref r) = self.right {
    //         r.satisfy(f, result);
    //     }
    // }
    pub fn map<F>(&mut self, f: &mut F)
    where
        F: FnMut(&mut Node<'ob>),
    {
        if let Some(ref mut l) = self.left {
            l.map(f);
        }
        f(self);
        if let Some(ref mut r) = self.right {
            r.map(f);
        }
    }

    pub fn advance(&mut self, position: usize, length: usize) {
        let cmp = self.key.start > position;
        if cmp {
            if let Some(ref mut l) = self.left {
                l.advance(position, length);
            }
            self.key.advance(length);
            if let Some(ref mut r) = self.right {
                r.map(&mut |n| n.key.advance(length));
            }
        } else {
            if self.key.end > position {
                // position is inside this interval
                self.key.end += length;
            }
            if let Some(ref mut l) = self.left {
                l.advance(position, length);
            }
            if let Some(ref mut r) = self.right {
                r.advance(position, length)
            }
        }
    }
}

#[derive(Debug)]
/// A interval tree using red-black tree, whereas keys are intervals, and values are
/// plists in elisp.
///
/// All intervals are half-opened intervals, i.e. `I = [start, end)`.These intervals
/// are sorted by their starting point, then their ending point.
///
/// NOTE When inserting an interval I, if I intersects with some existing interval
/// J but I != J, then we split & merge I and J into sub-intervals. Thus all intervals
/// inside a interval tree will not overlap. Adjacant intervals with identical props
/// should be merged afterwards, maybe during redisplay.
pub struct IntervalTree<'ob> {
    root: MaybeNode<'ob>,
}

impl<'ob> IntervalTree<'ob> {
    pub fn new() -> Self {
        Self { root: None }
    }

    pub fn insert<'a>(
        &'a mut self,
        key: TextRange,
        val: Object<'ob>,
    ) -> Option<&'a mut Box<Node<'ob>>> {
        Node::insert_at(&mut self.root, key, val)
    }

    pub fn get(&self, key: TextRange) -> Option<Object<'ob>> {
        match self.root {
            Some(ref r) => r.get(key),
            None => None,
        }
    }

    pub fn delete(&mut self, key: TextRange) -> MaybeNode<'ob> {
        let result = match self.root {
            Some(ref mut root) => {
                if !Node::red(&root.left) && !Node::red(&root.right) {
                    root.color = Color::Red;
                }

                Node::delete(&mut self.root, key)
            }
            None => None,
        };
        if let Some(ref mut root) = self.root {
            if root.n >= 1 {
                root.color = Color::Black;
            }
        }
        result
    }

    pub fn delete_min(&mut self) -> MaybeNode<'ob> {
        let root = self.root.as_mut()?;
        if !Node::red(&root.left) && !Node::red(&root.right) {
            root.color = Color::Red;
        }
        let result = Node::delete_min(&mut self.root);
        let root = self.root.as_mut()?;
        if root.n >= 1 {
            root.color = Color::Black;
        }
        result
    }

    pub fn delete_max(&mut self) -> MaybeNode<'ob> {
        let root = self.root.as_mut()?;
        if !Node::red(&root.left) && !Node::red(&root.right) {
            root.color = Color::Red;
        }
        let result = Node::delete_max(&mut self.root);
        let root = self.root.as_mut()?;
        if root.n >= 1 {
            root.color = Color::Black;
        }
        result
    }

    pub fn advance(&mut self, position: usize, length: usize) {
        if let Some(ref mut node) = self.root {
            node.advance(position, length);
        }
    }

    pub fn find_intersects(&self, range: TextRange) -> Vec<&Node<'ob>> {
        let mut result = Vec::new();
        if let Some(ref r) = self.root {
            r.find_intersects(range, &mut result);
            // r.satisfy(&|n| n.key.intersects(range), &mut result);
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use crate::core::gc::{Context, RootSet};
    use rune_core::macros::list;

    use super::*;

    fn build_tree<'ob>(val: Object<'ob>) -> IntervalTree<'ob> {
        let mut tree = IntervalTree::new();
        tree.insert(TextRange::new(0, 1), val);
        tree.insert(TextRange::new(1, 2), val);
        tree.insert(TextRange::new(2, 3), val);
        tree.insert(TextRange::new(3, 4), val);
        tree.insert(TextRange::new(4, 5), val);
        tree.insert(TextRange::new(5, 6), val);
        tree.insert(TextRange::new(6, 7), val);
        tree.insert(TextRange::new(7, 8), val);
        tree.insert(TextRange::new(8, 9), val);
        tree.insert(TextRange::new(9, 10), val);
        tree
    }

    #[test]
    fn rotate_left() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        let val = list![1, 2; cx];
        let mut node1 = Node::new_boxed(TextRange::new(0, 3), val);
        node1.color = Color::Black;
        let mut node2 = Node::new_boxed(TextRange::new(3, 6), val);
        let mut node3 = Node::new_boxed(TextRange::new(6, 9), val);
        node3.color = Color::Black;
        let mut node4 = Node::new_boxed(TextRange::new(9, 12), val);
        node4.color = Color::Black;
        let mut node5 = Node::new_boxed(TextRange::new(12, 15), val);
        node5.color = Color::Black;

        node2.left = Some(node3);
        node2.right = Some(node4);
        node2.color = Color::Red;
        node1.right = Some(node2);
        node1.left = Some(node5);
        // let mut node1 = dbg!(node1);
        Node::rotate_left(&mut node1);
        assert_eq!(node1.key.start, 3);
        let n2 = node1.left.unwrap();
        assert_eq!(n2.key.start, 0);
        let n3 = n2.right.unwrap();
        assert_eq!(n3.key.start, 6);
        let n4 = node1.right.unwrap();
        assert_eq!(n4.key.start, 9);
        let n5 = n2.left.unwrap();
        assert_eq!(n5.key.start, 12);

        assert_eq!(node1.color, Color::Black);
        assert_eq!(n2.color, Color::Red);
        assert_eq!(n2.n, 3);
    }

    #[test]
    fn insert() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        let val = list![1, 2; cx];
        let tree = build_tree(val);
        let root = tree.root.as_ref().unwrap();
        assert_eq!(root.key.start, 3);
        let n1 = root.left.as_ref().unwrap();
        assert_eq!(n1.key.start, 1);
        let n2 = root.right.as_ref().unwrap();
        assert_eq!(n2.key.start, 7);
        let n3 = n2.right.as_ref().unwrap();
        assert_eq!(n3.key.start, 9);
        let n4 = n3.left.as_ref().unwrap();
        assert_eq!(n4.key.start, 8);
        assert!(n3.right.is_none())
    }

    #[test]
    fn find_intersect() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        let val = list![1, 2; cx];
        let tree = build_tree(val);
        let re = tree.find_intersects(TextRange::new(2, 4));
        let k1 = re[0].key;
        let k2 = re[1].key;
        assert_eq!(k1, TextRange::new(2, 3));
        assert_eq!(k2, TextRange::new(3, 4));
        assert_eq!(re.len(), 2);
    }

    #[test]
    fn advance() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        let val = list![1, 2; cx];
        let mut tree = build_tree(val);
        tree.advance(7, 5);
        // let mut tree = dbg!(tree);
        tree.get(TextRange::new(6, 7)).unwrap();
        tree.get(TextRange::new(7, 13)).unwrap();
        tree.get(TextRange::new(13, 14)).unwrap();
        tree.get(TextRange::new(14, 15)).unwrap();
    }
}
