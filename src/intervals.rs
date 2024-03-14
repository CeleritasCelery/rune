#![allow(dead_code)]
use std::{cmp::Ordering, ops::Range};

use crate::{
    core::{gc::Context, object::Object},
    fns::eq,
    textprops::add_properties,
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
    parent: *mut Node<'ob>,
    is_right_child: bool,
    n: usize,
}
pub type BoxedNode<'ob> = Box<Node<'ob>>;
pub type MaybeNode<'ob> = Option<BoxedNode<'ob>>;

impl<'ob> Node<'ob> {
    #[inline]
    pub fn red(node: &MaybeNode) -> bool {
        node.as_ref().map(|n| n.color == Color::Red).unwrap_or(false)
    }

    #[inline]
    pub fn new(key: TextRange, val: Object<'ob>) -> Self {
        Self::new_with_parent(key, val, std::ptr::null_mut(), false)
    }

    #[inline]
    pub fn new_with_parent(
        key: TextRange,
        val: Object<'ob>,
        parent: *mut Node<'ob>,
        is_right_child: bool
    ) -> Node<'ob> {
        Self { key, val, left: None, right: None, color: Color::Red, n: 1, parent, is_right_child }
    }

    #[inline]
    pub fn new_boxed(key: TextRange, val: Object<'ob>) -> BoxedNode<'ob> {
        Box::new(Self::new(key, val))
    }

    #[inline]
    pub fn new_boxed_with_parent(
        key: TextRange,
        val: Object<'ob>,
        parent: *mut Node<'ob>,
        is_right_child: bool
    ) -> BoxedNode<'ob> {
        Box::new(Self::new_with_parent(key, val, parent, is_right_child))
    }

    #[inline]
    pub fn set_parent(&mut self, parent: &mut Node<'ob>) {
        self.parent = parent
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
        let mut c = x.left.take();
        if let Some(ref mut c) = c {
            c.parent = node.as_mut();
            c.is_right_child = true;
        }
        node.right = c;
        x.color = node.color;
        x.parent = node.parent;
        x.is_right_child = node.is_right_child;
        node.parent = x.as_mut();
        node.is_right_child = false;
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
        let mut c = x.right.take();
        if let Some(ref mut c) = c {
            c.parent = node.as_mut();
            c.is_right_child = false;
        }
        node.left = c;
        x.color = node.color;
        x.parent = node.parent;
        x.is_right_child = node.is_right_child;
        node.parent = x.as_mut();
        node.is_right_child = true;
        node.color = Color::Red;
        x.n = node.n;
        node.n = node.n();

        std::mem::swap(node, &mut x);
        node.right.replace(x);
        Some(node)
    }

    /// total number of items in this sub-tree
    #[inline]
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

    fn min(&self) -> &Node<'ob> {
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

    #[inline]
    fn parent(&self) -> Option<&Node<'ob>> {
        unsafe {
            self.parent.as_ref()
        }
    }

    #[inline]
    fn parent_mut(&self) -> Option<&mut Node<'ob>> {
        unsafe {
            self.parent.as_mut()
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
        cx: &'ob Context,
        parent: *mut Node<'ob>,
        is_right_child: bool,
    ) -> Option<&'a mut BoxedNode<'ob>> {
        match node {
            Some(ref mut n) => Node::insert_at_inner(n, key, val, cx),
            None => {
                node.replace(Node::new_boxed_with_parent(key, val, parent, is_right_child));
                node.as_mut()
            }
        }
    }

    pub fn insert_at_inner<'a>(
        node: &'a mut BoxedNode<'ob>,
        mut key: TextRange,
        val: Object<'ob>,
        cx: &'ob Context,
    ) -> Option<&'a mut BoxedNode<'ob>> {
        let intersect = key.intersects(node.key);
        // TODO too taunting, also, plist should be cloned?
        let ptr: *mut Node<'ob> = node.as_mut();
        if intersect {
            if key.start < node.key.start {
                let key_left = key.split_at(node.key.start, true);
                Node::insert_at(&mut node.left, key_left, val, cx, ptr, false);

                if key.end < node.key.end {
                    let key_right = node.key.split_at(key.end, false);
                    Node::insert_at(&mut node.right, key_right, node.val, cx, ptr, true);
                } else if key.end > node.key.end {
                    let key_right = key.split_at(node.key.end, false);
                    Node::insert_at(&mut node.right, key_right, val, cx, ptr, true);
                }
            } else {
                let key_left = node.key.split_at(key.start, true);
                Node::insert_at(&mut node.left, key_left, node.val, cx, ptr, false);

                if key.end < node.key.end {
                    let key_right = node.key.split_at(key.end, false);
                    Node::insert_at(&mut node.right, key_right, node.val, cx, ptr, true);
                } else if key.end > node.key.end {
                    let key_right = key.split_at(node.key.end, false);
                    Node::insert_at(&mut node.right, key_right, val, cx, ptr, true);
                }
            }
        }
        let cmp = key.cmp(&node.key);
        match cmp {
            Ordering::Less => {
                Node::insert_at(&mut node.left, key, val, cx, ptr, false)?;
            }
            Ordering::Equal => {
                let (val, changed) = add_properties(
                    val,
                    node.val,
                    crate::textprops::PropertySetType::Replace,
                    false,
                    cx,
                )
                .ok()?;
                if changed {
                    node.val = val;
                }
            }
            Ordering::Greater => {
                Node::insert_at(&mut node.right, key, val, cx, ptr, true)?;
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
        if let Some(ref mut l) = n.left {
            l.color = Color::Black;
        }
        if let Some(ref mut r) = n.right {
            r.color = Color::Black;
        }
        n.color = Color::Red;
    }

    /// rotate left if node.right is red
    fn balance(node: &mut BoxedNode<'ob>) -> Option<()> {
        if Node::red(&node.right) {
            Node::rotate_left(node)?;
        }
        Some(())
    }

    fn move_red_left(node: &mut BoxedNode<'ob>) -> Option<()> {
        Node::flip_colors_inner(node);
        let nr = node.right.as_mut()?;
        if Node::red(&nr.left) {
            Node::rotate_right(nr)?;
            Node::rotate_left(node)?;
            Node::flip_colors_inner(node);
        }
        Some(())
    }

    fn move_red_right(node: &mut BoxedNode<'ob>) -> Option<()> {
        Node::flip_colors_inner(node);
        // h.left.left == Red
        let cond = match node.left {
            Some(ref l) => Node::red(&l.left),
            None => false,
        };
        if cond {
            Node::rotate_right(node)?;
            Node::flip_colors_inner(node);
        }
        Some(())
    }

    fn delete_min(node: &mut MaybeNode<'ob>) -> MaybeNode<'ob> {
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

    fn delete_max(node: &mut MaybeNode<'ob>) -> MaybeNode<'ob> {
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
            // n.left != red && n.left.left != red
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
                return node.take();
            }
            // NOTE n.right will not be none
            let r = n.right.as_mut().unwrap();
            if r.color == Color::Black && !Node::red(&r.left) {
                Node::move_red_right(n).unwrap();
            }

            if key == n.key {
                let mut result = Node::delete_min(&mut n.right);
                let r_min = result.as_mut().unwrap();
                std::mem::swap(&mut n.val, &mut r_min.val);
                std::mem::swap(&mut n.key, &mut r_min.key);
                result
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
    pub fn next(&self) -> Option<&Node<'ob>> {
        let mut n = self;
        if let Some(ref r) = self.right {
            n = r;
            while let Some(ref l) = n.left {
                n = l;
            }
            return Some(n)
        }
        while let Some(parent) = n.parent_mut() {
            if !n.is_right_child {
                return Some(parent)
            }
            n = parent;
        }
        None
    }

    pub fn next_mut(&mut self) -> Option<&mut Node<'ob>> {
        fn safe_mut<'a, T>(n: *mut T) -> &'a mut T {
            unsafe {
                n.as_mut().unwrap()
            }
        }
        let mut n: *mut Node<'ob> = self;
        if let Some(r) = self.right.as_mut() {
            n = r.as_mut();
            while let Some(ref mut l) = safe_mut(n).left {
                n = l.as_mut();
            }
            return Some(safe_mut(n))
        }
        while let Some(parent) = (safe_mut(n)).parent_mut() {
            if !(safe_mut(n)).is_right_child {
                return Some(parent)
            }
            n = parent;
        }
        None
    }

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
        cx: &'ob Context,
    ) -> Option<&'a mut Box<Node<'ob>>> {
        if key.start == key.end {
            return None;
        }
        Node::insert_at(&mut self.root, key, val, cx, std::ptr::null_mut(), false)
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
            root.color = Color::Black;
        }
        result
    }

    pub fn delete_min(&mut self) -> MaybeNode<'ob> {
        let root = self.root.as_mut()?;
        if !Node::red(&root.left) && !Node::red(&root.right) {
            root.color = Color::Red;
        }
        let result = Node::delete_min(&mut self.root);
        if let Some(ref mut root) = self.root {
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
        if let Some(ref mut root) = self.root {
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

    pub fn min(&self) -> Option<&Node<'ob>> {
        self.root.as_ref().map(|n| n.min())
    }

    fn print(&self) {

        fn print_inner(node: &Node<'_>) {
            println!("key: {:?}, val: {:?}, color: {:?}", node.key, node.val, node.color);
            if let Some(parent) = unsafe {node.parent.as_ref()} {
                let direction = if node.is_right_child {
                    "right"
                } else {
                    "left"
                };
                println!("parent({} child): {:?}", direction, parent.key);
            } else {
                println!("parent: not found");
            }
            if let Some(ref l) = node.left {
                println!("left: ");
                print_inner(l);
                println!("left end for {:?}", node.key);
            }
            if let Some(ref r) = node.right {
                println!("right: ");
                print_inner(r);
                println!("right end for {:?}", node.key);
            }
        }
        let Some(ref root) = self.root else {return};
        print_inner(root);
    }
}

#[cfg(test)]
mod tests {
    use crate::core::gc::{Context, RootSet};
    use rune_core::macros::list;

    use super::*;

    fn build_tree<'ob>(val: Object<'ob>, cx: &'ob Context) -> IntervalTree<'ob> {
        let mut tree = IntervalTree::new();
        tree.insert(TextRange::new(0, 1), val, cx);
        tree.insert(TextRange::new(1, 2), val, cx);
        tree.insert(TextRange::new(2, 3), val, cx);
        tree.insert(TextRange::new(3, 4), val, cx);
        tree.insert(TextRange::new(4, 5), val, cx);
        tree.insert(TextRange::new(5, 6), val, cx);
        tree.insert(TextRange::new(6, 7), val, cx);
        tree.insert(TextRange::new(7, 8), val, cx);
        tree.insert(TextRange::new(8, 9), val, cx);
        tree.insert(TextRange::new(9, 10), val, cx);
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
        let tree = build_tree(val, cx);
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
    fn delete() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        let val = list![1, 2; cx];
        let mut tree = build_tree(val, cx);
        // let mut tree = dbg!(tree);
        for k in vec![8, 4, 5, 7, 3, 6].into_iter() {
            let i = TextRange::new(k, k + 1);
            let a = tree.delete(i).unwrap();
            assert_eq!(a.key, i);
        }
    }

    #[test]
    fn delete_min() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        let val = list![1, 2; cx];
        let mut tree = build_tree(val, cx);
        // let mut tree = dbg!(tree);
        let a = tree.delete_min().unwrap();
        assert_eq!(a.key.start, 0);
    }

    #[test]
    fn find_intersect() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        let val = list![1, 2; cx];
        let tree = build_tree(val, cx);
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
        let mut tree = build_tree(val, cx);
        tree.advance(7, 5);
        // let mut tree = dbg!(tree);
        tree.get(TextRange::new(6, 7)).unwrap();
        tree.get(TextRange::new(7, 13)).unwrap();
        tree.get(TextRange::new(13, 14)).unwrap();
        tree.get(TextRange::new(14, 15)).unwrap();
    }


    #[test]
    fn find_next() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        let val = list![1, 2; cx];
        let mut tree = build_tree(val, cx);
        // tree.print();
        tree.delete(TextRange::new(5, 6));
        let mut n = tree.min().unwrap();

        loop {
            match n.next() {
                Some(ne) => {
                    n = ne;
                    // println!("{:?}", ne.key);
                }
                None => break
            }
        }
        assert_eq!(n.key.start, 9)
    }
}
