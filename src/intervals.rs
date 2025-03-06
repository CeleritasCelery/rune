use anyhow::{bail, Result};
use interval_tree::{IntervalTree as Tree, Node};

use crate::{
    core::{
        cons::Cons,
        error::{Type, TypeError},
        gc::{IntoRoot, Slot, Trace},
        object::{Object, ObjectType, TagType, WithLifetime, NIL},
    },
    fns::eq,
    textprops::add_properties,
    Context,
};

#[derive(Debug)]
pub struct IntervalTree<'ob> {
    pub tree: Tree<Slot<Object<'ob>>>,
}

impl Trace for IntervalTree<'_> {
    fn trace(&self, state: &mut crate::core::gc::GcState) {
        self.tree.apply(&mut |x| x.trace(state));
    }
}

impl<'new> IntoRoot<IntervalTree<'new>> for IntervalTree<'_> {
    unsafe fn into_root(self) -> IntervalTree<'new> {
        self.with_lifetime()
    }
}

impl<'new> WithLifetime<'new> for IntervalTree<'_> {
    type Out = IntervalTree<'new>;
    unsafe fn with_lifetime(self) -> IntervalTree<'new> {
        let result: IntervalTree<'new> = std::mem::transmute(self);
        result
    }
}

unsafe impl Send for IntervalTree<'_> {}

impl<'ob> IntervalTree<'ob> {
    pub fn new() -> Self {
        IntervalTree { tree: Tree::new() }
    }

    /// Inserts a new interval with the specified range and value into the interval tree.
    ///
    /// If the interval overlaps with existing intervals, their properties will be merged
    /// using `add_properties`. The resulting object will be stored in the tree.
    pub fn insert(&mut self, start: usize, end: usize, val: Slot<Object<'ob>>, cx: &'ob Context) {
        self.tree
            .insert((start, end), val, |a, b| {
                add_properties(*a, *b, crate::textprops::PropertySetType::Append, false, cx)
                    .map(|obj| Slot::new(obj))
                    .unwrap()
            })
            .unwrap();
    }

    pub fn set_properties(
        &mut self,
        start: usize,
        end: usize,
        properties: Object<'ob>,
    ) {
        self.tree.insert((start, end), Slot::new(properties), |a, _b| a).unwrap();
    }

    pub fn find(&self, position: usize) -> Option<&Node<Slot<Object<'ob>>>> {
        self.tree.find(position)
    }

    /// Deletes properties from intervals within [start..end) that match properties in `list_of_props`.
    ///
    /// This function iterates through nodes in the specified range and removes any properties
    /// that are equal (using `eq`) to those in `list_of_props`.
    ///
    /// # Arguments
    ///
    /// * `start` - Start position of the range (inclusive)
    /// * `end` - End position of the range (exclusive)
    /// * `list_of_props` - The properties to delete (as an Object containing a list of properties)
    /// * `cx` - The context used for equality testing
    pub fn delete(
        &mut self,
        start: usize,
        end: usize,
        list_of_props: Object<'ob>,
        cx: &'ob Context,
    ) -> Result<()> {
        let props = list_of_props.as_list()?;
        self.tree.apply_with_split(
            |val| {
                let a = val.untag();
                let mut props = props.clone();
                let mut result = val.as_obj();
                while let Some(sym) = props.next() {
                    let sym = sym.ok()?;
                    result = remove_sym_from_props(sym, result).ok()?;
                }
                Some(unsafe { result.into_root() })
            },
            start..end,
        );
        Ok(())
    }

    pub fn clean(&mut self) {
        self.tree.clean(|a, b| eq(**a, **b), |n| n.is_nil());
    }

    pub(crate) fn iter<'a>(&'a self, start: usize, end: usize) -> IntervalIntersections<'ob, 'a> {
        IntervalIntersections::new(self, start, end)
    }
}

/// Removes all occurrences of `sym` from the property list `props`.
///
/// This function scans through a property list (a cons cell chain) and removes any
/// pairs where the car matches `sym`. The property list is modified in-place.
///
/// # Arguments
/// * `sym` - The symbol to remove from the property list
/// * `props` - The property list to modify (must be a cons cell chain)
///
/// # Returns
/// Returns the modified property list with all occurrences of `sym` removed.
/// If the input is not a cons cell, returns a TypeError.
fn remove_sym_from_props<'ob>(sym: Object<'ob>, props: Object<'ob>) -> Result<Object<'ob>> {
    let ObjectType::Cons(props) = props.untag() else {
        bail!(TypeError::new(Type::List, props))
    };

    // remove properties at the beginning of list

    let mut plist = Some(props);
    while let Some(cons) = plist {
        let prop = cons.car();
        if eq(prop, sym) {
            plist = cons.cddr().and_then(|n| n.try_into().ok());
            // .and_then(|cons: &Cons| cons.cdr().try_into().ok());
        } else {
            break;
        }
    }

    let result = plist.map(|c| ObjectType::Cons(c).tag()).unwrap_or(NIL);

    // now, first pair in this alist is ensured not to be sym. Let's call it `p1`
    while let Some(cons) = plist {
        println!("{cons:?}");
        let cdr = cons.cdr();
        if let ObjectType::Cons(cdr) = cdr.untag() {
            // let sym = cdr.car();
            let cddr = cdr.cdr();
            if let ObjectType::Cons(this) = cddr.untag() {
                // if the second pair(p2)'s key is `sym`, remove it and put its cons(p3)
                // right after p1.
                // if not, then advance p1 to p2, then continue the loop
                if eq(this.car(), sym) {
                    set_cdr(cdr, this.cdr());
                }
                plist = Some(this);
            } else {
                // cddr is not a cons; then it should be nil
                break;
            }
        }
    }

    return Ok(result);
}

fn set_cdr<'ob>(this: &'ob Cons, other: Object<'ob>) -> Option<&'ob Cons> {
    if let ObjectType::Cons(cons) = other.untag() {
        if let ObjectType::Cons(c) = cons.cdr().untag() {
            this.set_cdr(cons.cdr()).ok()?;
        }
        return Some(cons);
    }
    None
}

/// Get the value of property `prop` from `plist`,
/// which is the plist of an interval.
/// We check for direct properties, for categories with property `prop`,
/// and for `prop` appearing on the default-text-properties list.
pub(crate) fn textget<'ob>(plist: Object<'ob>, prop: Object<'ob>) -> Result<Object<'ob>> {
    lookup_char_property(plist, prop, true)
}

/// see `lookup_char_property in intervals.c`
fn lookup_char_property<'ob>(
    mut plist: Object<'ob>,
    prop: Object<'ob>,
    is_textprop: bool,
) -> Result<Object<'ob>> {
    if plist.is_nil() {
        // TODO should return default properties
        return Ok(NIL);
    }
    let ObjectType::Cons(cons) = plist.untag() else {
        bail!(TypeError::new(Type::Cons, plist))
    };

    while let ObjectType::Cons(cons) = plist.untag() {
        let p = cons.car();
        let val = cons.cadr().ok_or(anyhow::anyhow!(TypeError::new(Type::Cons, prop)))?;
        if eq(prop, p) {
            return Ok(val);
        }
        match cons.cddr() {
            Some(obj) => plist = obj,
            None => break,
        }
    }
    Ok(NIL)
}

/// An iterator that yields all intersections in a given interval range,
/// including empty gaps between nodes.
pub(crate) struct IntervalIntersections<'ob, 'tree> {
    tree: &'tree IntervalTree<'ob>,
    start: usize,
    end: usize,
    current: Option<&'tree Node<Slot<Object<'ob>>>>,
    next_start: usize,
}

impl<'ob, 'tree> IntervalIntersections<'ob, 'tree> {
    pub(crate) fn new(tree: &'tree IntervalTree<'ob>, start: usize, end: usize) -> Self {
        let current = tree.tree.find_intersect_min(start..end);
        // let next_start = current.map(|n| n.key.end).unwrap_or(start);
        let next_start = start;
        Self { tree, start, end, current, next_start }
    }
}

impl<'ob, 'tree> Iterator for IntervalIntersections<'ob, 'tree> {
    type Item = (std::ops::Range<usize>, Object<'ob>);

    fn next(&mut self) -> Option<Self::Item> {
        if self.next_start >= self.end {
            return None;
        }

        // Handle gap before first node or between nodes
        if let Some(node) = self.current {
            if self.next_start < node.key.start {
                let gap_end = node.key.start.min(self.end);
                let result = (self.next_start..gap_end, NIL);
                self.next_start = gap_end;
                return Some(result);
            }
        }

        // Handle current node
        if let Some(node) = self.current {
            let intersect_start = node.key.start.max(self.start);
            let intersect_end = node.key.end.min(self.end);

            if intersect_start < intersect_end {
                let result = (intersect_start..intersect_end, *node.val);
                self.next_start = intersect_end;
                self.current = node.next();
                return Some(result);
            }
        }

        // Handle final gap after last node
        if self.next_start < self.end {
            let result = (self.next_start..self.end, NIL);
            self.next_start = self.end;
            return Some(result);
        }

        None
    }
}

/// set `this`'s cdr to `other`'s cddr. i.e., skip a pair in an alist
#[cfg(test)]
mod tests {
    use rune_core::macros::list;

    use super::*;
    use crate::{intern, RootSet};

    #[test]
    fn test_remove_sym_from_props() {
        let roots = &RootSet::default();
        let cx = Context::new(roots);
        let sym = intern("foo", &cx);
        let props = list![ sym, 1, intern("bar", &cx), 2, sym, 3, intern("baz", &cx), 4; &cx];

        // Create a property list: (test 1 test 2 test 3)
        // Remove all 'test' symbols
        let result = remove_sym_from_props(sym.into(), props).unwrap();
        let rs = result.to_string();
        assert_eq!(rs, "(bar 2 baz 4)".to_string());
    }
}

#[cfg(test)]
mod interval_iterator_tests {
    use super::*;
    use crate::{intern, RootSet};

    #[test]
    fn test_empty_tree() {
        let roots = &RootSet::default();
        let cx = Context::new(roots);
        let tree = IntervalTree::new();

        let mut iter = IntervalIntersections::new(&tree, 0, 100);
        assert_eq!(iter.next(), Some((0..100, NIL)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_single_interval() {
        let roots = &RootSet::default();
        let cx = Context::new(roots);
        let val = intern("test", &cx);
        let mut tree = IntervalTree::new();
        tree.insert(10, 20, Slot::new(val.into()), &cx);

        let mut iter = IntervalIntersections::new(&tree, 0, 30);
        assert_eq!(iter.next(), Some((0..10, NIL)));
        assert_eq!(iter.next(), Some((10..20, val.into())));
        assert_eq!(iter.next(), Some((20..30, NIL)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_multiple_intervals() {
        let roots = &RootSet::default();
        let cx = Context::new(roots);
        let val1 = intern("test1", &cx);
        let val2 = intern("test2", &cx);
        let mut tree = IntervalTree::new();
        tree.insert(10, 20, Slot::new(val1.into()), &cx);
        tree.insert(25, 35, Slot::new(val2.into()), &cx);

        let mut iter = IntervalIntersections::new(&tree, 0, 40);
        assert_eq!(iter.next(), Some((0..10, NIL)));
        assert_eq!(iter.next(), Some((10..20, val1.into())));
        assert_eq!(iter.next(), Some((20..25, NIL)));
        assert_eq!(iter.next(), Some((25..35, val2.into())));
        assert_eq!(iter.next(), Some((35..40, NIL)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_overlapping_intervals() {
        let roots = &RootSet::default();
        let cx = Context::new(roots);
        let val1 = intern("test1", &cx);
        let val2 = intern("test2", &cx);
        let mut tree = IntervalTree::new();
        tree.insert(10, 20, Slot::new(val1.into()), &cx);
        tree.insert(20, 30, Slot::new(val1.into()), &cx);
        tree.insert(30, 40, Slot::new(val2.into()), &cx);
        // tree.tree.print();

        let mut iter = IntervalIntersections::new(&tree, 0, 50);
        assert_eq!(iter.next(), Some((0..10, NIL)));
        assert_eq!(iter.next(), Some((10..20, val1.into())));
        assert_eq!(iter.next(), Some((20..30, val1.into())));
        assert_eq!(iter.next(), Some((30..40, val2.into())));
        assert_eq!(iter.next(), Some((40..50, NIL)));
        assert_eq!(iter.next(), None);
    }
}
