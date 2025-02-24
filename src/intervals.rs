use interval_tree::{IntervalTree as Tree, Node};

use crate::{
    core::{
        gc::{IntoRoot, Slot, Trace},
        object::{Object, WithLifetime},
    },
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
        // result.tree.apply_mut(&mut |n| n.val = n.val.clone().with_lifetime());
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
                add_properties(
                    a.as_obj_copy(),
                    b.as_obj_copy(),
                    crate::textprops::PropertySetType::Append,
                    false,
                    cx,
                )
                .map(|obj| Slot::new(obj))
                .unwrap()
            })
            .unwrap();
    }

    pub fn find(&self, position: usize) -> Option<&Node<Slot<Object<'ob>>>> {
        self.tree.find(position)
    }

    pub fn clean(&mut self) {
        todo!()
    }
}
