use interval_tree::IntervalTree as Tree;

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
        let mut result: IntervalTree<'new> = std::mem::transmute(self);
        result.tree.apply_mut(&mut |n| n.val = n.val.clone().with_lifetime());
        result
    }
}

impl<'ob> IntervalTree<'ob> {
    pub fn new() -> Self {
        IntervalTree { tree: Tree::new() }
    }

    pub fn insert(&mut self, start: usize, end: usize, val: Slot<Object<'ob>>, cx: &'ob Context) {
        self.tree
            .insert((start, end), val, |a, b| {
                add_properties(
                    a.as_obj_copy(),
                    b.as_obj_copy(),
                    crate::textprops::PropertySetType::Append,
                    false,
                    cx,
                ).map(|(obj, c)| (Slot::new(obj), c))
            })
            .unwrap();
    }

    pub fn find(&self, position: usize) -> Option<&interval_tree::Node<Slot<crate::Gc<crate::core::object::ObjectType<'ob>>>>> {
        self.tree.find(position)
    }

    pub fn clean(&mut self) {
        todo!()
    }
}
