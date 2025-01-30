use super::{CloneIn, Gc, IntoObject, Object};
use crate::{
    core::gc::{Block, GcHeap, Slot},
    derive_markable,
};
use rune_core::hashmap::HashMap;
use rune_macros::Trace;
use std::fmt;

#[derive(Debug, Eq, Trace)]
pub struct CharTableInner<'ob> {
    parent: Option<Slot<&'ob CharTable>>,
    data: HashMap<usize, Object<'ob>>,
    init: Option<Object<'ob>>,
}

impl<'ob> CharTableInner<'ob> {
    pub fn new(init: Option<Object<'ob>>) -> Self {
        CharTableInner { parent: None, data: HashMap::default(), init }
    }
}

#[derive(PartialEq, Eq, Trace, Debug)]
pub(crate) struct CharTable(GcHeap<CharTableInner<'static>>);

derive_markable!(CharTable);

impl PartialEq for CharTableInner<'_> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl<'new> CloneIn<'new, &'new Self> for CharTable {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> Gc<&'new Self> {
        let parent = self.0.parent.as_ref().map(|p| Slot::new(p.clone_in(bk).untag()));

        let mut data = HashMap::default();
        for (key, value) in &self.0.data {
            let new_value = value.clone_in(bk);
            data.insert(*key, new_value);
        }

        let init = self.0.init.map(|i| i.clone_in(bk));
        CharTableInner { parent, data, init }.into_obj(bk)
    }
}

impl CharTable {
    pub(in crate::core) unsafe fn new(table: CharTableInner<'_>, constant: bool) -> Self {
        // transmute lifetime to static
        let table =
            unsafe { std::mem::transmute::<CharTableInner<'_>, CharTableInner<'static>>(table) };
        Self(GcHeap::new(table, constant))
    }

    pub fn get(&self, idx: &usize) -> Option<Object> {
        self.0.data.get(idx).copied().or(self.0.init)
    }
}

impl fmt::Display for CharTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        let mut values = self.0.data.values();
        if let Some(first) = values.next() {
            write!(f, "{}", first)?;
            for x in values {
                write!(f, " {}", x)?;
            }
        }
        write!(f, "]")
    }
}
