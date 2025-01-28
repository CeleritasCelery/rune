use super::{CloneIn, Gc, IntoObject, Object};
use crate::{
    core::gc::{Block, GcHeap, Slot},
    derive_markable,
};
use core::fmt;
use rune_core::hashmap::{HashMap, HashSet};
use rune_macros::Trace;

#[derive(Debug, Eq, Trace)]
pub struct CharTable<'ob> {
    parent: Option<Slot<&'ob LispCharTable>>,
    data: HashMap<usize, Object<'ob>>,
    init: Option<Object<'ob>>,
}

impl<'ob> CharTable<'ob> {
    pub fn new(init: Option<Object<'ob>>) -> Self {
        CharTable { parent: None, data: HashMap::default(), init }
    }
}

#[derive(PartialEq, Eq, Trace, Debug)]
pub(crate) struct LispCharTable(GcHeap<CharTable<'static>>);

derive_markable!(LispCharTable);

impl<'ob> PartialEq for CharTable<'ob> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl<'new> CloneIn<'new, &'new Self> for LispCharTable {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> Gc<&'new Self> {
        let parent = self.0.parent.as_ref().map(|p| Slot::new(p.clone_in(bk).untag()));

        let mut data = HashMap::default();
        for (key, value) in &self.0.data {
            let new_value = value.clone_in(bk);
            data.insert(*key, new_value);
        }

        let init = self.0.init.map(|i| i.clone_in(bk));
        CharTable { parent, data, init }.into_obj(bk)
    }
}

impl LispCharTable {
    pub(in crate::core) unsafe fn new(table: CharTable<'_>, constant: bool) -> Self {
        // transmute lifetime to static
        let table = unsafe { std::mem::transmute::<CharTable<'_>, CharTable<'static>>(table) };
        Self(GcHeap::new(table, constant))
    }

    pub(super) fn display_walk(
        &self,
        f: &mut fmt::Formatter,
        seen: &mut HashSet<*const u8>,
    ) -> fmt::Result {
        todo!()
    }
}
