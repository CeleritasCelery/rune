use super::{CloneIn, Gc, Object};
use crate::{
    core::gc::{Block, GcHeap, GcState, Slot, Trace},
    derive_markable,
};
use core::fmt;
use rune_core::hashmap::HashSet;
use rune_macros::Trace;
use std::collections::HashMap;

#[derive(Debug, Eq)]
pub struct CharTable<'ob> {
    parent: Option<Slot<&'ob CharTable<'ob>>>,
    data: HashMap<usize, Object<'ob>>,
    init: Option<Object<'ob>>,
}

impl Trace for CharTable<'static> {
    fn trace(&self, _state: &mut GcState) {
        todo!()
    }
}

#[derive(PartialEq, Eq, Trace)]
pub(crate) struct LispCharTable(GcHeap<CharTable<'static>>);

derive_markable!(LispCharTable);

impl<'ob> PartialEq for CharTable<'ob> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl<'new> CloneIn<'new, &'new Self> for LispCharTable {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> Gc<&'new Self> {
        todo!()
    }
}

impl LispCharTable {
    pub(super) fn display_walk(
        &self,
        f: &mut fmt::Formatter,
        seen: &mut HashSet<*const u8>,
    ) -> fmt::Result {
        todo!()
    }
}
