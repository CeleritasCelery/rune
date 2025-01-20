use super::{CloneIn, Gc, IntoObject, MutObjCell, ObjCell, Object};
use crate::{
    core::gc::{Block, GcHeap, GcState, Slot, Trace},
    NewtypeMarkable,
};
use macro_attr_2018::macro_attr;
use newtype_derive_2018::*;
use rune_macros::Trace;
use std::{collections::HashMap, fmt};

#[derive(Debug, Eq)]
pub(crate) struct CharTable<'ob> {
    parent: Slot<&'ob CharTable<'ob>>,
    data: HashMap<usize, Object<'ob>>,
    init: Option<Object<'ob>>,
}

macro_attr! {
    #[derive(PartialEq, Eq, Trace, NewtypeDebug!, NewtypeDisplay!, NewtypeDeref!, NewtypeMarkable!)]
    pub(crate) struct LispCharTable(GcHeap<CharTable<'static>>);
}

impl<'new> CloneIn<'new, &'new Self> for LispCharTable {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> Gc<&'new Self> {
        todo!()
    }
}

impl<'ob> Trace for CharTable<'ob> {
    fn trace(&self, state: &mut GcState) {
        todo!()
    }
}

impl<'ob> PartialEq for CharTable<'ob> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl<'ob> fmt::Display for CharTable<'ob> {
    fn fmt(&self, f: &mut std_fmt_Formatter<'_>) -> fmt::Result {
        todo!()
    }
}
