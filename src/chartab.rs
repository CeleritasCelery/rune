use crate::{
    core::{
        gc::{GcHeap, GcState, Slot, Trace},
        object::{IntoObject, Object, Symbol},
    },
    NewtypeMarkable,
};
use macro_attr_2018::macro_attr;
use newtype_derive_2018::*;
use rune_macros::{defun, Trace};
use std::collections::HashMap;

#[derive(Debug, Eq)]
pub(crate) struct CharTable<'ob> {
    parent: Slot<&'ob CharTable<'ob>>,
    data: HashMap<usize, Object<'ob>>,
    init: Option<Object<'ob>>,
}

impl<'ob> PartialEq for CharTable<'ob> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl<'ob> Trace for CharTable<'ob> {
    fn trace(&self, state: &mut GcState) {
        let _ = state;
        todo!()
    }
}

impl<'ob> std::fmt::Display for CharTable<'ob> {
    fn fmt(&self, f: &mut std_fmt_Formatter<'_>) -> std_fmt_Result {
        todo!()
    }
}

impl IntoObject for CharTable<'_> {
    type Out<'ob> = &'ob LispCharTable<'ob>;

    fn into_obj<const C: bool>(
        self,
        block: &crate::core::gc::Block<C>,
    ) -> crate::core::object::Gc<Self::Out<'_>> {
        todo!()
    }
}

macro_attr! {
    #[derive(PartialEq, Eq, Trace, NewtypeDebug!, NewtypeDisplay!, NewtypeDeref!, NewtypeMarkable!)]
    pub(crate) struct LispCharTable<'ob>(GcHeap<CharTable<'ob>>);
}

#[defun]
fn make_char_table<'ob>(purpose: Symbol<'ob>, init: Option<Object<'ob>>) -> Object<'ob> {
    CharTable { parent: None, data: HashMap::new(), init: init }
}
