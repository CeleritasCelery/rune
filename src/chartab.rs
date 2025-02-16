use crate::core::object::{CharTable, CharTableInner, Object, Symbol};
use rune_macros::defun;

#[defun]
fn make_char_table<'ob>(_purpose: Symbol<'ob>, init: Option<Object<'ob>>) -> CharTableInner<'ob> {
    CharTableInner::new(init)
}

#[defun]
fn set_char_table_parent<'ob>(
    table: &'ob CharTable,
    parent: Option<&'ob CharTable>,
) -> Option<&'ob CharTable> {
    table.set_parent(parent);
    parent
}
