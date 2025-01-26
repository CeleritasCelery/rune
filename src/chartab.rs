use crate::core::object::{CharTable, Object, Symbol};
use rune_macros::defun;

#[defun]
fn make_char_table<'ob>(_purpose: Symbol<'ob>, init: Option<Object<'ob>>) -> CharTable<'ob> {
    CharTable::new(init)
}
