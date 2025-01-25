use rune_macros::defun;

use crate::core::object::{CharTable, Object, Symbol};

#[defun]
fn make_char_table<'ob>(purpose: Symbol<'ob>, init: Option<Object<'ob>>) -> CharTable<'ob> {
    todo!()
}
