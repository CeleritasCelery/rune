use crate::core::object::{CharTable, Object, Symbol};
use rune_macros::defun;
use std::collections::HashMap;

#[defun]
fn make_char_table<'ob>(_purpose: Symbol<'ob>, init: Option<Object<'ob>>) -> CharTable<'ob> {
    CharTable { parent: None, data: HashMap::new(), init }
}
