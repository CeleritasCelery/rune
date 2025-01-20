use rune_macros::defun;
use std::collections::HashMap;

use crate::core::object::{CharTable, Object, Symbol};

#[defun]
fn make_char_table<'ob>(purpose: Symbol<'ob>, init: Option<Object<'ob>>) -> Object<'ob> {
    CharTable { parent: None, data: HashMap::new(), init: init }
}
