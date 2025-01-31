use crate::core::object::{CharTableInner, Object, Symbol};
use rune_macros::defun;

#[defun]
fn make_char_table<'ob>(_purpose: Symbol<'ob>, init: Option<Object<'ob>>) -> CharTableInner<'ob> {
    CharTableInner::new(init)
}
