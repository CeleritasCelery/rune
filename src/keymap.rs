
use fn_macros::defun;

use crate::object::Object;

#[defun]
pub(crate) fn make_keymap(_string: Option<&String>) -> Object {
    // TODO: implement
    Object::Nil
}

#[defun]
pub(crate) fn make_sparse_keymap(_string: Option<&String>) -> Object {
    // TODO: implement
    Object::Nil
}

#[defun]
pub(crate) fn define_key<'ob>(_keymap: Object<'ob>, _key: Object<'ob>, _def: Object<'ob>) -> Object<'ob> {
    // TODO: implement
    Object::Nil
}

defsubr!(make_keymap, make_sparse_keymap, define_key);
