use fn_macros::defun;

use crate::object::Object;

#[defun]
pub(crate) fn make_keymap(_string: Option<&String>) -> Object {
    // TODO: implement
    Object::NIL
}

#[defun]
pub(crate) fn make_sparse_keymap(_string: Option<&String>) -> Object {
    // TODO: implement
    Object::NIL
}

#[defun]
pub(crate) fn use_global_map(_keymap: Object) -> Object {
    // TODO: implement
    Object::NIL
}

#[defun]
pub(crate) fn set_keymap_parent<'ob>(_keymap: Object<'ob>, _parent: Object<'ob>) -> Object<'ob> {
    // TODO: implement
    Object::NIL
}

#[defun]
pub(crate) fn define_key<'ob>(
    _keymap: Object<'ob>,
    _key: Object<'ob>,
    _def: Object<'ob>,
) -> Object<'ob> {
    // TODO: implement
    Object::NIL
}

defsubr!(
    make_keymap,
    make_sparse_keymap,
    use_global_map,
    set_keymap_parent,
    define_key
);
