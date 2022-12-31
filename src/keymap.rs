use crate::core::object::{nil, GcObj};
use fn_macros::defun;

#[defun]
pub(crate) fn make_keymap(_string: Option<&str>) -> GcObj {
    // TODO: implement
    nil()
}

#[defun]
pub(crate) fn make_sparse_keymap(_string: Option<&str>) -> GcObj {
    // TODO: implement
    nil()
}

#[defun]
pub(crate) fn use_global_map(_keymap: GcObj) -> GcObj {
    // TODO: implement
    nil()
}

#[defun]
pub(crate) fn set_keymap_parent<'ob>(_keymap: GcObj<'ob>, _parent: GcObj<'ob>) -> GcObj<'ob> {
    // TODO: implement
    nil()
}

#[defun]
pub(crate) fn define_key<'ob>(
    _keymap: GcObj<'ob>,
    _key: GcObj<'ob>,
    _def: GcObj<'ob>,
) -> GcObj<'ob> {
    // TODO: implement
    nil()
}

defvar!(MINIBUFFER_LOCAL_MAP);
