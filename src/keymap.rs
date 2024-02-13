//! Keymap handling.
use crate::core::object::Object;
use rune_macros::defun;

// TODO: implement keymaps
#[defun]
fn make_keymap(_string: Option<&str>) {}

#[defun]
fn make_sparse_keymap(_string: Option<&str>) {}

#[defun]
fn use_global_map(_keymap: Object) {}

#[defun]
fn set_keymap_parent<'ob>(_keymap: Object<'ob>, _parent: Object<'ob>) {}

#[defun]
pub(crate) fn define_key<'ob>(_keymap: Object<'ob>, _key: Object<'ob>, _def: Object<'ob>) {}

defvar!(MINIBUFFER_LOCAL_MAP);
