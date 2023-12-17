//! Keymap handling.
use crate::core::object::GcObj;
use rune_macros::defun;

// TODO: implement keymaps
#[defun]
fn make_keymap(_string: Option<&str>) {}

#[defun]
fn make_sparse_keymap(_string: Option<&str>) {}

#[defun]
fn use_global_map(_keymap: GcObj) {}

#[defun]
fn set_keymap_parent<'ob>(_keymap: GcObj<'ob>, _parent: GcObj<'ob>) {}

#[defun]
pub(crate) fn define_key<'ob>(_keymap: GcObj<'ob>, _key: GcObj<'ob>, _def: GcObj<'ob>) {}

defvar!(MINIBUFFER_LOCAL_MAP);
