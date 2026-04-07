use crate::core::{
    object::Object,
};
use anyhow::Result;
use rune_macros::defun;

#[defun]
fn current_window_configuration<'ob>() -> Result<String> {
    Ok("wconf".to_string())
}

#[defun]
fn set_window_configuration<'ob>(_wc: Object<'ob>) -> Result<String> {
    Ok("wconf".to_string())
}
