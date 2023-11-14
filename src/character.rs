use crate::core::object::{Gc, GcObj, Object};
use anyhow::Result;
use fn_macros::defun;

#[defun]
fn unibyte_string(bytes: &[Gc<i64>]) -> Result<Vec<u8>> {
    let unibyte: Result<Vec<u8>, _> = bytes.iter().map(|x| u8::try_from(x.untag())).collect();
    Ok(unibyte?)
}

#[defun]
fn max_char(unicode: Option<()>) -> u32 {
    if unicode.is_some() {
        std::char::MAX as u32
    } else {
        0x3F_FFFF
    }
}

#[defun]
fn characterp(obj: GcObj) -> bool {
    match obj.untag() {
        Object::Int(c) => match u32::try_from(c) {
            Ok(c) => char::from_u32(c).is_some(),
            Err(_) => false,
        },
        _ => false,
    }
}
