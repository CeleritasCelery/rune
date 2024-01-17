//! Character and string utilities.
use crate::core::{
    gc::Context,
    object::{int_to_char, Gc, GcObj, Object},
};
use anyhow::Result;
use rune_macros::defun;

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

#[defun]
fn string(characters: &[Gc<i64>]) -> Result<String> {
    let string: Result<_, _> = characters.iter().map(|x| int_to_char(x.untag())).collect();
    Ok(string?)
}

#[defun]
fn make_string<'ob>(
    length: usize,
    init: usize,
    multibyte: Option<()>,
    cx: &'ob Context,
) -> Result<GcObj<'ob>> {
    if multibyte.is_some() {
        let chr = int_to_char(i64::try_from(init)?)?;
        let string: String = (0..length).map(|_| chr).collect();
        Ok(cx.add(string))
    } else {
        let chr = u8::try_from(init)?;
        let string: Vec<_> = (0..length).map(|_| chr).collect();
        Ok(cx.add(string))
    }
}
