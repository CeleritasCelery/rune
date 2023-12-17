//! Buffer editing utilities.
use crate::core::{
    env::Env,
    gc::Rt,
    object::{GcObj, Object},
};
use anyhow::{bail, ensure, Result};
use rune_macros::defun;
use std::{fmt::Write as _, io::Write};

#[defun]
fn message(format_string: &str, args: &[GcObj]) -> Result<String> {
    let message = format(format_string, args)?;
    println!("MESSAGE: {message}");
    std::io::stdout().flush()?;
    Ok(message)
}

defvar!(MESSAGE_NAME);
defvar!(MESSAGE_TYPE, "new message");

#[defun]
fn format(string: &str, objects: &[GcObj]) -> Result<String> {
    let mut result = String::new();
    let mut arguments = objects.iter();
    let mut remaining = string;

    let mut escaped = false;
    let mut is_format_char = |c: char| {
        if escaped {
            escaped = false;
            false
        } else if c == '\\' {
            escaped = true;
            false
        } else {
            c == '%'
        }
    };
    while let Some(start) = remaining.find(&mut is_format_char) {
        result += &remaining[..start];
        let Some(specifier) = remaining.as_bytes().get(start + 1) else {
            bail!("Format string ends in middle of format specifier")
        };
        // "%%" inserts a single "%" in the output
        if *specifier == b'%' {
            result.push('%');
        } else {
            // TODO: currently handles all format types the same. Need to check the modifier characters.
            let Some(val) = arguments.next() else {
                bail!("Not enough arguments for format string")
            };
            match val.untag() {
                Object::String(string) => write!(result, "{string}")?,
                obj => write!(result, "{obj}")?,
            }
        }
        remaining = &remaining[start + 2..];
    }
    result += remaining;
    ensure!(arguments.next().is_none(), "Too many arguments for format string");
    Ok(result)
}

#[defun]
fn format_message(string: &str, objects: &[GcObj]) -> Result<String> {
    let formatted = format(string, objects)?;
    // TODO: implement support for `text-quoting-style`.
    Ok(formatted
        .chars()
        .map(|c| if matches!(c, '`' | '\'') { '"' } else { c })
        .collect())
}

#[defun]
fn string_to_char(string: &str) -> char {
    string.chars().next().unwrap_or('\0')
}

#[defun]
fn char_to_string(chr: u64) -> Result<String> {
    let Some(chr) = std::char::from_u32(u32::try_from(chr)?) else {
        bail!("Invalid character")
    };
    Ok(format!("{chr}"))
}

// TODO: this should not throw and error. Buffer will always be present.
#[defun]
pub(crate) fn insert(args: &[GcObj], env: &mut Rt<Env>) -> Result<()> {
    let Some(buffer) = env.current_buffer.as_mut() else { bail!("No current buffer") };
    for arg in args {
        buffer.insert(*arg)?;
    }

    Ok(())
}

// TODO: this should not throw and error. Buffer will always be present.
#[defun]
pub(crate) fn goto_char(position: usize, env: &mut Rt<Env>) -> Result<()> {
    let Some(buffer) = env.current_buffer.as_mut() else { bail!("No current buffer") };
    buffer.text.set_cursor(position);
    Ok(())
}

// TODO: this should not throw and error. Buffer will always be present.
#[defun]
pub(crate) fn point_max(env: &mut Rt<Env>) -> Result<usize> {
    let Some(buffer) = env.current_buffer.as_mut() else { bail!("No current buffer") };
    // TODO: Handle narrowing
    Ok(buffer.text.len_chars() + 1)
}

// TODO: this should not throw and error. Buffer will always be present.
#[defun]
pub(crate) fn point_marker(env: &mut Rt<Env>) -> Result<usize> {
    let Some(buffer) = env.current_buffer.as_mut() else { bail!("No current buffer") };
    // TODO: Implement marker objects
    Ok(buffer.text.cursor().chars())
}

// TODO: this should not throw and error. Buffer will always be present.
#[defun]
fn delete_region(start: usize, end: usize, env: &mut Rt<Env>) -> Result<()> {
    let Some(buffer) = env.current_buffer.as_mut() else { bail!("No current buffer") };
    buffer.delete(start, end);
    Ok(())
}

#[defun]
fn bolp(env: &Rt<Env>) -> bool {
    env.with_buffer(None, |b| {
        let chars = b.text.cursor().chars();
        chars == 0 || b.text.char_at(chars - 1).unwrap() == '\n'
    })
    .unwrap_or(false)
}

#[defun]
fn point(env: &Rt<Env>) -> usize {
    env.with_buffer(None, |b| b.text.cursor().chars()).unwrap_or(0)
}

#[defun]
fn system_name() -> String {
    hostname::get()
        .expect("Failed to get hostname")
        .into_string()
        .expect("Failed to convert OsString to String")
}

#[cfg(test)]
mod test {
    use crate::core::object::nil;
    use crate::{
        buffer::{get_buffer_create, set_buffer},
        core::gc::{Context, RootSet},
    };
    use rune_core::macros::root;

    use super::*;

    #[test]
    fn test_format() {
        assert_eq!(&format("%s", &[1.into()]).unwrap(), "1");
        assert_eq!(&format("foo-%s", &[2.into()]).unwrap(), "foo-2");
        assert_eq!(&format("%%", &[]).unwrap(), "%");
        assert_eq!(&format("_%%_", &[]).unwrap(), "_%_");
        assert_eq!(&format("foo-%s %s", &[3.into(), 4.into()]).unwrap(), "foo-3 4");
        let sym = crate::core::env::sym::FUNCTION.into();
        assert_eq!(&format("%s", &[sym]).unwrap(), "function");

        assert!(&format("%s", &[]).is_err());
        assert!(&format("%s", &[1.into(), 2.into()]).is_err());

        assert!(format("`%s' %s%s%s", &[0.into(), 1.into(), 2.into(), 3.into()]).is_ok());
    }

    #[test]
    fn test_insert() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        root!(env, Env::default(), cx);
        let buffer = get_buffer_create(cx.add("test_insert"), Some(nil()), cx).unwrap();
        set_buffer(buffer, env, cx).unwrap();
        cx.garbage_collect(true);
        insert(&[104.into(), 101.into(), 108.into(), 108.into(), 111.into()], env).unwrap();
        assert_eq!(env.current_buffer.as_ref().unwrap(), "hello");
    }

    #[test]
    fn test_delete_region() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        root!(env, Env::default(), cx);
        let buffer = get_buffer_create(cx.add("test_delete_region"), Some(nil()), cx).unwrap();
        set_buffer(buffer, env, cx).unwrap();
        cx.garbage_collect(true);
        insert(&[cx.add("hello"), cx.add(" world")], env).unwrap();

        assert_eq!(env.current_buffer.as_ref().unwrap(), "hello world");
        delete_region(1, 3, env).unwrap();
        assert_eq!(env.current_buffer.as_ref().unwrap(), "hlo world");
    }
}
