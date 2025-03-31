//! Buffer editing utilities.
use crate::core::{
    env::{ArgSlice, Env},
    gc::{Context, Rt},
    object::{Object, ObjectType},
};
use anyhow::{Result, bail, ensure};
use rune_macros::defun;
use std::{fmt::Write as _, io::Write};

defsym!(WALL);

#[defun]
fn message(format_string: &str, args: &[Object]) -> Result<String> {
    let message = format(format_string, args)?;
    println!("MESSAGE: {message}");
    std::io::stdout().flush()?;
    Ok(message)
}

defvar!(MESSAGE_NAME);
defvar!(MESSAGE_TYPE, "new message");

#[defun]
fn format(string: &str, objects: &[Object]) -> Result<String> {
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
                ObjectType::String(string) => write!(result, "{string}")?,
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
fn format_message(string: &str, objects: &[Object]) -> Result<String> {
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
fn char_to_string(chr: char) -> String {
    format!("{chr}")
}

#[defun]
pub(crate) fn insert(args: ArgSlice, env: &mut Rt<Env>, cx: &Context) -> Result<()> {
    let env = &mut **env; // Deref into rooted type so we can split the borrow
    let buffer = env.current_buffer.get_mut();
    let args = Rt::bind_slice(env.stack.arg_slice(args), cx);
    for arg in args {
        buffer.insert(*arg)?;
    }
    Ok(())
}

// TODO: this should not throw and error. Buffer will always be present.
#[defun]
pub(crate) fn goto_char(position: usize, env: &mut Rt<Env>) -> Result<()> {
    let buffer = env.current_buffer.get_mut();
    buffer.text.set_cursor(position);
    Ok(())
}

// TODO: this should not throw and error. Buffer will always be present.
#[defun]
pub(crate) fn point_max(env: &mut Rt<Env>) -> Result<usize> {
    let buffer = env.current_buffer.get_mut();
    // TODO: Handle narrowing
    Ok(buffer.text.len_chars() + 1)
}

#[defun]
pub(crate) fn point_min() -> usize {
    // TODO: Handle narrowing
    1
}

#[defun]
pub(crate) fn point_marker(env: &mut Rt<Env>) -> usize {
    // TODO: Implement marker objects
    env.current_buffer.get_mut().text.cursor().chars()
}

#[defun]
fn delete_region(start: usize, end: usize, env: &mut Rt<Env>) -> Result<()> {
    env.current_buffer.get_mut().delete(start, end)
}

#[defun]
fn bolp(env: &Rt<Env>) -> bool {
    let buf = env.current_buffer.get();
    let chars = buf.text.cursor().chars();
    chars == 0 || buf.text.char_at(chars - 1).unwrap() == '\n'
}

#[defun]
fn point(env: &Rt<Env>) -> usize {
    env.current_buffer.get().text.cursor().chars()
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
    use crate::core::object::NIL;
    use crate::{
        buffer::{get_buffer_create, set_buffer},
        core::gc::RootSet,
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
        root!(env, new(Env), cx);
        let buffer = get_buffer_create(cx.add("test_insert"), Some(NIL), cx).unwrap();
        set_buffer(buffer, env, cx).unwrap();
        cx.garbage_collect(true);
        env.stack.push(104);
        env.stack.push(101);
        env.stack.push(108);
        env.stack.push(108);
        env.stack.push(111);
        insert(ArgSlice::new(5), env, cx).unwrap();
        assert_eq!(env.current_buffer.get(), "hello");
    }

    #[test]
    fn test_delete_region() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        root!(env, new(Env), cx);
        let buffer = get_buffer_create(cx.add("test_delete_region"), Some(NIL), cx).unwrap();
        set_buffer(buffer, env, cx).unwrap();
        cx.garbage_collect(true);
        env.stack.push(cx.add("hello"));
        env.stack.push(cx.add(" world"));
        insert(ArgSlice::new(2), env, cx).unwrap();

        assert_eq!(env.current_buffer.get(), "hello world");
        delete_region(2, 4, env).unwrap();
        assert_eq!(env.current_buffer.get(), "hlo world");
    }
}
