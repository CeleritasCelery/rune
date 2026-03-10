//! Buffer editing utilities.
use crate::core::{
    env::{ArgSlice, Env},
    gc::{Context, Rt},
    object::{Object, ObjectType},
};
use anyhow::{Result, bail, ensure};
use rune_macros::defun;
use std::{fmt::Write as _, io::Write};
use either::Either;

use crate::formatting;
use crate::formatting::{PendingAction, argument::{Argument, DoubleFormat, Specifier}};

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

    let mut tmpbuf = String::new();

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
            remaining = &remaining[start+2..];
        } else {
            remaining = &remaining[start + 1..];
            let (flags, sub) = formatting::parse_flags(remaining);
            let (width, sub) = match formatting::parse_width(sub) {
                (Either::Left(PendingAction::ReadFromArg), sub) =>
                    (arguments.next().unwrap().unwrap_int() as i32, sub),
                (Either::Right(width), sub) => (width, sub)
            };
            let (precision, sub) = match formatting::parse_precision(sub) {
                (Some(Either::Left(PendingAction::ReadFromArg)), sub) =>
                    (Some(arguments.next().unwrap().unwrap_int() as i32), sub),
                (Some(Either::Right(prec)), sub) => (Some(prec), sub),
                (None, sub) => (None, sub)
            };
            let (length, sub) = formatting::parse_length(sub);
            let (ch, sub) = match sub.as_bytes().get(0) {
                Some(ch) => (ch, &sub[1..]),
                None => if sub[1..].len() > 0 { // NOTE not sure if this is still needed...
                    (&b'%', &sub[1..])
                } else {
                    (&0, "")
                }
            };
            let specifier = match ch {
                b'%' => {
                    // we're handling this externally though, this arm shouldn't be reached
                    Specifier::Percent
                }

                b'd' | b'i' => Specifier::Int(length.parse_signed(arguments.next())),
                b'x' => Specifier::Hex(length.parse_unsigned(arguments.next())),
                b'X' => Specifier::UpperHex(length.parse_unsigned(arguments.next())),
                b'u' => Specifier::Uint(length.parse_unsigned(arguments.next())),
                b'o' => Specifier::Octal(length.parse_unsigned(arguments.next())),

                b'f' | b'F' => Specifier::Double {
                    value: f64::try_from(*arguments.next().unwrap()).unwrap(),
                    format: DoubleFormat::Normal.set_upper(ch.is_ascii_uppercase()),
                },
                b'e' | b'E' => Specifier::Double {
                    value: f64::try_from(*arguments.next().unwrap()).unwrap(),
                    format: DoubleFormat::Scientific.set_upper(ch.is_ascii_uppercase()),
                },
                b'g' | b'G' => Specifier::Double {
                    value: f64::try_from(*arguments.next().unwrap()).unwrap(),
                    format: DoubleFormat::Auto.set_upper(ch.is_ascii_uppercase()),
                },
                b'a' | b'A' => Specifier::Double {
                    value: f64::try_from(*arguments.next().unwrap()).unwrap(),
                    format: DoubleFormat::Hex.set_upper(ch.is_ascii_uppercase()),
                },

                b's' | b'S' => {
                    match arguments.next() {
                        Some(obj) => {
                            match obj.untag() {
                                ObjectType::String(string) => write!(tmpbuf, "{string}").unwrap(),
                                obj => write!(tmpbuf, "{obj}").unwrap(),
                            };
                        },
                        None => bail!("no string parameter?"),
                    };
                    // As a common extension supported by glibc, musl, and
                    // others, format a NULL pointer as "(null)".
                    // if arg.as_ptr().is_null() {
                    //     Specifier::Bytes(b"(null)")
                    // } else {
                        Specifier::String(tmpbuf.as_str())
                    // }
                }

                b'c' => {
                    /* NOTE not needed due to the different conversion?
                    trait CharToInt {
                        type IntType;
                    }

                    impl CharToInt for c_schar {
                        type IntType = c_int;
                    }

                    impl CharToInt for c_uchar {
                        type IntType = c_uint;
                    }
                    */
                    Specifier::Char(
                        char::try_from(*arguments.next().unwrap()).unwrap() as i8
                    )
                }

                b'p' => bail!("'p' specifier not supported"), //Specifier::Pointer(unsafe { args.arg() }),
                b'n' => bail!("'n' specifier not supported"), //Specifier::WriteBytesWritten(written, unsafe { args.arg() }),

                _ => continue,
            };
            remaining = sub;
            formatting::output::fmt_write(Argument { flags, width, precision, specifier }, &mut result);
            tmpbuf.clear();
        }
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
