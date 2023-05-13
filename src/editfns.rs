use crate::core::{
    env::Env,
    gc::{Context, Rt},
    object::{GcObj, Object},
};
use anyhow::{bail, ensure, Result};
use fn_macros::defun;
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
        result.push_str(&remaining[..start]);
        let Some(specifier) = remaining.as_bytes().get(start+1) else {bail!("Format string ends in middle of format specifier")};
        // "%%" inserts a single "%" in the output
        if *specifier == b'%' {
            result.push('%');
        } else {
            // TODO: currently handles all format types the same. Need to check the modifier characters.
            let Some(val) = arguments.next() else {bail!("Not enough arguments for format string")};
            match val.untag() {
                Object::String(s) => result.push_str(s.try_into().unwrap()),
                obj => write!(result, "{obj}")?,
            }
        }
        remaining = &remaining[start + 2..];
    }
    result.push_str(remaining);
    ensure!(
        arguments.next().is_none(),
        "Too many arguments for format string"
    );
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
fn insert(args: &[GcObj], env: &mut Rt<Env>, cx: &Context) -> Result<()> {
    let Some(buffer) = &*env.current_buffer else {bail!("No current buffer")};
    let buffer = buffer.bind(cx);
    for arg in args {
        buffer.insert(*arg)?;
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_format() {
        assert_eq!(&format("%s", &[1.into()]).unwrap(), "1");
        assert_eq!(&format("foo-%s", &[2.into()]).unwrap(), "foo-2");
        assert_eq!(&format("%%", &[]).unwrap(), "%");
        assert_eq!(&format("_%%_", &[]).unwrap(), "_%_");
        assert_eq!(
            &format("foo-%s %s", &[3.into(), 4.into()]).unwrap(),
            "foo-3 4"
        );
        let sym = crate::core::env::sym::FUNCTION.into();
        assert_eq!(&format("%s", &[sym]).unwrap(), "function");

        assert!(&format("%s", &[]).is_err());
        assert!(&format("%s", &[1.into(), 2.into()]).is_err());

        assert!(format("`%s' %s%s%s", &[0.into(), 1.into(), 2.into(), 3.into()]).is_ok());
    }
}
