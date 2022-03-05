use crate::arena::{Arena, Gc};
use crate::data::Environment;
use crate::object::Object;
use crate::reader;
use crate::symbol::Symbol;
use crate::{interpreter, root};
use fn_macros::defun;

use anyhow::{bail, ensure, Context, Result};

use std::fs;

fn check_lower_bounds(idx: Option<i64>, len: usize) -> Result<usize> {
    let len = len as i64;
    let idx = idx.unwrap_or(0);
    ensure!(
        -len <= idx && idx < len,
        "start index of {idx} is out of bounds for string of length {len}"
    );
    let idx = if idx < 0 { len + idx } else { idx };
    Ok(idx as usize)
}

fn check_upper_bounds(idx: Option<i64>, len: usize) -> Result<usize> {
    let len = len as i64;
    let idx = idx.unwrap_or(len);
    ensure!(
        -len <= idx && idx <= len,
        "end index of {idx} is out of bounds for string of length {len}"
    );
    let idx = if idx < 0 { len + idx } else { idx };
    Ok(idx as usize)
}

#[defun]
pub(crate) fn read_from_string<'ob>(
    string: &str,
    start: Option<i64>,
    end: Option<i64>,
    arena: &'ob Arena,
) -> Result<Object<'ob>> {
    let len = string.len();
    let start = check_lower_bounds(start, len)?;
    let end = check_upper_bounds(end, len)?;

    let (obj, new_pos) = match reader::read(&string[start..end], arena) {
        Ok((obj, pos)) => (obj, pos),
        Err(mut e) => {
            e.update_pos(start);
            bail!(e);
        }
    };
    Ok(cons!(obj, new_pos as i64; arena))
}

pub(crate) fn load_internal<'ob>(
    contents: &str,
    arena: &'ob mut Arena,
    env: &mut Gc<Environment>,
) -> Result<bool> {
    let mut pos = 0;
    loop {
        let (obj, new_pos) = match reader::read(&contents[pos..], arena) {
            Ok((obj, pos)) => (obj, pos),
            Err(reader::Error::EmptyStream) => return Ok(true),
            Err(mut e) => {
                e.update_pos(pos);
                bail!(e);
            }
        };
        if crate::debug::debug_enabled() {
            let content = &contents[pos..(new_pos + pos)];
            println!("-----READ START-----\n {content}");
            println!("-----READ END-----");
        }
        root!(obj, arena);
        interpreter::eval(obj, None, env, arena)?;
        assert_ne!(new_pos, 0);
        pos += new_pos;
    }
}

#[defun]
pub(crate) fn load<'ob>(
    file: &str,
    noerror: Option<bool>,
    _nomessage: Option<bool>,
    _nosuffix: Option<bool>,
    _must_suffix: Option<bool>,
    arena: &'ob mut Arena,
    env: &mut Gc<Environment>,
) -> Result<bool> {
    match fs::read_to_string(file).with_context(|| format!("Couldn't open file {file}")) {
        Ok(content) => load_internal(&content, arena, env),
        Err(e) => match noerror {
            Some(_) => Ok(false),
            None => Err(e),
        },
    }
}

#[defun]
pub(crate) fn intern(string: &str) -> Symbol {
    crate::symbol::intern(string)
}

defsubr!(load, read_from_string, intern);

#[cfg(test)]
mod test {

    use super::*;
    use crate::arena::RootSet;

    #[test]
    #[allow(clippy::float_cmp)] // Bug in Clippy
    fn test_load() {
        let roots = &RootSet::default();
        let arena = &mut Arena::new(roots);
        let env = &mut unsafe { Gc::new(Environment::default()) };
        load_internal("(setq foo 1) (setq bar 2) (setq baz 1.5)", arena, env).unwrap();

        let obj = reader::read("(+ foo bar baz)", arena).unwrap().0;
        root!(obj, arena);
        let val = interpreter::eval(obj, None, env, arena).unwrap();
        assert_eq!(val, 4.5);
    }
}
