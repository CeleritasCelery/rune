use crate::arena::Arena;
use crate::compile::compile;
use crate::data::Environment;
use crate::eval::Routine;
use crate::object::Object;
use crate::reader::{Error, Reader};
use crate::symbol::Symbol;
use fn_macros::defun;

use anyhow::{bail, ensure, Result};

use std::fs;

fn check_lower_bounds(idx: Option<i64>, len: usize) -> Result<usize> {
    let len = len as i64;
    let idx = idx.unwrap_or(0);
    ensure!(
        -len <= idx && idx < len,
        "start index of {} is out of bounds for string of length {}",
        idx,
        len
    );
    let idx = if idx < 0 { len + idx } else { idx };
    Ok(idx as usize)
}

fn check_upper_bounds(idx: Option<i64>, len: usize) -> Result<usize> {
    let len = len as i64;
    let idx = idx.unwrap_or(len);
    ensure!(
        -len <= idx && idx <= len,
        "end index of {} is out of bounds for string of length {}",
        idx,
        len
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

    let (obj, new_pos) = match Reader::read(&string[start..end], arena) {
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
    arena: &'ob Arena,
    env: &mut Environment<'ob>,
) -> Result<bool> {
    let mut pos = 0;
    loop {
        let (obj, new_pos) = match Reader::read(&contents[pos..], arena) {
            Ok((obj, pos)) => (obj, pos),
            Err(Error::EmptyStream) => return Ok(true),
            Err(mut e) => {
                e.update_pos(pos);
                bail!(e);
            }
        };
        println!("-----READ START-----\n {}", &contents[pos..(new_pos + pos)]);
        println!("-----READ END-----");
        println!("-----compiling-----");
        // this will go out of scope
        let exp = compile(obj, env, arena)?;
        println!("-----running-----");
        Routine::execute(&exp, env, arena)?;
        println!("-----run complete-----");
        assert_ne!(new_pos, 0);
        pos += new_pos;
    }
}

#[defun]
pub(crate) fn load<'ob>(file: &str, arena: &'ob Arena, env: &mut Environment<'ob>) -> Result<bool> {
    let file_contents = fs::read_to_string(file)?;
    load_internal(&file_contents, arena, env)
}

#[defun]
pub(crate) fn intern(string: &str) -> Symbol {
    crate::symbol::intern(string)
}

defsubr!(load, read_from_string, intern);

#[cfg(test)]
mod test {

    use super::*;
    use crate::object::IntoObject;

    #[test]
    fn test_load() {
        let arena = &Arena::new();
        let env = &mut Environment::default();
        load_internal("(setq foo 1) (setq bar 2) (setq baz 1.5)", arena, env).unwrap();
        println!("{:?}", env);
        println!("{:?}", arena);

        let obj = Reader::read("(+ foo bar baz)", arena).unwrap().0;
        let func = compile(obj, env, arena).unwrap();
        let val = Routine::execute(&func, env, arena).unwrap();
        assert_eq!(val, 4.5.into_obj(arena));
    }
}
