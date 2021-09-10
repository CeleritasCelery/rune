use crate::arena::Arena;
use crate::compile::compile;
use crate::data::Environment;
use crate::eval::Routine;
use crate::reader::{Error, Reader};
use crate::symbol::Symbol;
use fn_macros::defun;

use anyhow::{anyhow, Result};

use std::fs;

#[allow(clippy::panic_in_result_fn)]
pub(crate) fn read_from_string<'ob>(
    contents: &str,
    arena: &'ob Arena,
    env: &mut Environment<'ob>,
) -> Result<bool> {
    let mut pos = 0;
    loop {
        let (obj, new_pos) = match Reader::read(&contents[pos..], arena) {
            Ok((obj, pos)) => (obj, pos),
            Err(Error::EmptyStream) => return Ok(true),
            Err(e) => return Err(anyhow!(e)),
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
    read_from_string(&file_contents, arena, env)
}

#[defun]
pub(crate) fn intern(string: &str) -> Symbol {
    crate::symbol::intern(string)
}

defsubr!(load, intern);

#[cfg(test)]
mod test {

    use super::*;
    use crate::object::IntoObject;

    #[test]
    fn test_load() {
        let arena = &Arena::new();
        let env = &mut Environment::default();
        read_from_string("(setq foo 1) (setq bar 2) (setq baz 1.5)", arena, env).unwrap();
        println!("{:?}", env);
        println!("{:?}", arena);

        let obj = Reader::read("(+ foo bar baz)", arena).unwrap().0;
        let func = compile(obj, env, arena).unwrap();
        let val = Routine::execute(&func, env, arena).unwrap();
        assert_eq!(val, 4.5.into_obj(arena));
    }
}
