use crate::arena::Arena;
use crate::compile::Exp;
use crate::data::Environment;
use crate::eval::Routine;
use crate::reader::{Error, Reader};
use fn_macros::lisp_fn;

use anyhow::{anyhow, Result};

use std::fs;

fn read_from_string<'obj>(
    contents: &str,
    arena: &'obj Arena,
    env: &mut Environment<'obj>,
) -> Result<bool> {
    let mut pos = 0;
    loop {
        let (obj, new_pos) = match Reader::read(&contents[pos..], arena) {
            Ok((obj, pos)) => (obj, pos),
            Err(Error::EmptyStream) => return Ok(true),
            Err(e) => return Err(anyhow!(e)),
        };
        let func = Exp::compile(obj)?.into();
        Routine::execute(&func, env, arena)?;
        assert_ne!(new_pos, 0);
        println!("read: {}", &contents[pos..(new_pos + pos)]);
        pos += new_pos;
    }
}

#[lisp_fn]
#[allow(clippy::ptr_arg)]
fn load<'obj>(file: &String, arena: &'obj Arena, env: &mut Environment<'obj>) -> Result<bool> {
    let file_contents = fs::read_to_string(file)?;
    read_from_string(&file_contents, arena, env)
}

defsubr!(load);

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn test_load() {
        let arena = &Arena::new();
        let env = &mut Environment::default();
        read_from_string("(setq foo 1) (setq bar 2) (setq baz 1.5)", arena, env).unwrap();
        println!("{:?}", env);
        println!("{:?}", arena);

        let obj = Reader::read("(+ foo bar baz)", arena).unwrap().0;
        let func = Exp::compile(obj).unwrap().into();
        let val = Routine::execute(&func, env, arena).unwrap();
        assert_eq!(val, arena.add(4.5));
    }
}
