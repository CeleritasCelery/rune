use crate::arena::Arena;
use crate::compile::Exp;
use crate::eval::Environment;
use crate::eval::Routine;
use crate::reader::{Error, Reader};

use anyhow::{anyhow, Result};

use std::fs;

fn read_from_string(contents: &str, arena: &Arena, env: &mut Environment) -> Result<bool> {
    let mut pos = 0;
    loop {
        dbg!(pos);
        println!(">{}<", &contents[pos..]);
        let (obj, new_pos) = match Reader::read(&contents[pos..], arena) {
            Ok((obj, pos)) => (obj, pos),
            Err(Error::EmptyStream) => return Ok(true),
            Err(e) => return Err(anyhow!(e)),
        };
        dbg!(new_pos);
        let func = Exp::compile(obj)?.into();
        Routine::execute(&func, env, arena)?;
        assert_ne!(new_pos, 0);
        pos += new_pos;
    }
}

fn load(file: &str, arena: &Arena, env: &mut Environment) -> Result<bool> {
    let file_contents = fs::read_to_string(file)?;
    read_from_string(&file_contents, arena, env)
}

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
