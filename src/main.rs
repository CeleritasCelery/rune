#![warn(rust_2018_idioms)]
#![allow(elided_lifetimes_in_paths)]
#![warn(
    trivial_casts,
    trivial_numeric_casts,
    clippy::trivially_copy_pass_by_ref,
    clippy::explicit_iter_loop,
    clippy::inefficient_to_string,
    clippy::missing_const_for_fn,
    clippy::needless_borrow,
    clippy::unicode_not_nfc
)]
#![deny(
    macro_use_extern_crate,
    keyword_idents,
    absolute_paths_not_starting_with_crate
)]
#![forbid(non_ascii_idents)]

// potential useful lints with too many false positives
// #![deny(unused_qualifications)]
// #![deny(meta_variable_misuse)]

#[macro_use]
mod macros;
#[macro_use]
mod object;
mod arena;
mod arith;
mod compile;
mod error;
mod eval;
mod forms;
mod hashmap;
mod intern;
mod lread;
mod opcode;
mod reader;

use crate::{
    arena::Arena,
    compile::Exp,
    eval::{Environment, Routine},
    object::LispFn,
    reader::Reader,
};
use std::io::{self, Write};

fn parens_closed(buffer: &str) -> bool {
    let open = buffer.chars().filter(|&x| x == '(').count();
    let close = buffer.chars().filter(|&x| x == ')').count();
    open <= close
}

fn main() {
    println!("Hello, world!");
    let mut buffer = String::new();
    let stdin = io::stdin();
    let arena = Arena::new();
    let mut env = Environment::default();
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        stdin.read_line(&mut buffer).unwrap();
        if buffer == "exit\n" {
            std::process::exit(0);
        }
        if !parens_closed(&buffer) {
            continue;
        }
        let (obj, _) = match Reader::read(&buffer, &arena) {
            Ok(obj) => obj,
            Err(e) => {
                println!("Error: {}", e);
                buffer.clear();
                continue;
            }
        };
        let func: LispFn = match Exp::compile(obj) {
            Ok(obj) => obj.into(),
            Err(e) => {
                println!("Error: {}", e);
                buffer.clear();
                continue;
            }
        };
        match Routine::execute(&func, &mut env, &arena) {
            Ok(val) => println!("{}", val),
            Err(e) => println!("Error: {}", e),
        }
        buffer.clear();
    }
}
