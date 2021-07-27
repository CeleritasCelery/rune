#![warn(rust_2018_idioms)]
#![warn(clippy::all, clippy::pedantic, clippy::restriction)]
#![warn(
    unused_qualifications,
    meta_variable_misuse,
    absolute_paths_not_starting_with_crate
)]
#![allow(
    elided_lifetimes_in_paths,
    clippy::blanket_clippy_restriction_lints,
    clippy::print_stdout,
    clippy::as_conversions,
    clippy::similar_names,
    clippy::missing_docs_in_private_items,
    clippy::implicit_return,
    clippy::inline_always,
    clippy::wildcard_enum_match_arm,
    clippy::unreachable,
    clippy::integer_arithmetic,
    clippy::pattern_type_mismatch,
    clippy::indexing_slicing,
    clippy::cast_precision_loss,
    clippy::float_arithmetic,
    clippy::default_numeric_fallback,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss,
    clippy::cast_ptr_alignment,
    clippy::unwrap_in_result,
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::single_match_else,
    clippy::panic
)]
#![deny(
    macro_use_extern_crate,
    keyword_idents,
    absolute_paths_not_starting_with_crate
)]
#![forbid(non_ascii_idents)]

#[macro_use]
mod macros;
mod object;
#[macro_use]
mod cons;
mod arena;
mod arith;
mod compile;
mod data;
mod error;
mod eval;
mod forms;
mod hashmap;
mod lread;
mod opcode;
mod reader;
mod symbol;

use crate::arena::Arena;
use crate::compile::compile;
use crate::data::Environment;
use crate::eval::Routine;
use crate::reader::Reader;
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
    let arena = &Arena::new();
    let env = &mut Environment::default();
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        stdin.read_line(&mut buffer).unwrap();
        if buffer == "exit\n" {
            return;
        }
        if !parens_closed(&buffer) {
            continue;
        }
        let (obj, _) = match Reader::read(&buffer, arena) {
            Ok(obj) => obj,
            Err(e) => {
                println!("Error: {}", e);
                buffer.clear();
                continue;
            }
        };
        let func = match compile(obj, arena) {
            Ok(obj) => obj,
            Err(e) => {
                println!("Error: {}", e);
                buffer.clear();
                continue;
            }
        };
        match Routine::execute(&func, env, arena) {
            Ok(val) => println!("{}", val),
            Err(e) => println!("Error: {}", e),
        }
        buffer.clear();
    }

    // let mut buffer = String::from("(load \"/home/foco/remac/test/byte-run.el\")");
    // match crate::lread::read_from_string(&buffer, arena, env) {
    //     Ok(val) => println!("{}", val),
    //     Err(e) => println!("Error: {}", e),
    // }
}
