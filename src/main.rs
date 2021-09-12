#![deny(
    macro_use_extern_crate,
    keyword_idents,
    absolute_paths_not_starting_with_crate
)]
#![forbid(non_ascii_idents)]
#![warn(rust_2018_idioms)]
// This lint makes code more verbose with little benefit
#![allow(elided_lifetimes_in_paths)]
#![warn(
    unused_qualifications,
    meta_variable_misuse,
    explicit_outlives_requirements,
    missing_copy_implementations,
    noop_method_call,
    pointer_structural_match,
    semicolon_in_expressions_from_macros,
    trivial_casts,
    trivial_numeric_casts,
    unreachable_pub,
    unused_crate_dependencies,
    unused_lifetimes
)]
// Enable all clippy lints, and then selectivly disable the ones we don't want
#![warn(clippy::all, clippy::pedantic, clippy::restriction)]
#![allow(
    clippy::blanket_clippy_restriction_lints,
    clippy::print_stdout,
    clippy::as_conversions,
    clippy::similar_names,
    clippy::shadow_reuse,
    clippy::missing_docs_in_private_items,
    clippy::needless_pass_by_value,
    clippy::implicit_return,
    clippy::let_and_return,
    clippy::inline_always,
    clippy::wildcard_enum_match_arm,
    clippy::unreachable,
    clippy::panic_in_result_fn,
    clippy::integer_arithmetic,
    clippy::pattern_type_mismatch,
    clippy::indexing_slicing,
    clippy::cast_precision_loss,
    clippy::float_arithmetic,
    clippy::default_numeric_fallback,
    clippy::box_vec,
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

#[macro_use]
mod macros;
mod object;
#[macro_use]
mod cons;
mod alloc;
mod arena;
mod arith;
mod compile;
mod data;
mod error;
mod eval;
mod fns;
mod forms;
mod hashmap;
mod keymap;
mod lread;
mod opcode;
mod reader;
mod symbol;

use crate::arena::Arena;
use crate::compile::compile;
use crate::data::Environment;
use crate::eval::Routine;
use crate::object::Object;
use crate::reader::Reader;
use std::env;
use std::io::{self, Write};

fn parens_closed(buffer: &str) -> bool {
    let open = buffer.chars().filter(|&x| x == '(').count();
    let close = buffer.chars().filter(|&x| x == ')').count();
    open <= close
}

fn repl<'ob>(env: &mut Environment<'ob>, arena: &'ob Arena) {
    println!("Hello, world!");
    let mut buffer = String::new();
    let stdin = io::stdin();
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
        let func = match compile(obj, env, arena) {
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
}

fn load<'ob>(env: &mut Environment<'ob>, arena: &'ob Arena) {
    env.vars
        .insert(crate::symbol::intern("lexical-binding"), Object::True);
    env.vars
        .insert(crate::symbol::intern("system-type"), arena.add("gnu/linux"));
    env.vars
        .insert(crate::symbol::intern("minibuffer-local-map"), Object::Nil);
    let buffer = String::from(
        r#"
(progn (load "/home/foco/remac/lisp/byte-run.el")
       (load "/home/foco/remac/lisp/backquote.el")
       (load "/home/foco/remac/lisp/byte-run.el")
       (load "/home/foco/remac/lisp/subr.el")
)"#,
    );
    match crate::lread::load_internal(&buffer, arena, env) {
        Ok(val) => println!("{}", val),
        Err(e) => println!("Error: {}", e),
    }
}

fn main() {
    let arena = &Arena::new();
    let env = &mut Environment::default();
    match env::args().nth(1) {
        Some(arg) if arg == "--repl" => repl(env, arena),
        Some(arg) if arg == "--load" => load(env, arena),
        Some(arg) => panic!("unknown arg: {}", arg),
        None => {
            load(env, arena);
            repl(env, arena);
        }
    }
}
