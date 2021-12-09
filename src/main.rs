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
    clippy::todo,
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
    clippy::box_collection,
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
mod bytecode;
mod compile;
mod data;
mod debug;
mod error;
mod eval;
mod fns;
mod hashmap;
mod interpreter;
mod keymap;
mod lread;
mod opcode;
mod reader;
mod search;
mod symbol;

use arena::Arena;
use data::Environment;
use object::Object;
use std::env;
use std::io::{self, Write};
use symbol::intern;

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
        if buffer.trim() == "exit" {
            return;
        }
        if buffer.trim().is_empty() {
            continue;
        }
        if !parens_closed(&buffer) {
            continue;
        }
        let (obj, _) = match reader::read(&buffer, arena) {
            Ok(obj) => obj,
            Err(e) => {
                println!("Error: {}", e);
                buffer.clear();
                continue;
            }
        };

        match interpreter::eval(obj, None, env, arena) {
            Ok(val) => println!("{}", val),
            Err(e) => println!("Error: {}", e),
        }
        buffer.clear();
    }
}

fn load<'ob>(env: &mut Environment<'ob>, arena: &'ob Arena) {
    use crate::symbol::sym;
    env.vars.insert(&sym::EMACS_VERSION, arena.add("28.1"));
    env.vars.insert(&sym::LEXICAL_BINDING, Object::TRUE);
    env.vars.insert(&sym::SYSTEM_TYPE, arena.add("gnu/linux"));
    env.vars.insert(&sym::MINIBUFFER_LOCAL_MAP, Object::NIL);
    crate::data::defalias(intern("not"), (&sym::NULL).into(), None)
        .expect("null should be defined");

    let mut buffer = String::from(
        r#"
(progn 
    (load "lisp/byte-run.el")
    (load "lisp/backquote.el")
)"#,
    );


    match crate::lread::load_internal(&buffer, arena, env) {
        Ok(val) => println!("{}", val),
        Err(e) => {
            println!("Error: {}", e);
            return;
        }
    }

    // crate::debug::enable_debug();

    buffer = String::from(
        r#"
(progn 
    (load "lisp/subr.el")
)"#,
    );

    // (load "lisp/macroexp.el")
    // (load "lisp/cl-lib.el")
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
