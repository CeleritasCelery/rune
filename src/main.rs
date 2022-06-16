#![deny(macro_use_extern_crate, keyword_idents)]
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
    trivial_numeric_casts,
    unreachable_pub,
    unused_crate_dependencies,
    unused_lifetimes
)]
// Enable all clippy lints, and then selectivly disable the ones we don't want
#![warn(clippy::all, clippy::pedantic, clippy::restriction)]
#![allow(
    clippy::undocumented_unsafe_blocks, // Will enable this later
    clippy::todo,
    clippy::unimplemented,
    clippy::blanket_clippy_restriction_lints,
    clippy::print_stdout,
    clippy::as_conversions,
    clippy::similar_names,
    clippy::shadow_reuse,
    clippy::shadow_same,
    clippy::use_debug,
    clippy::single_char_lifetime_names,
    clippy::shadow_unrelated,
    clippy::missing_docs_in_private_items,
    clippy::needless_pass_by_value,
    clippy::implicit_return,
    clippy::let_and_return,
    clippy::inline_always,
    clippy::wildcard_enum_match_arm,
    clippy::match_bool,
    clippy::string_slice,
    clippy::separated_literal_suffix,
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
    clippy::mem_forget,
    clippy::unwrap_in_result,
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::single_match_else,
    clippy::panic
)]

#[macro_use]
mod macros;
#[macro_use]
mod core;
mod alloc;
mod arith;
mod buffer;
mod data;
mod debug;
mod editfns;
mod eval;
mod fileio;
mod fns;
mod hashmap;
mod interpreter;
mod keymap;
mod lread;
mod opcode;
mod reader;
mod search;

use crate::core::{
    arena::{Arena, Root, RootSet},
    env::{intern, Environment},
    object::GcObj,
};
use std::env;
use std::io::{self, Write};

fn parens_closed(buffer: &str) -> bool {
    let open = buffer.chars().filter(|&x| x == '(').count();
    let close = buffer.chars().filter(|&x| x == ')').count();
    open <= close
}

fn repl(env: &mut Root<Environment>, arena: &mut Arena) {
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
                println!("Error: {e}");
                buffer.clear();
                continue;
            }
        };

        root!(obj, arena);
        match interpreter::eval(obj, None, env, arena) {
            Ok(val) => println!("{val}"),
            Err(e) => println!("Error: {e}"),
        }
        buffer.clear();
    }
}

fn load(env: &mut Root<Environment>, arena: &mut Arena) {
    use crate::core::env::sym;
    {
        let env = env.deref_mut(arena);
        Environment::set_var(env, &sym::EMACS_VERSION, arena.add("28.1"));
        Environment::set_var(env, &sym::LEXICAL_BINDING, GcObj::TRUE);
        Environment::set_var(env, &sym::SYSTEM_TYPE, arena.add("gnu/linux"));
        Environment::set_var(env, &sym::MINIBUFFER_LOCAL_MAP, GcObj::NIL);
        Environment::set_var(env, &sym::CURRENT_LOAD_LIST, GcObj::NIL);
        Environment::set_var(env, &sym::LOAD_PATH, cons!("lisp"; arena));
        Environment::set_var(env, &sym::DUMP_MODE, GcObj::NIL);
        Environment::set_var(env, &sym::COMMAND_LINE_ARGS, cons!(""; arena));
        Environment::set_var(env, &sym::LOAD_HISTORY, GcObj::NIL);
    }
    crate::data::defalias(intern("not"), (&sym::NULL).into(), None)
        .expect("null should be defined");

    let buffer = String::from(r#"(load "lisp/bootstrap.el")"#);

    match crate::lread::load_internal(&buffer, arena, env) {
        Ok(val) => println!("{val}"),
        Err(e) => println!("Error: {e}"),
    }
}

fn main() {
    let roots = &RootSet::default();
    let arena = &mut Arena::new(roots);
    root!(env, Environment::default(), arena);
    let mut arg_load = false;
    let mut arg_repl = false;

    for arg in env::args() {
        match arg.as_str() {
            "--repl" => arg_repl = true,
            "--load" => arg_load = true,
            x => println!("unknown arg: {x}"),
        }
    }

    if !arg_load && !arg_repl {
        arg_load = true;
    }

    if arg_load {
        load(env, arena);
    }

    if arg_repl {
        repl(env, arena);
    }
}
