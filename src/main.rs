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
    clippy::blanket_clippy_restriction_lints,
    clippy::print_stdout,
    clippy::as_conversions,
    clippy::similar_names,
    clippy::shadow_reuse,
    clippy::shadow_same,
    clippy::shadow_unrelated,
    clippy::missing_docs_in_private_items,
    clippy::needless_pass_by_value,
    clippy::implicit_return,
    clippy::let_and_return,
    clippy::inline_always,
    clippy::wildcard_enum_match_arm,
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
mod object;
#[macro_use]
mod cons;
mod alloc;
mod arena;
mod arith;
mod data;
mod debug;
mod editfns;
mod error;
mod eval;
mod fns;
mod hashmap;
mod interpreter;
mod keymap;
mod lcell;
mod lread;
mod opcode;
mod reader;
mod search;
mod symbol;

use arena::{Arena, GcCell, RootSet};
use data::Environment;
use lcell::LCellOwner;
use object::Object;
use std::env;
use std::io::{self, Write};
use symbol::intern;

fn parens_closed(buffer: &str) -> bool {
    let open = buffer.chars().filter(|&x| x == '(').count();
    let close = buffer.chars().filter(|&x| x == ')').count();
    open <= close
}

fn repl<'id>(env: &GcCell<'id, Environment>, owner: &mut LCellOwner<'id>, arena: &mut Arena) {
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
        match interpreter::eval(obj, None, env, arena, owner) {
            Ok(val) => println!("{val}"),
            Err(e) => println!("Error: {e}"),
        }
        buffer.clear();
    }
}

fn load<'id>(env: &GcCell<'id, Environment>, owner: &mut LCellOwner<'id>, arena: &mut Arena) {
    use crate::symbol::sym;
    {
        let env = env.borrow_mut(owner, arena);
        Environment::set_var(env, &sym::EMACS_VERSION, arena.add("28.1"));
        Environment::set_var(env, &sym::LEXICAL_BINDING, Object::TRUE);
        Environment::set_var(env, &sym::SYSTEM_TYPE, arena.add("gnu/linux"));
        Environment::set_var(env, &sym::MINIBUFFER_LOCAL_MAP, Object::NIL);
        Environment::set_var(env, &sym::CURRENT_LOAD_LIST, Object::NIL);
    }
    crate::data::defalias(intern("not"), (&sym::NULL).into(), None)
        .expect("null should be defined");

    let mut buffer = String::from(
        r#"
(progn 
    (load "lisp/byte-run.el")
    (load "lisp/backquote.el")
    (load "lisp/subr.el")
    (load "lisp/pcase.el")
)"#,
    );

    match crate::lread::load_internal(&buffer, arena, env, owner) {
        Ok(val) => println!("{val}"),
        Err(e) => {
            println!("Error: {e}");
            return;
        }
    }

    crate::debug::enable_debug();

    buffer = String::from(
        r#"
(progn
    (load "lisp/gv.el")
)"#,
    );

    match crate::lread::load_internal(&buffer, arena, env, owner) {
        Ok(val) => println!("{val}"),
        Err(e) => println!("Error: {e}"),
    }
}

fn main() {
    let roots = &RootSet::default();
    let arena = &mut Arena::new(roots);
    root_struct!(env, Environment::default(), arena);
    make_lcell_owner!(owner);

    match env::args().nth(1) {
        Some(arg) if arg == "--repl" => repl(env, &mut owner, arena),
        Some(arg) if arg == "--load" => load(env, &mut owner, arena),
        Some(arg) => panic!("unknown arg: {arg}"),
        None => {
            load(env, &mut owner, arena);
            repl(env, &mut owner, arena);
        }
    }
}
