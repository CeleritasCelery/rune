#![deny(macro_use_extern_crate, keyword_idents)]
#![forbid(non_ascii_idents)]
#![warn(rust_2018_idioms)]
// This lint makes code more verbose with little benefit
#![allow(elided_lifetimes_in_paths)]
#![warn(
    clippy::all,
    clippy::pedantic,
    clippy::as_ptr_cast_mut,
    clippy::equatable_if_let,
    clippy::nonstandard_macro_braces,
    clippy::or_fun_call,
    unused_qualifications,
    meta_variable_misuse,
    explicit_outlives_requirements,
    missing_copy_implementations,
    noop_method_call,
    semicolon_in_expressions_from_macros,
    trivial_numeric_casts,
    unreachable_pub,
    unused_lifetimes
)]
// Will enable this lint in the future
// #![warn(clippy::undocumented_unsafe_blocks)]
#![allow(
    clippy::unused_self,
    clippy::similar_names,
    clippy::module_name_repetitions,
    clippy::needless_pass_by_value,
    clippy::let_and_return,
    clippy::inline_always,
    clippy::match_bool,
    clippy::cast_precision_loss,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss,
    clippy::cast_ptr_alignment,
    clippy::single_match_else
)]

#[macro_use]
mod macros;
#[macro_use]
mod core;
#[macro_use]
mod debug;
mod alloc;
mod arith;
mod buffer;
mod bytecode;
mod character;
mod data;
mod editfns;
mod emacs;
mod eval;
mod fileio;
mod floatfns;
mod fns;
mod gui;
mod hashmap;
mod interpreter;
mod keymap;
mod lread;
mod print;
mod reader;
mod search;
mod threads;

use crate::core::{
    env::{intern, Env},
    gc::{Context, RootSet, Rt},
};
use std::io::{self, Write};

fn parens_closed(buffer: &str) -> bool {
    let open = buffer.chars().filter(|&x| x == '(').count();
    let close = buffer.chars().filter(|&x| x == ')').count();
    open <= close
}

fn repl(env: &mut Rt<Env>, cx: &mut Context) {
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
        let (obj, _) = match reader::read(&buffer, cx) {
            Ok(obj) => obj,
            Err(e) => {
                println!("Error: {e}");
                buffer.clear();
                continue;
            }
        };

        root!(obj, cx);
        match interpreter::eval(obj, None, env, cx) {
            Ok(val) => println!("{val}"),
            Err(e) => println!("Error: {e}"),
        }
        buffer.clear();
    }
}

fn load(env: &mut Rt<Env>, cx: &mut Context) {
    crate::core::env::init_variables(cx, env);
    crate::data::defalias(
        intern("not", cx),
        (crate::core::env::sym::NULL).into(),
        None,
    )
    .expect("null should be defined");
    buffer::get_buffer_create(cx.add("*scratch*"), core::object::nil(), cx).unwrap();

    let buffer = String::from(r#"(load "lisp/bootstrap.el")"#);

    match crate::lread::load_internal(&buffer, cx, env) {
        Ok(val) => println!("{val}"),
        Err(e) => println!("Error: {e}"),
    }
}

#[derive(Default)]
struct Args {
    load: bool,
    repl: bool,
    gui: bool,
}

impl Args {
    fn empty(&self) -> bool {
        !self.load && !self.repl && !self.gui
    }

    fn parse() -> Self {
        let mut args = Args::default();
        for arg in std::env::args() {
            match arg.as_str() {
                "--repl" => args.repl = true,
                "--load" => args.load = true,
                "--gui" => args.gui = true,
                x => println!("unknown arg: {x}"),
            }
        }
        if args.empty() {
            args.load = true;
        }
        args
    }
}

fn main() {
    let roots = &RootSet::default();
    let cx = &mut Context::new(roots);
    root!(env, Env::default(), cx);

    let args = Args::parse();

    // Ensure this is always initalized before anything else. We need this to
    // code to run to properly initialize the symbol table.
    lazy_static::initialize(&crate::core::env::INTERNED_SYMBOLS);

    if args.load {
        load(env, cx);
    }

    if args.repl {
        repl(env, cx);
    }

    if args.gui {
        gui::launch();
    }
}
