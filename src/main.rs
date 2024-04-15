#[cfg(all(not(target_env = "msvc"), not(miri)))]
#[global_allocator]
#[doc(hidden)]
static GLOBAL: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

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
mod casefiddle;
mod character;
mod data;
mod dired;
mod editfns;
mod emacs;
mod eval;
mod fileio;
mod floatfns;
mod fns;
mod interpreter;
mod keymap;
mod lread;
mod print;
mod reader;
mod search;
mod threads;
mod timefns;

use crate::core::{
    env::{intern, sym, Env},
    gc::{Context, RootSet, Rt},
    object::{Gc, LispString, NIL},
};
use crate::eval::EvalError;
use clap::Parser;
use rune_core::macros::root;
use std::io::{self, Write};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short, long, value_name = "FILE")]
    load: Vec<String>,
    #[arg(short, long)]
    repl: bool,
    #[arg(short, long)]
    no_boostrap: bool,
}

fn main() {
    let args = Args::parse();

    let roots = &RootSet::default();
    let cx = &mut Context::new(roots);
    root!(env, new(Env), cx);

    sym::init_symbols();
    crate::core::env::init_variables(cx, env);
    crate::data::defalias(intern("not", cx), (sym::NULL).into(), None)
        .expect("null should be defined");

    if !args.no_boostrap {
        bootstrap(env, cx);
    }

    for file in args.load {
        load(&file, cx, env);
    }

    if args.repl {
        repl(env, cx);
    }
}

fn parens_closed(buffer: &str) -> bool {
    let open = buffer.chars().filter(|&x| x == '(').count();
    let close = buffer.chars().filter(|&x| x == ')').count();
    open <= close
}

fn repl(env: &mut Rt<Env>, cx: &mut Context) {
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
            Err(e) => {
                print!("Error: {e}");
                if let Ok(e) = e.downcast::<EvalError>() {
                    e.print_backtrace();
                }
            }
        }
        buffer.clear();
    }
}

fn load(file: &str, cx: &mut Context, env: &mut Rt<Env>) {
    let file: Gc<&LispString> = cx.add_as(file);
    root!(file, cx);
    match crate::lread::load(file, None, None, cx, env) {
        Ok(val) => print!("{val}"),
        Err(e) => {
            print!("Error: {e}");
            if let Ok(e) = e.downcast::<EvalError>() {
                e.print_backtrace();
            }
        }
    }
}

fn bootstrap(env: &mut Rt<Env>, cx: &mut Context) {
    buffer::get_buffer_create(cx.add("*scratch*"), Some(NIL), cx).unwrap();
    load("bootstrap.el", cx, env);
}

#[test]
fn verify_cli() {
    use clap::CommandFactory;
    Args::command().debug_assert()
}
