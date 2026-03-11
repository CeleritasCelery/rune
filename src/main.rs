#![allow(unsafe_op_in_unsafe_fn)]
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
mod channel_manager;
mod channels;
mod character;
mod chartab;
mod data;
mod dired;
mod editfns;
mod emacs;
mod eval;
mod fileio;
mod filelock;
mod floatfns;
mod fns;
mod interpreter;
mod intervals;
mod keymap;
mod library;
mod lisp;
mod lread;
mod print;
mod reader;
mod search;
mod textprops;
mod threads;
mod timefns;

use crate::core::{
    env::{Env, intern, sym},
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
    no_bootstrap: bool,
    #[arg(long)]
    eval_stdin: bool,
}

fn main() -> Result<(), ()> {
    let args = Args::parse();

    let roots = &RootSet::default();
    let cx = &mut Context::new(roots);
    root!(env, new(Env), cx);

    sym::init_symbols();
    crate::core::env::init_variables(cx, env);
    crate::data::defalias(intern("not", cx), (sym::NULL).into(), None)
        .expect("null should be defined");

    if args.eval_stdin {
        return eval_stdin(cx, env);
    }

    if !args.no_bootstrap {
        bootstrap(env, cx)?;
    }

    for file in args.load {
        load(&file, cx, env)?;
    }

    if args.repl {
        repl(env, cx);
    }
    Ok(())
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
                eprintln!("Error: {e}");
                buffer.clear();
                continue;
            }
        };

        root!(obj, cx);
        match interpreter::eval(obj, None, env, cx) {
            Ok(val) => println!("{val}"),
            Err(e) => {
                eprintln!("Error: {e}");
                if let Ok(e) = e.downcast::<EvalError>() {
                    e.print_backtrace();
                }
            }
        }
        buffer.clear();
    }
}

fn load(file: &str, cx: &mut Context, env: &mut Rt<Env>) -> Result<(), ()> {
    let file: Gc<&LispString> = cx.add_as(file);
    root!(file, cx);
    match crate::lread::load(file, None, None, cx, env) {
        Ok(val) => {
            println!("{val}");
            Ok(())
        }
        Err(e) => {
            eprintln!("Error: {e}");
            if let Ok(e) = e.downcast::<EvalError>() {
                e.print_backtrace();
            }
            Err(())
        }
    }
}

fn eval_stdin(cx: &mut Context, env: &mut Rt<Env>) -> Result<(), ()> {
    let mut buffer = String::new();
    let mut point = 0;
    let mut count = 0;
    loop {
        io::stdin().read_line(&mut buffer).unwrap();
        let obj = match reader::read(&buffer[point..], cx) {
            Ok((obj, offset)) => {
                point += offset;
                obj
            }
            Err(reader::Error::EmptyStream) => continue,
            Err(e) => {
                eprintln!("Error: {e}");
                break;
            }
        };

        root!(obj, cx);
        match interpreter::eval(obj, None, env, cx) {
            Ok(val) => println!("{val}\n"),
            Err(e) => println!("Error: {e}\n"),
        }
        count += 1;
        std::thread::sleep(std::time::Duration::from_millis(10));
        // timeout after ~1 minute
        if count > 6000 {
            break;
        }
    }
    Err(())
}

fn bootstrap(env: &mut Rt<Env>, cx: &mut Context) -> Result<(), ()> {
    buffer::get_buffer_create(cx.add("*scratch*"), Some(NIL), cx).unwrap();
    load("bootstrap.el", cx, env)
}

#[test]
fn verify_cli() {
    use clap::CommandFactory;
    Args::command().debug_assert()
}
