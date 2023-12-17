#[cfg(all(not(target_env = "msvc"), not(miri)))]
use tikv_jemallocator::Jemalloc;

#[cfg(all(not(target_env = "msvc"), not(miri)))]
#[global_allocator]
#[doc(hidden)]
static GLOBAL: Jemalloc = Jemalloc;

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
mod editfns;
mod emacs;
mod eval;
mod fileio;
mod floatfns;
mod fns;
mod gui;
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
    error::EvalError,
    gc::{Context, RootSet, Rt},
    object::{nil, Gc, LispString},
};
use rune_core::macros::root;
use std::io::{self, Write};

fn main() {
    let roots = &RootSet::default();
    let cx = &mut Context::new(roots);
    root!(env, Env::default(), cx);

    let args = Args::parse();

    sym::init_symbols();
    crate::core::env::init_variables(cx, env);
    crate::data::defalias(intern("not", cx), (sym::NULL).into(), None)
        .expect("null should be defined");

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

fn load(env: &mut Rt<Env>, cx: &mut Context) {
    buffer::get_buffer_create(cx.add("*scratch*"), Some(nil()), cx).unwrap();
    let bootstrap: Gc<&LispString> = cx.add_as("lisp/bootstrap.el");
    root!(bootstrap, cx);
    match crate::lread::load(bootstrap, None, None, cx, env) {
        Ok(val) => print!("{val}"),
        Err(e) => {
            print!("Error: {e}");
            if let Ok(e) = e.downcast::<EvalError>() {
                e.print_backtrace();
            }
        }
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
