
#[macro_use]
mod macros;
#[macro_use]
mod lisp_object;
mod reader;
mod compile;
mod arith;
mod eval;
mod intern;
mod hashmap;
mod gc;
mod error;
mod func;
#[macro_use]
extern crate fn_macros;

fn main() {
    compile::run();
}
