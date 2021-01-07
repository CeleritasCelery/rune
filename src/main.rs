
#[macro_use]
mod macros;
#[macro_use]
mod lisp_object;
mod reader;
mod compile;
mod arith;
mod eval;
mod symbol;
mod hashmap;
mod gc;
mod error;

fn main() {
    compile::run();
}
