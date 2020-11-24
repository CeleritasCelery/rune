
#[macro_use]
mod macros;
mod lisp_object;
mod reader;
mod compile;
mod arith;
mod eval;
mod symbol;
mod gc;

fn main() {
    compile::run();
}
