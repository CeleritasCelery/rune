
mod lisp_object;
mod lex;
mod arith;
mod byte_code;
mod symbol;
mod gc;

fn main() {
    byte_code::run();
}
