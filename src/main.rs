
mod lisp_object;
mod lex;
mod parser;
mod arith;
mod byte_code;
mod symbol;
mod gc;

fn main() {
    parser::run();
}
