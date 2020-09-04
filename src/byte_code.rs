#![allow(dead_code)]

use crate::lispobject::{LispObj, LispFn};
use std::mem::transmute;
use crate::arith;

#[repr(u8)]
enum OpCode {
    StackRef = 0,
    Constant,
    Add,
    Sub,
    Mul,
    Div,
    Call,
    Unknown,
}

impl OpCode {
    unsafe fn from(x: &u8) -> Self {
         transmute(*x)
    }
}

struct LispState {
    stack: Vec<LispObj>,
    functions: Vec<LispFn>,
    variables: Vec<LispObj>,
}

fn execute(code: &Vec<u8>, stack: &mut Vec<LispObj>) {
    let mut code_iter = code.iter();

    use OpCode as op;
    while let Some(op_code) = code_iter.next() {
        match unsafe {op::from(op_code)} {
            op::StackRef => {
                let offset = *code_iter.next().unwrap() as usize;
                let val = stack.get(stack.len() - offset - 1).unwrap().clone();
                stack.push(val);
            }
            op::Add => {
                let op1 = stack.pop().unwrap();
                let op2 = stack.pop().unwrap();
                stack.push(arith::add(&op1, &op2));
            }
            op::Sub => {
                let op1 = stack.pop().unwrap();
                let op2 = stack.pop().unwrap();
                stack.push(arith::sub(&op1, &op2));
            }
            op::Mul => {
                let op1 = stack.pop().unwrap();
                let op2 = stack.pop().unwrap();
                stack.push(arith::mul(&op1, &op2));
            }
            op::Div => {
                let op1 = stack.pop().unwrap();
                let op2 = stack.pop().unwrap();
                stack.push(arith::div(&op1, &op2));
            }
            _ => {
                panic!("unknown OpCode");
            }
        }
    }

}

pub fn run() {
    let mut stack = vec![LispObj::from(4), LispObj::from(16)];
    let code: Vec<u8> = vec![0, 0, 0, 2, 2, 4, 3];
    execute(&code, &mut stack);

    for x in stack {
        println!("{}", x.as_int().unwrap());
    }


}
