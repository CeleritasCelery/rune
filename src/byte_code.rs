#![allow(dead_code)]

use crate::lispobject::{LispObj, LispFn};
use std::mem::transmute;
use crate::arith;

#[repr(u8)]
enum OpCode {
    StackRef1 = 0,
    StackRef2,
    StackRef3,
    StackRef4,
    StackRef5,
    StackRef6,
    Constant,
    Add,
    Sub,
    Mul,
    Div,
    Call0,
    Call1,
    Call2,
    Call3,
    Call4,
    Call5,
    CallN,
    CallN2,
    CallSym0,
    CallSym1,
    CallSym2,
    CallSym3,
    CallSym4,
    CallSym5,
    CallSymN,
    CallSymN2,
    Ret,
    Unknown,
}

impl Into<u8> for OpCode {
    fn into(self: Self) -> u8 {
        self as u8
    }
}

impl OpCode {
    unsafe fn from_unchecked(x: &u8) -> Self {
        transmute(*x)
    }
}

type Stack = Vec<LispObj>;

pub fn execute(code: &Vec<u8>, stack: &mut Stack, functions: &Vec<LispFn>) {
    let mut code_iter = code.iter();

    use OpCode as op;
    while let Some(op_code) = code_iter.next() {
        match unsafe {op::from_unchecked(op_code)} {
            op::StackRef1 => {
                let val = stack.get(stack.len() - 1).unwrap().clone();
                stack.push(val);
            }
            op::StackRef2 => {
                let val = stack.get(stack.len() - 2).unwrap().clone();
                stack.push(val);
            }
            op::StackRef3 => {
                let val = stack.get(stack.len() - 3).unwrap().clone();
                stack.push(val);
            }
            op::Add => {
                let i = stack.len() - 2;
                stack[i] = arith::add(&stack[i..]);
                stack.truncate(i + 1);
            }
            op::Sub => {
                let i = stack.len() - 2;
                stack[i] = arith::sub(&stack[i..]);
                stack.truncate(i + 1);
            }
            op::Mul => {
                let i = stack.len() - 2;
                stack[i] = arith::mul(&stack[i..]);
                stack.truncate(i + 1);
            }
            op::Div => {
                let i = stack.len() - 2;
                stack[i] = arith::div(&stack[i..]);
                stack.truncate(i + 1);
            }
            op::Call0 => {
                let op1 = stack.pop().unwrap();
                call(op1.as_int().unwrap() as usize, &[], stack, functions);
            }
            op::Ret => {
                return;
            }
            x => {
                panic!("unknown OpCode {}", x as u8);
            }
        }
    }
}

fn call(fn_idx: usize, args: &[LispObj], stack: &mut Stack, functions: &Vec<LispFn>) {
    let func = &functions[fn_idx];
    if (args.len() as u16) < func.required_args  {
        panic!("missing required args");
    }
    execute(&func.op_codes, stack, functions);
}

pub fn run() {}

#[cfg(test)]
mod test {

    use super::*;
    use OpCode as Op;

    #[test]
    fn compute() {
        let mut stack = vec![
            LispObj::from(7),
            LispObj::from(13),
            LispObj::from(3),
            LispObj::from(0), // call function
        ];
        let mut functions = vec![LispFn{
            op_codes: vec![
                Op::StackRef3.into(),
                Op::StackRef3.into(),
                Op::StackRef3.into(),
                Op::Add.into(),
                Op::Add.into(),
                Op::Mul.into(),
                Op::Sub.into(),
                Op::Sub.into(),
                Op::Ret.into(),
            ],
            constants: vec![],
            rest_args: false,
            required_args: 0,
            optional_args: 0,
            max_stack_usage: 5,
        }];

        let code = LispFn{
            op_codes: vec![Op::Call0.into()],
            constants: vec![],
            rest_args: false,
            required_args: 0,
            optional_args: 0,
            max_stack_usage: 5,
        };

        execute(&code.op_codes, &mut stack, &mut functions);

        assert_eq!(63, stack.get(0).unwrap().as_int().unwrap());
    }
}
