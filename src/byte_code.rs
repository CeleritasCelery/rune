#![allow(dead_code)]

use crate::lisp_object::{LispObj, LispFn};
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
    StackRef7,
    StackRef8,
    StackRefN,
    StackRefN2,
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

trait LispStack {
    fn push_ref(&mut self, i: usize);
    fn ref_at(&self, i: usize) -> &LispObj;
}

impl LispStack for Stack {
    fn push_ref(&mut self, i: usize) {
        debug_assert!(i > 0);
        debug_assert!(i <= self.len());
        let val = self.get(self.len() - i).unwrap().clone();
        self.push(val);
    }

    fn ref_at(&self, i: usize) -> &LispObj {
        self.get(self.len() - i).unwrap()
    }
}

pub fn execute(code: &Vec<u8>, stack: &mut Stack, functions: &Vec<LispFn>) {
    let mut code_iter = code.iter();

    use OpCode as op;
    while let Some(op_code) = code_iter.next() {
        match unsafe {op::from_unchecked(op_code)} {
            op::StackRef1 => {stack.push_ref(1);}
            op::StackRef2 => {stack.push_ref(2);}
            op::StackRef3 => {stack.push_ref(3);}
            op::StackRef4 => {stack.push_ref(4);}
            op::StackRef5 => {stack.push_ref(5);}
            op::StackRef6 => {stack.push_ref(6);}
            op::StackRef7 => {stack.push_ref(7);}
            op::StackRef8 => {stack.push_ref(8);}
            op::StackRefN => {
                let index = code_iter.next().unwrap();
                stack.push_ref(*index as usize);
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
            op::Call0 => {call(0, stack, functions);}
            op::Call1 => {call(1, stack, functions);}
            op::Call2 => {call(2, stack, functions);}
            op::Call3 => {call(3, stack, functions);}
            op::Ret => {return;}
            x => {
                panic!("unknown OpCode {}", x as u8);
            }
        }
    }
}

fn call(arg_cnt: u16, stack: &mut Stack, functions: &Vec<LispFn>) {
    let stack_offset = stack.len() - 1 - arg_cnt as usize;
    let idx = stack.get(stack_offset).unwrap().as_int().unwrap();
    let func = &functions[idx as usize];
    if arg_cnt < func.required_args  {
        panic!("Function {} called with {} arguments which is less then the required {}",
               func.name,
               arg_cnt,
               func.required_args,
        );
    }
    let total_args = func.required_args + func.optional_args;
    if !func.rest_args && (arg_cnt > total_args) {
        panic!("Function {} called with {} arguments which is more then the aloud {}",
               func.name,
               arg_cnt,
               total_args,
        );
    }
    execute(&func.op_codes, stack, functions);
    let ret_val = stack.pop().unwrap();
    stack[stack_offset] = ret_val;

}

pub fn run() {}

#[cfg(test)]
mod test {

    use super::*;
    use OpCode as Op;

    #[test]
    fn compute() {
        let mut stack = vec![
            LispObj::from(0), // fn index
            LispObj::from(7),
            LispObj::from(13),
            LispObj::from(3),
        ];
        let mut functions = vec![LispFn{
            name: "Test-Add".into(),
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
            required_args: 3,
            optional_args: 0,
            max_stack_usage: 5,
        }];

        let code = LispFn{
            name: "main-loop".into(),
            op_codes: vec![Op::Call3.into()],
            constants: vec![],
            rest_args: false,
            required_args: 0,
            optional_args: 0,
            max_stack_usage: 5,
        };

        execute(&code.op_codes, &mut stack, &mut functions);
        assert_eq!(63, stack.get(0).unwrap().as_int().unwrap());
    }

    // #[test]
    // #[should_panic]
    // fn too_few_args() {

    // }
}
