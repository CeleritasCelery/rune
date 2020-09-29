#![allow(dead_code)]

use crate::lisp_object::{LispObj, LispFn};
use std::mem::transmute;
use std::rc::Rc;
use crate::arith;

#[derive(Copy, Clone)]
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
    Constant0,
    Constant1,
    Constant2,
    Constant3,
    Constant4,
    Constant5,
    Constant6,
    ConstantN,
    ConstantN2,
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
    Ret,
    End,
    Unknown
}

impl Into<u8> for OpCode {
    fn into(self: Self) -> u8 {
        self as u8
    }
}

impl OpCode {
    unsafe fn from_unchecked(x: u8) -> Self {
        transmute(x)
    }
}

#[derive(Clone)]
struct CallFrame {
    pub ip: *const u8,
    pub func: Rc<LispFn>,
}

impl CallFrame {
    pub fn new(func: Rc<LispFn>) -> CallFrame {
        CallFrame{ip: func.op_codes.as_ptr(), func}
    }

    pub fn get_const(&self, i: usize) -> LispObj {
        self.func.constants.get(i).unwrap().clone()
    }

    fn next(&mut self) -> u8 {
        unsafe {
            let x = *self.ip;
            self.ip = self.ip.add(1);
            x
        }
    }

    fn take_arg(&mut self) -> usize {
        self.next() as usize
    }

    fn take_double_arg(&mut self) -> usize {
        let upper = self.next();
        let lower = self.next();
        (upper as usize) << 8 | lower as usize
    }
}

type Stack = Vec<LispObj>;

trait LispStack {
    fn from_end(&self, i: usize) -> usize;
    fn shrink(&mut self, i: usize);
    fn push_ref(&mut self, i: usize);
    fn ref_at(&self, i: usize) -> &LispObj;
    fn take_slice(&self, i: usize) -> &[LispObj];
}

impl LispStack for Vec<LispObj> {
    fn from_end(&self, i: usize) -> usize {
        debug_assert!(i > 0);
        debug_assert!(i <= self.len());
        self.len() - i
    }

    fn push_ref(&mut self, i: usize) {
        self.push(self.ref_at(i).clone());
    }

    fn ref_at(&self, i: usize) -> &LispObj {
        self.get(self.from_end(i)).unwrap()
    }

    fn shrink(&mut self, i: usize) {
        self.truncate(self.from_end(i));
    }

    fn take_slice(&self, i: usize) -> &[LispObj] {
        &self[self.from_end(i)..]
    }
}

pub struct Routine {
    stack: Vec<LispObj>,
    call_frames: Vec<CallFrame>,
}

impl Routine {

    fn new() -> Routine {
        let base_fn = Rc::new(LispFn::new(OpCode::End.into()));
        Routine{
            stack: Vec::new(),
            call_frames: vec![CallFrame::new(base_fn)],
        }
    }

    fn call(&mut self, arg_cnt: u16, functions: &Vec<Rc<LispFn>>) -> CallFrame {
        let idx = self.stack.ref_at(arg_cnt as usize).as_int().unwrap();
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
        let fill_args = total_args + if func.rest_args {1} else {0} - arg_cnt;
        if fill_args > 0 {
            for _ in 0..fill_args {
                self.stack.push(LispObj::nil());
            }
        }

        CallFrame::new(func.clone())
    }

    pub fn execute(&mut self, func: Rc<LispFn>, functions: & Vec<Rc<LispFn>>) {
        let mut frame = CallFrame::new(func);
        loop {
            use OpCode as op;
            match unsafe {op::from_unchecked(frame.next())} {
                op::StackRef1 => {self.stack.push_ref(1);}
                op::StackRef2 => {self.stack.push_ref(2);}
                op::StackRef3 => {self.stack.push_ref(3);}
                op::StackRef4 => {self.stack.push_ref(4);}
                op::StackRef5 => {self.stack.push_ref(5);}
                op::StackRef6 => {self.stack.push_ref(6);}
                op::StackRef7 => {self.stack.push_ref(7);}
                op::StackRef8 => {self.stack.push_ref(8);}
                op::StackRefN => {
                    let idx = frame.take_arg();
                    self.stack.push_ref(idx);
                }
                op::StackRefN2 => {
                    let idx = frame.take_double_arg();
                    self.stack.push_ref(idx);
                }
                op::Constant0 => {self.stack.push(frame.get_const(0))}
                op::Constant1 => {self.stack.push(frame.get_const(1))}
                op::Constant2 => {self.stack.push(frame.get_const(2))}
                op::Constant3 => {self.stack.push(frame.get_const(3))}
                op::Constant4 => {self.stack.push(frame.get_const(4))}
                op::Constant5 => {self.stack.push(frame.get_const(5))}
                op::Add => {
                    let i = self.stack.from_end(2);
                    self.stack[i] = arith::add(self.stack.take_slice(2));
                    self.stack.truncate(i + 1);
                }
                op::Sub => {
                    let i = self.stack.len() - 2;
                    self.stack[i] = arith::sub(&self.stack[i..]);
                    self.stack.truncate(i + 1);
                }
                op::Mul => {
                    let i = self.stack.len() - 2;
                    self.stack[i] = arith::mul(&self.stack[i..]);
                    self.stack.truncate(i + 1);
                }
                op::Div => {
                    let i = self.stack.len() - 2;
                    self.stack[i] = arith::div(&self.stack[i..]);
                    self.stack.truncate(i + 1);
                }
                op::Call0 => {
                    self.call_frames.push(frame);
                    frame = self.call(0, &functions);
                }
                op::Call1 => {
                    self.call_frames.push(frame);
                    frame = self.call(1, &functions);
                }
                op::Call2 => {
                    self.call_frames.push(frame);
                    frame = self.call(2, &functions);
                }
                op::Call3 => {
                    self.call_frames.push(frame);
                    frame = self.call(3, &functions);
                }
                op::Ret => {
                    frame = self.call_frames.pop().unwrap();
                }
                op::End => {return;}
                x => {panic!("unknown OpCode {}", x as u8);}
            }
        }
    }
}




pub fn run() {
    println!("{}", std::mem::size_of::<crate::lisp_object::Symbol>());
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::lisp_object::symbol_intern;
    use OpCode as Op;

    #[test]
    fn compute() {
        let func = Rc::new(LispFn{
            name: "Test-Add".into(),
            op_codes: vec![
                Op::Constant0.into(),
                Op::Constant1.into(),
                Op::Constant2.into(),
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
            constants: vec![
                LispObj::from(7),
                LispObj::from(13),
                LispObj::from(3),
            ],
            rest_args: false,
            required_args: 0,
            optional_args: 0,
            max_stack_usage: 5,
        });

        let test_add = symbol_intern::intern_mut("test-add");

        test_add.func = Some(func.clone());

        let mut functions: Vec<Rc<LispFn>> = Vec::new();

        let mut routine = Routine::new();

        routine.execute(func, &mut functions);

        assert_eq!("Test-Add", symbol_intern::intern("test-add").func.as_ref().unwrap().name);

        assert_eq!(63, routine.stack.get(0).unwrap().as_int().unwrap());
    }

    // #[test]
    // #[should_panic]
    // fn too_few_args() {

    // }
}
