#![allow(dead_code)]

use crate::lisp_object::{LispObj, LispFn, Value};
use crate::compile::{OpCode, Exp};
use crate::gc::Gc;
use std::mem::transmute;
use crate::arith;

impl OpCode {
    unsafe fn from_unchecked(x: u8) -> Self {
        transmute(x)
    }
}

#[derive(Clone)]
struct CallFrame {
    pub ip: *const u8,
    pub func: Gc<LispFn>,
}

impl CallFrame {
    pub fn new(func: Gc<LispFn>) -> CallFrame {
        CallFrame{ip: func.as_ref().op_codes.as_ptr(), func}
    }

    pub fn get_const(&self, i: usize) -> LispObj {
        self.func.as_ref().constants.get(i).unwrap().clone()
    }

    fn next(&mut self) -> u8 {
        unsafe {
            let x = *self.ip;
            self.ip = self.ip.add(1);
            x
        }
    }

    fn jump(&mut self, offset: isize) {
        unsafe {
            self.ip = self.ip.offset(offset);
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
        let base_fn = Gc::new(LispFn::new(vec![OpCode::End.into()], vec![], 0, 0, false));
        Routine{
            stack: vec![LispObj::nil()],
            call_frames: vec![CallFrame::new(base_fn)],
        }
    }

    fn take_top(vars: &[LispObj]) -> LispObj {
        debug_assert!(vars.len() == 2);
        vars[1]
    }

    fn call(&mut self, arg_cnt: u16) -> CallFrame {
        let fn_idx = arg_cnt as usize + 1;
        let sym = match self.stack.ref_at(fn_idx).val() {
            Value::Symbol(x) => x,
            _ => panic!()
        };
        let func_ref = match sym.get_func(){
            Some(x) => x.clone(),
            None => {
                panic!("void function {}", sym.get_name())
            }
        };
        let func = func_ref.as_ref();
        if arg_cnt < func.required_args  {
            panic!("Function {} called with {} arguments which is less then the required {}",
                   sym.get_name(),
                   arg_cnt,
                   func.required_args,
            );
        }
        let total_args = func.required_args + func.optional_args;
        if !func.rest_args && (arg_cnt > total_args) {
            panic!("Function {} called with {} arguments which is more then the aloud {}",
                   sym.get_name(),
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

        CallFrame::new(func_ref)
    }

    fn call_subst(&mut self, func: fn(&[LispObj]) -> LispObj, args: usize) {
        let i = self.stack.from_end(args);
        self.stack[i] = func(self.stack.take_slice(args));
        self.stack.truncate(i + 1);
    }

    pub fn execute(&mut self, func: Gc<LispFn>) {
        let mut frame = CallFrame::new(func);
        loop {
            use OpCode as op;
            match unsafe {op::from_unchecked(frame.next())} {
                op::StackRef1 => {self.stack.push_ref(1)}
                op::StackRef2 => {self.stack.push_ref(2)}
                op::StackRef3 => {self.stack.push_ref(3)}
                op::StackRef4 => {self.stack.push_ref(4)}
                op::StackRef5 => {self.stack.push_ref(5)}
                op::StackRef6 => {self.stack.push_ref(6)}
                op::StackRef7 => {self.stack.push_ref(7)}
                op::StackRef8 => {self.stack.push_ref(8)}
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
                    self.call_subst(arith::add, 2);
                }
                op::Sub => {
                    self.call_subst(arith::sub, 2);
                }
                op::Mul => {
                    self.call_subst(arith::mul, 2);
                }
                op::Div => {
                    self.call_subst(arith::div, 2);
                }
                op::Call0 => {
                    self.call_frames.push(frame);
                    frame = self.call(0);
                }
                op::Call1 => {
                    self.call_frames.push(frame);
                    frame = self.call(1);
                }
                op::Call2 => {
                    self.call_frames.push(frame);
                    frame = self.call(2);
                }
                op::Call3 => {
                    self.call_frames.push(frame);
                    frame = self.call(3);
                }
                op::Jump => {
                    let offset = frame.take_double_arg();
                    frame.jump(offset as isize);
                }
                op::JumpNil => {
                    let cond = self.stack.pop().unwrap();
                    let offset = frame.take_double_arg();
                    if matches!(cond.val(), Value::Nil) {
                        frame.jump(offset as isize);
                    }
                }
                op::Ret => {
                    self.call_subst(Self::take_top, 2);
                    frame = self.call_frames.pop().unwrap();
                }
                op::End => {return;}
                x => {panic!("unknown OpCode {}", x as u8);}
            }
        }
    }
}

pub fn run() {}

#[cfg(test)]
mod test {
    use super::*;
    use OpCode as Op;
    use crate::symbol::INTERNED_SYMBOLS;

    #[test]
    fn compute() {
        let func = LispFn::new(
            vec_into![
                Op::Constant0,
                Op::Constant1,
                Op::Constant2,
                Op::StackRef3,
                Op::StackRef3,
                Op::StackRef3,
                Op::Add,
                Op::Add,
                Op::Mul,
                Op::Sub,
                Op::Sub,
                Op::Ret
            ],
            vec_into![
                7,
                13,
                3
            ], 0, 0, false,) ;
        let mut routine = Routine::new();
        routine.execute(Gc::new(func));
        assert_eq!(63, *routine.stack.get(0).unwrap());
    }

    #[test]
    fn jump() {
        let func = LispFn::new(
            vec_into![
                Op::Constant1,
                Op::Constant2,
                Op::Constant0,
                Op::JumpNil,
                0, 2,
                Op::Constant3,
                Op::Add,
                Op::Add,
                Op::Ret,
            ],
            vec_into![false, 7, 3, 11],
            0, 0, false,) ;
        let mut routine = Routine::new();
        routine.execute(Gc::new(func));
        assert_eq!(10, *routine.stack.get(0).unwrap());
    }

    #[test]
    fn call() {
        let codes = vec_into![
                Op::StackRef3,
                Op::StackRef3,
                Op::StackRef3,
                Op::Add,
                Op::Add,
                Op::Mul,
                Op::Sub,
                Op::Sub,
                Op::Ret,
            ];

        let func = LispFn::new(codes, vec![], 3, 0, false,);

        let mut symbol_map = INTERNED_SYMBOLS.lock().unwrap();
        let test_add = symbol_map.intern("test-add");
        test_add.set_func(func);
        let middle = symbol_map.intern("middle");
        middle.set_func(LispFn::new(
            vec_into![
                Op::Constant0,
                Op::Constant1,
                Op::Constant2,
                Op::Constant3,
                Op::Call3,
                Op::Ret,
            ],
            vec_into![
                test_add,
                7,
                13,
                3,
            ],
            0, 0, false,));

        let top = LispFn::new(
            vec_into![
                Op::Constant0,
                Op::Call0,
                Op::Ret,
            ],
            vec_into![middle],
            0, 0, false);

        let mut routine = Routine::new();
        routine.execute(Gc::new(top));
        assert_eq!(63, *routine.stack.get(0).unwrap());
    }
}
