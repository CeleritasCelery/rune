#![allow(dead_code)]

use crate::lisp_object::{LispObj, LispFn, Value};
use crate::compile::{OpCode, Error};
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
    ip: IP,
    func: Gc<LispFn>,
}

#[derive(Clone)]
struct IP {
    range: std::ops::Range<*const u8>,
    ip: *const u8,
}

impl IP {
    fn new(vec: &Vec<u8>) -> Self {
        IP {
            range: vec.as_ptr_range(),
            ip: vec.as_ptr(),
        }
    }

    fn next(&mut self) -> u8 {
        unsafe {
            debug_assert!(self.range.contains(&self.ip));
            let x = *self.ip;
            self.ip = self.ip.add(1);
            x
        }
    }

    fn jump(&mut self, offset: isize) {
        unsafe {
            self.ip = self.ip.offset(offset);
            debug_assert!(self.range.contains(&self.ip));
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

impl CallFrame {
    pub fn new(func: Gc<LispFn>) -> CallFrame {
        CallFrame{ip: IP::new(&func.as_ref().op_codes), func}
    }

    pub fn get_const(&self, i: usize) -> LispObj {
        self.func.as_ref().constants.get(i).unwrap().clone()
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
        Routine{
            stack: vec![],
            call_frames: vec![],
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

    pub fn execute(&mut self, func: Gc<LispFn>) -> Result<LispObj, Error> {
        let mut frame = CallFrame::new(func);
        loop {
            use OpCode as op;
            match unsafe {op::from_unchecked(frame.ip.next())} {
                op::StackRef1 => {self.stack.push_ref(1)}
                op::StackRef2 => {self.stack.push_ref(2)}
                op::StackRef3 => {self.stack.push_ref(3)}
                op::StackRef4 => {self.stack.push_ref(4)}
                op::StackRef5 => {self.stack.push_ref(5)}
                op::StackRef6 => {self.stack.push_ref(6)}
                op::StackRef7 => {self.stack.push_ref(7)}
                op::StackRef8 => {self.stack.push_ref(8)}
                op::StackRefN => {
                    let idx = frame.ip.take_arg();
                    self.stack.push_ref(idx);
                }
                op::StackRefN2 => {
                    let idx = frame.ip.take_double_arg();
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
                    let offset = frame.ip.take_double_arg();
                    frame.ip.jump(offset as isize);
                }
                op::JumpNil => {
                    let cond = self.stack.pop().unwrap();
                    let offset = frame.ip.take_double_arg();
                    if matches!(cond.val(), Value::Nil) {
                        frame.ip.jump(offset as isize);
                    }
                }
                op::JumpNilElsePop => {
                    let cond = self.stack.get(self.stack.len() - 1).unwrap();
                    let offset = frame.ip.take_double_arg();
                    if matches!(cond.val(), Value::Nil) {
                        frame.ip.jump(offset as isize);
                    } else {
                        self.stack.pop();
                    }
                }
                op::Ret => {
                    if self.call_frames.len() == 0 {
                        return Ok(self.stack.pop().unwrap());
                    } else {
                        self.call_subst(Self::take_top, 2);
                        frame = self.call_frames.pop().unwrap();
                    }
                }
                x => return Err(Error::UnknownOpcode(x as u8))
            }
        }
    }
}

pub fn run() {}

#[cfg(test)]
mod test {
    use super::*;
    use crate::symbol;
    use crate::reader::LispReader;
    use crate::compile::Exp;

    #[test]
    fn compute() {
        let obj = LispReader::new("(- 7 (- 13 (* 3 (+ 7 (+ 13 3)))))").next().unwrap().unwrap();
        let func: LispFn = Exp::compile(obj).unwrap().into();
        let mut routine = Routine::new();
        let val = routine.execute(Gc::new(func));
        assert_eq!(63, val.unwrap());

        let obj = LispReader::new("7").next().unwrap().unwrap();
        let func: LispFn = Exp::compile(obj).unwrap().into();
        let mut routine = Routine::new();
        let val = routine.execute(Gc::new(func));
        assert_eq!(7, val.unwrap());
    }

    #[test]
    fn let_form() {
        let obj = LispReader::new("(let ((foo 5) (bar 8)) (+ foo bar))").next().unwrap().unwrap();
        let func: LispFn = Exp::compile(obj).unwrap().into();
        let mut routine = Routine::new();
        let val = routine.execute(Gc::new(func));
        assert_eq!(13, val.unwrap());

        let obj = LispReader::new("(let ((foo 5) (bar 8)) (+ 1 bar))").next().unwrap().unwrap();
        let func: LispFn = Exp::compile(obj).unwrap().into();
        let mut routine = Routine::new();
        let val = routine.execute(Gc::new(func));
        assert_eq!(9, val.unwrap());
    }

    #[test]
    fn jump() {
        let obj = LispReader::new("(+ 7 (if nil 11 3))").next().unwrap().unwrap();
        let func: LispFn = Exp::compile(obj).unwrap().into();
        let mut routine = Routine::new();
        let val = routine.execute(Gc::new(func));
        assert_eq!(10, val.unwrap());

        let obj = LispReader::new("(+ 7 (if t 11 3))").next().unwrap().unwrap();
        let func: LispFn = Exp::compile(obj).unwrap().into();
        let mut routine = Routine::new();
        let val = routine.execute(Gc::new(func));
        assert_eq!(18, val.unwrap());

        let obj = LispReader::new("(if nil 11)").next().unwrap().unwrap();
        let func: LispFn = Exp::compile(obj).unwrap().into();
        let mut routine = Routine::new();
        let val = routine.execute(Gc::new(func));
        assert_eq!(false, val.unwrap());

        let obj = LispReader::new("(if t 11)").next().unwrap().unwrap();
        let func: LispFn = Exp::compile(obj).unwrap().into();
        let mut routine = Routine::new();
        let val = routine.execute(Gc::new(func));
        assert_eq!(11, val.unwrap());
    }

    #[test]
    fn call() {
        let test_add = symbol::intern("test-add");
        let obj = LispReader::new("(lambda (x y z) (* x (+ y z)))").next().unwrap().unwrap();
        let exp: LispFn = Exp::compile(obj).unwrap().into();
        let func = match exp.constants[0].val() {
            Value::Function(x) => x.clone(),
            _ => unreachable!(),
        };
        test_add.set_func(func);

        let middle = symbol::intern("middle");
        let obj = LispReader::new("(lambda () (test-add 7 13 3))").next().unwrap().unwrap();
        let exp: LispFn = Exp::compile(obj).unwrap().into();
        let func = match exp.constants[0].val() {
            Value::Function(x) => x.clone(),
            _ => unreachable!(),
        };
        middle.set_func(func);

        let obj = LispReader::new("(middle)").next().unwrap().unwrap();
        let func: LispFn = Exp::compile(obj).unwrap().into();
        let mut routine = Routine::new();
        let val = routine.execute(Gc::new(func));
        assert_eq!(112, val.unwrap());
    }
}
