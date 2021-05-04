#![allow(dead_code)]

use crate::arena::Arena;
use crate::error::{Error, Result};
use crate::hashmap::{HashMap, HashMapDefault};
use crate::object::{BuiltInFn, FnArgs, FunctionValue, GcObject, LispFn, Object, Symbol, Value};
use crate::opcode::OpCode;
use std::convert::TryInto;

#[derive(Clone)]
struct CallFrame<'a> {
    ip: Ip,
    func: &'a LispFn,
    start: usize,
}

#[derive(Clone)]
struct Ip {
    range: std::ops::Range<*const u8>,
    ip: *const u8,
}

impl Ip {
    #[allow(clippy::missing_const_for_fn)]
    fn new(vec: &[u8]) -> Self {
        Ip {
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

    fn jump(&mut self, offset: i16) {
        unsafe {
            self.ip = self.ip.offset(offset as isize);
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

impl<'a> CallFrame<'a> {
    pub fn new(func: &'a LispFn, frame_start: usize) -> CallFrame {
        CallFrame {
            ip: Ip::new(&func.op_codes),
            func,
            start: frame_start,
        }
    }

    pub fn get_const(&self, i: usize) -> GcObject {
        *self.func.constants.get(i).unwrap()
    }
}

type Stack = Vec<GcObject>;

trait LispStack {
    fn from_end(&self, i: usize) -> usize;
    fn push_ref(&mut self, i: usize);
    fn set_ref(&mut self, i: usize);
    fn ref_at(&self, i: usize) -> &GcObject;
    fn take_slice(&self, i: usize) -> &[GcObject];
}

impl LispStack for Vec<GcObject> {
    fn from_end(&self, i: usize) -> usize {
        debug_assert!(i < self.len());
        self.len() - (i + 1)
    }

    fn push_ref(&mut self, i: usize) {
        self.push(*self.ref_at(i));
    }

    fn set_ref(&mut self, i: usize) {
        self.swap_remove(self.from_end(i));
    }

    fn ref_at(&self, i: usize) -> &GcObject {
        self.get(self.from_end(i)).unwrap()
    }

    fn take_slice(&self, i: usize) -> &[GcObject] {
        &self[self.from_end(i - 1)..]
    }
}

pub struct Routine<'a> {
    stack: Vec<GcObject>,
    vars: HashMap<Symbol, GcObject>,
    call_frames: Vec<CallFrame<'a>>,
    frame: CallFrame<'a>,
    arena: Arena,
}

#[lisp_fn]
pub fn set<'obj>(
    place: Symbol,
    newlet: Object<'obj>,
    vars: &mut HashMap<Symbol, Object<'obj>>,
) -> Object<'obj> {
    vars.insert(place, unsafe { newlet.into_gc() });
    newlet
}

defsubr!(set);

impl<'a> Routine<'a> {
    fn new(func: &'a LispFn) -> Routine {
        Routine {
            stack: vec![],
            vars: HashMap::create(),
            call_frames: vec![],
            frame: CallFrame::new(func, 0),
            arena: Arena::new(),
        }
    }

    fn process_args(&mut self, count: u16, args: FnArgs, _sym: Symbol) -> Result<()> {
        if count < args.required {
            return Err(Error::ArgCount(args.required, count));
        }
        let total_args = args.required + args.optional;
        if !args.rest && (count > total_args) {
            return Err(Error::ArgCount(total_args, count));
        }
        if total_args > count {
            for _ in 0..(total_args - count) {
                self.stack.push(GcObject::nil());
            }
        }
        Ok(())
    }

    fn varref(&mut self, idx: usize) -> Result<()> {
        let symbol = self.frame.get_const(idx);
        if let Value::Symbol(sym) = symbol.val() {
            let value = match self.vars.get(&sym) {
                Some(x) => x,
                None => return Err(Error::VoidVariable),
            };
            self.stack.push(*value);
            Ok(())
        } else {
            panic!("Varref was not a symbol: {:?}", symbol);
        }
    }

    fn varset(&mut self, idx: usize) -> Result<()> {
        let symbol: Symbol = self.frame.get_const(idx).try_into()?;
        let value = self.stack.pop().unwrap();
        set(symbol, value, &mut self.vars);
        Ok(())
    }

    fn call(&mut self, arg_cnt: u16) -> Result<()> {
        let fn_idx = arg_cnt as usize;
        let sym = match self.stack.ref_at(fn_idx).val() {
            Value::Symbol(x) => x,
            x => panic!("Expected symbol for call found {:?}", x),
        };
        match sym.get_func().ok_or(Error::VoidFunction)?.val() {
            FunctionValue::LispFn(func) => {
                self.process_args(arg_cnt, func.args, sym)?;
                self.call_frames.push(self.frame.clone());
                self.frame = CallFrame::new(
                    // TODO: This is unsound
                    unsafe { std::mem::transmute(func) },
                    self.stack.from_end(fn_idx),
                );
            }
            FunctionValue::SubrFn(func) => {
                self.process_args(arg_cnt, func.args, sym)?;
                self.call_subr(func.subr, arg_cnt as usize)?;
            }
        };
        Ok(())
    }

    fn fix_lifetime<'old, 'unbound>(x: &'old Arena) -> &'unbound Arena {
        unsafe { std::mem::transmute(x) }
    }

    fn call_subr(&mut self, func: BuiltInFn, args: usize) -> Result<()> {
        let i = self.stack.from_end(args);
        let slice = self.stack.take_slice(args);
        self.stack[i] = func(slice, &mut self.vars, Self::fix_lifetime(&self.arena))?;
        self.stack.truncate(i + 1);
        Ok(())
    }

    pub fn execute(&mut self) -> Result<GcObject> {
        use OpCode as op;
        loop {
            println!("{:?}", self.stack);
            let op = unsafe { op::from_unchecked(self.frame.ip.next()) };
            println!("op : {:?}", op);
            match op {
                op::StackRef0 => self.stack.push_ref(0),
                op::StackRef1 => self.stack.push_ref(1),
                op::StackRef2 => self.stack.push_ref(2),
                op::StackRef3 => self.stack.push_ref(3),
                op::StackRef4 => self.stack.push_ref(4),
                op::StackRef5 => self.stack.push_ref(5),
                op::StackRefN => {
                    let idx = self.frame.ip.take_arg();
                    self.stack.push_ref(idx);
                }
                op::StackRefN2 => {
                    let idx = self.frame.ip.take_double_arg();
                    self.stack.push_ref(idx);
                }
                op::StackSet0 => self.stack.set_ref(0),
                op::StackSet1 => self.stack.set_ref(1),
                op::StackSet2 => self.stack.set_ref(2),
                op::StackSet3 => self.stack.set_ref(3),
                op::StackSet4 => self.stack.set_ref(4),
                op::StackSet5 => self.stack.set_ref(5),
                op::StackSetN => {
                    let idx = self.frame.ip.take_arg();
                    self.stack.set_ref(idx);
                }
                op::StackSetN2 => {
                    let idx = self.frame.ip.take_double_arg();
                    self.stack.set_ref(idx);
                }
                op::Constant0 => self.stack.push(self.frame.get_const(0)),
                op::Constant1 => self.stack.push(self.frame.get_const(1)),
                op::Constant2 => self.stack.push(self.frame.get_const(2)),
                op::Constant3 => self.stack.push(self.frame.get_const(3)),
                op::Constant4 => self.stack.push(self.frame.get_const(4)),
                op::Constant5 => self.stack.push(self.frame.get_const(5)),
                op::ConstantN => {
                    let idx = self.frame.ip.take_arg();
                    self.stack.push(self.frame.get_const(idx))
                }
                op::ConstantN2 => {
                    let idx = self.frame.ip.take_double_arg();
                    self.stack.push(self.frame.get_const(idx))
                }
                op::VarRef0 => self.varref(0)?,
                op::VarRef1 => self.varref(1)?,
                op::VarRef2 => self.varref(2)?,
                op::VarRef3 => self.varref(3)?,
                op::VarRef4 => self.varref(4)?,
                op::VarRef5 => self.varref(5)?,
                op::VarRefN => {
                    let idx = self.frame.ip.take_arg();
                    self.varref(idx)?
                }
                op::VarRefN2 => {
                    let idx = self.frame.ip.take_double_arg();
                    self.varref(idx)?
                }
                op::VarSet0 => self.varset(0)?,
                op::VarSet1 => self.varset(1)?,
                op::VarSet2 => self.varset(2)?,
                op::VarSet3 => self.varset(3)?,
                op::VarSet4 => self.varset(4)?,
                op::VarSet5 => self.varset(5)?,
                op::VarSetN => {
                    let idx = self.frame.ip.take_arg();
                    self.varset(idx)?
                }
                op::VarSetN2 => {
                    let idx = self.frame.ip.take_double_arg();
                    self.varset(idx)?
                }
                op::Call0 => self.call(0)?,
                op::Call1 => self.call(1)?,
                op::Call2 => self.call(2)?,
                op::Call3 => self.call(3)?,
                op::Discard => {
                    self.stack.pop();
                }
                op::Duplicate => {
                    let value = *self.stack.last().unwrap();
                    self.stack.push(value);
                }
                op::Jump => {
                    let offset = self.frame.ip.take_double_arg();
                    self.frame.ip.jump(offset as i16);
                }
                op::JumpNil => {
                    let cond = self.stack.pop().unwrap();
                    let offset = self.frame.ip.take_double_arg();
                    if matches!(cond.val(), Value::Nil) {
                        self.frame.ip.jump(offset as i16);
                    }
                }
                op::JumpNilElsePop => {
                    let cond = self.stack.last().unwrap();
                    let offset = self.frame.ip.take_double_arg();
                    if matches!(cond.val(), Value::Nil) {
                        self.frame.ip.jump(offset as i16);
                    } else {
                        self.stack.pop();
                    }
                }
                op::Ret => {
                    if self.call_frames.is_empty() {
                        return Ok(self.stack.pop().unwrap());
                    } else {
                        let var = self.stack.pop().unwrap();
                        self.stack[self.frame.start] = var;
                        self.stack.truncate(self.frame.start + 1);
                        self.frame = self.call_frames.pop().unwrap();
                    }
                }
                x => return Err(Error::UnknownOpcode(x as u8)),
            }
        }
    }
}

pub const fn run() {}

#[cfg(test)]
mod test {
    use super::*;
    use crate::arena::Arena;
    use crate::compile::Exp;
    use crate::object::IntoObject;
    use crate::reader::LispReader;

    fn test_eval(sexp: &str, expect: Object) {
        let arena = Arena::new();
        let obj = LispReader::new(sexp).read_from(&arena).unwrap().unwrap();
        let func: LispFn = Exp::compile(obj).unwrap().into();
        let box_func = Box::new(func);
        let mut routine = Routine::new(box_func.as_ref());
        let val = routine.execute();
        assert_eq!(expect, val.unwrap());
    }

    #[test]
    fn compute() {
        let arena = &Arena::new();
        test_eval("(- 7 (- 13 (* 3 (+ 7 (+ 13 1 2)))))", 63.into_obj(arena));
        test_eval("7", (7).into_obj(arena));
    }

    #[test]
    fn let_form() {
        let arena = &Arena::new();
        test_eval("(let ((foo 5) (bar 8)) (+ foo bar))", 13.into_obj(arena));
        test_eval("(let ((foo 5) (bar 8)) (+ 1 bar))", 9.into_obj(arena));
    }

    #[test]
    fn jump() {
        let arena = &Arena::new();
        test_eval("(+ 7 (if nil 11 3))", 10.into_obj(arena));
        test_eval("(+ 7 (if t 11 3))", 18.into_obj(arena));
        test_eval("(if nil 11)", false.into_obj(arena));
        test_eval("(if t 11)", 11.into_obj(arena));
    }

    #[test]
    fn loops() {
        let arena = &Arena::new();
        test_eval("(while nil)", false.into_obj(arena));
        test_eval("(while nil (set 'foo 7))", false.into_obj(arena));
        test_eval(
            "(let ((foo t)) (while foo (setq foo nil)))",
            false.into_obj(arena),
        );
        test_eval(
            "(let ((foo 10) (bar 0)) (while (> foo 3) (setq bar (1+ bar)) (setq foo (1- foo))) bar)",
            7.into_obj(arena),
        );
    }

    #[test]
    fn variables() {
        let arena = &Arena::new();
        test_eval("(progn (set 'foo 5) foo)", 5.into_obj(arena));
        test_eval("(let ((foo 1)) (setq foo 2) foo)", 2.into_obj(arena));
        test_eval("(progn (setq foo 2) foo)", 2.into_obj(arena));
    }

    #[test]
    fn call() {
        let arena = &Arena::new();
        test_eval(
            "(progn
(defalias 'bottom (lambda (x y z) (+ x z) (* x (+ y z))))
(defalias 'middle (lambda (x y z) (+ (bottom x z y) (bottom x z y))))
(middle 7 3 13))",
            (224).into_obj(arena),
        );
    }

    fn test_eval_error(sexp: &str, error: Error) {
        let arena = &Arena::new();
        let obj = LispReader::new(sexp).read_from(arena).unwrap().unwrap();
        let func: LispFn = Exp::compile(obj).unwrap().into();
        let func_box = Box::new(func);
        let mut routine = Routine::new(func_box.as_ref());
        let val = routine.execute();
        assert_eq!(val.err().unwrap(), error);
    }

    #[test]
    fn errors() {
        test_eval_error("(bad-function-name)", Error::VoidFunction);
        test_eval_error("(1+ 1 2)", Error::ArgCount(1, 2));
        test_eval_error("(/)", Error::ArgCount(1, 0));
    }
}
