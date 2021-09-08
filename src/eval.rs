use crate::arena::Arena;
use crate::compile::{compile, compile_lambda};
use crate::data::Environment;
use crate::object::{Callable, Expression, LispFn, Object, SubrFn};
use crate::opcode::OpCode;
use crate::symbol::Symbol;
use fn_macros::defun;
use std::convert::TryInto;

use anyhow::{bail, Result};

#[derive(Debug, PartialEq)]
pub(crate) enum Error {
    VoidFunction(Symbol),
    VoidVariable(Symbol),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::VoidFunction(func) => write!(f, "Void function: {}", func.name()),
            Error::VoidVariable(var) => write!(f, "Void variable: {}", var.name()),
        }
    }
}

impl std::error::Error for Error {}

#[derive(Clone)]
struct CallFrame<'brw, 'ob> {
    ip: Ip,
    code: &'brw Expression<'ob>,
    start: usize,
}

#[derive(Clone)]
struct Ip {
    range: std::ops::Range<*const u8>,
    ip: *const u8,
}

impl Ip {
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

    fn take_arg(&mut self) -> u8 {
        let x = self.next();
        #[cfg(feature = "debug_bytecode")]
        println!("arg = {}", x);
        x
    }

    fn take_double_arg(&mut self) -> u16 {
        let upper: u16 = self.next().into();
        let lower: u16 = self.next().into();
        let x = upper << 8 | lower;
        #[cfg(feature = "debug_bytecode")]
        println!("dbl arg = {}", x);
        x
    }
}

impl<'brw, 'ob> CallFrame<'brw, 'ob> {
    fn new(func: &'brw Expression<'ob>, frame_start: usize) -> CallFrame<'brw, 'ob> {
        CallFrame {
            ip: Ip::new(&func.op_codes),
            code: func,
            start: frame_start,
        }
    }

    fn get_const(&self, i: usize) -> Object<'ob> {
        self.code.constants[i]
    }
}

trait LispStack<T> {
    fn from_end(&self, i: usize) -> usize;
    fn push_ref(&mut self, i: usize);
    fn set_ref(&mut self, i: usize);
    fn ref_at(&self, i: usize) -> T;
    fn take_slice(&self, i: usize) -> &[T];
}

impl<'ob> LispStack<Object<'ob>> for Vec<Object<'ob>> {
    fn from_end(&self, i: usize) -> usize {
        debug_assert!(i < self.len());
        self.len() - (i + 1)
    }

    fn push_ref(&mut self, i: usize) {
        self.push(self.ref_at(i));
    }

    fn set_ref(&mut self, i: usize) {
        self.swap_remove(self.from_end(i));
    }

    fn ref_at(&self, i: usize) -> Object<'ob> {
        self[self.from_end(i)]
    }

    fn take_slice(&self, i: usize) -> &[Object<'ob>] {
        &self[self.from_end(i - 1)..]
    }
}

pub(crate) struct Routine<'brw, 'ob> {
    stack: Vec<Object<'ob>>,
    call_frames: Vec<CallFrame<'brw, 'ob>>,
    frame: CallFrame<'brw, 'ob>,
}

fn fill_extra_args(stack: &mut Vec<Object>, fill_args: u16) {
    for _ in 0..fill_args {
        stack.push(Object::Nil);
    }
}

impl<'ob, 'brw> Routine<'brw, 'ob> {
    fn varref(&mut self, idx: usize, env: &Environment<'ob>) -> Result<()> {
        let symbol = self.frame.get_const(idx);
        if let Object::Symbol(sym) = symbol {
            let value = match env.vars.get(&!sym) {
                Some(x) => x,
                None => bail!(Error::VoidVariable(!sym)),
            };
            self.stack.push(*value);
            Ok(())
        } else {
            unreachable!("Varref was not a symbol: {:?}", symbol);
        }
    }

    fn varset(&mut self, idx: usize, env: &mut Environment<'ob>) -> Result<()> {
        let obj: Object = self.frame.get_const(idx);
        let symbol: Symbol = obj.try_into()?;
        let value = self.stack.pop().unwrap();
        crate::data::set(symbol, value, env);
        Ok(())
    }

    fn prepare_arguments(
        &mut self,
        func: &'brw LispFn<'ob>,
        arg_cnt: u16,
        arena: &'ob Arena,
    ) -> Result<u16> {
        let fill_args = func.args.num_of_fill_args(arg_cnt)?;
        fill_extra_args(&mut self.stack, fill_args);
        let total_args = arg_cnt + fill_args;
        let rest_size = total_args - func.args.rest_args_start();
        if rest_size > 0 {
            let slice = self.stack.take_slice(rest_size as usize);
            let list = crate::fns::slice_into_list(slice, None, arena);
            let i = self.stack.from_end(rest_size as usize - 1);
            self.stack[i] = list;
            self.stack.truncate(i + 1);
            Ok(total_args - rest_size + 1)
        } else {
            Ok(total_args)
        }
    }

    fn call(&mut self, arg_cnt: u16, env: &mut Environment<'ob>, arena: &'ob Arena) -> Result<()> {
        let fn_idx = arg_cnt as usize;
        let sym = match self.stack.ref_at(fn_idx) {
            Object::Symbol(x) => !x,
            x => unreachable!("Expected symbol for call found {:?}", x),
        };
        println!("calling: {}", sym.name());
        match sym.func(arena) {
            Some(func) => match func {
                Callable::LispFn(func) => {
                    self.call_lisp(!func, arg_cnt, arena)?;
                }
                Callable::SubrFn(func) => {
                    self.call_subr(*func, arg_cnt, env, arena)?;
                }
                Callable::Macro(_) => {
                    bail!("Attempt to call macro {} at runtime", sym.name())
                }
            },
            None => match env.funcs.get(&sym) {
                Some(Object::Cons(uncompiled_func)) => {
                    let func = compile_lambda(uncompiled_func.cdr(), env, arena)?;
                    let obj = arena.add(func);
                    crate::data::defalias(sym, obj, None, env);
                    if let Callable::LispFn(func) = obj.try_into()? {
                        self.call_lisp(!func, arg_cnt, arena)?;
                    } else {
                        unreachable!("type was no longer lisp fn");
                    }
                }
                Some(other) => bail!("Type {:?} is not a valid function", other.get_type()),
                None => bail!(Error::VoidFunction(sym)),
            },
        };
        Ok(())
    }

    fn call_lisp(
        &mut self,
        func: &'brw LispFn<'ob>,
        arg_cnt: u16,
        arena: &'ob Arena,
    ) -> Result<()> {
        let total_args = self.prepare_arguments(func, arg_cnt, arena)?;
        self.call_frames.push(self.frame.clone());
        let tmp = self.stack.from_end(total_args as usize);
        self.frame = CallFrame::new(&func.body, tmp);
        Ok(())
    }

    fn call_subr(
        &mut self,
        func: SubrFn,
        arg_cnt: u16,
        env: &mut Environment<'ob>,
        arena: &'ob Arena,
    ) -> Result<()> {
        let fill_args = func.args.num_of_fill_args(arg_cnt)?;
        fill_extra_args(&mut self.stack, fill_args);
        let total_args = (arg_cnt + fill_args) as usize;
        let i = self.stack.from_end(total_args);
        let slice = self.stack.take_slice(total_args);
        let result = (func.subr)(slice, env, arena)?;
        self.stack[i] = result;
        self.stack.truncate(i + 1);
        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    pub(crate) fn run(
        &mut self,
        env: &mut Environment<'ob>,
        arena: &'ob Arena,
    ) -> Result<Object<'ob>> {
        use OpCode as op;
        #[cfg(debug_assertions)]
        let init_stack_size = self.stack.len();
        loop {
            let op = self.frame.ip.next().try_into()?;
            #[cfg(feature = "debug_bytecode")]
            {
                println!("[");
                for (idx, x) in self.stack.iter().enumerate() {
                    println!("    {}: {:?},", idx, x);
                }
                println!("]");
                let byte_offset = self.frame.ip.ip as i64 - self.frame.ip.range.start as i64 - 1;
                println!("op :{}: {:?}", byte_offset, op);
            }
            match op {
                op::StackRef0 => self.stack.push_ref(0),
                op::StackRef1 => self.stack.push_ref(1),
                op::StackRef2 => self.stack.push_ref(2),
                op::StackRef3 => self.stack.push_ref(3),
                op::StackRef4 => self.stack.push_ref(4),
                op::StackRef5 => self.stack.push_ref(5),
                op::StackRefN => {
                    let idx = self.frame.ip.take_arg();
                    self.stack.push_ref(idx.into());
                }
                op::StackRefN2 => {
                    let idx = self.frame.ip.take_double_arg();
                    self.stack.push_ref(idx.into());
                }
                op::StackSet0 => self.stack.set_ref(0),
                op::StackSet1 => self.stack.set_ref(1),
                op::StackSet2 => self.stack.set_ref(2),
                op::StackSet3 => self.stack.set_ref(3),
                op::StackSet4 => self.stack.set_ref(4),
                op::StackSet5 => self.stack.set_ref(5),
                op::StackSetN => {
                    let idx = self.frame.ip.take_arg();
                    self.stack.set_ref(idx.into());
                }
                op::StackSetN2 => {
                    let idx = self.frame.ip.take_double_arg();
                    self.stack.set_ref(idx.into());
                }
                op::Constant0 => self.stack.push(self.frame.get_const(0)),
                op::Constant1 => self.stack.push(self.frame.get_const(1)),
                op::Constant2 => self.stack.push(self.frame.get_const(2)),
                op::Constant3 => self.stack.push(self.frame.get_const(3)),
                op::Constant4 => self.stack.push(self.frame.get_const(4)),
                op::Constant5 => self.stack.push(self.frame.get_const(5)),
                op::ConstantN => {
                    let idx = self.frame.ip.take_arg();
                    self.stack.push(self.frame.get_const(idx.into()));
                }
                op::ConstantN2 => {
                    let idx = self.frame.ip.take_double_arg();
                    self.stack.push(self.frame.get_const(idx.into()));
                }
                op::VarRef0 => self.varref(0, env)?,
                op::VarRef1 => self.varref(1, env)?,
                op::VarRef2 => self.varref(2, env)?,
                op::VarRef3 => self.varref(3, env)?,
                op::VarRef4 => self.varref(4, env)?,
                op::VarRef5 => self.varref(5, env)?,
                op::VarRefN => {
                    let idx = self.frame.ip.take_arg();
                    self.varref(idx.into(), env)?;
                }
                op::VarRefN2 => {
                    let idx = self.frame.ip.take_double_arg();
                    self.varref(idx.into(), env)?;
                }
                op::VarSet0 => self.varset(0, env)?,
                op::VarSet1 => self.varset(1, env)?,
                op::VarSet2 => self.varset(2, env)?,
                op::VarSet3 => self.varset(3, env)?,
                op::VarSet4 => self.varset(4, env)?,
                op::VarSet5 => self.varset(5, env)?,
                op::VarSetN => {
                    let idx = self.frame.ip.take_arg();
                    self.varset(idx.into(), env)?;
                }
                op::VarSetN2 => {
                    let idx = self.frame.ip.take_double_arg();
                    self.varset(idx.into(), env)?;
                }
                op::Call0 => self.call(0, env, arena)?,
                op::Call1 => self.call(1, env, arena)?,
                op::Call2 => self.call(2, env, arena)?,
                op::Call3 => self.call(3, env, arena)?,
                op::Call4 => self.call(4, env, arena)?,
                op::Call5 => self.call(5, env, arena)?,
                op::CallN => {
                    let idx = self.frame.ip.take_arg();
                    self.call(idx.into(), env, arena)?;
                }
                op::CallN2 => {
                    let idx = self.frame.ip.take_double_arg();
                    self.call(idx, env, arena)?;
                }
                op::Discard => {
                    self.stack.pop();
                }
                op::DiscardN => {
                    let num: usize = self.frame.ip.take_arg().into();
                    let cur_len = self.stack.len();
                    self.stack.truncate(cur_len - num);
                }
                op::DiscardNKeepTOS => {
                    let tos = self
                        .stack
                        .pop()
                        .expect("stack was empty when discard called");
                    let num: usize = self.frame.ip.take_arg().into();
                    let cur_len = self.stack.len();
                    self.stack.truncate(cur_len - num);
                    self.stack.push(tos);
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
                    if cond == Object::Nil {
                        self.frame.ip.jump(offset as i16);
                    }
                }
                op::JumpNotNil => {
                    let cond = self.stack.pop().unwrap();
                    let offset = self.frame.ip.take_double_arg();
                    if cond != Object::Nil {
                        self.frame.ip.jump(offset as i16);
                    }
                }
                op::JumpNilElsePop => {
                    let cond = self.stack.last().unwrap();
                    let offset = self.frame.ip.take_double_arg();
                    if *cond == Object::Nil {
                        self.frame.ip.jump(offset as i16);
                    } else {
                        self.stack.pop();
                    }
                }
                op::JumpNotNilElsePop => {
                    let cond = self.stack.last().unwrap();
                    let offset = self.frame.ip.take_double_arg();
                    if *cond == Object::Nil {
                        self.stack.pop();
                    } else {
                        self.frame.ip.jump(offset as i16);
                    }
                }
                op::Ret => {
                    if self.call_frames.is_empty() {
                        debug_assert_eq!(self.stack.len(), init_stack_size + 1);
                        return Ok(self.stack.pop().unwrap());
                    }
                    let var = self.stack.pop().unwrap();
                    self.stack[self.frame.start] = var;
                    self.stack.truncate(self.frame.start + 1);
                    self.frame = self.call_frames.pop().unwrap();
                }
                op @ op::Unknown => {
                    panic!("Unimplemented opcode: {:?}", op);
                }
            }
        }
    }

    pub(crate) fn execute(
        exp: &Expression<'ob>,
        env: &mut Environment<'ob>,
        arena: &'ob Arena,
    ) -> Result<Object<'ob>> {
        let mut rout = Routine {
            stack: vec![],
            call_frames: vec![],
            frame: CallFrame::new(exp, 0),
        };
        rout.run(env, arena)
    }
}

pub(crate) fn call_lisp<'brw, 'ob>(
    func: &'brw LispFn<'ob>,
    args: Vec<Object<'ob>>,
    env: &mut Environment<'ob>,
    arena: &'ob Arena,
) -> Result<Object<'ob>> {
    let arg_cnt = args.len() as u16;
    let mut rout = Routine {
        stack: args,
        call_frames: vec![],
        frame: CallFrame::new(&func.body, 0),
    };
    rout.prepare_arguments(func, arg_cnt, arena)?;
    rout.run(env, arena)
}

pub(crate) fn call_subr<'ob>(
    func: SubrFn,
    mut args: Vec<Object<'ob>>,
    env: &mut Environment<'ob>,
    arena: &'ob Arena,
) -> Result<Object<'ob>> {
    let arg_cnt = args.len() as u16;
    let fill_args = func.args.num_of_fill_args(arg_cnt)?;
    fill_extra_args(&mut args, fill_args);
    (func.subr)(&args, env, arena)
}

#[defun]
pub(crate) fn eval<'ob>(
    form: Object<'ob>,
    env: &mut Environment<'ob>,
    arena: &'ob Arena,
) -> Result<Object<'ob>> {
    let func = compile(form, env, arena)?;
    Routine::execute(&func, env, arena)
}

defsubr!(eval);

#[cfg(test)]
mod test {
    use super::*;
    use crate::arena::Arena;
    use crate::object::IntoObject;
    use crate::reader::Reader;

    macro_rules! test_eval {
        ($sexp:expr, $expect:expr) => {{
            println!("Test String: {}", $sexp);
            let arena = &Arena::new();
            let env = &mut Environment::default();
            let obj = Reader::read($sexp, arena).unwrap().0;
            let exp = compile(obj, env, arena).unwrap();
            println!("codes: {:?}", exp.op_codes);
            println!("const: {:?}", exp.constants);
            let val = Routine::execute(&exp, env, arena).unwrap();
            assert_eq!(val, $expect.into_obj(arena));
        }};
    }

    macro_rules! test_eval_serial {
        ($sexp:expr, $expect:expr, $env:expr, $arena:expr) => {{
            println!("Test String: {}", $sexp);
            let obj = Reader::read($sexp, $arena).unwrap().0;
            let exp = compile(obj, $env, $arena).unwrap();
            println!("codes: {:?}", exp.op_codes);
            println!("const: {:?}", exp.constants);
            let val = Routine::execute(&exp, $env, $arena).unwrap();
            assert_eq!(val, $expect.into_obj($arena));
        }};
    }

    #[test]
    fn compute() {
        test_eval!("(- 7 (- 13 (* 3 (+ 7 (+ 13 1 2)))))", 63);
        test_eval!("7", 7);
        test_eval!("(+ 1 2.5)", 3.5);
    }

    #[test]
    fn let_form() {
        test_eval!("(let ((foo 5) (bar 8)) (+ foo bar))", 13);
        test_eval!("(let ((foo 5) (bar 8)) (+ 1 bar))", 9);
    }

    #[test]
    fn jump() {
        test_eval!("(+ 7 (if nil 11 3))", 10);
        test_eval!("(+ 7 (if t 11 3) 4)", 22);
        test_eval!("(let ((foo 7) (bar t)) (+ 7 (if bar foo 3)))", 14);
        test_eval!("(let ((foo 7) (bar nil)) (+ 7 (if bar foo 3)))", 10);
        test_eval!("(let ((foo (+ 3 4)) (bar t)) (+ 7 (if bar foo 3)))", 14);
        test_eval!("(if nil 11)", false);
        test_eval!("(if t 11)", 11);
        test_eval!("(cond)", false);
        test_eval!("(cond ())", false);
        test_eval!("(cond (1))", 1);
        test_eval!("(cond (1 2))", 2);
        test_eval!("(cond (nil 2)(3))", 3);
        test_eval!("(cond (nil 2)(3 4))", 4);
        test_eval!("(cond (t 2)(3 4))", 2);
        test_eval!("(and)", true);
        test_eval!("(and 1 2 3)", 3);
        test_eval!("(and 1 nil 3)", false);
        test_eval!("(or)", false);
        test_eval!("(or 1 2 3)", 1);
        test_eval!("(or nil 2 3)", 2);
        test_eval!("(let ((foo 7)) (cond (2)(3)(4)) foo)", 7);
        test_eval!("(let ((foo 7)) (cond (foo 2)(3 4)))", 2);
        test_eval!("(let ((foo 7)) (cond (nil 3)(foo 4)))", 4);
        test_eval!("(let ((foo 7) (bar nil))(cond (bar 3)(foo 11) (t 13)))", 11);
    }

    #[test]
    fn loops() {
        test_eval!("(while nil)", false);
        test_eval!("(while nil (set 'foo 7))", false);
        test_eval!("(let ((foo t)) (while foo (setq foo nil)))", false);
        test_eval!(
            "(let ((foo 10) (bar 0)) (while (> foo 3) (setq bar (1+ bar)) (setq foo (1- foo))) bar)",
            7
        );
    }

    #[test]
    fn variables() {
        test_eval!("(progn (set 'foo 5) foo)", 5);
        test_eval!("(let ((foo 1)) (setq foo 2) foo)", 2);
        test_eval!("(progn (setq foo 2) foo)", 2);
        test_eval!("(progn (defvar foo 1) foo)", 1);
        test_eval!("(prog1 1 2)", 1);
        test_eval!("(prog1 1)", 1);
        test_eval!("(prog2 1 2)", 2);
        test_eval!("(prog2 1 2 3)", 2);
    }

    #[test]
    fn call() {
        test_eval!(
            "(progn
                 (defalias 'bottom #'(lambda (x y z) (+ x z) (* x (+ y z))))
                 (defalias 'middle #'(lambda (x y z) (+ (bottom x z y) (bottom x z y))))
                 (middle 7 3 13))",
            224
        );

        test_eval!(
            "(progn
                 (defalias 'has_rest #'(lambda (&rest z) (car z)))
                 (has_rest 7 9 11 13))",
            7
        );

        test_eval!(
            "(progn
                 (defalias 'has_rest #'(lambda (x &rest z) (car z)))
                 (has_rest 7 9 11 13))",
            9
        );

        test_eval!(
            "(progn
                 (defalias 'has_rest #'(lambda (x &optional y &rest z) (car z)))
                 (has_rest 7 9 11 13))",
            11
        );

        test_eval!(
            "(progn
                 (defalias 'has_rest #'(lambda (x &optional y &rest z) y))
                 (has_rest 7 9 11 13))",
            9
        );

        test_eval!(
            "(progn
                 (defalias 'test-jump (lambda (name arglist &optional docstring &rest body)
                     (if docstring nil nil)
                     (+ arglist name)))
                 (test-jump 1 2 3 4))",
            3
        );

        test_eval!(
            "(progn
                 (defalias 'has_rest #'(lambda (x &optional y &rest z) (car z)))
                 (has_rest 7))",
            false
        );
    }

    #[test]
    fn test_macro() {
        let arena = &Arena::new();
        let env = &mut Environment::default();
        test_eval_serial!(
            "(defalias 'test_macro_1 (cons 'macro #'(lambda (x y) (list '+ x 3))))",
            crate::symbol::intern("test_macro_1"),
            env,
            arena
        );
        test_eval_serial!("(test_macro_1 5 1)", 8, env, arena);
        test_eval_serial!("(+ (test_macro_1 2 4) (test_macro_1 4 9))", 12, env, arena);
    }

    fn test_eval_error<E>(sexp: &str, error: E)
    where
        E: std::error::Error + PartialEq + Send + Sync + 'static,
    {
        let arena = &Arena::new();
        let env = &mut Environment::default();
        let obj = Reader::read(sexp, arena).unwrap().0;
        let exp = compile(obj, env, arena).unwrap();
        let val = Routine::execute(&exp, env, arena);
        assert_eq!(val.err().unwrap().downcast::<E>().unwrap(), (error));
    }

    #[test]
    fn errors() {
        use crate::error::Error::ArgCount;
        use crate::symbol::intern;
        test_eval_error(
            "(bad-function-name)",
            Error::VoidFunction(intern("bad-function-name")),
        );
        test_eval_error("(1+ 1 2)", ArgCount(1, 2));
        test_eval_error("(/)", ArgCount(1, 0));
    }
}
