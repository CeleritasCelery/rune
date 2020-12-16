#![allow(dead_code)]

use crate::lisp_object::{LispObj, Cons, Value, LispFn, Symbol, get_type};
use crate::error::{Error, Type};
use std::convert::TryInto;
use std::fmt;

#[derive(Copy, Clone, Debug)]
#[repr(u8)]
pub enum OpCode {
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
    ConstantN,
    ConstantN2,
    Call0,
    Call1,
    Call2,
    Call3,
    Call4,
    Call5,
    CallN,
    CallN2,
    Jump,
    JumpNil,
    JumpNilElsePop,
    Ret,
    End,
    Unknown
}

impl OpCode {
    pub unsafe fn from_unchecked(x: u8) -> Self {
        std::mem::transmute(x)
    }
}

impl From<OpCode> for u8 {
    fn from(x: OpCode) -> u8 { x as u8 }
}

impl Default for LispFn {
    fn default() -> Self {
        LispFn::new(
            vec_into![OpCode::Constant0, OpCode::Ret],
            vec![LispObj::nil()],
            0, 0, false)
    }
}

#[derive(Debug, PartialEq)]
struct ConstVec(Vec<LispObj>);

impl ConstVec {
    pub fn new() -> Self {ConstVec(Vec::new())}

    pub fn into_vec(self) -> Vec<LispObj> { self.0 }

    // TODO: Don't use rust equal because it will compare an entire list
    fn insert_or_get(&mut self, obj: LispObj) -> usize {
        match self.0.iter().position(|&x| obj == x) {
            None => {
                self.0.push(obj);
                self.0.len() - 1
            }
            Some(x) => x
        }
    }

    fn insert(&mut self, obj: LispObj) -> Result<u16, Error> {
        let idx = self.insert_or_get(obj);
        match idx.try_into() {
            Ok(x) => Ok(x),
            Err(_) => Err(Error::ConstOverflow),
        }
    }
}

#[derive(PartialEq)]
struct CodeVec(Vec<u8>);

impl CodeVec {
    pub fn new() -> Self {CodeVec(Vec::new())}

    pub fn into_vec(self) -> Vec<u8> { self.0 }

    pub fn push_op(&mut self, op: OpCode) {
        self.0.push(op.into());
    }

    fn push_op_n(&mut self, op: OpCode, arg: u8) {
        self.push_op(op);
        self.0.push(arg);
    }

    fn push_op_n2(&mut self, op: OpCode, arg: u16) {
        self.push_op(op);
        self.0.push((arg >> 8) as u8);
        self.0.push(arg as u8);
    }

    fn push_jump_placeholder(&mut self) -> usize {
        let idx = self.0.len();
        self.0.push(0);
        self.0.push(0);
        idx
    }

    fn set_jump_placeholder(&mut self, index: usize) {
        let offset = self.0.len() - index - 2;
        self.0[index] = (offset >> 8) as u8;
        self.0[index+1] = offset as u8;
    }

    fn emit_const(&mut self, idx: u16) {
        match idx {
            0 => self.push_op(OpCode::Constant0),
            1 => self.push_op(OpCode::Constant1),
            2 => self.push_op(OpCode::Constant2),
            3 => self.push_op(OpCode::Constant3),
            4 => self.push_op(OpCode::Constant4),
            5 => self.push_op(OpCode::Constant5),
            _ => {
                // TODO: look at the asm for this
                match idx.try_into() {
                    Ok(n) => self.push_op_n(OpCode::ConstantN, n),
                    Err(_) => self.push_op_n2(OpCode::ConstantN2, idx),
                }
            }
        }
    }

    fn emit_call(&mut self, idx: u16) {
        match idx {
            0 => self.push_op(OpCode::Call0),
            1 => self.push_op(OpCode::Call1),
            2 => self.push_op(OpCode::Call2),
            3 => self.push_op(OpCode::Call3),
            4 => self.push_op(OpCode::Call4),
            5 => self.push_op(OpCode::Call5),
            _ => {
                // TODO: look at the asm for this
                match idx.try_into() {
                    Ok(n) => self.push_op_n(OpCode::CallN, n),
                    Err(_) => self.push_op_n2(OpCode::CallN2, idx),
                }
            }
        }
    }

    pub fn emit_stack_ref(&mut self, idx: u16) {
        match idx {
            1 => self.push_op(OpCode::StackRef1),
            2 => self.push_op(OpCode::StackRef2),
            3 => self.push_op(OpCode::StackRef3),
            4 => self.push_op(OpCode::StackRef4),
            5 => self.push_op(OpCode::StackRef5),
            6 => self.push_op(OpCode::StackRef6),
            7 => self.push_op(OpCode::StackRef7),
            8 => self.push_op(OpCode::StackRef8),
            _ => {
                match idx.try_into() {
                    Ok(n) => self.push_op_n(OpCode::StackRefN, n),
                    Err(_) => self.push_op_n2(OpCode::StackRefN2, idx),
                }
            }
        }
    }
}

impl fmt::Debug for CodeVec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut display: Vec<String> = vec![];
        let mut iter = self.0.iter();
        while let Some(i) = iter.next() {
            let op = unsafe {OpCode::from_unchecked(*i)};
            display.push(format!("{:?}", op));
            match op {
                OpCode::StackRefN | OpCode::ConstantN | OpCode::CallN => {
                    display.push(format!("{:?}", iter.next()));
                }
                OpCode::StackRefN2 | OpCode::ConstantN2 | OpCode::CallN2 |
                OpCode::JumpNil | OpCode::Jump | OpCode::JumpNilElsePop => {
                    display.push(format!("{:?}", iter.next()));
                    display.push(format!("{:?}", iter.next()));
                }
                _ => {},
            }
        }
        write!(f, "{:?}", display)
   }
}

fn expect_type(exp_type: Type, obj: LispObj) -> Error {
    Error::Type(exp_type, get_type(obj))
}

fn into_list(obj: LispObj) -> Result<Vec<LispObj>, Error> {
    match obj.val() {
        Value::Cons(mut cons) => {
            let mut vec: Vec<LispObj> = vec![cons.car];

            while let Value::Cons(next) = cons.cdr.val() {
                vec.push(next.car);
                cons = next;
            }
            match cons.cdr.val() {
                Value::Nil => Ok(vec),
                _ => Err(expect_type(Type::List, cons.cdr)),
            }
        },
        _ => Err(expect_type(Type::Cons, obj)),
    }
}

fn into_arg_list(obj: LispObj) -> Result<Vec<LispObj>, Error> {
    match obj.val() {
        Value::Nil => Ok(vec![]),
        Value::Cons(_) => into_list(obj),
        _ => Err(expect_type(Type::List, obj))
    }
}

#[derive(Debug, PartialEq)]
pub struct Exp {
    codes: CodeVec,
    constants: ConstVec,
    vars: Vec<Option<Symbol>>,
}

impl std::convert::From<Exp> for LispFn {
    fn from(exp: Exp) -> Self {
        LispFn::new(exp.codes.0 , exp.constants.0 , 0, 0, false)
    }
}

impl Exp {
    fn add_const(&mut self, obj: LispObj, var_ref: Option<Symbol>) -> Result<(), Error> {
        self.vars.push(var_ref);
        let idx = self.constants.insert(obj)?;
        Ok(self.codes.emit_const(idx))
    }

    fn stack_ref(&mut self, idx: usize, var_ref: Symbol) -> Result<(), Error> {
        match (self.vars.len() - idx).try_into() {
            Ok(x) => {
                self.vars.push(Some(var_ref));
                Ok(self.codes.emit_stack_ref(x))
            }
            Err(_) => Err(Error::StackSizeOverflow),
        }
    }

    fn quote(&mut self, value: LispObj) -> Result<(), Error> {
        let list = into_arg_list(value)?;
        match list.len() {
            1 => self.add_const(list[0], None),
            x => Err(Error::ArgCount(1, x as u16)),
        }
    }

    fn let_form(&mut self, form: LispObj) -> Result<(), Error> {
        let prev_len = self.vars.len();
        let list = into_arg_list(form)?;
        let mut iter = list.iter();
        match iter.next() {
            Some(x) => self.let_bind(*x),
            None => Err(Error::ArgCount(1, 0)),
        }?;
        for sexp in iter {
            self.compile_form(*sexp)?;
        }
        self.vars.truncate(prev_len);
        Ok(())
    }

    fn let_bind_call(&mut self, cons: &Cons) -> Result<(), Error> {
        let var = cons.car.try_into()?;
        let list = into_arg_list(cons.cdr)?;
        let mut iter = list.iter();
        match iter.next() {
            Some(v) => self.add_const(*v, Some(var))?,
            None => self.add_const(LispObj::nil(), Some(var))?,
        };
        match iter.next() {
            None => Ok(()),
            Some(_) => Err(Error::LetValueCount(list.len() as u16)),
        }
    }

    fn let_bind_nil(&mut self, sym: Symbol) -> Result<(), Error> {
        self.add_const(LispObj::nil(), Some(sym))
    }

    fn let_bind(&mut self, obj: LispObj) -> Result<(), Error> {
        for binding in into_list(obj)? {
            match binding.val() {
                Value::Cons(cons) => self.let_bind_call(cons)?,
                Value::Symbol(sym) => self.let_bind_nil(sym)?,
                _ => return Err(expect_type(Type::Cons, binding)),
            }
        }
        Ok(())
    }

    fn compile_funcall(&mut self, cons: &Cons) -> Result<(), Error> {
        self.add_const(cons.car, None)?;
        let prev_len = self.vars.len();
        let list = into_arg_list(cons.cdr)?;
        for form in list.iter() {
            self.compile_form(*form)?;
        }
        self.codes.emit_call(list.len() as u16);
        self.vars.truncate(prev_len);
        Ok(())
    }

    fn compile_operator(&mut self, obj: LispObj, op: OpCode) -> Result<(), Error> {
        let list = into_arg_list(obj)?;
        match list.len() {
            2 => {
                self.compile_form(list[0])?;
                self.compile_form(list[1])?;
                self.codes.push_op(op);
                Ok(())
            }
            len => Err(Error::ArgCount(2, len as u16))
        }
    }

    fn compile_conditional(&mut self, obj: LispObj) -> Result<(), Error> {
        let list = into_arg_list(obj)?;
        match list.len() {
            3 => {
                self.compile_form(list[0])?;
                self.codes.push_op(OpCode::JumpNil);
                let place = self.codes.push_jump_placeholder();
                self.compile_form(list[1])?;
                self.codes.push_op(OpCode::Jump);
                let place2 = self.codes.push_jump_placeholder();
                self.codes.set_jump_placeholder(place);
                self.compile_form(list[2])?;
                self.codes.set_jump_placeholder(place2);
                Ok(())
            }
            2 => {
                self.compile_form(list[0])?;
                self.codes.push_op(OpCode::JumpNilElsePop);
                let place = self.codes.push_jump_placeholder();
                self.compile_form(list[1])?;
                self.codes.set_jump_placeholder(place);
                Ok(())
            }
            len => {
                Err(Error::ArgCount(2, len as u16))
            }
        }
    }

    fn compile_lambda(&mut self, obj: LispObj) -> Result<(), Error> {
        let list = into_arg_list(obj)?;
        let mut iter = list.iter();
        let mut vars: Vec<Option<Symbol>> = vec![];
        match iter.next() {
            None => return self.add_const(LispFn::default().into(), None),
            Some(bindings) => {
                for binding in into_arg_list(*bindings)?.iter() {
                    match binding.val() {
                        Value::Symbol(x) => vars.push(Some(x)),
                        _ => return Err(Error::Type(Type::Symbol, get_type(*binding))),
                    }
                }
            }
        };
        match iter.next() {
            None => self.add_const(LispFn::default().into(), None),
            Some(x) => {
                let len = vars.len();
                let mut func: LispFn = Self::compile_func_body(*x, vars)?.into();
                func.args.required = len as u16;
                self.add_const(func.into(), None)
            }
        }
    }

    fn dispatch_special_form(&mut self, cons: &Cons) -> Result<(), Error> {
        let sym: Symbol = cons.car.try_into()?;
        match sym.get_name() {
            "lambda" => self.compile_lambda(cons.cdr),
            "quote" => self.quote(cons.cdr),
            "let" => self.let_form(cons.cdr),
            "if" => self.compile_conditional(cons.cdr),
            _ => self.compile_funcall(cons),
        }
    }

    fn compile_variable_reference(&mut self, sym: Symbol) -> Result<(), Error> {
        match self.vars.iter().rposition(|&x| x == Some(sym)) {
            Some(idx) => self.stack_ref(idx, sym),
            None => panic!("dynamic variables not implemented"),
        }
    }

    fn compile_form(&mut self, obj: LispObj) -> Result<(), Error> {
        match obj.val() {
            Value::Cons(cons) => self.dispatch_special_form(cons),
            Value::Symbol(sym) => self.compile_variable_reference(sym),
            _ => self.add_const(obj, None)
        }
    }

    fn compile_func_body(obj: LispObj, vars: Vec<Option<Symbol>>) -> Result<Self, Error> {
        let mut exp = Self{
            codes: CodeVec::new(),
            constants: ConstVec::new(),
            vars,
        };
        exp.compile_form(obj)?;
        exp.codes.push_op(OpCode::Ret);
        exp.vars.truncate(0);
        Ok(exp)
    }

    pub fn compile(obj: LispObj) -> Result<Self, Error> {
        Self::compile_func_body(obj, vec![])
    }
}

pub fn run() {}

#[cfg(test)]
mod test {

    use super::*;
    use OpCode::*;
    use crate::reader::LispReader;
    use crate::symbol::intern;

    fn check_error(compare: &str, expect: Error) {
        let obj = LispReader::new(compare).next().unwrap().unwrap();
        assert_eq!(Exp::compile(obj).err().unwrap(), expect);
    }

    macro_rules! check_compiler {
        ($compare:expr, [$($op:expr),+], [$($const:expr),+]) => {
            let obj = LispReader::new($compare).next().unwrap().unwrap();
            let expect = Exp{
                codes: CodeVec(vec_into![$($op),+]),
                constants: ConstVec(vec_into![$($const),+]),
                vars: Vec::new(),
            };
            assert_eq!(Exp::compile(obj).unwrap(), expect);
        }
    }

    #[test]
    fn test_basic() {
        check_compiler!("1", [Constant0, Ret], [1]);
        check_compiler!("'foo", [Constant0, Ret], [intern("foo")]);
        check_compiler!("'(1 2)", [Constant0, Ret], [list!(1, 2)]);
    }

    #[test]
    fn variable() {
        check_compiler!("(let (foo))", [Constant0, Ret], [false]);
        check_compiler!("(let ((foo 1)(bar 2)(baz 3)))", [Constant0, Constant1, Constant2, Ret], [1, 2, 3]);
        check_compiler!("(let ((foo 1)) foo)", [Constant0, StackRef1, Ret], [1]);
        check_error("(let (foo 1))", Error::Type(Type::Cons, Type::Int));
    }

    #[test]
    fn conditional() {
        check_compiler!("(if nil 1 2)",
                        [Constant0, JumpNil, 0, 4, Constant1, Jump, 0, 1, Constant2, Ret],
                        [LispObj::nil(), 1, 2]);
        check_compiler!("(if t 2)", [Constant0, JumpNilElsePop, 0, 1, Constant1, Ret], [LispObj::t(), 2]);
        check_error("(if 1)", Error::ArgCount(2, 1));
    }

    #[test]
    fn function() {
        check_compiler!("(foo)", [Constant0, Call0, Ret], [intern("foo")]);
        check_compiler!("(foo 1 2)",
                        [Constant0, Constant1, Constant2, Call2, Ret],
                        [intern("foo"), 1, 2]);
        check_compiler!("(foo (bar 1) 2)",
                        [Constant0, Constant1, Constant2, Call1, Constant3, Call2, Ret],
                        [intern("foo"), intern("bar"), 1, 2]);
        check_compiler!("(foo (bar 1) (baz 1))",
                        [Constant0, Constant1, Constant2, Call1, Constant3, Constant2, Call1, Call2, Ret],
                        [intern("foo"), intern("bar"), 1, intern("baz")]);
        check_error("(foo . 1)", Error::Type(Type::List, Type::Int));
    }

    #[test]
    fn lambda() {
        check_compiler!("(lambda)", [Constant0, Ret], [LispFn::default()]);
        check_compiler!("(lambda ())", [Constant0, Ret], [LispFn::default()]);
        check_compiler!("(lambda () nil)", [Constant0, Ret], [LispFn::default()]);

        let func = LispFn::new(vec_into![Constant0, Ret], vec_into![1], 0, 0, false);
        check_compiler!("(lambda () 1)", [Constant0, Ret], [func]);

        let func = LispFn::new(vec_into![StackRef1, Ret], vec![], 1, 0, false);
        check_compiler!("(lambda (x) x)", [Constant0, Ret], [func]);

        let func = LispFn::new(vec_into![Constant0, StackRef3, StackRef3, Call2, Ret],
                               vec_into![intern("+")], 2, 0, false);
        check_compiler!("(lambda (x y) (+ x y))", [Constant0, Ret], [func]);

        check_error("(lambda (x 1) x)", Error::Type(Type::Symbol, Type::Int));
    }

    #[test]
    fn errors() {
        check_error("(\"foo\")", Error::Type(Type::Symbol, Type::String));
        check_error("(quote)", Error::ArgCount(1, 0));
        check_error("(quote 1 2)", Error::ArgCount(1, 2))
    }

    #[test]
    fn let_errors() {
        check_error("(let (1))", Error::Type(Type::Cons, Type::Int));
        check_error("(let ((foo 1 2)))", Error::LetValueCount(2));
        check_error("(let ((foo . 1)))", Error::Type(Type::List, Type::Int));
        check_error("(let ((foo 1 . 2)))", Error::Type(Type::List, Type::Int));
        check_error("(let (()))", Error::Type(Type::Cons, Type::Nil));
        check_error("(let ())", Error::Type(Type::Cons, Type::Nil));
        check_error("(let)", Error::ArgCount(1, 0));
    }
}
