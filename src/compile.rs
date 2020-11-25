#![allow(dead_code)]

use crate::lisp_object::{LispObj, Cons, Value, LispFn};
use crate::symbol::Symbol;
use std::convert::{TryInto, TryFrom};

#[derive(Copy, Clone)]
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
    Jump,
    JumpNil,
    Ret,
    End,
    Unknown
}

impl From<OpCode> for u8 {
    fn from(x: OpCode) -> u8 { x as u8 }
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

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum Error {
    ConstOverflow,
    ArgOverflow,
    ArgCount(u16, u16),
    LetValueCount(u16),
    StackSizeOverflow,
    Type(Type, Type),
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Int,
    True,
    Nil,
    Cons,
    String,
    Symbol,
    Float,
    Void,
    Marker,
    Func,
    Number,
    List,
}

fn get_type(obj: LispObj) -> Type {
    use Type::*;
    match obj.val() {
        Value::Symbol(_) => Symbol,
        Value::Float(_) => Float,
        Value::Void => Void,
        Value::String(_) => String,
        Value::Nil => Nil,
        Value::True => True,
        Value::Cons(_) => Cons,
        Value::Int(_) => Int,
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

impl TryFrom<LispObj> for Symbol {
    type Error = Error;
    fn try_from(value: LispObj) -> Result<Self, Self::Error> {
        match value.val() {
            Value::Symbol(x) => Ok(x),
            _ => Err(expect_type(Type::Symbol, value)),
        }
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
        let list = into_arg_list(cons.cdr)?;
        for form in list.iter() {
            self.compile_form(*form)?;
        }
        self.codes.emit_call(list.len() as u16);
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

    fn dispatch_special_form(&mut self, cons: &Cons) -> Result<(), Error> {
        let sym: Symbol = cons.car.try_into()?;
        match sym.get_name() {
            "quote" => self.quote(cons.cdr),
            "let" => self.let_form(cons.cdr),
            "+" => self.compile_operator(cons.cdr, OpCode::Add),
            "*" => self.compile_operator(cons.cdr, OpCode::Mul),
            "/" => self.compile_operator(cons.cdr, OpCode::Div),
            "-" => self.compile_operator(cons.cdr, OpCode::Sub),
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

    pub fn compile(obj: LispObj) -> Result<Self, Error> {
        let mut exp = Self{
            codes: CodeVec::new(),
            constants: ConstVec::new(),
            vars: Vec::new(),
        };
        exp.compile_form(obj)?;
        exp.codes.push_op(OpCode::Ret);
        exp.vars.truncate(0);
        Ok(exp)
    }
}

pub fn run() {}

#[cfg(test)]
mod test {

    use super::*;
    use OpCode::*;
    use crate::reader::LispReader;
    use crate::symbol;

    fn create_expect(codes: Vec<u8>, constants: Vec<LispObj>) -> Exp {
        Exp{
            codes: CodeVec(codes),
            constants: ConstVec(constants),
            vars: Vec::new(),
        }
    }

    #[test]
    fn test_basic() {
        let obj = LispReader::new("1").next().unwrap().unwrap();
        let expect = create_expect(
            vec_into![Constant0, Ret],
            vec_into![1],
        );
        assert_eq!(expect, Exp::compile(obj).unwrap());

        let obj = LispReader::new("'foo").next().unwrap().unwrap();
        let expect = create_expect(
            vec_into![Constant0, Ret],
            vec_into![symbol::intern("foo")],
        );
        assert_eq!(expect, Exp::compile(obj).unwrap());

        let obj = LispReader::new("'(1 2)").next().unwrap().unwrap();
        let expect = create_expect(
            vec_into![Constant0, Ret],
            vec_into![list!(1, 2)],
        );
        assert_eq!(expect, Exp::compile(obj).unwrap());
    }

    #[test]
    fn variable() {
        let obj = LispReader::new("(let (foo))").next().unwrap().unwrap();
        let expect = create_expect(vec_into![Constant0, Ret], vec_into![false]);
        assert_eq!(expect, Exp::compile(obj).unwrap());

        let obj = LispReader::new("(let ((foo 1)(bar 2)(baz 3)))").next().unwrap().unwrap();
        let expect = create_expect(
            vec_into![Constant0, Constant1, Constant2, Ret],
            vec_into![1, 2, 3]
        );
        assert_eq!(expect, Exp::compile(obj).unwrap());

        let obj = LispReader::new("(let ((foo 1)) foo)").next().unwrap().unwrap();
        let expect = create_expect(
            vec_into![Constant0, StackRef1, Ret],
            vec_into![1]
        );
        assert_eq!(expect, Exp::compile(obj).unwrap());

        let obj = LispReader::new("(let (foo 1))").next().unwrap().unwrap();
        assert!(Exp::compile(obj).is_err());
    }

    #[test]
    fn function() {
        let obj = LispReader::new("(foo)").next().unwrap().unwrap();
        let expect = create_expect(
            vec_into![Constant0, Call0, Ret],
            vec_into![symbol::intern("foo")],
        );
        assert_eq!(expect, Exp::compile(obj).unwrap());

        let obj = LispReader::new("(foo 1 2)").next().unwrap().unwrap();
        let expect = create_expect(
            vec_into![Constant0, Constant1, Constant2, Call2, Ret],
            vec_into![symbol::intern("foo"), 1, 2],
        );
        assert_eq!(expect, Exp::compile(obj).unwrap());

        let obj = LispReader::new("(foo (bar 1) 2)").next().unwrap().unwrap();
        let expect = create_expect(
            vec_into![
                Constant0,
                Constant1,
                Constant2,
                Call1,
                Constant3,
                Call2,
                Ret,
            ],
            vec_into![symbol::intern("foo"), symbol::intern("bar"), 1, 2],
        );
        assert_eq!(expect, Exp::compile(obj).unwrap());
    }

    #[test]
    fn errors() {
        let obj = LispReader::new("(\"foo\")").next().unwrap().unwrap();
        assert_eq!(Exp::compile(obj).err().unwrap(), Error::Type(Type::Symbol, Type::String));

        let obj = LispReader::new("(foo . 1)").next().unwrap().unwrap();
        assert_eq!(Exp::compile(obj).err().unwrap(), Error::Type(Type::List, Type::Int));
    }

    #[test]
    fn quote_errors() {
        let obj = LispReader::new("(quote)").next().unwrap().unwrap();
        assert_eq!(Exp::compile(obj).err().unwrap(), Error::ArgCount(1, 0));

        let obj = LispReader::new("(quote 1 2)").next().unwrap().unwrap();
        assert_eq!(Exp::compile(obj).err().unwrap(), Error::ArgCount(1, 2));
    }

    #[test]
    fn let_errors() {
        let obj = LispReader::new("(let (1))").next().unwrap().unwrap();
        assert_eq!(Exp::compile(obj).err().unwrap(), Error::Type(Type::Cons, Type::Int));

        let obj = LispReader::new("(let ((foo 1 2)))").next().unwrap().unwrap();
        assert_eq!(Exp::compile(obj).err().unwrap(), Error::LetValueCount(2));

        let obj = LispReader::new("(let ((foo . 1)))").next().unwrap().unwrap();
        assert_eq!(Exp::compile(obj).err().unwrap(), Error::Type(Type::List, Type::Int));

        let obj = LispReader::new("(let ((foo 1 . 2)))").next().unwrap().unwrap();
        assert_eq!(Exp::compile(obj).err().unwrap(), Error::Type(Type::List, Type::Int));

        let obj = LispReader::new("(let (()))").next().unwrap().unwrap();
        assert_eq!(Exp::compile(obj).err().unwrap(), Error::Type(Type::Cons, Type::Nil));

        let obj = LispReader::new("(let ())").next().unwrap().unwrap();
        assert_eq!(Exp::compile(obj).err().unwrap(), Error::Type(Type::Cons, Type::Nil));

        let obj = LispReader::new("(let)").next().unwrap().unwrap();
        assert_eq!(Exp::compile(obj).err().unwrap(), Error::ArgCount(1, 0));
    }
}
