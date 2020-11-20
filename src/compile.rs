#![allow(dead_code)]

use crate::lisp_object::{LispObj, Cons, ConsIter, Type};
use crate::byte_code::OpCode;
use crate::symbol::Symbol;
use std::convert::TryInto;

#[derive(Debug, PartialEq)]
struct ConstVec(Vec<LispObj>);

impl ConstVec {
    pub fn new() -> Self {ConstVec(Vec::new())}

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

    pub fn emit_const(&mut self, idx: u16) {
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

    pub fn emit_call(&mut self, idx: u16) {
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
enum Error {
    ConstOverflow,
    ArgOverflow,
    ArgCount(u16, u16),
    StackSizeOverflow,
    Token,
    Type(Type, Type),
}

#[derive(Debug, PartialEq)]
struct Exp {
    codes: CodeVec,
    constants: ConstVec,
    vars: Vec<&'static Symbol>,
}

fn into_list<'a>(obj: &'a LispObj) -> Result<ConsIter<'a>, Error> {
    match obj.as_cons() {
        Some(cons) => Ok(cons.iter()),
        None => Err(Error::Type(Type::Cons, obj.get_type())),
    }
}

fn verify_end(obj: LispObj, size: u16) -> Result<(), Error> {
    if obj.is_nil() {
        Ok(())
    } else if let Some(cons) = obj.as_cons() {
        let len: u16 = cons.iter().map(|_| 1).sum();
        Err(Error::ArgCount(size, size + len))
    } else {
        Err(Error::Type(Type::Cons, obj.get_type()))
    }
}

fn type_error(obj: LispObj, obj_type: Type) -> Result<(), Error> {
    Err(Error::Type(obj_type, obj.get_type()))
}

fn expect_symbol(obj: &LispObj) -> Result<&'static Symbol, Error> {
    obj.as_symbol().ok_or(Error::Type(Type::Symbol, obj.get_type()))
}

impl Exp {
    fn add_const(&mut self, obj: LispObj) -> Result<(), Error> {
        let idx = self.constants.insert(obj)?;
        Ok(self.codes.emit_const(idx))
    }

    fn quote(&mut self, value: LispObj) -> Result<(), Error> {
        match value.as_cons() {
            Some(cons) => {
                self.add_const(cons.car)?;
                verify_end(cons.cdr, 1)
            }
            None => Err(Error::ArgCount(1, 0)),
        }
    }

    fn let_form(&mut self, form: LispObj) -> Result<(), Error> {
        let prev_len = self.vars.len();
        let mut list = into_list(&form)?;
        self.let_bind(list.next().unwrap())?;
        for sexp in list {
            self.compile_form(sexp)?;
        }
        self.vars.truncate(prev_len);
        Ok(())
    }

    fn let_bind(&mut self, obj: LispObj) -> Result<(), Error> {
        for binding in into_list(&obj)? {
            if let Some(cons) = binding.as_cons() {
                let mut list = cons.iter();
                let var = expect_symbol(&list.next().unwrap())?;
                match list.next() {
                    Some(v) =>  self.add_const(v)?,
                    None =>  self.add_const(LispObj::nil())?,
                };
                self.vars.push(var);
            } else if let Some(var) = binding.as_symbol() {
                self.vars.push(var);
                self.add_const(LispObj::nil())?;
            } else {
                return type_error(binding, Type::Cons);
            }
        }
        Ok(())
    }

    fn compile_list(&mut self, obj: LispObj) -> Result<u16, Error> {
        match obj.as_cons() {
            Some(cons) => {
                self.compile_form(cons.car)?;
                Ok(1 + self.compile_list(cons.cdr)?)
            }
            None => Ok(0)
        }
    }

    fn compile_form(&mut self, obj: LispObj) -> Result<(), Error> {
        if let Some(cons) = obj.as_cons() {
            match expect_symbol(&cons.car)?.get_name() {
                "quote" => self.quote(cons.cdr),
                "let" => self.let_form(cons.cdr),
                _ => {
                    self.add_const(cons.car)?;
                    let args = self.compile_list(cons.cdr)?;
                    self.codes.emit_call(args);
                    Ok(())
                }
            }
        } else if let Some(sym) = obj.as_symbol() {
            match self.vars.iter().rposition(|&x| x == sym) {
                Some(idx) => {
                    match (self.vars.len() - idx).try_into() {
                        Ok(x) => Ok(self.codes.emit_stack_ref(x)),
                        Err(_) => Err(Error::StackSizeOverflow),
                    }
                }
                None => panic!("dynamic variables not implemented"),
            }
        } else {
            self.add_const(obj)
        }
    }

    fn compile(obj: LispObj) -> Result<Self, Error> {
        let mut exp = Self{
            codes: CodeVec::new(),
            constants: ConstVec::new(),
            vars: Vec::new(),
        };
        exp.compile_form(obj)?;
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
            vec_into![Constant0],
            vec_into![1],
        );
        assert_eq!(expect, Exp::compile(obj).unwrap());

        let obj = LispReader::new("'foo").next().unwrap().unwrap();
        let expect = create_expect(
            vec_into![Constant0],
            vec_into![symbol::intern("foo")],
        );
        assert_eq!(expect, Exp::compile(obj).unwrap());

        let obj = LispReader::new("'(1 2)").next().unwrap().unwrap();
        let expect = create_expect(
            vec_into![Constant0],
            vec_into![list!(1, 2)],
        );
        assert_eq!(expect, Exp::compile(obj).unwrap());
    }

    #[test]
    fn variable() {
        let obj = LispReader::new("(let (foo))").next().unwrap().unwrap();
        let expect = create_expect(vec_into![Constant0], vec_into![false]);
        assert_eq!(expect, Exp::compile(obj).unwrap());

        let obj = LispReader::new("(let ((foo 1)(bar 2)(baz 3)))").next().unwrap().unwrap();
        let expect = create_expect(
            vec_into![Constant0, Constant1, Constant2],
            vec_into![1, 2, 3]
        );
        assert_eq!(expect, Exp::compile(obj).unwrap());

        let obj = LispReader::new("(let ((foo 1)) foo)").next().unwrap().unwrap();
        let expect = create_expect(
            vec_into![Constant0, StackRef1],
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
            vec_into![Constant0, Call0],
            vec_into![symbol::intern("foo")],
        );
        assert_eq!(expect, Exp::compile(obj).unwrap());

        let obj = LispReader::new("(foo 1 2)").next().unwrap().unwrap();
        let expect = create_expect(
            vec_into![Constant0, Constant1, Constant2, Call2],
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
            ],
            vec_into![symbol::intern("foo"), symbol::intern("bar"), 1, 2],
        );
        assert_eq!(expect, Exp::compile(obj).unwrap());
    }

    #[test]
    fn errors() {
        let obj = LispReader::new("(\"foo\")").next().unwrap().unwrap();
        assert_eq!(Exp::compile(obj).err().unwrap(), Error::Type(Type::Symbol, Type::String));

        let obj = LispReader::new("(let (1))").next().unwrap().unwrap();
        assert_eq!(Exp::compile(obj).err().unwrap(), Error::Type(Type::Cons, Type::Int));

        let obj = LispReader::new("(quote)").next().unwrap().unwrap();
        assert_eq!(Exp::compile(obj).err().unwrap(), Error::ArgCount(1, 0));

        let obj = LispReader::new("(quote 1 2)").next().unwrap().unwrap();
        assert_eq!(Exp::compile(obj).err().unwrap(), Error::ArgCount(1, 2));
    }
}
