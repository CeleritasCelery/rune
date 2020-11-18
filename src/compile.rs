#![allow(dead_code)]

use crate::lisp_object::{LispObj, Cons};
use crate::byte_code::OpCode;
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
}

#[derive(Debug)]
enum Error {
    ConstOverflow,
    ArgOverflow,
    UnexpectedToken,
}

#[derive(Debug, PartialEq)]
struct Exp {
    codes: CodeVec,
    constants: ConstVec,
}

impl Exp {
    fn add_const(&mut self, obj: LispObj) -> Result<(), Error> {
        let idx = self.constants.insert(obj)?;
        self.codes.emit_const(idx);
        Ok(())
    }

    fn quote(&mut self, value: LispObj) -> Result<(), Error> {
        match value.as_cons() {
            Some(cons) => self.add_const(cons.car),
            None => Err(Error::UnexpectedToken),
        }
    }

    fn compile_list(&mut self, obj: &LispObj) -> Result<u16, Error> {
        if let Some(cons) = obj.as_cons() {
            if let Some(func) = cons.car.as_cons() {
                self.compile_form(func)?;
            } else {
                self.add_const(cons.car)?;
            }
            Ok(1 + self.compile_list(&cons.cdr)?)
        } else {
            Ok(0)
        }
    }

    fn compile_form(&mut self, cons: &Cons) -> Result<(), Error> {
        if let Some(sym) = cons.car.as_symbol() {
            match sym.get_name() {
                "quote" => self.quote(cons.cdr),
                _ => {
                    self.add_const(cons.car)?;
                    let args = self.compile_list(&cons.cdr)?;
                    self.codes.emit_call(args);
                    Ok(())
                }
            }
        } else {
            Err(Error::UnexpectedToken)
        }
    }

    fn compile(obj: LispObj) -> Result<Self, Error> {
        let mut exp = Self{
            codes: CodeVec::new(),
            constants: ConstVec::new(),
        };
        if let Some(cons) = obj.as_cons() {
            exp.compile_form(cons)?;
        } else {
            exp.add_const(obj)?;
        }
        Ok(exp)
    }
}

pub fn run() {}

#[cfg(test)]
mod test {

    use super::*;
    use crate::reader::LispReader;
    use crate::symbol;

    #[test]
    fn test_basic() {
        let obj = LispReader::new("1").next().unwrap().unwrap();
        let expect = Exp{
            codes: CodeVec(vec_into![OpCode::Constant0]),
            constants: ConstVec(vec_into![1]),
        };
        assert_eq!(expect, Exp::compile(obj).unwrap());

        let obj = LispReader::new("'foo").next().unwrap().unwrap();
        let expect = Exp{
            codes: CodeVec(vec_into![OpCode::Constant0]),
            constants: ConstVec(vec_into![symbol::intern("foo")]),
        };
        assert_eq!(expect, Exp::compile(obj).unwrap());

        let obj = LispReader::new("'(1 2)").next().unwrap().unwrap();
        let expect = Exp{
            codes: CodeVec(vec_into![OpCode::Constant0]),
            constants: ConstVec(vec_into![list!(1, 2)]),
        };
        assert_eq!(expect, Exp::compile(obj).unwrap());
    }

    #[test]
    fn function() {
        let obj = LispReader::new("(foo)").next().unwrap().unwrap();
        let expect = Exp{
            codes: CodeVec(vec_into![OpCode::Constant0, OpCode::Call0]),
            constants: ConstVec(vec_into![symbol::intern("foo")]),
        };
        assert_eq!(expect, Exp::compile(obj).unwrap());

        let obj = LispReader::new("(foo 1 2)").next().unwrap().unwrap();
        let expect = Exp{
            codes: CodeVec(vec_into![
                OpCode::Constant0,
                OpCode::Constant1,
                OpCode::Constant2,
                OpCode::Call2,
            ]),
            constants: ConstVec(vec_into![symbol::intern("foo"), 1, 2]),
        };
        assert_eq!(expect, Exp::compile(obj).unwrap());

        let obj = LispReader::new("(foo (bar 1) 2)").next().unwrap().unwrap();
        let expect = Exp{
            codes: CodeVec(vec_into![
                OpCode::Constant0,
                OpCode::Constant1,
                OpCode::Constant2,
                OpCode::Call1,
                OpCode::Constant3,
                OpCode::Call2,
            ]),
            constants: ConstVec(vec_into![symbol::intern("foo"), symbol::intern("bar"), 1, 2]),
        };
        assert_eq!(expect, Exp::compile(obj).unwrap());
    }
}
