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

    fn insert(&mut self, obj: LispObj) -> Option<u16> {
        let idx = self.insert_or_get(obj);
        match idx.try_into() {
            Ok(x) => Some(x),
            Err(_) => None,
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

#[derive(Debug, PartialEq)]
struct Exp {
    codes: CodeVec,
    constants: ConstVec,
}

impl Exp {
    pub fn new() -> Self {
        Self{
            codes: CodeVec::new(),
            constants: ConstVec::new(),
        }
    }
}

fn compile_list(obj: &LispObj, exp: &mut Exp) -> u16 {
    if let Some(cons) = obj.as_cons() {
        if let Some(func) = cons.car.as_cons() {
            compile_form(func, exp);
        } else {
            let idx = exp.constants.insert(cons.car).unwrap();
            exp.codes.emit_const(idx);
        }
        1 + compile_list(&cons.cdr, exp)
    } else {
        0
    }
}

fn compile_form(cons: &Cons, exp: &mut Exp) {
    if cons.car.is_symbol() {
        let idx = exp.constants.insert(cons.car).unwrap();
        exp.codes.emit_const(idx);
    }
    let args = compile_list(&cons.cdr, exp);
    exp.codes.emit_call(args);
}

fn compile(obj: LispObj) -> Exp {
    let mut exp = Exp::new();
    if let Some(cons) = obj.as_cons() {
        compile_form(cons, &mut exp);
    }
    else {
        let idx = exp.constants.insert(obj).unwrap();
        exp.codes.emit_const(idx);
    }
    exp
}

pub fn run() {}

#[cfg(test)]
mod test {

    use super::*;
    use crate::reader::LispReader;
    use crate::symbol;

    #[test]
    fn test_number() {
        let obj = LispReader::new("1").next().unwrap().unwrap();
        let expect = Exp{
            codes: CodeVec(vec_into![OpCode::Constant0]),
            constants: ConstVec(vec_into![1]),
        };
        assert_eq!(expect, compile(obj));
    }

    #[test]
    fn function() {
        let obj = LispReader::new("(foo)").next().unwrap().unwrap();
        let expect = Exp{
            codes: CodeVec(vec_into![OpCode::Constant0, OpCode::Call0]),
            constants: ConstVec(vec_into![symbol::intern("foo")]),
        };
        assert_eq!(expect, compile(obj));

        let obj = LispReader::new("(foo 1 2)").next().unwrap().unwrap();
        let expect = Exp{
            codes: CodeVec(vec_into![
                OpCode::Constant0,
                OpCode::Constant1,
                OpCode::Constant2,
                OpCode::Call2]
            ),
            constants: ConstVec(vec_into![symbol::intern("foo"), 1, 2]),
        };
        assert_eq!(expect, compile(obj));

        let obj = LispReader::new("(foo (bar 1))").next().unwrap().unwrap();
        let expect = Exp{
            codes: CodeVec(vec_into![
                OpCode::Constant0,
                OpCode::Constant1,
                OpCode::Constant2,
                OpCode::Call1,
                OpCode::Call1,
            ]
            ),
            constants: ConstVec(vec_into![symbol::intern("foo"), symbol::intern("bar"), 1]),
        };
        assert_eq!(expect, compile(obj));

    }
}
