#![allow(dead_code)]
use crate::lex::{Lexer, Token};
use crate::lisp_object::LispObj;
use crate::symbol::INTERNED_SYMBOLS;
use std::convert::TryInto;
use crate::byte_code::OpCode;

struct ParserState<'a> {
    string: &'a str,
    lexer: Lexer<'a>,
}

#[derive(Debug)]
struct ParserError {
    message: String,
    pos: usize,
}

#[derive(Debug, PartialEq)]
struct ConstVec(Vec<LispObj>);
#[derive(Debug, PartialEq)]
struct CodeVec(Vec<u8>);

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


impl ConstVec {
    pub fn new() -> Self {ConstVec(Vec::new())}

    fn insert_or_get<T>(&mut self, obj: T) -> usize
    where T: PartialEq<LispObj> + Into<LispObj> {
        match self.0.iter().position(|&x| obj == x) {
            None => {
                self.0.push(obj.into());
                self.0.len() - 1
            }
            Some(x) => x,
        }
    }

    pub fn insert(&mut self, token: Token) -> Result<u16, ParserError> {
        let idx = match token {
            Token::Symbol(x) => {
                let mut map = INTERNED_SYMBOLS.lock().unwrap();
                let obj = LispObj::from(map.intern(x));
                self.insert_or_get(obj)
            }
            Token::Integer(x) => {
                let i = match x.parse::<i64>() {
                    Ok(i) => i,
                    Err(_) => { panic!("failed to parse {} to integer", x); }
                };
                self.insert_or_get(i)
            }
            Token::Float(x) => {
                let f = match x.parse::<f64>() {
                    Ok(i) => i,
                    Err(_) => { panic!("failed to parse {} to Float", x); }
                };
                self.insert_or_get(f)
            }
            Token::String(x) => {
                self.insert_or_get(x)
            }
            x => { panic!("can't create constant from {}", x); }
        };
        match idx.try_into() {
            Ok(x) => Ok(x),
            Err(_) => Err(ParserError::new(format!("constant vector overflowed max size of {}", std::u16::MAX), 0)),
        }
    }
}


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

impl ParserError {
    fn new(message: String, pos: usize) -> ParserError {
        ParserError{message, pos}
    }
}

impl<'a> ParserState<'a> {
    pub fn new(form: &'a str) -> Self {
        Self{ string: form, lexer: Lexer::new(form) }
    }

    fn get_pos(&self, ptr: *const u8) -> usize {
        ptr as usize - self.string.as_ptr() as usize
    }

    fn dispatch_func(&mut self, mut exp: Exp) -> Result<Exp, ParserError> {
        match self.lexer.next() {
            Some(token) => {
                match token {
                    Token::Symbol(_) => {
                        let idx = exp.constants.insert(token)?;
                        exp.codes.emit_const(idx);
                        self.parse_args(exp)
                    }
                    _ => Err(ParserError::new(format!("expected symbol got {}", token), self.get_pos(token.start()))),
                }
            }
            None => Err(ParserError::new("missing close paren".to_string(), 0)),
        }
    }

    fn parse_args(&mut self, mut exp: Exp) -> Result<Exp, ParserError> {
        let mut arg_count = 0;
        while let Some(token) = self.lexer.next() {
            match token {
                Token::Integer(_) | Token::Float(_) | Token::String(_) => {
                    let idx = exp.constants.insert(token)?;
                    exp.codes.emit_const(idx);
                    arg_count += 1;
                }
                Token::OpenParen(_) => {
                    exp = self.dispatch_func(exp)?;
                }
                Token::CloseParen(_) => {
                    exp.codes.emit_call(arg_count);
                    return Ok(exp);
                }
                _ => {
                    panic!("unimplemented token: {}", token)
                }
            }
        }
        Err(ParserError::new("missing close paren".to_string(), 0))
    }
}

impl<'a> Iterator for ParserState<'a> {
    type Item = Result<Exp, ParserError>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.lexer.next()? {
            Token::OpenParen(paren) => {
                let exp = self.dispatch_func(Exp::new());
                match exp {
                    Ok(_) => Some(exp),
                    Err(mut e) => {
                        if e.pos == 0 {
                            e.pos = self.get_pos(paren.as_ptr())
                        }
                        Some(Err(e))
                    }
                }
            }
            _ => None,
        }
    }
}

pub fn run() {
    let parser = ParserState::new("(func 1 (sub 0 -5)) (foo) (bob -0.5)");

    for sexp in parser {
        println!("{:?}", sexp.unwrap());

    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn basic() {
        let (func, sub) = {
            let mut map = INTERNED_SYMBOLS.lock().unwrap();
            (map.intern("func"), map.intern("sub"))
        };
        let mut parser = ParserState::new("(func 1 (sub 0 -0.5))");
        let golden = Exp{
            codes: CodeVec(
                vec_into![
                    OpCode::Constant0,
                    OpCode::Constant1,
                    OpCode::Constant2,
                    OpCode::Constant3,
                    OpCode::Constant4,
                    OpCode::Call2,
                    OpCode::Call1,
                ]),
            constants: ConstVec(
                vec_into![func, 1, sub, 0, -0.5])};

        assert_eq!(golden, parser.next().unwrap().unwrap());
    }
}
