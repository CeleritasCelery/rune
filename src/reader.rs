#![allow(dead_code)]

use std::str;
use std::fmt;
use crate::lisp_object::{LispObj, Cons};
use crate::symbol;

pub struct Stream<'a> {
    prev: str::Chars<'a>,
    iter: str::Chars<'a>,
}

#[derive(PartialEq, Debug)]
pub enum ReadErr {
    MissingCloseParen(Option<StreamStart>),
    MissingStringDel(StreamStart),
    UnexpectedChar(char, StreamStart),
    EndOfStream,
}

impl fmt::Display for ReadErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ReadErr::*;
        match self {
            MissingCloseParen(_) => write!(f, "Missing close paren"),
            MissingStringDel(_) => write!(f, "Missing closing string quote"),
            UnexpectedChar(chr, _) => write!(f, "Unexpected character {}", chr),
            EndOfStream => write!(f, "End of Stream"),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct LispReaderErr {
    message: String,
    pos: usize,
}

impl LispReaderErr {
    fn new(message: String, pos: usize) -> Self {
        Self{message, pos}
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct StreamStart(*const u8);

impl StreamStart {
    fn new(ptr: *const u8) -> Self {
        StreamStart(ptr)
    }

    fn get(&self) -> *const u8 {
        self.0
    }
}

impl<'a> Stream<'a> {
    pub fn new(slice: &str) -> Stream {
        let chars = slice.chars();
        Stream{iter: chars.clone(), prev: chars}
    }

    pub fn peek(&mut self) -> Option<char> {
        self.iter.clone().next()
    }

    pub fn back(&mut self) {
        self.iter = self.prev.clone();
    }

    pub fn get_pos(&self) -> StreamStart {
        StreamStart::new(self.iter.as_str().as_ptr())
    }

    pub fn get_prev(&self) -> StreamStart {
        StreamStart::new(self.prev.as_str().as_ptr())
    }

    pub fn slice_till(&self, start: StreamStart) -> &str {
        let ptr = start.get();
        let size = self.iter.as_str().as_ptr() as usize - (ptr as usize);
        unsafe {
            let slice = std::slice::from_raw_parts(ptr, size);
            str::from_utf8_unchecked(slice)
        }
    }

    pub fn slice_without_end_delimiter(&self, start: StreamStart) -> &str {
        let ptr = start.get();
        let size = self.prev.as_str().as_ptr() as usize - (ptr as usize);
        unsafe {
            let slice = std::slice::from_raw_parts(ptr, size);
            str::from_utf8_unchecked(slice)
        }
    }

    pub fn pos(&self) -> usize {
        self.iter.as_str().as_ptr() as usize
    }
}

impl<'a> Iterator for Stream<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        self.prev = self.iter.clone();
        self.iter.next()
    }
}

fn intern_symbol(symbol: &str) -> LispObj {
    let mut escaped = false;
    let is_not_escape = |c: &char| {
        if escaped {
            escaped = false;
            true
        } else if *c == '\\' {
            escaped = true;
            false
        } else {
            true
        }
    };
    if symbol.contains("\\") {
        let escaped_slice: String = symbol.chars().filter(is_not_escape).collect();
        symbol::intern(escaped_slice.as_str()).into()
    } else {
        symbol::intern(symbol).into()
    }
}

fn parse_symbol(slice: &str) -> LispObj {
    match slice.parse::<i64>() {
        Ok(num) => num.into(),
        Err(_) => {
            match slice.parse::<f64>() {
                Ok(num) => num.into(),
                Err(_) => intern_symbol(slice),
            }
        },
    }
}

// TODO: Handle unicode, hex, and octal escapes
fn unescape_string(string: &str) -> LispObj {
    let mut escaped = false;
    let unescape = |c: char| {
        if escaped {
            escaped = false;
            match c {
                'n' => Some('\n'),
                't' => Some('\t'),
                'r' => Some('\r'),
                '\n' => None,
                ' ' => None,
                _ => Some(c),
            }
        } else if c == '\\' {
            escaped = true;
            None
        } else {
            Some(c)
        }
    };
    if string.contains("\\") {
        let unescaped_string: String = string.chars().filter_map(unescape).collect();
        unescaped_string.as_str().into()
    } else {
        string.into()
    }
}

fn symbol_char(chr: char) -> bool {
    match chr {
        '\x00'..=' ' |
        '(' | ')' | '[' | ']' |
        '#' | ',' | '`' | ';' |
        '"' | '\'' => false,
        _ => true,
    }
}

struct LispReader<'a> {
    slice: &'a str,
    stream: Stream<'a>,
}

impl<'a> LispReader<'a> {
    pub fn new(slice: &'a str) -> Self {
        LispReader{slice, stream: Stream::new(slice)}
    }

    fn get_error_pos(&self, stream_end: StreamStart) -> usize {
        let end = stream_end.get() as usize;
        end - self.slice.as_ptr() as usize
    }
}

impl<'a> Iterator for LispReader<'a> {
    type Item = Result<LispObj, LispReaderErr>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.read() {
            Err(ReadErr::EndOfStream) => {
                None
            }
            Err(ReadErr::MissingCloseParen(x))  => {
                Some(Err(LispReaderErr::new("foo".to_string(),
                                            self.get_error_pos(x.unwrap()))))
            }
            Err(ReadErr::MissingStringDel(x))  => {
                Some(Err(LispReaderErr::new("foo".to_string(), self.get_error_pos(x))))
            }
            Err(ReadErr::UnexpectedChar(c, x))  => {
                Some(Err(LispReaderErr::new(format!("{}", c), self.get_error_pos(x))))
            }
            Ok(x) => Some(Ok(x))
        }
    }
}

impl<'a> LispReader<'a> {

    fn read_symbol(&mut self) -> LispObj {
        let pos = self.stream.get_pos();
        while let Some(chr) = self.stream.next() {
            if chr == '\\' {
                self.stream.next();
            } else if !symbol_char(chr) {
                self.stream.back();
                break;
            }
        }
        let slice = self.stream.slice_till(pos);
        parse_symbol(slice)
    }


    fn read_string(&mut self) -> Result<LispObj, ReadErr> {
        let pos = self.stream.get_pos();
        let prev = self.stream.get_prev();
        while let Some(chr) = self.stream.next() {
            if  chr == '\\' {
                self.stream.next();
            } else if chr == '"' {
                let slice = self.stream.slice_without_end_delimiter(pos);
                return Ok(unescape_string(slice));
            }
        }
        Err(ReadErr::MissingStringDel(prev))
    }

    fn read_cons(&mut self) -> Result<LispObj, ReadErr> {
        if self.read_char() == Some(')') {
            return Ok(LispObj::nil())
        } else {
            self.stream.back();
        }
        let car = self.read()?;
        match self.read_char() {
            Some('.') => {
                let cdr = self.read()?;
                match self.read_char() {
                    None => Err(ReadErr::MissingCloseParen(None)),
                    Some(')') => Ok(cons!(car, cdr).into()),
                    Some(c) => Err(ReadErr::UnexpectedChar(c, self.stream.get_prev())),
                }
            }
            Some(')') => Ok(cons!(car).into()),
            Some(_) => {
                self.stream.back();
                let rest = self.read_cons()?;
                Ok(cons!(car, rest).into())
            }
            None => Err(ReadErr::MissingCloseParen(None)),
        }
    }

    fn read_quote(&mut self) -> Result<LispObj, ReadErr> {
        let obj = self.read()?;
        Ok(list!(symbol::intern("quote"), obj).into())
    }

    fn read_char(&mut self) -> Option<char> {
        self.stream.find(|x| !x.is_ascii_whitespace())
    }

    fn read(&mut self) -> Result<LispObj, ReadErr> {
        let found = self.read_char();
        let chr = found.ok_or(ReadErr::EndOfStream)?;
        match chr {
            '"' => self.read_string(),
            '(' => {
                let pos = self.stream.get_prev();
                match self.read_cons() {
                    Err(ReadErr::MissingCloseParen(None)) => {
                        Err(ReadErr::MissingCloseParen(Some(pos)))
                    }
                    x => x
                }
            },
            '\'' => self.read_quote(),
            c if symbol_char(c) => {
                self.stream.back();
                Ok(self.read_symbol())
            }
            c => Err(ReadErr::UnexpectedChar(c, self.stream.get_prev()))
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn stream() {
        let mut stream = Stream::new("fox");
        assert_eq!('f', stream.next().unwrap());
        assert_eq!('o', stream.peek().unwrap());
        assert_eq!('o', stream.next().unwrap());
        assert_eq!('x', stream.next().unwrap());
        stream.back();
        stream.back();
        assert_eq!('x', stream.next().unwrap());
        assert_eq!(None, stream.next());
    }

    #[test]
    fn stream_slice() {
        let mut stream = Stream::new("fox");
        let start = stream.get_pos();
        assert_eq!("", stream.slice_till(start));
        stream.next();
        stream.next();
        assert_eq!("fo", stream.slice_till(start));
        stream.next();
        assert_eq!("fox", stream.slice_till(start));
        assert_eq!("fo", stream.slice_without_end_delimiter(start));
        let start2 = stream.get_pos();
        assert_eq!("", stream.slice_till(start2));
    }

    macro_rules! check_reader {
        ($expect:expr, $compare:expr) => {
            let mut reader = LispReader::new($compare);
            assert_eq!(LispObj::from($expect), reader.next().unwrap().unwrap())
        }
    }

    #[test]
    fn test_read_number() {
        check_reader!(5, "5");
        check_reader!(49, "49");
        check_reader!(-105, "-105");
        check_reader!(1.5, "1.5");
        check_reader!(-3.0, "-3.0");
        check_reader!(1, "+1");
        check_reader!(1, "001");
    }

    #[test]
    fn test_read_symbol() {
        check_reader!(symbol::intern("foo"), "foo");
        check_reader!(symbol::intern("--1"), "--1");
        check_reader!(symbol::intern("1"), "\\1");
        check_reader!(symbol::intern("3.0.0"), "3.0.0");
        check_reader!(symbol::intern("1+"), "1+");
        check_reader!(symbol::intern("+1"), "\\+1");
        check_reader!(symbol::intern(" x"), "\\ x");
        check_reader!(symbol::intern("\\x"), "\\\\x");
        check_reader!(symbol::intern("(* 1 2)"), "\\(*\\ 1\\ 2\\)");
        check_reader!(symbol::intern("+-*/_~!@$%^&=:<>{}"), "+-*/_~!@$%^&=:<>{}");
    }

    #[test]
    fn test_read_string() {
        check_reader!("foo", r#""foo""#);
        check_reader!("foo bar", r#""foo bar""#);
        check_reader!("foo\nbar\t\r", r#""foo\nbar\t\r""#);
        check_reader!("foobarbaz", r#""foo\ bar\
baz""#);
    }

    #[test]
    fn test_read_cons() {
        check_reader!(false, "()");
        check_reader!(cons!(1, 2), "(1 . 2)");
        check_reader!(list!(1), "(1)");
        check_reader!(list!("foo"), "(\"foo\")");
        check_reader!(cons!(1, cons!(1.5, "foo")), "(1 1.5 . \"foo\")");
        check_reader!(list!(1, 1.5), "(1 1.5)");
        check_reader!(list!(1, 1.5, -7), "(1 1.5 -7)");
    }

    #[test]
    fn read_quote() {
        let quote = symbol::intern("quote");
        check_reader!(list!(quote, symbol::intern("foo")), "(quote foo)");
        check_reader!(list!(quote, symbol::intern("foo")), "'foo");
        check_reader!(list!(quote, list!(1, 2, 3)), "'(1 2 3)");
    }

    #[test]
    fn error() {
        let mut reader = LispReader::new(" (1 2");
        assert_eq!(reader.next().unwrap().err().unwrap().pos, 1);
        let mut reader = LispReader::new(" \"foo");
        assert_eq!(reader.next().unwrap().err().unwrap().pos, 1);
        let mut reader = LispReader::new("(1 2 . 3 4)");
        assert_eq!(reader.next().unwrap().err().unwrap().pos, 9);
        let mut reader = LispReader::new("");
        assert!(reader.next().is_none());
    }
}
