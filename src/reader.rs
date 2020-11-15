#![allow(dead_code)]

use std::str;
use crate::lisp_object::{LispObj, Cons};
use crate::symbol;

pub struct Stream<'a> {
    prev: str::Chars<'a>,
    iter: str::Chars<'a>,
}

#[derive(Debug)]
pub struct ReadError {
    message: String,
    pos: StreamStart,
}

impl ReadError {
    pub fn new(message: String, stream: &Stream) -> ReadError {
        ReadError{message, pos: stream.get_pos()}
    }
}

#[derive(Copy, Clone, Debug)]
pub struct StreamStart(*const u8);

impl StreamStart {
    fn new(ptr: *const u8) -> Self {
        StreamStart(ptr)
    }

    pub fn get(&self) -> *const u8 {
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

fn symbol_char(chr: char) -> bool {
    match chr {
        '\x00'..=' ' |
        '(' | ')' | '[' | ']' |
        '#' | ',' | '`' | ';' |
        '"' | '\'' => false,
        _ => true,
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

fn read_symbol(stream: &mut Stream) -> LispObj {
    let pos = stream.get_pos();
    while let Some(chr) = stream.next() {
        if chr == '\\' {
            stream.next();
        } else if !symbol_char(chr) {
            stream.back();
            break;
        }
    }
    let slice = stream.slice_till(pos);
    parse_symbol(slice)
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

fn read_string(stream: &mut Stream) -> Result<LispObj, ReadError> {
    let pos = stream.get_pos();
    while let Some(chr) = stream.next() {
        if  chr == '\\' {
            stream.next();
        } else if chr == '"' {
            let slice = stream.slice_without_end_delimiter(pos);
            return Ok(unescape_string(slice));
        }
    }
    Err(ReadError::new("Missing string close quote".to_string(), stream))
}

fn read_cons(stream: &mut Stream) -> Result<LispObj, ReadError> {
    let car = read(stream)?;
    let dot = read_char(stream);
    match dot {
        Some('.') => {
            let cdr = read(stream)?;
            match read_char(stream) {
                Some(')') => {
                    Ok(cons!(car, cdr).into())
                }
                _ => {
                    Err(ReadError::new("Missing Close paren".into(), stream))
                }
            }
        }
        Some(')') => {
            Ok(cons!(car, false).into())
        }
        _ => {
            Err(ReadError::new("Missing Close paren".into(), stream))
        }
    }
}

fn read_char(stream: &mut Stream) -> Option<char> {
    stream.find(|x| !x.is_ascii_whitespace())
}

fn read(stream: &mut Stream) -> Result<LispObj, ReadError> {
    let found = stream.find(|x| !x.is_ascii_whitespace());
    let chr = found.ok_or(ReadError::new("Empty stream".into(), stream))?;
    match chr {
        c if symbol_char(c) => {
            stream.back();
            Ok(read_symbol(stream))
        }
        '"' => read_string(stream),
        '(' => read_cons(stream),
        c => Err(ReadError::new(format!("Unexpected character {}", c), stream))
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
        let start2 = stream.get_pos();
        assert_eq!("", stream.slice_till(start2));
    }

    macro_rules! check_reader {
        ($expect:expr, $compare:expr) => {
            let mut stream = Stream::new($compare);
            assert_eq!(LispObj::from($expect), read(&mut stream).unwrap())
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
        let expect: LispObj = cons!(1, 2).into();
        let mut stream = Stream::new("(1 . 2)");
        let compare = read(&mut stream).unwrap();
        assert_eq!(expect.as_cons().unwrap().car, compare.as_cons().unwrap().car);
        assert_eq!(expect.as_cons().unwrap().cdr, compare.as_cons().unwrap().cdr);
    }
}
