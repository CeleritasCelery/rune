#![allow(dead_code)]
use crate::arena::Arena;
use crate::intern::intern;
use crate::object::{IntoObject, Object, Symbol};
use std::fmt;
use std::str;

pub struct Stream<'a> {
    prev: str::Chars<'a>,
    iter: str::Chars<'a>,
}

#[derive(PartialEq, Debug)]
pub enum Error {
    MissingCloseParen(Option<StreamStart>),
    MissingStringDel(StreamStart),
    UnexpectedChar(char, StreamStart),
    ExtraCloseParen(StreamStart),
    EndOfStream,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::MissingCloseParen(_) => write!(f, "Missing close paren"),
            Error::MissingStringDel(_) => write!(f, "Missing closing string quote"),
            Error::ExtraCloseParen(_) => write!(f, "Extra Closing Paren"),
            Error::UnexpectedChar(chr, _) => write!(f, "Unexpected character {}", chr),
            Error::EndOfStream => write!(f, "End of Stream"),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct LispReaderErr {
    message: String,
    pos: usize,
}

impl LispReaderErr {
    const fn new(message: String, pos: usize) -> Self {
        Self { message, pos }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct StreamStart(*const u8);

impl StreamStart {
    const fn new(ptr: *const u8) -> Self {
        StreamStart(ptr)
    }

    const fn get(self) -> *const u8 {
        self.0
    }
}

impl<'a> Stream<'a> {
    fn new(slice: &str) -> Stream {
        let chars = slice.chars();
        Stream {
            iter: chars.clone(),
            prev: chars,
        }
    }

    fn back(&mut self) {
        self.iter = self.prev.clone();
    }

    fn get_pos(&self) -> StreamStart {
        StreamStart::new(self.iter.as_str().as_ptr())
    }

    fn get_prev_pos(&self) -> StreamStart {
        StreamStart::new(self.prev.as_str().as_ptr())
    }

    fn slice(&self, start: StreamStart) -> &str {
        let ptr = start.get();
        let size = self.iter.as_str().as_ptr() as usize - (ptr as usize);
        unsafe {
            let slice = std::slice::from_raw_parts(ptr, size);
            str::from_utf8_unchecked(slice)
        }
    }

    fn slice_without_end_delimiter(&self, start: StreamStart) -> &str {
        let ptr = start.get();
        let size = self.prev.as_str().as_ptr() as usize - (ptr as usize);
        unsafe {
            let slice = std::slice::from_raw_parts(ptr, size);
            str::from_utf8_unchecked(slice)
        }
    }
}

impl<'a> Iterator for Stream<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        self.prev = self.iter.clone();
        self.iter.next()
    }
}

fn intern_symbol(symbol: &str) -> Symbol {
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
    if symbol.contains('\\') {
        let escaped_slice: String = symbol.chars().filter(is_not_escape).collect();
        intern(escaped_slice.as_str())
    } else {
        intern(symbol)
    }
}

fn parse_symbol<'a>(slice: &str, arena: &'a Arena) -> Object<'a> {
    match slice.parse::<i64>() {
        Ok(num) => num.into_obj(arena),
        Err(_) => match slice.parse::<f64>() {
            Ok(num) => num.into_obj(arena),
            Err(_) => match slice {
                "nil" => Object::nil(),
                "t" => Object::t(),
                _ => intern_symbol(slice).into_obj(arena),
            },
        },
    }
}

// TODO: Handle unicode, hex, and octal escapes
fn unescape_string(string: &str) -> String {
    let mut escaped = false;
    let unescape = |c: char| {
        if escaped {
            escaped = false;
            match c {
                'n' => Some('\n'),
                't' => Some('\t'),
                'r' => Some('\r'),
                '\n' | ' ' => None,
                _ => Some(c),
            }
        } else if c == '\\' {
            escaped = true;
            None
        } else {
            Some(c)
        }
    };
    string.chars().filter_map(unescape).collect::<String>()
}

const fn symbol_char(chr: char) -> bool {
    !matches!(
        chr,
        '\x00'..=' ' | '(' | ')' | '[' | ']' | '#' | ',' | '`' | ';' | '"' | '\''
    )
}

pub struct Reader<'a> {
    slice: &'a str,
    stream: Stream<'a>,
}

impl<'a> Reader<'a> {
    fn get_error_pos(&self, stream_end: StreamStart) -> usize {
        let end = stream_end.get() as usize;
        end - self.slice.as_ptr() as usize
    }

    fn convert_error(&self, err: Error) -> LispReaderErr {
        let message = format!("{}", err);
        match err {
            Error::MissingCloseParen(x) => LispReaderErr::new(
                message,
                self.get_error_pos(x.expect("read should determine open paren position")),
            ),
            Error::ExtraCloseParen(x)
            | Error::MissingStringDel(x)
            | Error::UnexpectedChar(_, x) => LispReaderErr::new(message, self.get_error_pos(x)),
            Error::EndOfStream => {
                LispReaderErr::new(message, 0)
            }
        }
    }
}

impl<'a, 'obj> Reader<'a> {
    fn char_pos(&self) -> StreamStart {
        self.stream.get_prev_pos()
    }

    fn read_symbol(&mut self, arena: &'obj Arena) -> Object<'obj> {
        let pos = self.stream.get_pos();
        while let Some(chr) = self.stream.next() {
            if chr == '\\' {
                self.stream.next();
            } else if !symbol_char(chr) {
                self.stream.back();
                break;
            }
        }
        parse_symbol(self.stream.slice(pos), arena)
    }

    fn read_string(&mut self, arena: &'obj Arena) -> Result<Object<'obj>, Error> {
        let pos = self.stream.get_pos();
        let open_delim_pos = self.char_pos();
        while let Some(chr) = self.stream.next() {
            if chr == '\\' {
                self.stream.next();
            } else if chr == '"' {
                let slice = self.stream.slice_without_end_delimiter(pos);
                let string = unescape_string(slice);
                return Ok(string.into_obj(arena));
            }
        }
        Err(Error::MissingStringDel(open_delim_pos))
    }

    fn read_cons(&mut self, arena: &'obj Arena) -> Result<Object<'obj>, Error> {
        if self.read_char() == Some(')') {
            return Ok(Object::nil());
        } else {
            self.stream.back();
        }
        let car = self.read(arena)?;
        match self.read_char() {
            Some('.') => {
                let cdr = self.read(arena)?;
                match self.read_char() {
                    None => Err(Error::MissingCloseParen(None)),
                    Some(')') => Ok(cons!(car, cdr; arena).into_obj(arena)),
                    Some(c) => Err(Error::UnexpectedChar(c, self.char_pos())),
                }
            }
            Some(')') => Ok(cons!(car; arena).into_obj(arena)),
            Some(_) => {
                self.stream.back();
                let rest = self.read_cons(arena)?;
                Ok(cons!(car, rest; arena).into_obj(arena))
            }
            None => Err(Error::MissingCloseParen(None)),
        }
    }

    fn read_list(&mut self, arena: &'obj Arena) -> Result<Object<'obj>, Error> {
        let pos = self.char_pos();
        match self.read_cons(arena) {
            Err(Error::MissingCloseParen(None)) => Err(Error::MissingCloseParen(Some(pos))),
            x => x,
        }
    }

    fn read_quote(&mut self, arena: &'obj Arena) -> Result<Object<'obj>, Error> {
        let obj = self.read(arena)?;
        let quoted = list!(intern("quote"), obj; arena);
        Ok(quoted.into_obj(arena))
    }

    fn read_char(&mut self) -> Option<char> {
        let mut in_comment = false;
        let valid_char = |chr: &char| {
            if in_comment {
                if *chr == '\n' {
                    in_comment = false;
                }
                false
            } else if chr.is_ascii_whitespace() {
                false
            } else if *chr == ';' {
                in_comment = true;
                false
            } else {
                true
            }
        };
        self.stream.find(valid_char)
    }

    fn read(&mut self, arena: &'obj Arena) -> Result<Object<'obj>, Error> {
        match self.read_char().ok_or(Error::EndOfStream)? {
            '"' => self.read_string(arena),
            '(' => self.read_list(arena),
            '\'' => self.read_quote(arena),
            ')' => Err(Error::ExtraCloseParen(self.char_pos())),
            c if symbol_char(c) => {
                self.stream.back();
                Ok(self.read_symbol(arena))
            }
            c => Err(Error::UnexpectedChar(c, self.char_pos())),
        }
    }

    pub fn read_into(slice: &'a str, arena: &'obj Arena) -> Result<Object<'obj>, LispReaderErr> {
        let mut reader = Reader {
            slice,
            stream: Stream::new(slice),
        };
        match reader.read(arena) {
            Ok(x) => Ok(x),
            Err(e) => Err(reader.convert_error(e)),
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
        assert_eq!("", stream.slice(start));
        stream.next();
        stream.next();
        assert_eq!("fo", stream.slice(start));
        stream.next();
        assert_eq!("fox", stream.slice(start));
        assert_eq!("fo", stream.slice_without_end_delimiter(start));
        let start2 = stream.get_pos();
        assert_eq!("", stream.slice(start2));
    }

    macro_rules! check_reader {
        ($expect:expr, $compare:expr) => {
            let arena = &Arena::new();
            let obj: Object = $expect.into_obj(arena);
            assert_eq!(obj, Reader::read_into($compare, &arena).unwrap())
        };
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
    fn read_bool() {
        check_reader!(false, "nil");
        check_reader!(false, "()");
        check_reader!(true, "t");
    }

    #[test]
    fn test_read_symbol() {
        check_reader!(intern("foo"), "foo");
        check_reader!(intern("--1"), "--1");
        check_reader!(intern("1"), "\\1");
        check_reader!(intern("3.0.0"), "3.0.0");
        check_reader!(intern("1+"), "1+");
        check_reader!(intern("+1"), "\\+1");
        check_reader!(intern(" x"), "\\ x");
        check_reader!(intern("\\x"), "\\\\x");
        check_reader!(intern("(* 1 2)"), "\\(*\\ 1\\ 2\\)");
        check_reader!(intern("+-*/_~!@$%^&=:<>{}"), "+-*/_~!@$%^&=:<>{}");
    }

    #[test]
    fn test_read_string() {
        check_reader!("foo", r#""foo""#);
        check_reader!("foo bar", r#""foo bar""#);
        check_reader!("foo\nbar\t\r", r#""foo\nbar\t\r""#);
        check_reader!(
            "foobarbaz",
            r#""foo\ bar\
baz""#
        );
    }

    #[test]
    fn test_read_cons() {
        let arena = &Arena::new();
        check_reader!(false, "()");
        check_reader!(cons!(1, 2; arena), "(1 . 2)");
        check_reader!(list!(1; arena), "(1)");
        check_reader!(list!("foo"; arena), "(\"foo\")");
        check_reader!(
            cons!(1, cons!(1.5, "foo"; arena); arena),
            "(1 1.5 . \"foo\")"
        );
        check_reader!(list!(1, 1.5; arena), "(1 1.5)");
        check_reader!(list!(1, 1.5, -7; arena), "(1 1.5 -7)");
    }

    #[test]
    fn read_quote() {
        let arena = &Arena::new();
        let quote = intern("quote");
        check_reader!(list!(quote, intern("foo"); arena), "(quote foo)");
        check_reader!(list!(quote, intern("foo"); arena), "'foo");
        check_reader!(list!(quote, list!(1, 2, 3; arena); arena), "'(1 2 3)");
    }

    fn assert_error(input: &str, pos: usize, error: Error) {
        let arena = &Arena::new();
        let result = Reader::read_into(input, arena).err().unwrap();
        assert_eq!(result.pos, pos);
        assert_eq!(result.message, format!("{}", error));
    }

    #[test]
    fn error() {
        use Error::*;
        let arena = &Arena::new();
        assert!((Reader::read_into("", arena).is_err()));
        let null = StreamStart::new(std::ptr::null());
        assert_error(" (1 2", 1, MissingCloseParen(Some(null)));
        assert_error(" \"foo", 1, MissingStringDel(null));
        assert_error("(1 2 . 3 4)", 9, UnexpectedChar('4', null));
        assert_error(")", 0, ExtraCloseParen(null));
    }

    #[test]
    fn comments() {
        assert_error(" ; comment ", 0, Error::EndOfStream);
        check_reader!(1, "; comment \n  1");
    }
}
