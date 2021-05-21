#![allow(dead_code)]
use crate::arena::Arena;
use crate::intern::intern;
use crate::object::{IntoObject, Object, Symbol};
use std::fmt;
use std::str;

#[derive(PartialEq, Debug)]
pub enum Error {
    MissingCloseParen(usize),
    MissingStringDel(usize),
    UnexpectedChar(char, usize),
    ExtraCloseParen(usize),
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

impl Error {
    const fn position(&self) -> usize {
        match self {
            Error::MissingCloseParen(x) => *x,
            Error::MissingStringDel(x) => *x,
            Error::UnexpectedChar(_, x) => *x,
            Error::ExtraCloseParen(x) => *x,
            Error::EndOfStream => 0,
        }
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

fn escaped(escaped: &mut bool, chr: char) -> bool {
    if *escaped {
        *escaped = false;
        true
    } else if chr == '\\' {
        *escaped = true;
        true
    } else {
        false
    }
}

pub struct Reader<'a> {
    slice: &'a str,
    pos: usize,
}

impl<'a, 'obj> Reader<'a> {
    fn take_slice(&mut self, beg: usize, len: Option<usize>) -> &str {
        match len {
            Some(len) => {
                self.pos = beg + len;
                &self.slice[beg..self.pos]
            }
            None => {
                self.pos = self.slice.len();
                &self.slice[beg..]
            }
        }
    }

    fn back_by(&self, chr: char) -> usize {
        self.pos - chr.len_utf8()
    }

    fn forward_by(&self, chr: char) -> usize {
        self.pos + chr.len_utf8()
    }

    fn read_symbol(&mut self, arena: &'obj Arena) -> Object<'obj> {
        let mut skip = false;
        let slice = &self.slice[self.pos..];
        let idx = slice.find(|c| !escaped(&mut skip, c) && !symbol_char(c));
        parse_symbol(self.take_slice(self.pos, idx), arena)
    }

    fn read_string(&mut self, delim: usize, arena: &'obj Arena) -> Result<Object<'obj>, Error> {
        let slice = &self.slice[self.pos..];
        let mut skip = false;
        let idx = slice.find(|c| !escaped(&mut skip, c) && c == '"');
        match idx {
            Some(i) => {
                let slice = self.take_slice(self.pos, Some(i));
                let string = unescape_string(slice);
                // skip over end delimiter
                self.pos = self.forward_by('"');
                Ok(string.into_obj(arena))
            }
            None => Err(Error::MissingStringDel(delim)),
        }
    }

    fn read_list(&mut self, delim: usize, arena: &'obj Arena) -> Result<Object<'obj>, Error> {
        match self.read_char() {
            Some(')') => return Ok(Object::nil()),
            Some(c) => {self.pos = self.back_by(c);},
            None => {},
        }
        let car = self.read(arena)?;
        match self.read_char() {
            Some('.') => {
                let cdr = self.read(arena)?;
                match self.read_char() {
                    None => Err(Error::MissingCloseParen(delim)),
                    Some(')') => Ok(cons!(car, cdr; arena).into_obj(arena)),
                    Some(c) => Err(Error::UnexpectedChar(c, self.back_by(c))),
                }
            }
            Some(')') => Ok(cons!(car; arena).into_obj(arena)),
            Some(c) => {
                self.pos = self.back_by(c);
                // This could overflow with a very long list
                let rest = self.read_list(delim, arena)?;
                Ok(cons!(car, rest; arena).into_obj(arena))
            }
            None => Err(Error::MissingCloseParen(delim)),
        }
    }

    fn read_quote(&mut self, arena: &'obj Arena) -> Result<Object<'obj>, Error> {
        let obj = self.read(arena)?;
        let quoted = list!(intern("quote"), obj; arena);
        Ok(quoted.into_obj(arena))
    }

    fn read_char(&mut self) -> Option<char> {
        let mut in_comment = false;
        let valid_char = |(_, chr): &(usize, char)| {
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
        let idx = self.slice[self.pos..].char_indices().find(valid_char);
        idx.map(|(idx, chr)| {
            self.pos += idx + chr.len_utf8();
            chr
        })
    }

    fn read(&mut self, arena: &'obj Arena) -> Result<Object<'obj>, Error> {
        let chr = self.read_char().ok_or(Error::EndOfStream)?;
        match chr {
            '"' => self.read_string(self.back_by(chr), arena),
            '(' => self.read_list(self.back_by(chr), arena),
            '\'' => self.read_quote(arena),
            ')' => Err(Error::ExtraCloseParen(self.back_by(chr))),
            chr if symbol_char(chr) => {
                self.pos = self.back_by(chr);
                Ok(self.read_symbol(arena))
            }
            chr => Err(Error::UnexpectedChar(chr, self.back_by(chr))),
        }
    }

    pub fn read_into(slice: &'a str, arena: &'obj Arena) -> Result<Object<'obj>, Error> {
        let mut reader = Reader {
            slice,
            pos: 0,
        };
        reader.read(arena)
    }
}

#[cfg(test)]
mod test {
    use super::*;

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

    fn assert_error(input: &str, error: Error) {
        let arena = &Arena::new();
        let result = Reader::read_into(input, arena).err().unwrap();
        assert_eq!(result, error);
    }

    #[test]
    fn reader_error() {
        use Error::*;
        let arena = &Arena::new();
        assert!((Reader::read_into("", arena).is_err()));
        assert_error(" (1 2", MissingCloseParen(1));
        assert_error(" \"foo", MissingStringDel(1));
        assert_error("(1 2 . 3 4)", UnexpectedChar('4', 9));
        assert_error(")", ExtraCloseParen(0));
    }

    #[test]
    fn comments() {
        assert_error(" ; comment ", Error::EndOfStream);
        check_reader!(1, "; comment \n  1");
    }
}
