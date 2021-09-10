use crate::arena::Arena;
use crate::fns;
use crate::object::{IntoObject, Object};
use crate::symbol::{intern, Symbol};
use std::str;
use std::{fmt, iter::Peekable, str::CharIndices};

#[derive(PartialEq, Debug)]
pub(crate) enum Error {
    MissingCloseParen(usize),
    MissingCloseBracket(usize),
    MissingStringDel(usize),
    UnexpectedChar(char, usize),
    ExtraItemInCdr(usize),
    // Emacs does not have this error. If the reader is given '(1 .) it will
    // translate that to '(1 \.)
    MissingCdr(usize),
    ExtraCloseParen(usize),
    ExtraCloseBracket(usize),
    MissingQuotedItem(usize),
    UnknownMacroCharacter(char, usize),
    InvalidRadix(u8, usize),
    EmptyStream,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::MissingCloseParen(i) => write!(f, "Missing close paren: at {}", i),
            Error::MissingCloseBracket(i) => write!(f, "Missing close bracket: at {}", i),
            Error::MissingStringDel(i) => write!(f, "Missing closing string quote: at {}", i),
            Error::ExtraCloseParen(i) => write!(f, "Extra Closing paren: at {}", i),
            Error::ExtraCloseBracket(i) => write!(f, "Extra Closing brace: at {}", i),
            Error::UnexpectedChar(chr, i) => write!(f, "Unexpected character {}: at {}", chr, i),
            Error::EmptyStream => write!(f, "Empty Stream"),
            Error::ExtraItemInCdr(i) => write!(f, "Extra item in cdr: at {}", i),
            Error::MissingCdr(i) => write!(f, "Missing cdr: at {}", i),
            Error::MissingQuotedItem(i) => write!(f, "Missing element after quote: at {}", i),
            Error::InvalidRadix(radix, i) => {
                write!(f, "invalid character for radix {}: at {}", radix, i)
            }
            Error::UnknownMacroCharacter(chr, i) => {
                write!(f, "Unkown reader macro character {}: at {}", chr, i)
            }
        }
    }
}

impl std::error::Error for Error {}

impl Error {
    const fn position(&self) -> usize {
        match self {
            Error::MissingQuotedItem(x)
            | Error::MissingCloseParen(x)
            | Error::MissingCloseBracket(x)
            | Error::MissingStringDel(x)
            | Error::ExtraCloseParen(x)
            | Error::ExtraCloseBracket(x)
            | Error::ExtraItemInCdr(x)
            | Error::MissingCdr(x)
            | Error::UnexpectedChar(_, x)
            | Error::InvalidRadix(_, x)
            | Error::UnknownMacroCharacter(_, x) => *x,
            Error::EmptyStream => 0,
        }
    }
}

#[derive(PartialEq, Debug, Copy, Clone)]
enum Token<'a> {
    OpenParen(usize),
    CloseParen(usize),
    OpenBracket(usize),
    CloseBracket(usize),
    Quote(usize),
    Backquote(usize),
    Unquote(usize),
    Splice(usize),
    Sharp(usize),
    Dot(usize),
    Ident(&'a str),
    String(&'a str),
    Error(TokenError),
}

#[derive(PartialEq, Debug, Copy, Clone)]
enum TokenError {
    MissingStringDel(usize),
    UnexpectedChar(char, usize),
}

impl From<TokenError> for Error {
    fn from(e: TokenError) -> Self {
        match e {
            TokenError::MissingStringDel(i) => Error::MissingStringDel(i),
            TokenError::UnexpectedChar(c, i) => Error::UnexpectedChar(c, i),
        }
    }
}

struct Tokenizer<'a> {
    slice: &'a str,
    iter: Peekable<CharIndices<'a>>,
}

impl<'a> Tokenizer<'a> {
    fn new(slice: &'a str) -> Self {
        Self {
            slice,
            iter: slice.char_indices().peekable(),
        }
    }

    fn position(&self, token: Token<'a>) -> usize {
        match token {
            Token::OpenParen(x)
            | Token::CloseParen(x)
            | Token::OpenBracket(x)
            | Token::CloseBracket(x)
            | Token::Quote(x)
            | Token::Backquote(x)
            | Token::Unquote(x)
            | Token::Splice(x)
            | Token::Sharp(x)
            | Token::Dot(x) => x,
            Token::Ident(slice) | Token::String(slice) => {
                let beg = self.slice.as_ptr() as usize;
                let end = slice.as_ptr() as usize;
                end - beg
            }
            Token::Error(e) => {
                let error: Error = e.into();
                error.position()
            }
        }
    }

    fn cur_pos(&mut self) -> usize {
        match self.iter.peek() {
            Some((idx, _)) => *idx,
            None => self.slice.len(),
        }
    }

    fn skip_till(&mut self, mut func: impl FnMut(char) -> bool) -> usize {
        while self.iter.next_if(|x| !func(x.1)).is_some() {}
        match self.iter.peek() {
            Some((idx, _)) => *idx,
            None => self.slice.len(),
        }
    }

    fn skip_till_char(&mut self) {
        let mut in_comment = false;
        let valid_char = |chr: char| {
            if in_comment {
                if chr == '\n' {
                    in_comment = false;
                }
                false
            } else if chr.is_ascii_whitespace() {
                false
            } else if chr == ';' {
                in_comment = true;
                false
            } else {
                true
            }
        };
        self.skip_till(valid_char);
    }

    fn get_string(&mut self, open_delim_pos: usize) -> Token<'a> {
        let mut skip = false;
        let idx_chr = self
            .iter
            .find(|(_, chr)| !escaped(&mut skip, *chr) && *chr == '"');
        match idx_chr {
            Some((end, '"')) => Token::String(&self.slice[(open_delim_pos + 1)..end]),
            _ => Token::Error(TokenError::MissingStringDel(open_delim_pos)),
        }
    }

    fn get_symbol(&mut self, beg: usize, chr: char) -> Token<'a> {
        let mut skip = chr == '\\';
        let end = self.skip_till(|chr| !escaped(&mut skip, chr) && !symbol_char(chr));
        Token::Ident(&self.slice[beg..end])
    }

    fn get_macro_char(&mut self, idx: usize) -> Token<'a> {
        match self.iter.peek() {
            Some((_, '@')) => {
                self.iter.next();
                Token::Splice(idx)
            }
            _ => Token::Unquote(idx),
        }
    }

    fn read_char(&mut self) -> Option<char> {
        self.iter.next().map(|x| x.1)
    }

    // fn read_sharp(&mut self, idx: usize) -> Self {
    //     match self.iter.next_if(|(_, x)| *x == 'o') {
    //         Some(_) => {
    //             let num = 0;
    //             while let Some((idx, chr)) = self.iter.next_if(|(_ , x)| matches!(x, '0'..='9')) {
    //                 match chr.to_digit(8) {
    //                     Some(x) => todo!(),
    //                     None => return Token::Error(),
    //                 }

    //             }
    //             Token::Sharp(idx)
    //         }
    //         _ => Token::Sharp(idx),
    //     }
    // }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_till_char();
        let (idx, chr) = self.iter.next()?;
        let token = match chr {
            '(' => Token::OpenParen(idx),
            ')' => Token::CloseParen(idx),
            '[' => Token::OpenBracket(idx),
            ']' => Token::CloseBracket(idx),
            '\'' => Token::Quote(idx),
            ',' => self.get_macro_char(idx),
            '`' => Token::Backquote(idx),
            '#' => Token::Sharp(idx),
            '.' => Token::Dot(idx),
            '"' => self.get_string(idx),
            other if symbol_char(other) => self.get_symbol(idx, other),
            unknown => Token::Error(TokenError::UnexpectedChar(unknown, idx)),
        };
        Some(token)
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
                "nil" => Object::Nil,
                "t" => Object::True,
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

pub(crate) struct Reader<'a, 'ob> {
    tokens: Tokenizer<'a>,
    arena: &'ob Arena,
}

impl<'a, 'ob> Reader<'a, 'ob> {
    fn read_cdr(&mut self, delim: usize) -> Result<Object<'ob>, Error> {
        match self.tokens.next() {
            Some(Token::CloseParen(i)) => Err(Error::MissingCdr(i)),
            Some(sexp) => {
                let obj = self.read_sexp(sexp);
                match self.tokens.next() {
                    Some(Token::CloseParen(_)) => obj,
                    Some(token) => Err(Error::ExtraItemInCdr(self.tokens.position(token))),
                    None => Err(Error::MissingCloseParen(delim)),
                }
            }
            None => Err(Error::MissingCloseParen(delim)),
        }
    }

    fn read_list(&mut self, delim: usize) -> Result<Object<'ob>, Error> {
        let mut objects = Vec::new();
        while let Some(token) = self.tokens.next() {
            match token {
                Token::CloseParen(_) => {
                    return Ok(fns::slice_into_list(&objects, None, self.arena))
                }
                Token::Dot(_) => {
                    let cdr = self.read_cdr(delim)?;
                    return Ok(fns::slice_into_list(&objects, Some(cdr), self.arena));
                }
                tok => objects.push(self.read_sexp(tok)?),
            }
        }
        Err(Error::MissingCloseParen(delim))
    }

    fn read_vec(&mut self, delim: usize) -> Result<Object<'ob>, Error> {
        let mut objects = Vec::new();
        while let Some(token) = self.tokens.next() {
            match token {
                Token::CloseBracket(_) => return Ok(self.arena.add(objects)),
                tok => objects.push(self.read_sexp(tok)?),
            }
        }
        Err(Error::MissingCloseBracket(delim))
    }

    fn quote_item(&mut self, pos: usize, symbol_name: &str) -> Result<Object<'ob>, Error> {
        let obj = match self.tokens.next() {
            Some(token) => self.read_sexp(token)?,
            None => return Err(Error::MissingQuotedItem(pos)),
        };
        let quoted = list!(intern(symbol_name), obj; self.arena);
        Ok(quoted.into_obj(self.arena))
    }

    fn read_octal(&mut self, pos: usize) -> Result<Object<'ob>, Error> {
        match self.tokens.next() {
            Some(Token::Ident(ident)) => {
                let mut accum = 0;
                for chr in ident.chars() {
                    match chr.to_digit(8) {
                        Some(digit) => accum = (accum * 8) + digit,
                        None => return Err(Error::InvalidRadix(8, pos)),
                    }
                }
                Ok(self.arena.add(i64::from(accum)))
            }
            _ => Err(Error::InvalidRadix(8, pos)),
        }
    }

    fn read_sharp(&mut self, pos: usize) -> Result<Object<'ob>, Error> {
        match self.tokens.read_char() {
            Some('\'') => match self.tokens.next() {
                Some(Token::OpenParen(i)) => {
                    let list = self.read_list(i)?;
                    let quoted = list!(intern("function"), list; self.arena);
                    Ok(quoted.into_obj(self.arena))
                }
                Some(token) => {
                    let obj = self.read_sexp(token)?;
                    let quoted = list!(intern("function"), obj; self.arena);
                    Ok(quoted.into_obj(self.arena))
                }
                None => Err(Error::MissingQuotedItem(pos)),
            },
            Some('o') => self.read_octal(pos),
            Some(chr) => Err(Error::UnknownMacroCharacter(chr, pos)),
            None => Err(Error::MissingQuotedItem(pos)),
        }
    }

    fn read_sexp(&mut self, token: Token<'a>) -> Result<Object<'ob>, Error> {
        match token {
            Token::OpenParen(i) => self.read_list(i),
            Token::CloseParen(i) => Err(Error::ExtraCloseParen(i)),
            Token::OpenBracket(i) => self.read_vec(i),
            Token::CloseBracket(i) => Err(Error::ExtraCloseBracket(i)),
            Token::Quote(i) => self.quote_item(i, "quote"),
            Token::Unquote(i) => self.quote_item(i, ","),
            Token::Splice(i) => self.quote_item(i, ",@"),
            Token::Backquote(i) => self.quote_item(i, "`"),
            Token::Sharp(i) => self.read_sharp(i),
            Token::Dot(i) => Err(Error::UnexpectedChar('.', i)),
            Token::Ident(x) => Ok(parse_symbol(x, self.arena)),
            Token::String(x) => Ok(unescape_string(x).into_obj(self.arena)),
            Token::Error(e) => Err(e.into()),
        }
    }

    pub(crate) fn read(slice: &'a str, arena: &'ob Arena) -> Result<(Object<'ob>, usize), Error> {
        let mut reader = Reader {
            tokens: Tokenizer::new(slice),
            arena,
        };
        match reader.tokens.next() {
            Some(t) => reader.read_sexp(t).map(|x| (x, reader.tokens.cur_pos())),
            None => Err(Error::EmptyStream),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn tokens() {
        let mut iter = Tokenizer::new("1 foo (\"bar\" . 1.3)");
        assert_eq!(iter.next(), Some(Token::Ident("1")));
        assert_eq!(iter.next(), Some(Token::Ident("foo")));
        assert_eq!(iter.next(), Some(Token::OpenParen(6)));
        assert_eq!(iter.next(), Some(Token::String("bar")));
        assert_eq!(iter.next(), Some(Token::Dot(13)));
        assert_eq!(iter.next(), Some(Token::Ident("1.3")));
        assert_eq!(iter.next(), Some(Token::CloseParen(18)));
        assert_eq!(iter.next(), None);
    }

    macro_rules! check_reader {
        ($expect:expr, $compare:expr) => {{
            let read_arena = &Arena::new();
            let obj: Object = $expect.into_obj(read_arena);
            assert_eq!(obj, Reader::read($compare, &read_arena).unwrap().0)
        }};
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
        check_reader!(1, "#o001");
        check_reader!(8, "#o10");
        check_reader!(2385, "#o4521");
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
        check_reader!(
            list!("foo", cons!(1, 1.5; arena); arena),
            "(\"foo\" (1 . 1.5))"
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

    #[test]
    fn read_sharp() {
        let arena = &Arena::new();
        let quote = intern("function");
        check_reader!(list!(quote, intern("foo"); arena), "#'foo");
        check_reader!(
            list!(quote, list!(intern("lambda"), intern("foo"), false, false; arena); arena),
            "#'(lambda foo () nil)"
        );
        assert_error("#", Error::MissingQuotedItem(0));
        assert_error("#'", Error::MissingQuotedItem(0));
        assert_error("#a", Error::UnknownMacroCharacter('a', 0));
    }

    #[test]
    fn test_read_vec() {
        check_reader!(vec![], "[]");
        check_reader!(vec_into![1], "[1]");
        check_reader!(vec_into![1, 2], "[1 2]");
        check_reader!(vec_into![1, 2, 3], "[1 2 3]");
    }

    fn assert_error(input: &str, error: Error) {
        let arena = &Arena::new();
        let result = Reader::read(input, arena).err().unwrap();
        assert_eq!(result, error);
    }

    #[test]
    fn reader_error() {
        assert_error("", Error::EmptyStream);
        assert_error(" (1 2", Error::MissingCloseParen(1));
        assert_error("  (1 (2 3) 4", Error::MissingCloseParen(2));
        assert_error("  (1 (2 3 4", Error::MissingCloseParen(5));
        assert_error(" \"foo", Error::MissingStringDel(1));
        assert_error("(1 2 . 3 4)", Error::ExtraItemInCdr(9));
        assert_error("(1 2 . )", Error::MissingCdr(7));
        assert_error("(1 3 )", Error::UnexpectedChar('', 5));
        assert_error(" '", Error::MissingQuotedItem(1));
        assert_error(" )", Error::ExtraCloseParen(1));
    }

    #[test]
    fn comments() {
        assert_error(" ; comment ", Error::EmptyStream);
        check_reader!(1, "; comment \n  1");
    }
}
