//! Lisp reader that reads an object from a string. Note that currently reader
//! macros are not supported, but could be implemented in the future.

use crate::arena::Arena;
use crate::fns;
use crate::object::{IntoObject, Object};
use crate::symbol::{intern, sym, Symbol};
use std::fmt::Display;
use std::str;
use std::{fmt, iter::Peekable, str::CharIndices};

type Result<T> = std::result::Result<T, Error>;

/// Errors that can occur during reading a sexp from a string
#[derive(PartialEq, Debug, Copy, Clone)]
pub(crate) enum Error {
    MissingCloseParen(usize),
    MissingCloseBracket(usize),
    MissingStringDel(usize),
    MissingQuotedItem(usize),
    ExtraItemInCdr(usize),
    ExtraCloseParen(usize),
    ExtraCloseBracket(usize),
    UnexpectedChar(char, usize),
    UnknownMacroCharacter(char, usize),
    ParseInt(u8, usize),
    EmptyStream,
}

impl Display for Error {
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
            Error::MissingQuotedItem(i) => write!(f, "Missing element after quote: at {}", i),
            Error::ParseInt(radix, i) => {
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
            | Error::UnexpectedChar(_, x)
            | Error::ParseInt(_, x)
            | Error::UnknownMacroCharacter(_, x) => *x,
            Error::EmptyStream => 0,
        }
    }

    fn mut_pos(&mut self) -> Option<&mut usize> {
        match self {
            Error::MissingCloseParen(i)
            | Error::MissingCloseBracket(i)
            | Error::MissingStringDel(i)
            | Error::UnexpectedChar(_, i)
            | Error::ExtraItemInCdr(i)
            | Error::ExtraCloseParen(i)
            | Error::ExtraCloseBracket(i)
            | Error::MissingQuotedItem(i)
            | Error::UnknownMacroCharacter(_, i)
            | Error::ParseInt(_, i) => Some(i),
            Error::EmptyStream => None,
        }
    }

    pub(crate) fn update_pos(&mut self, offset: usize) {
        if let Some(pos) = self.mut_pos() {
            *pos += offset;
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
    QuestionMark(usize),
    Ident(&'a str),
    String(&'a str),
    Error(Error),
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::OpenParen(_) => write!(f, "("),
            Token::CloseParen(_) => write!(f, ")"),
            Token::OpenBracket(_) => write!(f, "["),
            Token::CloseBracket(_) => write!(f, "]"),
            Token::Quote(_) => write!(f, "'"),
            Token::Backquote(_) => write!(f, "`"),
            Token::Unquote(_) => write!(f, ","),
            Token::Splice(_) => write!(f, ",@"),
            Token::Sharp(_) => write!(f, "#"),
            Token::QuestionMark(_) => write!(f, "?"),
            Token::Ident(x) => write!(f, "{}", x),
            Token::String(x) => write!(f, "\"{}\"", x),
            Token::Error(_) => write!(f, "error"),
        }
    }
}

#[derive(Clone)]
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

    /// Given a [`Token`] calculate it's position relative to this `Tokenizer`.
    fn relative_pos(&self, token: Token<'a>) -> usize {
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
            | Token::QuestionMark(x) => x,
            Token::Ident(slice) | Token::String(slice) => {
                let beg = self.slice.as_ptr() as usize;
                let end = slice.as_ptr() as usize;
                end - beg
            }
            Token::Error(e) => e.position(),
        }
    }

    /// Return the current position of the Tokenizer. This is the index of the
    /// next character.
    fn cur_pos(&mut self) -> usize {
        match self.iter.peek() {
            Some((idx, _)) => *idx,
            None => self.slice.len(),
        }
    }

    /// Skip characters until the closure returns true.
    fn skip_till(&mut self, mut func: impl FnMut(char) -> bool) -> usize {
        while self.iter.next_if(|x| !func(x.1)).is_some() {}
        match self.iter.peek() {
            Some((idx, _)) => *idx,
            None => self.slice.len(),
        }
    }

    /// Skip whitespace and comments until the next valid read character.
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
            _ => Token::Error(Error::MissingStringDel(open_delim_pos)),
        }
    }

    fn get_symbol(&mut self, beg: usize, chr: char) -> Token<'a> {
        let mut skip = chr == '\\';
        let end = self.skip_till(|c| !escaped(&mut skip, c) && !symbol_char(c));
        Token::Ident(&self.slice[beg..end])
    }

    /// After having found a `,`, see if the next token is a `@` or not.
    fn get_macro_char(&mut self, idx: usize) -> Token<'a> {
        match self.iter.next_if(|(_, chr)| *chr == '@') {
            Some(_) => Token::Splice(idx),
            None => Token::Unquote(idx),
        }
    }

    fn read_char(&mut self) -> Option<char> {
        self.iter.next().map(|x| x.1)
    }
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
            '?' => Token::QuestionMark(idx),
            '"' => self.get_string(idx),
            other if symbol_char(other) => self.get_symbol(idx, other),
            unknown => Token::Error(Error::UnexpectedChar(unknown, idx)),
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

/// Parse a symbol from a string. This will either by a true symbol or a number
/// literal.
fn parse_symbol<'a>(slice: &str, arena: &'a Arena) -> Object<'a> {
    match slice.parse::<i64>() {
        Ok(num) => num.into_obj(arena),
        Err(_) => match slice.parse::<f64>() {
            Ok(num) => num.into_obj(arena),
            Err(_) => match slice {
                "nil" => Object::NIL,
                "t" => Object::TRUE,
                _ => intern_symbol(slice).into_obj(arena),
            },
        },
    }
}

/// process escape characters in the string slice and return the resulting
/// string.
fn unescape_string(string: &str) -> String {
    let mut escaped = false;
    let unescape = |c: char| {
        if escaped {
            escaped = false;
            // TODO: Handle unicode, hex, and octal escapes
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
    string.chars().filter_map(unescape).collect()
}

/// Return true if `chr` is a valid symbol character.
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

/// State of the reader.
struct Reader<'a, 'ob> {
    /// The iterator over the tokens in the current slice.
    tokens: Tokenizer<'a>,
    /// New objects are allocated in the arena.
    arena: &'ob Arena,
}

impl<'a, 'ob> Reader<'a, 'ob> {
    /// Read the cdr of a literal list.
    /// ```lisp
    /// '(1 2 3 . 45)
    ///           ^^^
    /// ```
    fn read_cdr(&mut self, delim: usize) -> Result<Option<Object<'ob>>> {
        match self.tokens.next() {
            Some(Token::CloseParen(_)) => Ok(None),
            Some(sexp) => {
                let obj = self.read_sexp(sexp);
                match self.tokens.next() {
                    Some(Token::CloseParen(_)) => obj.map(Some),
                    Some(token) => Err(Error::ExtraItemInCdr(self.tokens.relative_pos(token))),
                    None => Err(Error::MissingCloseParen(delim)),
                }
            }
            None => Err(Error::MissingCloseParen(delim)),
        }
    }

    fn read_list(&mut self, delim: usize) -> Result<Object<'ob>> {
        let mut objects = Vec::new();
        while let Some(token) = self.tokens.next() {
            match token {
                Token::CloseParen(_) => {
                    return Ok(fns::slice_into_list(&objects, None, self.arena))
                }
                Token::Ident(".") => {
                    let cdr = self.read_cdr(delim)?;
                    if cdr.is_none() {
                        objects.push(parse_symbol(".", self.arena));
                    }
                    return Ok(fns::slice_into_list(&objects, cdr, self.arena));
                }
                tok => objects.push(self.read_sexp(tok)?),
            }
        }
        Err(Error::MissingCloseParen(delim))
    }

    fn read_vec(&mut self, delim: usize) -> Result<Object<'ob>> {
        let mut objects = Vec::new();
        while let Some(token) = self.tokens.next() {
            match token {
                Token::CloseBracket(_) => return Ok(self.arena.add(objects)),
                tok => objects.push(self.read_sexp(tok)?),
            }
        }
        Err(Error::MissingCloseBracket(delim))
    }

    /// Quote an item using `symbol`.
    fn quote_item(&mut self, pos: usize, symbol: Symbol) -> Result<Object<'ob>> {
        let obj: Object = match self.tokens.next() {
            Some(token) => self.read_sexp(token)?,
            None => return Err(Error::MissingQuotedItem(pos)),
        };
        let ret: Object = list!(symbol, obj; self.arena);
        Ok(ret)
    }

    /// read a quoted character (e.g. `?a`)
    fn read_char_quote(&mut self, pos: usize) -> Result<Object<'ob>> {
        match self.tokens.next() {
            // TODO: Implement actual parsing
            Some(Token::Ident(_)) => Ok(0.into()),
            Some(tok) => Err(Error::MissingQuotedItem(self.tokens.relative_pos(tok))),
            None => Err(Error::MissingQuotedItem(pos)),
        }
    }

    /// Read an octal escape (e.g. `#0759`)
    fn read_octal(&mut self, pos: usize) -> Result<Object<'ob>> {
        match self.tokens.next() {
            Some(Token::Ident(ident)) => match usize::from_str_radix(ident, 8) {
                Ok(x) => Ok(self.arena.add(x as i64)),
                Err(_) => Err(Error::ParseInt(8, pos)),
            },
            _ => Err(Error::ParseInt(8, pos)),
        }
    }

    /// read a sharp quoted character. This could be used for reader macro's in
    /// the future, but right now it just handles the special cases from elisp.
    fn read_sharp(&mut self, pos: usize) -> Result<Object<'ob>> {
        match self.tokens.read_char() {
            Some('\'') => match self.tokens.next() {
                Some(Token::OpenParen(i)) => {
                    let list = self.read_list(i)?;
                    Ok(list!(&sym::FUNCTION, list; self.arena))
                }
                Some(token) => {
                    let obj = self.read_sexp(token)?;
                    Ok(list!(&sym::FUNCTION, obj; self.arena))
                }
                None => Err(Error::MissingQuotedItem(pos)),
            },
            Some('o') => self.read_octal(pos),
            Some(chr) => Err(Error::UnknownMacroCharacter(chr, pos)),
            None => Err(Error::MissingQuotedItem(pos)),
        }
    }

    fn read_sexp(&mut self, token: Token<'a>) -> Result<Object<'ob>> {
        match token {
            Token::OpenParen(i) => self.read_list(i),
            Token::CloseParen(i) => Err(Error::ExtraCloseParen(i)),
            Token::OpenBracket(i) => self.read_vec(i),
            Token::CloseBracket(i) => Err(Error::ExtraCloseBracket(i)),
            Token::Quote(i) => self.quote_item(i, &sym::QUOTE),
            Token::Unquote(i) => self.quote_item(i, &sym::UNQUOTE),
            Token::Splice(i) => self.quote_item(i, &sym::SPLICE),
            Token::Backquote(i) => self.quote_item(i, &sym::BACKQUOTE),
            Token::Sharp(i) => self.read_sharp(i),
            Token::QuestionMark(i) => self.read_char_quote(i),
            Token::Ident(x) => Ok(parse_symbol(x, self.arena)),
            Token::String(x) => Ok(unescape_string(x).into_obj(self.arena)),
            Token::Error(e) => Err(e),
        }
    }
}

/// read a lisp object from `slice`. Return the object and index of next
/// remaining character in the slice.
pub(crate) fn read<'a, 'ob>(slice: &'a str, arena: &'ob Arena) -> Result<(Object<'ob>, usize)> {
    let mut reader = Reader {
        tokens: Tokenizer::new(slice),
        arena,
    };
    match reader.tokens.next() {
        Some(t) => reader.read_sexp(t).map(|x| (x, reader.tokens.cur_pos())),
        None => Err(Error::EmptyStream),
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
        assert_eq!(iter.next(), Some(Token::Ident(".")));
        assert_eq!(iter.next(), Some(Token::Ident("1.3")));
        assert_eq!(iter.next(), Some(Token::CloseParen(18)));
        assert_eq!(iter.next(), None);
    }

    macro_rules! check_reader {
        ($expect:expr, $compare:expr) => {{
            let read_arena = &Arena::new();
            let obj: Object = $expect.into_obj(read_arena);
            assert_eq!(obj, read($compare, &read_arena).unwrap().0)
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
        check_reader!(&sym::test::FOO, "foo");
        check_reader!(intern("--1"), "--1");
        check_reader!(intern("1"), "\\1");
        check_reader!(intern("3.0.0"), "3.0.0");
        check_reader!(intern("1+"), "1+");
        check_reader!(intern("+1"), "\\+1");
        check_reader!(intern(" x"), "\\ x");
        check_reader!(intern("\\x"), "\\\\x");
        check_reader!(intern("x.y"), "x.y");
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
        check_reader!(list!(1, 1.5, intern("."); arena), "(1 1.5 .)");
        check_reader!(list!(1, 1.5, intern("..."), 2; arena), "(1 1.5 ... 2)");
    }

    #[test]
    fn read_quote() {
        let arena = &Arena::new();
        let quote = &sym::QUOTE;
        check_reader!(list!(quote, &sym::test::FOO; arena), "(quote foo)");
        check_reader!(list!(quote, &sym::test::FOO; arena), "'foo");
        check_reader!(list!(quote, list!(1, 2, 3; arena); arena), "'(1 2 3)");
    }

    #[test]
    fn read_sharp() {
        let arena = &Arena::new();
        let quote = &sym::FUNCTION;
        check_reader!(list!(quote, &sym::test::FOO; arena), "#'foo");
        check_reader!(
            list!(quote, list!(intern("lambda"), &sym::test::FOO, false, false; arena); arena),
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
        let result = read(input, arena).err().unwrap();
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
