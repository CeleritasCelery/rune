//! Lisp reader that reads an object from a string.
use crate::core::{
    env::{intern, sym, Symbol},
    gc::Context,
    object::GcObj,
};
use crate::fns;
use rune_core::macros::list;
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
    MalformedUnicdoe(usize),
    EmptyStream,
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::MissingCloseParen(i) => write!(f, "Missing close paren: at {i}"),
            Error::MissingCloseBracket(i) => write!(f, "Missing close bracket: at {i}"),
            Error::MissingStringDel(i) => write!(f, "Missing closing string quote: at {i}"),
            Error::ExtraCloseParen(i) => write!(f, "Extra Closing paren: at {i}"),
            Error::ExtraCloseBracket(i) => write!(f, "Extra Closing brace: at {i}"),
            Error::UnexpectedChar(chr, i) => write!(f, "Unexpected character {chr}: at {i}"),
            Error::MalformedUnicdoe(i) => write!(f, "Malformed unicode: at {i}"),
            Error::EmptyStream => write!(f, "Empty Stream"),
            Error::ExtraItemInCdr(i) => write!(f, "Extra item in cdr: at {i}"),
            Error::MissingQuotedItem(i) => write!(f, "Missing element after quote: at {i}"),
            Error::ParseInt(radix, i) => {
                write!(f, "invalid character for radix {radix}: at {i}")
            }
            Error::UnknownMacroCharacter(chr, i) => {
                write!(f, "Unkown reader macro character {chr}: at {i}")
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
            | Error::MalformedUnicdoe(x)
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
            | Error::MalformedUnicdoe(i)
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
    QuestionMark(usize, char),
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
            Token::QuestionMark(_, chr) => write!(f, "?{chr}"),
            Token::Ident(x) => write!(f, "{x}"),
            Token::String(x) => write!(f, "\"{x}\""),
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
        Self { slice, iter: slice.char_indices().peekable() }
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
            | Token::QuestionMark(x, _) => x,
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
        let idx_chr = self.iter.find(|(_, chr)| !escaped(&mut skip, *chr) && *chr == '"');
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

    fn read_quoted_char(&mut self, idx: usize) -> Token<'a> {
        match self.iter.next() {
            Some((start, item)) => {
                if item == '\\' {
                    let Token::Ident(tok) = self.get_symbol(start, item) else { unreachable!() };
                    let Some(chr) = tok.chars().nth(1) else {
                        return Token::Error(Error::MissingQuotedItem(start));
                    };
                    if chr == 'u' || chr == 'x' {
                        match u32::from_str_radix(&tok[2..], 16) {
                            Ok(digits) => match char::from_u32(digits) {
                                Some(c) => Token::QuestionMark(start, c),
                                None => Token::Error(Error::MalformedUnicdoe(start)),
                            },
                            Err(_) => Token::Error(Error::MalformedUnicdoe(start)),
                        }
                    } else if tok.chars().count() == 2 {
                        let new = match chr {
                            'a' => '\u{07}',
                            'b' => '\u{08}',
                            'e' => '\u{1B}',
                            'f' => '\u{0C}',
                            'n' => '\n',
                            'r' => '\r',
                            's' => ' ',
                            't' => '\t',
                            'v' => '\u{0B}',
                            c => c,
                        };
                        Token::QuestionMark(start, new)
                    } else {
                        // TODO implement keycode parsing
                        Token::QuestionMark(start, '\0')
                    }
                } else {
                    match self.iter.peek() {
                        Some((i, chr)) if symbol_char(*chr) && *chr != '?' => {
                            Token::Error(Error::UnexpectedChar(*chr, *i)) // ?aa
                        }
                        _ => Token::QuestionMark(idx, item), // ?a
                    }
                }
            }
            None => Token::Error(Error::MissingQuotedItem(idx)),
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
            '?' => self.read_quoted_char(idx),
            '"' => self.get_string(idx),
            other if symbol_char(other) => self.get_symbol(idx, other),
            unknown => Token::Error(Error::UnexpectedChar(unknown, idx)),
        };
        Some(token)
    }
}

fn intern_symbol<'ob>(symbol: &str, cx: &'ob Context) -> Symbol<'ob> {
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
        intern(escaped_slice.as_str(), cx)
    } else {
        intern(symbol, cx)
    }
}

/// Parse a symbol from a string. This will either by a true symbol or a number
/// literal.
fn parse_symbol<'a>(slice: &str, cx: &'a Context) -> GcObj<'a> {
    match slice.parse::<i64>() {
        Ok(num) => cx.add(num),
        Err(_) => match slice.parse::<f64>() {
            Ok(num) => cx.add(num),
            Err(_) => cx.add(intern_symbol(slice, cx)),
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
    !matches!(chr, '\x00'..=' ' | '(' | ')' | '[' | ']' | '#' | ',' | '`' | ';' | '"' | '\'')
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
    /// New objects are allocated in the context.
    cx: &'ob Context<'ob>,
}

impl<'a, 'ob> Reader<'a, 'ob> {
    /// Read the cdr of a literal list.
    /// ```lisp
    /// '(1 2 3 . 45)
    ///           ^^^
    /// ```
    fn read_cdr(&mut self, delim: usize) -> Result<Option<GcObj<'ob>>> {
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

    fn read_list(&mut self, delim: usize) -> Result<GcObj<'ob>> {
        let mut objects = Vec::new();
        while let Some(token) = self.tokens.next() {
            match token {
                Token::CloseParen(_) => return Ok(fns::slice_into_list(&objects, None, self.cx)),
                Token::Ident(".") => {
                    let cdr = self.read_cdr(delim)?;
                    if cdr.is_none() {
                        objects.push(parse_symbol(".", self.cx));
                    }
                    return Ok(fns::slice_into_list(&objects, cdr, self.cx));
                }
                tok => objects.push(self.read_sexp(tok)?),
            }
        }
        Err(Error::MissingCloseParen(delim))
    }

    fn read_vec(&mut self, delim: usize) -> Result<GcObj<'ob>> {
        let mut objects = Vec::new();
        while let Some(token) = self.tokens.next() {
            match token {
                Token::CloseBracket(_) => return Ok(self.cx.add(objects)),
                tok => objects.push(self.read_sexp(tok)?),
            }
        }
        Err(Error::MissingCloseBracket(delim))
    }

    /// Quote an item using `symbol`.
    fn quote_item(&mut self, pos: usize, symbol: Symbol) -> Result<GcObj<'ob>> {
        let obj: GcObj = match self.tokens.next() {
            Some(token) => self.read_sexp(token)?,
            None => return Err(Error::MissingQuotedItem(pos)),
        };
        Ok(list!(symbol, obj; self.cx))
    }

    /// Read number with specificed radix
    fn read_radix(&mut self, pos: usize, radix: u8) -> Result<GcObj<'ob>> {
        match self.tokens.next() {
            Some(Token::Ident(ident)) => match usize::from_str_radix(ident, radix.into()) {
                Ok(x) => Ok(self.cx.add(x as i64)),
                Err(_) => Err(Error::ParseInt(radix, pos)),
            },
            _ => Err(Error::ParseInt(radix, pos)),
        }
    }

    /// read a sharp quoted character. This could be used for reader macro's in
    /// the future, but right now it just handles the special cases from elisp.
    fn read_sharp(&mut self, pos: usize) -> Result<GcObj<'ob>> {
        match self.tokens.read_char() {
            Some('\'') => match self.tokens.next() {
                Some(Token::OpenParen(i)) => {
                    let list = self.read_list(i)?;
                    Ok(list!(sym::FUNCTION, list; self.cx))
                }
                Some(token) => {
                    let obj = self.read_sexp(token)?;
                    Ok(list!(sym::FUNCTION, obj; self.cx))
                }
                None => Err(Error::MissingQuotedItem(pos)),
            },
            Some('b') => self.read_radix(pos, 2),
            Some('o') => self.read_radix(pos, 8),
            Some('x') => self.read_radix(pos, 16),
            Some(chr) => Err(Error::UnknownMacroCharacter(chr, pos)),
            None => Err(Error::MissingQuotedItem(pos)),
        }
    }

    fn read_sexp(&mut self, token: Token<'a>) -> Result<GcObj<'ob>> {
        match token {
            Token::OpenParen(i) => self.read_list(i),
            Token::CloseParen(i) => Err(Error::ExtraCloseParen(i)),
            Token::OpenBracket(i) => self.read_vec(i),
            Token::CloseBracket(i) => Err(Error::ExtraCloseBracket(i)),
            Token::Quote(i) => self.quote_item(i, sym::QUOTE),
            Token::Unquote(i) => self.quote_item(i, sym::UNQUOTE),
            Token::Splice(i) => self.quote_item(i, sym::SPLICE),
            Token::Backquote(i) => self.quote_item(i, sym::BACKQUOTE),
            Token::Sharp(i) => self.read_sharp(i),
            Token::QuestionMark(_, c) => Ok((c as i64).into()),
            Token::Ident(x) => Ok(parse_symbol(x, self.cx)),
            Token::String(x) => Ok(self.cx.add(unescape_string(x))),
            Token::Error(e) => Err(e),
        }
    }
}

/// read a lisp object from `slice`. Return the object and index of next
/// remaining character in the slice.
pub(crate) fn read<'ob>(slice: &str, cx: &'ob Context) -> Result<(GcObj<'ob>, usize)> {
    let mut reader = Reader { tokens: Tokenizer::new(slice), cx };
    match reader.tokens.next() {
        Some(t) => reader.read_sexp(t).map(|x| (x, reader.tokens.cur_pos())),
        None => Err(Error::EmptyStream),
    }
}

#[cfg(test)]
mod test {
    use crate::core::gc::RootSet;
    use rune_core::macros::cons;

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
        ($expect:expr, $compare:expr, $cx:expr) => {{
            let obj = $cx.add($expect);
            assert_eq!(obj, read($compare, $cx).unwrap().0)
        }};
    }

    #[test]
    fn test_read_number() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        check_reader!(5, "5", cx);
        check_reader!(49, "49", cx);
        check_reader!(-105, "-105", cx);
        check_reader!(1.5, "1.5", cx);
        check_reader!(-3.0, "-3.0", cx);
        check_reader!(1, "+1", cx);
        check_reader!(1, "001", cx);
        check_reader!(1, "#o001", cx);
        check_reader!(8, "#o10", cx);
        check_reader!(2385, "#o4521", cx);
        check_reader!(0b1, "#b001", cx);
        check_reader!(0b10, "#b10", cx);
        check_reader!(0b101_1101, "#b1011101", cx);
        check_reader!(0x1, "#x001", cx);
        check_reader!(0x10, "#x10", cx);
        check_reader!(0xdead_beef_i64, "#xDeAdBeEf", cx);
    }

    #[test]
    #[allow(clippy::non_ascii_literal)]
    fn test_read_char() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        check_reader!(97, "?a", cx);
        check_reader!(21, "?", cx);
        check_reader!(225, "?á", cx);
        check_reader!(97, "?a?a", cx);
        check_reader!(97, "?a#'foo ?a", cx);
        assert_error("?aa", Error::UnexpectedChar('a', 2), cx);
        assert_error("?", Error::MissingQuotedItem(0), cx);
    }

    #[test]
    fn read_bool() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        check_reader!(false, "nil", cx);
        check_reader!(false, "()", cx);
        check_reader!(true, "t", cx);
    }

    #[test]
    fn test_read_symbol() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        check_reader!(sym::IF, "if", cx);
        check_reader!(intern("--1", cx), "--1", cx);
        check_reader!(intern("1", cx), "\\1", cx);
        check_reader!(intern("3.0.0", cx), "3.0.0", cx);
        check_reader!(intern("1+", cx), "1+", cx);
        check_reader!(intern("+1", cx), "\\+1", cx);
        check_reader!(intern(" x", cx), "\\ x", cx);
        check_reader!(intern("\\x", cx), "\\\\x", cx);
        check_reader!(intern("x.y", cx), "x.y", cx);
        check_reader!(intern("(* 1 2)", cx), "\\(*\\ 1\\ 2\\)", cx);
        check_reader!(intern("+-*/_~!@$%^&=:<>{}", cx), "+-*/_~!@$%^&=:<>{}", cx);
    }

    #[test]
    fn test_read_string() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        check_reader!("foo", r#""foo""#, cx);
        check_reader!("foo bar", r#""foo bar""#, cx);
        check_reader!("foo\nbar\t\r", r#""foo\nbar\t\r""#, cx);
        check_reader!(
            "foobarbaz",
            r#""foo\ bar\
baz""#,
            cx
        );
    }

    #[test]
    fn test_read_cons() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        check_reader!(false, "()", cx);
        check_reader!(cons!(1, 2; cx), "(1 . 2)", cx);
        check_reader!(list!(1; cx), "(1)", cx);
        check_reader!(list!("foo"; cx), "(\"foo\")", cx);
        check_reader!(cons!(1, cons!(1.5, "foo"; cx); cx), "(1 1.5 . \"foo\")", cx);
        check_reader!(list!("foo", cons!(1, 1.5; cx); cx), "(\"foo\" (1 . 1.5))", cx);
        check_reader!(list!(1, 1.5; cx), "(1 1.5)", cx);
        check_reader!(list!(1, 1.5, -7; cx), "(1 1.5 -7)", cx);
        check_reader!(list!(1, 1.5, intern(".", cx); cx), "(1 1.5 .)", cx);
        check_reader!(list!(1, 1.5, intern("...", cx), 2; cx), "(1 1.5 ... 2)", cx);
    }

    #[test]
    fn read_quote() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        let quote = sym::QUOTE;
        check_reader!(list!(quote, sym::IF; cx), "(quote if)", cx);
        check_reader!(list!(quote, sym::IF; cx), "'if", cx);
        check_reader!(list!(quote, list!(1, 2, 3; cx); cx), "'(1 2 3)", cx);
        check_reader!(u32::from('a'), "?a", cx);
        check_reader!(u32::from(' '), "?\\s", cx);
        check_reader!(u32::from('\t'), "?\\t", cx);
        check_reader!(u32::from('\u{AFD}'), "?\\uafd", cx);
        check_reader!(0xabc_u32, "?\\xabc", cx);
    }

    #[test]
    fn read_sharp() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        let quote = sym::FUNCTION;
        check_reader!(list!(quote, sym::IF; cx), "#'if", cx);
        check_reader!(
            list!(quote, list!(intern("lambda", cx), sym::IF, false, false; cx); cx),
            "#'(lambda if () nil)",
            cx
        );
        assert_error("#", Error::MissingQuotedItem(0), cx);
        assert_error("#'", Error::MissingQuotedItem(0), cx);
        assert_error("#a", Error::UnknownMacroCharacter('a', 0), cx);
    }

    #[test]
    fn test_read_vec() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        check_reader!(Vec::<GcObj>::new(), "[]", cx);
        let vec: Vec<GcObj> = vec![1.into()];
        check_reader!(vec, "[1]", cx);
        let vec: Vec<GcObj> = vec![1.into(), 2.into()];
        check_reader!(vec, "[1 2]", cx);
        let vec: Vec<GcObj> = vec![1.into(), 2.into(), 3.into()];
        check_reader!(vec, "[1 2 3]", cx);
    }

    fn assert_error(input: &str, error: Error, cx: &Context) {
        let result = read(input, cx).err().unwrap();
        assert_eq!(result, error);
    }

    #[test]
    fn reader_error() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        assert_error("", Error::EmptyStream, cx);
        assert_error(" (1 2", Error::MissingCloseParen(1), cx);
        assert_error("  (1 (2 3) 4", Error::MissingCloseParen(2), cx);
        assert_error("  (1 (2 3 4", Error::MissingCloseParen(5), cx);
        assert_error(" \"foo", Error::MissingStringDel(1), cx);
        assert_error("(1 2 . 3 4)", Error::ExtraItemInCdr(9), cx);
        assert_error("(1 3 \0)", Error::UnexpectedChar('\0', 5), cx);
        assert_error(" '", Error::MissingQuotedItem(1), cx);
        assert_error(" )", Error::ExtraCloseParen(1), cx);
    }

    #[test]
    fn comments() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        assert_error(" ; comment ", Error::EmptyStream, cx);
        check_reader!(1, "; comment \n  1", cx);
    }
}
