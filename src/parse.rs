#![allow(dead_code)]
use std::str;

struct Lexer<'a> {
    slice: &'a str,
    start: *const u8,
    error: Option<LexerError>
}

#[derive(PartialEq, Debug)]
enum Token<'a> {
    Symbol(&'a str),
    String(&'a str),
    Integer(&'a str),
    Float(&'a str),
    Comment(&'a str),
    OpenParen(usize),
    CloseParen(usize),
    Quote(usize),
    QuasiQuote(usize),
    MacroEval(usize),
    MacroSplice(usize),
    Error,
}

impl<'a> Token<'a> {
    pub fn classify(token: &'a str) -> Token<'a> {
        use Token::*;
        let mut chars = token.chars();
        let mut point_found = false;
        match chars.next() {
            None => return Symbol(token),
            Some(chr) => {
                match chr {
                    '.' => point_found = true,
                    '0'..='9' | '+' | '-' => {},
                    _ => return Symbol(token)
                }
            }
        };

        while let Some(chr) = chars.next() {
            match chr {
                '.' if point_found => return Symbol(token),
                '.' => point_found = true,
                '0'..='9' => {},
                _ => return Symbol(token),
            }
        }
        if point_found {
            Float(token)
        } else {
            Integer(token)
        }
    }
}

#[derive(Debug)]
struct LexerError {
    message: &'static str,
    position: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(slice: &'a str) -> Self {
        Lexer {
            slice,
            start: slice.as_ptr(),
            error: None,
        }
    }

    pub fn into_error(mut self) -> Option<LexerError> {
        let error = self.error;
        self.error = None;
        error
    }

    fn clear(&mut self) {
        self.slice = &self.slice[self.slice.len()..];
    }

    fn advance(&mut self, amount: usize) {
        self.slice = &self.slice[amount..];
    }

    fn get_abs_pos(&self, idx: usize) -> usize {
        self.slice.as_ptr() as usize + idx - self.start as usize
    }

    fn get_symbol(&mut self, beg: usize, mut chars: str::CharIndices) -> &'a str {
        let mut escaped = false;
        while let Some((end, chr)) = chars.next() {
            if escaped || chr == '\\' {
                escaped = !escaped;
                chars.next();
            } else if !symbol_char(chr) {
                return &self.slice[beg..end];
            }
        }
        &self.slice[beg..]
    }

    fn get_string(&mut self, beg: usize, mut chars: str::CharIndices) -> Result<&'a str, LexerError> {
        let mut escaped = false;
        while let Some((end, chr)) = chars.next() {
            if escaped || chr == '\\' {
                escaped = !escaped;
                chars.next();
            } else if chr == '"' {
                return Ok(&self.slice[beg..end+1]);
            }
        }
        Err(LexerError{
            message: "String missing terminator",
            position: self.get_abs_pos(beg),
        })
    }

    fn get_comment(&mut self, beg: usize, mut chars: str::CharIndices) -> &'a str {
        // TODO: Handle different line endings
        match chars.find(|x| x.1 == '\n') {
            None => &self.slice[beg..],
            Some((end, _)) => &self.slice[beg..end+1],
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        let mut chars = self.slice.char_indices();

        let chr_idx = match chars.find(|x| !x.1.is_ascii_whitespace()) {
            Some(x) => x,
            None => {
                self.clear();
                return None;
            }
        };

        let (idx, chr) = chr_idx;

        if symbol_char(chr) {
            let symbol = self.get_symbol(idx, chars);
            self.advance(idx + symbol.len());
            return Some(Token::classify(symbol));
        }

        if chr == '"' {
            return match self.get_string(idx, chars) {
                Err(e) => {
                    self.error = Some(e);
                    self.clear();
                    Some(Token::Error)
                }
                Ok(string) => {
                    self.advance(idx + string.len());
                    Some(Token::String(string))
                }
            }
        }

        if chr == ';' {
            let comment = self.get_comment(idx, chars);
            self.advance(idx + comment.len());
            return Some(Token::Comment(comment));
        }

        let pos = self.get_abs_pos(idx);
        self.slice = chars.as_str();
        println!("idx = {}", idx);
        match chr {
            '(' => Some(Token::OpenParen(pos)),
            ')' => Some(Token::CloseParen(pos)),
            '`' => Some(Token::QuasiQuote(pos)),
            '\'' => Some(Token::Quote(pos)),
            x => { panic!("unknown token {}", x); }
        }
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

pub fn run() {
    let mut lexer = Lexer::new(r#"(foo (bar) -2.3 'word) +1 "this is a string ; \" with stuff in " ; comment"#);
    while let Some(s) = lexer.next() {
        println!("\"{:?}\"", s);
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn size() {
        assert_eq!(24, std::mem::size_of::<Token>());
    }

    #[test]
    fn parse() {
        let symbols: Vec<Token> = Lexer::new("(foo (bar) baz 'word) bob").collect();

        let golden = vec![
            Token::OpenParen(0),
            Token::Symbol("foo"),
            Token::OpenParen(5),
            Token::Symbol("bar"),
            Token::CloseParen(9),
            Token::Symbol("baz"),
            Token::Quote(15),
            Token::Symbol("word"),
            Token::CloseParen(20),
            Token::Symbol("bob")
        ];

        assert_eq!(golden, symbols);
    }

    #[test]
    fn string() {
        let symbols: Vec<Token> = Lexer::new(r#"before "string with \" stuff" after"#).collect();
        let golden = vec![
            Token::Symbol("before"),
            Token::String(r#""string with \" stuff""#),
            Token::Symbol("after"),
        ];

        assert_eq!(golden, symbols);
    }

    #[test]
    fn comments() {
        let symbols: Vec<Token> = Lexer::new("before ;; comment \n after").collect();
        let golden = vec![
            Token::Symbol("before"),
            Token::Comment(";; comment \n"),
            Token::Symbol("after"),
        ];
        assert_eq!(golden, symbols);
    }

    #[test]
    fn error() {
        let mut lexer = Lexer::new("this \" never ends");

        assert_eq!(Token::Symbol("this"), lexer.next().unwrap());
        assert_eq!(Token::Error, lexer.next().unwrap());
        assert_eq!(None, lexer.next());

        let error = lexer.into_error();
        assert!(error.is_some());
        assert_eq!(5, error.unwrap().position);
    }

    #[test]
    fn numbers() {
        let symbols: Vec<Token> = Lexer::new("+1 1+ 8. -1 \\-1 .1 2.0 3.0.0 --1").collect();
        let golden = vec![
            Token::Integer("+1"),
            Token::Symbol("1+"),
            Token::Float("8."),
            Token::Integer("-1"),
            Token::Symbol("\\-1"),
            Token::Float(".1"),
            Token::Float("2.0"),
            Token::Symbol("3.0.0"),
            Token::Symbol("--1"),
        ];
        assert_eq!(golden, symbols);
    }
}
