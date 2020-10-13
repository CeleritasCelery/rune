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
    Comment(&'a str),
    OpenParen(&'a str),
    CloseParen(&'a str),
    Quote(&'a str),
    QuasiQuote(&'a str),
    MacroEval(&'a str),
    MacroSplice(&'a str),
    Error,
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
            position: self.slice.as_ptr() as usize + beg - self.start as usize,
        })
    }

    fn get_comment(&mut self, beg: usize, mut chars: str::CharIndices) -> &'a str {
        // Handle different line endings
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

        let chr_idx = match chars.find(|x| !x.1.is_whitespace()) {
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
            return Some(Token::Symbol(symbol));
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

        let string = &self.slice[idx..idx+1];
        self.slice = chars.as_str();
        match chr {
            '(' => Some(Token::OpenParen(string)),
            ')' => Some(Token::CloseParen(string)),
            '`' => Some(Token::QuasiQuote(string)),
            '\'' => Some(Token::Quote(string)),
            x => { panic!("unknown token {}", x); }
        }
    }
}

fn symbol_char(chr: char) -> bool {
    match chr {
        '\x00'..=' ' |
        '(' | ')' | '[' | ']' |
        '#' | ',' | '.' | '`' |
        ';' | '"' | '\'' => false,
        _ => true,
    }
}

pub fn run() {
    let mut lexer = Lexer::new(r#"(foo (bar) baz 'word) bob "this is a string ; \" with stuff in " ; comment"#);
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
            Token::OpenParen("("),
            Token::Symbol("foo"),
            Token::OpenParen("("),
            Token::Symbol("bar"),
            Token::CloseParen(")"),
            Token::Symbol("baz"),
            Token::Quote("'"),
            Token::Symbol("word"),
            Token::CloseParen(")"),
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
}
