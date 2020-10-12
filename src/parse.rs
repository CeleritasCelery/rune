#![allow(dead_code)]
use std::str;
use std::fmt;

struct Lexer<'a> {
    slice: &'a str,
    cursor: usize,
    line: usize,
}

#[derive(PartialEq)]
enum Token<'a> {
    Symbol(&'a str),
    String(&'a str),
    Comment(&'a str),
    Integer(i64),
    Float(f64),
    OpenParen,
    CloseParen,
    Quote,
    QuasiQuote,
    MacroEval,
    MacroSplice,
}

impl<'a> fmt::Debug for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Symbol(x) => write!(f, "Symbol: {}", x),
            Token::String(x) => write!(f, "String: \"{}\"", x),
            Token::Comment(x) => write!(f, "Comment: {}", x),
            Token::Integer(x) => write!(f, "Int: {}", x),
            Token::Float(x) => write!(f, "Float: {}", x),
            Token::OpenParen => write!(f, "("),
            Token::CloseParen => write!(f, ")"),
            Token::Quote => write!(f, "'"),
            Token::QuasiQuote => write!(f, "`"),
            Token::MacroEval => write!(f, ","),
            Token::MacroSplice => write!(f, ",@"),
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(slice: &'a str) -> Self {
        Lexer {
            slice,
            cursor: 0,
            line: 0,
        }
    }

    fn get_symbol(mut chars: str::Chars) -> *const u8 {
        let mut prev_ptr = chars.as_str().as_ptr();
        let mut escaped = false;
        while let Some(chr) = chars.next() {
            if escaped || chr == '\\' {
                escaped = !escaped;
                chars.next();
            } else if !symbol_char(chr) {
                return prev_ptr;
            }
            prev_ptr = chars.as_str().as_ptr();
        }
        prev_ptr
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        let mut prev_ptr = self.slice.as_ptr();
        let anchor = prev_ptr as usize;
        let mut iter = self.slice.chars();
        while let Some(chr) = iter.next() {
            match chr {
                ' ' | '\t' => {}
                '(' => {
                    self.slice = iter.as_str();
                    return Some(Token::OpenParen)
                }
                ')' => {
                    self.slice = iter.as_str();
                    return Some(Token::CloseParen)
                }
                '`' => {
                    self.slice = iter.as_str();
                    return Some(Token::QuasiQuote)
                }
                '\'' => {
                    self.slice = iter.as_str();
                    return Some(Token::Quote)
                }
                _ => {
                    let beg = prev_ptr as usize - anchor;
                    let end = Self::get_symbol(iter) as usize - anchor;
                    let slice = &self.slice[beg..end];
                    self.slice = &self.slice[end..];
                    return Some(Token::Symbol(slice));
                }
            }
            prev_ptr = iter.as_str().as_ptr();
        }
        self.slice = &self.slice[self.slice.len()..];
        None
    }
}

fn symbol_char(char: char) -> bool {
    match char {
        '\x00'..=' ' |
        '(' | ')' | '[' | ']' |
        '#' | ',' | '.' | '`' |
        ';' | '"' | '\'' | '\\' => false,
        _ => true,
    }
}

pub fn run() {
    let mut lexer = Lexer::new("(foo (bar) baz 'word) bob");
    while let Some(s) = lexer.next() {
        println!("\"{:?}\"", s);
    }
}

#[cfg(test)]
mod test {

    use super::*;
    // macro_rules! vec_of_strings {
    //     ($($x:expr),*) => (vec![$($x.to_string()),*]);
    // }

    #[test]
    fn parse() {
        let symbols: Vec<Token> = Lexer::new("(foo (bar) baz 'word) bob").collect();

        let golden = vec![
            Token::OpenParen,
            Token::Symbol("foo"),
            Token::OpenParen,
            Token::Symbol("bar"),
            Token::CloseParen,
            Token::Symbol("baz"),
            Token::Quote,
            Token::Symbol("word"),
            Token::CloseParen,
            Token::Symbol("bob")
        ];

        assert_eq!(golden, symbols);
    }
}
