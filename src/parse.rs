#![allow(dead_code)]
use std::str;

struct Lexer<'a> {
    slice: &'a str,
    line: usize,
}

#[derive(PartialEq, Debug)]
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

impl<'a> Lexer<'a> {
    pub fn new(slice: &'a str) -> Self {
        Lexer {
            slice,
            line: 0,
        }
    }

    fn clear_slice(&mut self) {
        self.slice = &self.slice[self.slice.len()..];
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
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        let mut chars = self.slice.char_indices();

        let chr_idx = match chars.find(|x| !x.1.is_whitespace()) {
            Some(x) => x,
            None => {
                self.clear_slice();
                return None;
            }
        };

        let (idx, chr) = chr_idx;

        if symbol_char(chr) {
            let symbol = self.get_symbol(idx, chars);
            self.slice = &self.slice[idx + symbol.len()..];
            return Some(Token::Symbol(symbol));
        }

        let token = match chr {
            '(' => Token::OpenParen,
            ')' => Token::CloseParen,
            '`' => Token::QuasiQuote,
            '\'' => Token::Quote,
            x => { panic!("unknown token {}", x); }
        };
        self.slice = chars.as_str();
        Some(token)
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
