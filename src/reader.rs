#![allow(dead_code)]

use std::str;
use crate::lisp_object::LispObj;
use crate::symbol::INTERNED_SYMBOLS;


pub struct Stream<'a> {
    prev: str::Chars<'a>,
    iter: str::Chars<'a>,
}

#[derive(Copy, Clone)]
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


fn parse_symbol(slice: &str) -> LispObj {
    match slice.parse::<i64>() {
        Ok(num) => num.into(),
        Err(_) => {
            match slice.parse::<f64>() {
                Ok(num) => num.into(),
                Err(_) => {
                    let mut map = INTERNED_SYMBOLS.lock().unwrap();
                    map.intern(slice).into()
                }
            }
        },
    }
}

fn read_symbol(stream: &mut Stream) -> LispObj {
    let pos = stream.get_pos();
    while let Some(chr) = stream.next() {
        if !symbol_char(chr) {
            stream.back();
            break;
        }
    }
    let slice = stream.slice_till(pos);
    parse_symbol(slice)
}

fn read(stream: &mut Stream) -> Option<LispObj> {
    match stream.find(|x| !x.is_ascii_whitespace())? {
        c if symbol_char(c) => {
            stream.back();
            Some(read_symbol(stream))
        }
        _ => None
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

    #[test]
    fn test_read_number() {
        let mut stream = Stream::new("5 49 -105 1.5 -3.0");
        let mut nums: Vec<LispObj> = vec![];
        while let Some(n) = read(&mut stream) {
            nums.push(n);
        }
        let golden: Vec<LispObj> = vec_into![
            5, 49, -105, 1.5, -3.0,
        ];
        assert_eq!(golden, nums);
    }

    #[test]
    fn test_read_symbol() {
        let sym = {
            let mut map = INTERNED_SYMBOLS.lock().unwrap();
            LispObj::from(map.intern("foo"))
        };

        let mut stream = Stream::new("foo");
        assert_eq!(sym, read(&mut stream).unwrap());
    }
}
