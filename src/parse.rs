use std::str;
use std::slice;

trait PrevIter {
    fn prev(&mut self);
}

impl PrevIter for str::Chars<'_> {
    fn prev(&mut self) {
        let orig_str = self.as_str();
        let orig_ptr = orig_str.as_ptr();
        let char_len = unsafe {
            const MAX_UTF8_CHARS: usize = 4;
            let tmp_ptr = orig_ptr.sub(MAX_UTF8_CHARS);
            let tmp_slice = slice::from_raw_parts(tmp_ptr, MAX_UTF8_CHARS);
            let mut iter = str::from_utf8_unchecked(tmp_slice).chars();
            iter.next_back();
            MAX_UTF8_CHARS - iter.as_str().len()
        };
        *self = unsafe {
            let new_len = orig_str.len() + char_len;
            let new_slice = slice::from_raw_parts(orig_ptr.sub(char_len), new_len);
            str::from_utf8_unchecked(new_slice).chars()
        };
    }
}

pub fn parse(sexp: &str) -> Vec<String> {
    let mut chars = sexp.chars();
    let mut lexems: Vec<String> = Vec::new();
    while let Some(char) = chars.next() {
        match char {
            '(' | ')' => {
                lexems.push(char.to_string());
            },
            ' ' | '\t' => {
                lexems.push("space".to_string());
            }
            '\'' => {
                lexems.push("quote".to_string());
            }
            _   => {
                chars.prev();
                // TODO: unchecked
                let symbol = parse_symbol(&mut chars);
                println!("\"{}\"", symbol);
                lexems.push(symbol);
            }
        }
        println!("\"{}\"", chars.as_str());
    }
    return lexems;
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

unsafe fn string_from_ptrs(beg: *const u8, end: *const u8) -> String {
    let len = end as usize - beg as usize;
    let slice = std::slice::from_raw_parts(beg, len);
    str::from_utf8_unchecked(slice).to_owned()
}

fn parse_symbol(chars: &mut str::Chars) -> String {
    let mut escaped = false;
    let beg_str = chars.as_str();

    while let Some(char) = chars.next() {
        if escaped || char == '\\' {
            escaped = !escaped;
        } else if !symbol_char(char) {
            unsafe {
                chars.prev();
                let end = chars.as_str().as_ptr();
                return string_from_ptrs(beg_str.as_ptr(), end);
            }
        }
    }
    beg_str.to_owned()
}

pub fn run() {
    let symbols = parse("(foo (bar) baz 'word) bob");
    for s in symbols {
        println!("\"{}\"", s);
    }
}

#[cfg(test)]
mod test {
    macro_rules! vec_of_strings {
        ($($x:expr),*) => (vec![$($x.to_string()),*]);
    }

    #[test]
    fn parse() {
        let symbols = super::parse("(foo (bar) baz 'word) bob");

        let golden = vec_of_strings![
            "(", "foo", "space", "(", "bar", ")", "space",
            "baz", "space", "quote", "word", ")", "space",
            "bob"
        ];

        assert_eq!(golden, symbols);
    }
}
