struct CharIndex {
    front_offset: usize,
    char: char,
}

struct ParseIter<'a> {
    next: Option<(usize, char)>,
    iter: std::str::CharIndices<'a>,
}

impl<'a> Iterator for ParseIter<'a> {
    type Item = (usize, char);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self.next {
            Some(i) => {
                let item = i;
                self.next = None;
                Some(item)
            }
            None => self.iter.next()
        }
    }
}

impl<'a> ParseIter<'a> {
    fn put_back(&mut self, char_index: (usize, char)) {
        self.next = Some(char_index);
    }
}

pub fn parse(sexp: &str) -> Vec<String> {
    let mut chars = ParseIter{next: None, iter: sexp.char_indices()};
    let mut lexems: Vec<String> = Vec::new();
    while let Some(char_index) = chars.next() {
        let char = char_index.1;
        match char {
            '(' | ')' => {
                lexems.push(char.to_string());
            },
            ' ' | '\t' => {
                lexems.push("space".to_string());
            }
            _   => {
                chars.put_back(char_index);
                // TODO: unchecked
                let symbol: String = parse_symbol(&sexp, &mut chars);
                lexems.push(symbol);
            }
        }
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


fn parse_symbol(sexp: &str, chars: &mut ParseIter) -> String {
    let mut escaped = false;
    let beg = chars.next().unwrap().0;

    while let Some(char_index) = chars.next() {
        let char = char_index.1;
        if escaped || char == '\\' {
            escaped = !escaped;
        } else if !symbol_char(char) {
            chars.put_back(char_index);
            let end = char_index.0;
            unsafe {
                return sexp.get_unchecked(beg..end).to_string();
            }
        }
    }
    sexp.get(beg..).unwrap().to_string()
}

fn run() {
    let symbols = parse::parse("(foo (bar) baz 'word) bob");
    for s in symbols {
        println!("{}", s);
    }
}
