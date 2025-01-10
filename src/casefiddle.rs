//! String and character case conversion.
use std::ops::Range;

use crate::core::{
    gc::Rt,
    object::{Object, NIL},
};
use crate::fns::StringOrChar;
use crate::{Context, Env};
use rune_macros::defun;
use text_buffer::Buffer as TextBuffer;

#[defun]
fn capitalize<'ob>(string_or_char: StringOrChar<'ob>, cx: &'ob Context<'ob>) -> Object<'ob> {
    match string_or_char {
        StringOrChar::String(s) => cx.add(casify_string(s, CaseMode::Capitalize)),
        StringOrChar::Char(c) => cx.add(upcase_char(c)),
    }
}

#[defun]
fn upcase<'ob>(string_or_char: StringOrChar<'ob>, cx: &'ob Context<'ob>) -> Object<'ob> {
    match string_or_char {
        StringOrChar::String(s) => cx.add(casify_string(s, CaseMode::Upcase)),
        StringOrChar::Char(c) => cx.add(upcase_char(c)),
    }
}

#[defun]
fn downcase<'ob>(string_or_char: StringOrChar<'ob>, cx: &'ob Context<'ob>) -> Object<'ob> {
    match string_or_char {
        StringOrChar::String(s) => cx.add(casify_string(s, CaseMode::Downcase)),
        StringOrChar::Char(c) => cx.add(downcase_char(c)),
    }
}

#[defun]
fn upcase_initials<'ob>(string_or_char: StringOrChar<'ob>, cx: &'ob Context<'ob>) -> Object<'ob> {
    match string_or_char {
        StringOrChar::String(s) => cx.add(casify_string(s, CaseMode::UpcaseInitials)),
        StringOrChar::Char(c) => cx.add(upcase_char(c)),
    }
}

#[defun]
fn upcase_word<'ob>(offset: i64, env: &mut Rt<Env>) -> Object<'ob> {
    let text_buf = &mut env.current_buffer.get_mut().text;
    let forward_upcase = offset >= 0;
    let range = if forward_upcase {
        find_forward_word(text_buf)
    } else {
        find_backward_word(text_buf)
    };
    let (start, end) = (range.start, range.end);
    let (a, b) = text_buf.slice(range);
    let upcase = |x| casify_string(x, CaseMode::Upcase);
    let upcased = upcase(a) + &upcase(b);
    text_buf.delete_range(start, end);
    text_buf.insert(&upcased);
    NIL
}

#[defun]
fn downcase_word<'ob>(offset: i64, env: &mut Rt<Env>) -> Object<'ob> {
    let text_buf = &mut env.current_buffer.get_mut().text;
    let forward_downcase = offset >= 0;
    let range = if forward_downcase {
        find_forward_word(text_buf)
    } else {
        find_backward_word(text_buf)
    };
    let (start, end) = (range.start, range.end);
    let (a, b) = text_buf.slice(range);
    let downcase = |x| casify_string(x, CaseMode::Downcase);
    let downcased = downcase(a) + &downcase(b);
    text_buf.delete_range(start, end);
    text_buf.insert(&downcased);
    NIL
}

#[defun]
fn capitalize_word<'ob>(offset: i64, env: &mut Rt<Env>) -> Object<'ob> {
    let text_buf = &mut env.current_buffer.get_mut().text;
    let forward_capitalize = offset >= 0;
    let range = if forward_capitalize {
        find_forward_word(text_buf)
    } else {
        find_backward_word(text_buf)
    };
    let (start, end) = (range.start, range.end);
    let (a, b) = text_buf.slice(range);
    let capitalize = |x| casify_string(x, CaseMode::Capitalize);
    let capitalized = capitalize(a) + &capitalize(b);
    text_buf.delete_range(start, end);
    text_buf.insert(&capitalized);
    NIL
}

fn casify_string(s: &str, mode: CaseMode) -> String {
    let mut out = String::with_capacity(s.len());

    for word in s.split_inclusive(|c: char| precedes_capitalization(c)) {
        let mut chars = word.chars();
        if let Some(c) = chars.next() {
            match mode {
                CaseMode::Downcase => out.extend(c.to_lowercase()),
                CaseMode::Upcase | CaseMode::Capitalize | CaseMode::UpcaseInitials => {
                    out.extend(c.to_uppercase())
                }
            }
        }
        for c in chars {
            match mode {
                CaseMode::Upcase => out.extend(c.to_uppercase()),
                CaseMode::Downcase | CaseMode::Capitalize => out.extend(c.to_lowercase()),
                CaseMode::UpcaseInitials => out.push(c),
            }
        }
    }
    out
}

fn precedes_capitalization(c: char) -> bool {
    !c.is_alphanumeric()
}

enum CaseMode {
    Downcase,
    Upcase,
    Capitalize,
    UpcaseInitials,
}

fn upcase_char(c: u64) -> u64 {
    // emacs uses an identity function for invalid codepoints
    if c > crate::lisp::CHAR_MODIFIER_MASK {
        return c;
    }
    let c: u32 = match c.try_into() {
        Ok(c) => c,
        Err(_) => return c,
    };
    let c: char = match c.try_into() {
        Ok(c) => c,
        Err(_) => return c as u64,
    };
    let mut upper_c = c.to_uppercase();
    // if the char capitalizes to multiple characters, don't change case
    if upper_c.len() > 1 {
        return c as u64;
    }
    upper_c.next().expect("char should upcase") as u64
}

fn downcase_char(c: u64) -> u64 {
    // see `upcase_char` comment
    if c > crate::lisp::CHAR_MODIFIER_MASK {
        return c;
    }
    let c: u32 = match c.try_into() {
        Ok(c) => c,
        Err(_) => return c,
    };
    let c: char = match c.try_into() {
        Ok(c) => c,
        Err(_) => return c as u64,
    };
    let mut lower_c = c.to_lowercase();
    // if the char lowercases to multiple characters, don't change case
    if lower_c.len() > 1 {
        return c as u64;
    }
    lower_c.next().expect("char should downcase") as u64
}

fn find_forward_word(buf: &TextBuffer) -> Range<usize> {
    let cursor = buf.cursor().chars();
    let (s1, s2) = buf.slice(cursor..);
    let end = cursor
        + s1.chars()
            .chain(s2.chars())
            .enumerate()
            .skip_while(|(_, c)| !c.is_alphanumeric())
            .find(|(_, c)| c.is_whitespace())
            .map(|(idx, _)| idx)
            .unwrap_or_else(|| buf.len_chars() - cursor);
    cursor..end
}

fn find_backward_word(buf: &TextBuffer) -> Range<usize> {
    let cursor = buf.cursor().chars();
    let (s1, s2) = buf.slice(..cursor);
    let start = cursor
        - s1.chars()
            .chain(s2.chars())
            .rev()
            .enumerate()
            .skip_while(|(_, c)| !c.is_alphanumeric())
            .find(|(_, c)| c.is_whitespace())
            .map(|(idx, _)| idx)
            .unwrap_or(0);
    start..cursor
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::RootSet;

    #[test]
    fn test_downcase() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        assert_eq!(downcase("The cat in the hat".into(), cx), cx.add("the cat in the hat"));
        assert_eq!(downcase('x'.into(), cx), cx.add('x'));
        assert_eq!(downcase('X'.into(), cx), cx.add('x'));
    }

    #[test]
    fn test_upcase() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        // Emacs Doc Tests
        assert_eq!(upcase("The cat in the hat".into(), cx), cx.add("THE CAT IN THE HAT"));
        assert_eq!(upcase("ﬁ".into(), cx), cx.add("FI"));
        assert_eq!(upcase('ﬁ'.into(), cx), cx.add('ﬁ'));
        assert_eq!(upcase('x'.into(), cx), cx.add('X'));
        assert_eq!(upcase('X'.into(), cx), cx.add('X'));

        // Basic escape characters
        assert_eq!(upcase("\n".into(), cx), cx.add("\n"));
        assert_eq!(upcase("\t".into(), cx), cx.add("\t"));
        assert_eq!(upcase("\r".into(), cx), cx.add("\r"));

        // Control characters
        assert_eq!(upcase("\u{0}".into(), cx), cx.add("\u{0}"));
        assert_eq!(upcase("\u{1B}".into(), cx), cx.add("\u{1B}"));
        assert_eq!(upcase("\u{7F}".into(), cx), cx.add("\u{7F}"));

        // Non-ASCII characters
        assert_eq!(upcase("αβγ".into(), cx), cx.add("ΑΒΓ"));
        assert_eq!(upcase("åäö".into(), cx), cx.add("ÅÄÖ"));

        // Mixed content
        assert_eq!(upcase("hello\nworld".into(), cx), cx.add("HELLO\nWORLD"));
        assert_eq!(upcase("foo\tbar".into(), cx), cx.add("FOO\tBAR"));
        assert_eq!(upcase("path\\to\\file\"name\"".into(), cx), cx.add("PATH\\TO\\FILE\"NAME\""));

        // Invalid code points
        assert_eq!(upcase(StringOrChar::Char(0xD800), cx), cx.add(0xD800));
        assert_eq!(upcase(StringOrChar::Char(u64::MAX), cx), cx.add(u64::MAX));
    }

    #[test]
    fn test_capitalize() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);

        // Emacs doc tests
        assert_eq!(capitalize("The cat in the hat".into(), cx), "The Cat In The Hat");
        assert_eq!(capitalize("THE 77TH-HATTED CAT".into(), cx), "The 77th-Hatted Cat");
        assert_eq!(capitalize('x'.into(), cx), 'X');
        assert_eq!(capitalize('X'.into(), cx), 'X');
        assert_eq!(capitalize('ß'.into(), cx), 'ß');
        assert_eq!(capitalize("ß".into(), cx), "SS");

        // from elprop
        // TODO: implement syntax tables so it's known whether a character makes a word or symbol
        // // U+1D100 MUSICAL SYMBOL SINGLE BARLINE (Other-Symbol)
        // // U+0041 LATIN CAPITAL LETTER A
        // assert_eq!(capitalize(cx.add("𝄀A"), cx), Ok(cx.add("𝄀a")));
        // // U+0024 DOLLAR SIGN (Currency-Symbol)
        // // U+0041 LATIN CAPITAL LETTER A
        // assert_eq!(capitalize(cx.add("$A"), cx), Ok(cx.add("$a")));
        // // U+002D HYPHEN-MINUS (Dash-Punctuation)
        // // U+0041 LATIN CAPITAL LETTER A
        // assert_eq!(capitalize(cx.add("-A"), cx), Ok(cx.add("-A")));
        // // U+005E CIRCUMFLEX ACCENT (Modifier-Symbol)
        // // U+0041 LATIN CAPITAL LETTER A
        // assert_eq!(capitalize(cx.add("^A"), cx), Ok(cx.add("^A")));
        // // U+0FBE TIBETAN KU RU KHA (Other-Symbol)
        // // U+0041 LATIN CAPITAL LETTER A
        // assert_eq!(capitalize(cx.add("྾A"), cx), Ok(cx.add("྾A")));
        // // U+10A50 KHAROSHTHI PUNCTUATION DOT (Other-Punctuation)
        // // U+104B0 OSAGE CAPITAL LETTER A
        // // (becomes) U+104D8 OSAGE SMALL LETTER A
        // assert_eq!(capitalize(cx.add("𐩐𐒰"), cx), Ok(cx.add("𐩐𐓘")));
    }

    #[test]
    fn test_upcase_initials() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);

        // Emacs Doc Tests
        assert_eq!(upcase_initials("The CAT in the hAt".into(), cx), cx.add("The CAT In The HAt"));
        assert_eq!(upcase_initials('x'.into(), cx), cx.add('X'));
        assert_eq!(upcase_initials('X'.into(), cx), cx.add('X'));
    }

    #[cfg(not(miri))] // Uses SIMD
    mod upcase_word {
        use crate::core::gc::Context;
        use crate::core::gc::RootSet;
        use rune_core::macros::root;

        use super::*;

        #[test]
        fn forward() {
            let roots = &RootSet::default();
            let cx = &mut Context::new(roots);
            root!(env, new(Env), cx);

            // αβγ word
            // ^-----
            env.current_buffer.get_mut().text.insert("αβγ word");
            env.current_buffer.get_mut().text.set_cursor(0);
            upcase_word(1, env);
            assert_eq!(env.current_buffer.get().text, "ΑΒΓ word");
            env.current_buffer.get_mut().text = text_buffer::Buffer::default();
            env.current_buffer.get_mut().text.insert("ΑΒΓ woRd");
            env.current_buffer.get_mut().text.set_cursor(0);
            downcase_word(1, env);
            assert_eq!(env.current_buffer.get().text, "αβγ woRd");
            env.current_buffer.get_mut().text = text_buffer::Buffer::default();
            env.current_buffer.get_mut().text.insert("αΒΓ wORD");
            env.current_buffer.get_mut().text.set_cursor(0);
            capitalize_word(1, env);
            assert_eq!(env.current_buffer.get().text, "Αβγ wORD");
        }

        #[test]
        fn backward() {
            let roots = &RootSet::default();
            let cx = &mut Context::new(roots);
            root!(env, new(Env), cx);

            // upcase αβγword
            //        -------^
            env.current_buffer.get_mut().text.insert("upcase αβγword ");
            env.current_buffer.get_mut().text.set_cursor(15);
            upcase_word(-1, env);
            assert_eq!(env.current_buffer.get().text, "upcase ΑΒΓWORD ");
            env.current_buffer.get_mut().text = text_buffer::Buffer::default();
            env.current_buffer.get_mut().text.insert("dOwNcAsE αΒΓWord ");
            env.current_buffer.get_mut().text.set_cursor(17);
            downcase_word(-1, env);
            assert_eq!(env.current_buffer.get().text, "dOwNcAsE αβγword ");
            env.current_buffer.get_mut().text = text_buffer::Buffer::default();
            env.current_buffer.get_mut().text.insert("cAPITALIZE αΒΓWORD ");
            env.current_buffer.get_mut().text.set_cursor(19);
            capitalize_word(-1, env);
            assert_eq!(env.current_buffer.get().text, "cAPITALIZE Αβγword ");
        }

        #[test]
        fn forward_cursor_inside_of_word() {
            let roots = &RootSet::default();
            let cx = &mut Context::new(roots);
            root!(env, new(Env), cx);

            // upcase word
            //  ^----
            env.current_buffer.get_mut().text.insert("upcase word");
            env.current_buffer.get_mut().text.set_cursor(2);
            upcase_word(1, env);
            assert_eq!(env.current_buffer.get().text, "upCASE word");
            env.current_buffer.get_mut().text = text_buffer::Buffer::default();
            env.current_buffer.get_mut().text.insert("DOWNCASE WORD");
            env.current_buffer.get_mut().text.set_cursor(2);
            downcase_word(1, env);
            assert_eq!(env.current_buffer.get().text, "DOwncase WORD");
            env.current_buffer.get_mut().text = text_buffer::Buffer::default();
            env.current_buffer.get_mut().text.insert("capitalize word");
            env.current_buffer.get_mut().text.set_cursor(2);
            capitalize_word(1, env);
            assert_eq!(env.current_buffer.get().text, "caPitalize word");
        }

        #[test]
        fn backward_cursor_inside_of_word() {
            let roots = &RootSet::default();
            let cx = &mut Context::new(roots);
            root!(env, new(Env), cx);

            // upcase word
            //        --^
            env.current_buffer.get_mut().text.insert("upcase word");
            env.current_buffer.get_mut().text.set_cursor(9);
            upcase_word(-1, env);
            assert_eq!(env.current_buffer.get().text, "upcase WOrd");
            env.current_buffer.get_mut().text = text_buffer::Buffer::default();
            env.current_buffer.get_mut().text.insert("downcase WORD");
            env.current_buffer.get_mut().text.set_cursor(11);
            downcase_word(-1, env);
            assert_eq!(env.current_buffer.get().text, "downcase woRD");
            env.current_buffer.get_mut().text = text_buffer::Buffer::default();
            env.current_buffer.get_mut().text.insert("capitalize word");
            env.current_buffer.get_mut().text.set_cursor(13);
            capitalize_word(-1, env);
            assert_eq!(env.current_buffer.get().text, "capitalize Word");
        }
    }
}
