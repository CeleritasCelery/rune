//! String and character case conversion.
use std::ops::Range;

use crate::core::{
    error::TypeError,
    gc::Rt,
    object::{IntoObject, Object, ObjectType, NIL},
};
use crate::fns::StringOrChar;
use crate::{Context, Env};
use rune_macros::{defun, elprop};
use text_buffer::Buffer as TextBuffer;

#[defun]
#[elprop(StringOrChar)]
fn capitalize<'ob>(
    string_or_char: Object<'ob>,
    cx: &'ob Context<'ob>,
) -> Result<Object<'ob>, TypeError> {
    let string_or_char: StringOrChar = string_or_char.try_into()?;
    match string_or_char {
        StringOrChar::String(s) => Ok(casify_string(s, CaseMode::Capitalize).into_obj(cx).into()),
        StringOrChar::Char(c) => Ok(upcase_char(c).into_obj(cx).into()),
    }
}

#[defun]
// skip control characters and characters that give a different answer in unicode 16 (Rust) and 15 (Emacs)
#[elprop(StringOrChar)]
fn upcase<'ob>(
    string_or_char: Object<'ob>,
    cx: &'ob Context<'ob>,
) -> Result<Object<'ob>, TypeError> {
    let string_or_char: StringOrChar = string_or_char.try_into()?;
    match string_or_char {
        StringOrChar::String(s) => Ok(casify_string(s, CaseMode::Upcase).into_obj(cx).into()),
        StringOrChar::Char(c) => Ok(upcase_char(c).into_obj(cx).into()),
    }
}

#[defun]
#[elprop(StringOrChar)]
fn downcase<'ob>(
    string_or_char: Object<'ob>,
    cx: &'ob Context<'ob>,
) -> Result<Object<'ob>, TypeError> {
    let string_or_char: StringOrChar = string_or_char.try_into()?;
    match string_or_char {
        StringOrChar::String(s) => Ok(casify_string(s, CaseMode::Downcase).into_obj(cx).into()),
        StringOrChar::Char(c) => Ok(downcase_char(c).into_obj(cx).into()),
    }
}

#[defun]
#[elprop(StringOrChar)]
fn upcase_initials<'ob>(
    string_or_char: Object<'ob>,
    cx: &'ob Context<'ob>,
) -> Result<Object<'ob>, TypeError> {
    let string_or_char: StringOrChar = string_or_char.try_into()?;
    match string_or_char {
        StringOrChar::String(s) => {
            Ok(casify_string(s, CaseMode::UpcaseInitials).into_obj(cx).into())
        }
        StringOrChar::Char(c) => Ok(upcase_char(c).into_obj(cx).into()),
    }
}

#[defun]
fn upcase_word<'ob>(offset: i64, env: &mut Rt<Env>, cx: &'ob Context<'ob>) -> Object<'ob> {
    let text_buf = &mut env.current_buffer.get_mut().text;
    let forward_upcase = offset >= 0;
    let range = if forward_upcase {
        find_forward_word(text_buf)
    } else {
        find_backward_word(text_buf)
    };
    let (start, end) = (range.start, range.end);
    let (a, b) = text_buf.slice(range);
    // we're passing in a string, so we'll successfully get a string back
    let upcase_a = match upcase(cx.add(a), cx).unwrap().untag() {
        ObjectType::String(s) => s,
        v => panic!("non-string object {v}"),
    };
    let upcase_b = match upcase(cx.add(b), cx).unwrap().untag() {
        ObjectType::String(s) => s,
        v => panic!("non-string object {v}"),
    };
    let mut upcased = String::from(upcase_a.as_ref());
    upcased.push_str(upcase_b.as_ref());
    text_buf.delete_range(start, end);
    text_buf.insert(&upcased);
    NIL
}

#[defun]
fn downcase_word<'ob>(offset: i64, env: &mut Rt<Env>, cx: &'ob Context<'ob>) -> Object<'ob> {
    let text_buf = &mut env.current_buffer.get_mut().text;
    let forward_downcase = offset >= 0;
    let range = if forward_downcase {
        find_forward_word(text_buf)
    } else {
        find_backward_word(text_buf)
    };
    let (start, end) = (range.start, range.end);
    let (a, b) = text_buf.slice(range);
    // we're passing in a string, so we'll successfully get a string back
    let downcase_a = match downcase(cx.add(a), cx).unwrap().untag() {
        ObjectType::String(s) => s,
        v => panic!("non-string object {v}"),
    };
    let downcase_b = match downcase(cx.add(b), cx).unwrap().untag() {
        ObjectType::String(s) => s,
        v => panic!("non-string object {v}"),
    };
    let mut downcased = String::from(downcase_a.as_ref());
    downcased.push_str(downcase_b.as_ref());
    text_buf.delete_range(start, end);
    text_buf.insert(&downcased);
    NIL
}

#[defun]
fn capitalize_word<'ob>(offset: i64, env: &mut Rt<Env>, cx: &'ob Context<'ob>) -> Object<'ob> {
    let text_buf = &mut env.current_buffer.get_mut().text;
    let forward_capitalize = offset >= 0;
    let range = if forward_capitalize {
        find_forward_word(text_buf)
    } else {
        find_backward_word(text_buf)
    };
    let (start, end) = (range.start, range.end);
    let (a, b) = text_buf.slice(range);
    // we're passing in a string, so we'll successfully get a string back
    let capitalize_a = match capitalize(cx.add(a), cx).unwrap().untag() {
        ObjectType::String(s) => s,
        _ => unreachable!(),
    };
    let capitalize_b = match capitalize(cx.add(b), cx).unwrap().untag() {
        ObjectType::String(s) => s,
        _ => unreachable!(),
    };
    let mut capitalized = String::from(capitalize_a.as_ref());
    capitalized.push_str(capitalize_b.as_ref());
    text_buf.delete_range(start, end);
    text_buf.insert(&capitalized);
    NIL
}

fn casify_string(s: &str, mode: CaseMode) -> String {
    let mut out = String::with_capacity(s.len());

    for word in s.split_inclusive(|c: char| precedes_capitalization(c)) {
        for (i, c) in word.char_indices() {
            if i == 0 {
                match mode {
                    CaseMode::Downcase => out.push_str(&c.to_lowercase().to_string()),
                    CaseMode::Upcase | CaseMode::Capitalize | CaseMode::UpcaseInitials => {
                        out.push_str(&c.to_uppercase().to_string())
                    }
                };
            } else {
                match mode {
                    CaseMode::Upcase => out.push_str(&c.to_uppercase().to_string()),
                    CaseMode::Downcase | CaseMode::Capitalize => {
                        out.push_str(&c.to_lowercase().to_string())
                    }
                    CaseMode::UpcaseInitials => out.push(c),
                }
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

fn upcase_char(c: i64) -> i64 {
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
        Err(_) => return c as i64,
    };
    let mut upper_c = c.to_uppercase();
    // if the char capitalizes to multiple characters, don't change case
    if upper_c.len() > 1 {
        return c as i64;
    }
    upper_c.next().expect("'{c}' should upcase") as i64
}

fn downcase_char(c: i64) -> i64 {
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
        Err(_) => return c as i64,
    };
    let mut lower_c = c.to_lowercase();
    // if the char lowercases to multiple characters, don't change case
    if lower_c.len() > 1 {
        return c as i64;
    }
    lower_c.next().expect("'{c}' should downcase") as i64
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
        assert_eq!(downcase(cx.add("The cat in the hat"), cx), Ok(cx.add("the cat in the hat")));
        assert_eq!(downcase(cx.add('x'), cx), Ok(cx.add('x')));
        assert_eq!(downcase(cx.add('X'), cx), Ok(cx.add('x')));
        assert!(downcase(cx.add(3.17), cx).is_err());
        assert!(downcase(cx.add(-1), cx).is_err());
    }

    #[test]
    fn test_upcase() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        // Emacs Doc Tests
        assert_eq!(upcase(cx.add("The cat in the hat"), cx), Ok(cx.add("THE CAT IN THE HAT")));
        assert_eq!(upcase(cx.add("ﬁ"), cx), Ok(cx.add("FI")));
        assert_eq!(upcase(cx.add('ﬁ'), cx), Ok(cx.add('ﬁ')));
        assert_eq!(upcase(cx.add('x'), cx), Ok(cx.add('X')));
        assert_eq!(upcase(cx.add('X'), cx), Ok(cx.add('X')));

        // Basic escape characters
        assert_eq!(Ok(cx.add("\n")), upcase(cx.add("\n"), cx));
        assert_eq!(Ok(cx.add("\t")), upcase(cx.add("\t"), cx));
        assert_eq!(Ok(cx.add("\r")), upcase(cx.add("\r"), cx));

        // Control characters
        assert_eq!(Ok(cx.add("\u{0}")), upcase(cx.add("\u{0}"), cx));
        assert_eq!(Ok(cx.add("\u{1B}")), upcase(cx.add("\u{1B}"), cx));
        assert_eq!(Ok(cx.add("\u{7F}")), upcase(cx.add("\u{7F}"), cx));

        // Non-ASCII characters
        assert_eq!(Ok(cx.add("ΑΒΓ")), upcase(cx.add("αβγ"), cx));
        assert_eq!(Ok(cx.add("ÅÄÖ")), upcase(cx.add("åäö"), cx));

        // Mixed content
        assert_eq!(Ok(cx.add("HELLO\nWORLD")), upcase(cx.add("hello\nworld"), cx));
        assert_eq!(Ok(cx.add("FOO\tBAR")), upcase(cx.add("foo\tbar"), cx));
        assert_eq!(
            Ok(cx.add("PATH\\TO\\FILE\"NAME\"")),
            upcase(cx.add("path\\to\\file\"name\""), cx)
        );

        // Invalid code points
        assert_eq!(upcase(cx.add(0xD800), cx), Ok(cx.add(0xD800)));
        assert_eq!(upcase(cx.add(i64::MAX), cx), Ok(cx.add(i64::MAX)));

        // Wrong type input
        assert!(upcase(cx.add(3.17), cx).is_err());
        assert!(upcase(cx.add(-1), cx).is_err());
    }

    #[test]
    fn test_capitalize() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);

        // Emacs doc tests
        assert_eq!(capitalize(cx.add("The cat in the hat"), cx), Ok(cx.add("The Cat In The Hat")));
        assert_eq!(
            capitalize(cx.add("THE 77TH-HATTED CAT"), cx),
            Ok(cx.add("The 77th-Hatted Cat"))
        );
        assert_eq!(capitalize(cx.add('x'), cx), Ok(cx.add('X')));
        assert_eq!(capitalize(cx.add('X'), cx), Ok(cx.add('X')));
        assert_eq!(capitalize(cx.add('ß'), cx), Ok(cx.add('ß')));
        assert_eq!(capitalize(cx.add("ß"), cx), Ok(cx.add("SS")));

        // Wrong type input
        assert!(capitalize(cx.add(3.17), cx).is_err());
        assert!(capitalize(cx.add(-1), cx).is_err());

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
        assert_eq!(
            upcase_initials(cx.add("The CAT in the hAt"), cx),
            Ok(cx.add("The CAT In The HAt"))
        );
        assert_eq!(upcase_initials(cx.add('x'), cx), Ok(cx.add('X')));
        assert_eq!(upcase_initials(cx.add('X'), cx), Ok(cx.add('X')));
        assert!(upcase_initials(cx.add(3.17), cx).is_err());
        assert!(upcase_initials(cx.add(-1), cx).is_err());
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
            upcase_word(1, env, cx);
            assert_eq!(env.current_buffer.get().text, "ΑΒΓ word");
            env.current_buffer.get_mut().text = text_buffer::Buffer::default();
            env.current_buffer.get_mut().text.insert("ΑΒΓ woRd");
            env.current_buffer.get_mut().text.set_cursor(0);
            downcase_word(1, env, cx);
            assert_eq!(env.current_buffer.get().text, "αβγ woRd");
            env.current_buffer.get_mut().text = text_buffer::Buffer::default();
            env.current_buffer.get_mut().text.insert("αΒΓ wORD");
            env.current_buffer.get_mut().text.set_cursor(0);
            capitalize_word(1, env, cx);
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
            upcase_word(-1, env, cx);
            assert_eq!(env.current_buffer.get().text, "upcase ΑΒΓWORD ");
            env.current_buffer.get_mut().text = text_buffer::Buffer::default();
            env.current_buffer.get_mut().text.insert("dOwNcAsE αΒΓWord ");
            env.current_buffer.get_mut().text.set_cursor(17);
            downcase_word(-1, env, cx);
            assert_eq!(env.current_buffer.get().text, "dOwNcAsE αβγword ");
            env.current_buffer.get_mut().text = text_buffer::Buffer::default();
            env.current_buffer.get_mut().text.insert("cAPITALIZE αΒΓWORD ");
            env.current_buffer.get_mut().text.set_cursor(19);
            capitalize_word(-1, env, cx);
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
            upcase_word(1, env, cx);
            assert_eq!(env.current_buffer.get().text, "upCASE word");
            env.current_buffer.get_mut().text = text_buffer::Buffer::default();
            env.current_buffer.get_mut().text.insert("DOWNCASE WORD");
            env.current_buffer.get_mut().text.set_cursor(2);
            downcase_word(1, env, cx);
            assert_eq!(env.current_buffer.get().text, "DOwncase WORD");
            env.current_buffer.get_mut().text = text_buffer::Buffer::default();
            env.current_buffer.get_mut().text.insert("capitalize word");
            env.current_buffer.get_mut().text.set_cursor(2);
            capitalize_word(1, env, cx);
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
            upcase_word(-1, env, cx);
            assert_eq!(env.current_buffer.get().text, "upcase WOrd");
            env.current_buffer.get_mut().text = text_buffer::Buffer::default();
            env.current_buffer.get_mut().text.insert("downcase WORD");
            env.current_buffer.get_mut().text.set_cursor(11);
            downcase_word(-1, env, cx);
            assert_eq!(env.current_buffer.get().text, "downcase woRD");
            env.current_buffer.get_mut().text = text_buffer::Buffer::default();
            env.current_buffer.get_mut().text.insert("capitalize word");
            env.current_buffer.get_mut().text.set_cursor(13);
            capitalize_word(-1, env, cx);
            assert_eq!(env.current_buffer.get().text, "capitalize Word");
        }
    }
}
