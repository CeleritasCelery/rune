//! String and character case conversion.
use std::ops::Range;

use crate::core::gc::Rt;
use crate::core::object::Object;
use crate::core::object::NIL;
use crate::Env;
use rune_macros::defun;
use rune_macros::elprop;
use text_buffer::Buffer as TextBuffer;

#[defun]
fn capitalize(s: &str) -> String {
    titlecase::titlecase(s)
}

#[defun]
// skip control characters and characters that give a different answer in unicode 16 (Rust) and 15 (Emacs)
#[elprop(r"[^\u{0}-\u{1F}\u{17F}\u{10D70}]*")]
fn upcase(s: &str) -> String {
    s.to_uppercase()
}

#[defun]
fn upcase_word<'ob>(arg: i64, env: &mut Rt<Env>) -> Object<'ob> {
    fn find_forward_range(buf: &TextBuffer) -> Range<usize> {
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

    fn find_backward_range(buf: &TextBuffer) -> Range<usize> {
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

    let text_buf = &mut env.current_buffer.get_mut().text;
    let forward_upcase = arg >= 0;
    let range = if forward_upcase {
        find_forward_range(text_buf)
    } else {
        find_backward_range(text_buf)
    };
    let (start, end) = (range.start, range.end);
    let (a, b) = text_buf.slice(range);
    let upcased = upcase(a) + &upcase(b);
    text_buf.delete_range(start, end);
    text_buf.insert(&upcased);
    NIL
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_upcase() {
        // Basic escape characters
        assert_eq!("\n", upcase("\n"));
        assert_eq!("\t", upcase("\t"));
        assert_eq!("\r", upcase("\r"));

        // Control characters
        assert_eq!("\u{0}", upcase("\u{0}"));
        assert_eq!("\u{1B}", upcase("\u{1B}"));
        assert_eq!("\u{7F}", upcase("\u{7F}"));

        // Non-ASCII characters
        assert_eq!("ΑΒΓ", upcase("αβγ"));
        assert_eq!("ÅÄÖ", upcase("åäö"));

        // Mixed content
        assert_eq!("HELLO\nWORLD", upcase("hello\nworld"));
        assert_eq!("FOO\tBAR", upcase("foo\tbar"));
        assert_eq!("PATH\\TO\\FILE\"NAME\"", upcase("path\\to\\file\"name\""));
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
            env.current_buffer.get_mut().text.insert("αβγ word");

            // αβγ word
            // ^-----
            env.current_buffer.get_mut().text.set_cursor(0);
            upcase_word(1, env);

            assert_eq!(env.current_buffer.get().text, "ΑΒΓ word");
        }

        #[test]
        fn backward() {
            let roots = &RootSet::default();
            let cx = &mut Context::new(roots);
            root!(env, new(Env), cx);
            env.current_buffer.get_mut().text.insert("upcase αβγword ");

            // upcase αβγword
            //        -------^
            env.current_buffer.get_mut().text.set_cursor(15);
            upcase_word(-1, env);

            assert_eq!(env.current_buffer.get().text, "upcase ΑΒΓWORD ");
        }

        #[test]
        fn forward_cursor_inside_of_word() {
            let roots = &RootSet::default();
            let cx = &mut Context::new(roots);
            root!(env, new(Env), cx);
            env.current_buffer.get_mut().text.insert("upcase word");

            // upcase word
            //  ^----
            env.current_buffer.get_mut().text.set_cursor(2);
            upcase_word(1, env);

            assert_eq!(env.current_buffer.get().text, "upCASE word");
        }

        #[test]
        fn backward_cursor_inside_of_word() {
            let roots = &RootSet::default();
            let cx = &mut Context::new(roots);
            root!(env, new(Env), cx);
            env.current_buffer.get_mut().text.insert("upcase word");

            // upcase word
            //        --^
            env.current_buffer.get_mut().text.set_cursor(9);
            upcase_word(-1, env);

            assert_eq!(env.current_buffer.get().text, "upcase WOrd");
        }
    }
}
