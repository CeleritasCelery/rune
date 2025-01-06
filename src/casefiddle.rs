//! String and character case conversion.
use std::ops::Range;

use crate::core::gc::Rt;
use crate::core::object::Object;
use crate::core::object::NIL;
use crate::Env;
use anyhow::Result;
use rune_macros::defun;
use text_buffer::Buffer as TextBuffer;

#[defun]
fn capitalize(s: &str) -> String {
    titlecase::titlecase(s)
}

#[defun]
fn upcase(s: &str) -> String {
    s.to_uppercase()
}

#[defun]
fn upcase_word<'ob>(arg: i64, env: &mut Rt<Env>) -> Result<Object<'ob>> {
    fn find_forward_range(buf: &TextBuffer) -> Range<usize> {
        let cursor = buf.cursor().chars();
        let num_chars = buf.len_chars() - cursor;
        let (s1, s2) = buf.slice(cursor..);
        let end = cursor
            + s1.chars()
                .chain(s2.chars())
                .enumerate()
                .skip_while(|(_, c)| !c.is_alphanumeric())
                .find(|(_, c)| c.is_whitespace())
                .map(|(idx, _)| idx)
                .unwrap_or(num_chars);
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
    Ok(NIL)
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

    mod upcase_word {
        use crate::buffer::generate_new_buffer_name;
        use crate::core::gc::Context;
        use crate::core::gc::RootSet;
        use rune_core::macros::root;

        use super::*;

        fn setup_buffer(cx: &mut Context, env: &mut Rt<Env>, text: &str) {
            generate_new_buffer_name(" ", None);
            env.current_buffer.get_mut().insert(cx.add(String::from(text))).unwrap();
        }

        #[test]
        fn forward() {
            let roots = &RootSet::default();
            let cx = &mut Context::new(roots);
            cx.garbage_collect(true);
            root!(env, new(Env), cx);
            setup_buffer(cx, env, "αβγ word");

            // αβγ word
            // ^-----
            env.current_buffer.get_mut().text.set_cursor(0);
            upcase_word(1, env).unwrap();

            assert_eq!(env.current_buffer.get().text.slice_whole(..), "ΑΒΓ word");
            assert_eq!(env.current_buffer.get().text.cursor().chars(), 3);
        }

        #[test]
        fn backward() {
            let roots = &RootSet::default();
            let cx = &mut Context::new(roots);
            cx.garbage_collect(true);
            root!(env, new(Env), cx);
            setup_buffer(cx, env, "upcase αβγword ");

            // upcase αβγword
            //        -------^
            env.current_buffer.get_mut().text.set_cursor(15);
            upcase_word(-1, env).unwrap();

            assert_eq!(env.current_buffer.get().text.slice_whole(..), "upcase ΑΒΓWORD ");
            assert_eq!(env.current_buffer.get().text.cursor().chars(), 15);
        }

        #[test]
        fn forward_cursor_inside_of_word() {
            let roots = &RootSet::default();
            let cx = &mut Context::new(roots);
            cx.garbage_collect(true);
            root!(env, new(Env), cx);
            setup_buffer(cx, env, "upcase word");

            // upcase word
            //  ^----
            env.current_buffer.get_mut().text.set_cursor(2);
            upcase_word(1, env).unwrap();

            assert_eq!(env.current_buffer.get().text.slice_whole(..), "upCASE word");
            assert_eq!(env.current_buffer.get().text.cursor().chars(), 6);
        }

        #[test]
        fn backward_cursor_inside_of_word() {
            let roots = &RootSet::default();
            let cx = &mut Context::new(roots);
            cx.garbage_collect(true);
            root!(env, new(Env), cx);
            setup_buffer(cx, env, "upcase word");

            // upcase word
            //        --^
            env.current_buffer.get_mut().text.set_cursor(9);
            upcase_word(-1, env).unwrap();

            assert_eq!(env.current_buffer.get().text.slice_whole(..), "upcase WOrd");
            assert_eq!(env.current_buffer.get().text.cursor().chars(), 9);
        }
    }
}
