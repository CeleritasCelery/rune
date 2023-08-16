#![cfg(not(miri))]

use proptest::prelude::*;
use str_indices::chars::to_byte_idx;
use text_buffer::Buffer;

fn string_insert(text: &mut String, char_idx: usize, text_ins: &str) {
    let byte_idx = to_byte_idx(text, char_idx);
    text.insert_str(byte_idx, text_ins);
}

fn string_remove(text: &mut String, char_start: usize, char_end: usize) {
    let byte_start = to_byte_idx(text, char_start);
    let byte_end = to_byte_idx(text, char_end);
    let text_r = text.split_off(byte_end);
    text.truncate(byte_start);
    text.push_str(&text_r);
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(1000))]

    #[test]
    fn pt_build(ref text in "\\PC*") {
        let buffer = Buffer::from(&**text);
        assert_eq!(buffer, &**text);
    }

    #[test]
    fn pt_insert(char_idx in any::<usize>(), ref mut text in "\\PC*", ref ins_text in "\\PC*") {
        let mut buffer = Buffer::from(&**text);

        let len = buffer.len_chars();
        buffer.set_cursor(char_idx % (len + 1));
        buffer.insert(ins_text);
        string_insert(text, char_idx % (len + 1), ins_text);

        assert_eq!(buffer, text);
    }

    #[test]
    fn pt_delete(beg in any::<usize>(), end in any::<usize>(), ref mut text in "\\PC*") {
        let mut buffer = Buffer::from(&**text);

        let len = buffer.len_chars();
        let beg = beg % (len + 1);
        let end = end % (len + 1);
        buffer.delete_range(beg, end);

        if beg > end {
            string_remove(text, end, beg);
        } else {
            string_remove(text, beg, end);
        }

        assert_eq!(buffer, text);
    }
}
