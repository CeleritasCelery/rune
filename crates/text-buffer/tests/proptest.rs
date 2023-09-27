#![cfg(not(miri))]

use proptest::prelude::*;
use str_indices::chars::to_byte_idx;
use text_buffer::Buffer;

fn string_insert(text: &mut String, char_idx: usize, text_ins: &str) {
    let byte_idx = to_byte_idx(text, char_idx);
    text.insert_str(byte_idx, text_ins);
}

fn string_remove(text: &mut String, mut char_start: usize, mut char_end: usize) {
    if char_start > char_end {
        std::mem::swap(&mut char_start, &mut char_end);
    }
    let byte_start = to_byte_idx(text, char_start);
    let byte_end = to_byte_idx(text, char_end);
    let text_r = text.split_off(byte_end);
    text.truncate(byte_start);
    text.push_str(&text_r);
}

fn insert(buffer: &mut Buffer, string: &mut String, pos: usize, ins_text: &str) {
    let len = buffer.len_chars();
    buffer.set_cursor(pos % (len + 1));
    buffer.insert(ins_text);
    string_insert(string, pos % (len + 1), ins_text);

    assert_eq!(buffer, string);
}

fn delete(buffer: &mut Buffer, string: &mut String, beg: usize, end: usize) {
    let len = buffer.len_chars();
    let beg = beg % (len + 1);
    let end = end % (len + 1);
    buffer.delete_range(beg, end);
    string_remove(string, beg, end);

    assert_eq!(buffer, string);
}

proptest! {
    #[test]
    fn pt_build(ref text in "\\PC*") {
        let buffer = Buffer::from(&**text);
        assert_eq!(buffer, &**text);
    }

    #[test]
    fn pt_insert(char_idx in any::<usize>(), ref mut text in "\\PC*", ref ins_text in "\\PC*") {
        let buffer = &mut Buffer::from(&**text);
        insert(buffer, text, char_idx, ins_text);
    }

    #[test]
    fn pt_delete(beg in any::<usize>(), end in any::<usize>(), ref mut text in "\\PC*") {
        let buffer = &mut Buffer::from(&**text);
        delete(buffer, text, beg, end);
    }

    #[test]
    fn pt_combo(beg in any::<usize>(), end in any::<usize>(), char_idx in any::<usize>(), ref mut text in "\\PC*", ref ins_text in "\\PC*", ins_first in any::<bool>()) {
        // specifically test From<String>
        let buffer = &mut Buffer::from(String::from(&**text));
        if ins_first {
            insert(buffer, text, char_idx, ins_text);
            delete(buffer, text, beg, end);
        } else {
            delete(buffer, text, beg, end);
            insert(buffer, text, char_idx, ins_text);
        }
    }
}
