#![expect(unused_qualifications)]
#![cfg(not(miri))]
use proptest::prelude::*;
use proptest_derive::Arbitrary;
use str_indices::chars::to_byte_idx;
use text_buffer::Buffer;

fn string_slice(text: &str, char_beg: usize, char_end: usize) -> &str {
    let byte_beg = to_byte_idx(text, char_beg);
    let byte_end = to_byte_idx(text, char_end);
    &text[byte_beg..byte_end]
}

fn string_insert(text: &mut String, char_idx: usize, text_ins: &str) {
    let byte_idx = to_byte_idx(text, char_idx);
    text.insert_str(byte_idx, text_ins);
}

fn string_remove(text: &mut String, mut char_beg: usize, mut char_end: usize) {
    if char_beg > char_end {
        std::mem::swap(&mut char_beg, &mut char_end);
    }
    let byte_start = to_byte_idx(text, char_beg);
    let byte_end = to_byte_idx(text, char_end);
    let text_r = text.split_off(byte_end);
    text.truncate(byte_start);
    text.push_str(&text_r);
}

fn slice(buffer: &Buffer, string: &str, char_beg: usize, char_end: usize) {
    let len = buffer.len_chars();
    let mut beg = char_beg % (len + 1);
    let mut end = char_end % (len + 1);
    if beg > end {
        std::mem::swap(&mut beg, &mut end);
    }
    assert_eq!(beg, buffer.byte_to_char(buffer.char_to_byte(beg)));
    assert_eq!(end, buffer.byte_to_char(buffer.char_to_byte(end)));
    let (s1, s2) = buffer.slice(beg..end);
    let string_slice = string_slice(string, beg, end);
    let buffer_slice = s1.to_owned() + s2;
    assert_eq!(buffer_slice, string_slice);
}

fn insert(buffer: &mut Buffer, string: &mut String, pos: usize, ins_text: &str) {
    let len = buffer.len_chars();
    let point = pos % (len + 1);
    assert_eq!(point, buffer.byte_to_char(buffer.char_to_byte(point)));
    buffer.set_cursor(point);
    buffer.insert(ins_text);
    string_insert(string, point, ins_text);

    assert_eq!(buffer, string);
}

fn delete(buffer: &mut Buffer, string: &mut String, beg: usize, end: usize) {
    let len = buffer.len_chars();
    let beg = beg % (len + 1);
    let end = end % (len + 1);
    assert_eq!(beg, buffer.byte_to_char(buffer.char_to_byte(beg)));
    assert_eq!(end, buffer.byte_to_char(buffer.char_to_byte(end)));
    buffer.delete_range(beg, end);
    string_remove(string, beg, end);

    assert_eq!(buffer, string);
}

#[derive(Arbitrary, Debug)]
struct Insert {
    idx: usize,
    ins_text: String,
}

#[derive(Arbitrary, Debug)]
struct Delete {
    beg: usize,
    end: usize,
}

#[derive(Arbitrary, Debug)]
struct Slice {
    beg: usize,
    end: usize,
}

#[derive(Arbitrary, Debug)]
enum Edit {
    Insert(Insert),
    Delete(Delete),
    Slice(Slice),
}

proptest! {
    #[test]
    fn pt_build(ref text in "\\PC*") {
        let buffer = Buffer::from(&**text);
        assert_eq!(buffer, &**text);
    }

    #[test]
    fn pt_insert(ins in any::<Insert>(), ref mut text in "\\PC*") {
        let buffer = &mut Buffer::from(&**text);
        insert(buffer, text, ins.idx, &ins.ins_text);
    }

    #[test]
    fn pt_delete(del in any::<Delete>(), ref mut text in "\\PC*") {
        let buffer = &mut Buffer::from(&**text);
        delete(buffer, text, del.beg, del.end);
    }

    #[test]
    fn pt_combo(edits in any::<[Edit; 20]>(), ref mut text in "\\PC*") {
        // specifically test From<String>
        let buffer = &mut Buffer::from(String::from(&**text));
        for edit in edits {
            match edit {
                Edit::Insert(Insert { idx, ins_text }) => {
                    insert(buffer, text, idx, &ins_text);
                }
                Edit::Delete(Delete { beg, end }) => {
                    delete(buffer, text, beg, end);
                }
                Edit::Slice(Slice { beg, end }) => {
                    slice(buffer, text, beg, end);
                }
            }
        }
    }
}
