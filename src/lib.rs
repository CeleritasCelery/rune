#![allow(dead_code)]
#![warn(clippy::all, clippy::pedantic)]
#![allow(clippy::uninlined_format_args)]
#![allow(clippy::must_use_candidate)]
use std::{
    fmt::{Debug, Display},
    ops::{Bound, RangeBounds},
};

use bytecount::num_chars;
use str_indices::chars;

/// A Gap buffer. This represents the text of a buffer, and allows for
/// efficient insertion and deletion of text.
pub struct Buffer {
    /// The buffer data
    data: Box<[u8]>,
    /// start of the gap. Both gap_start and gap_end are the same point, but
    /// gap_start is never a valid byte index, and gap_end is always used
    /// instead.
    gap_start: usize,
    /// The end of the gap in bytes
    gap_end: usize,
    /// The number of characters until the gap
    gap_chars: usize,
    /// The current cursor. The first field is the byte index, the second is the
    /// character count.
    cursor: Point,
    total_chars: usize,
}

impl Display for Buffer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_str(..self.gap_start))?;
        f.write_str(self.to_str(self.gap_end..))
    }
}

impl Debug for Buffer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let start = self.to_str(..self.gap_start);
        let end = self.to_str(self.gap_end..);
        // repeat _ for the gap length
        let gap = "_".repeat(self.gap_len());
        f.debug_struct("Buffer")
            .field("data", &format!("{start}{gap}{end}"))
            .field("gap_start", &self.gap_start)
            .field("gap_end", &self.gap_end)
            .field("gap_chars", &self.gap_chars)
            .field("cursor", &self.cursor)
            .field("total_chars", &self.total_chars)
            .finish()
    }
}

#[derive(Debug, Default, Copy, Clone)]
struct Point {
    byte: usize,
    char: usize,
}

impl Buffer {
    const GAP_SIZE: usize = 5;

    #[must_use]
    pub fn new(data: &str) -> Self {
        let storage = {
            let capacity = data.len() + Self::GAP_SIZE;
            let mut storage = Vec::with_capacity(capacity);
            storage.resize(Self::GAP_SIZE, 0);
            storage.extend_from_slice(data.as_bytes());
            debug_assert_eq!(storage.len(), capacity);
            storage.into_boxed_slice()
        };
        Self {
            data: storage,
            gap_start: 0,
            gap_end: Self::GAP_SIZE,
            gap_chars: 0,
            cursor: Point {
                byte: Self::GAP_SIZE,
                char: 0,
            },
            total_chars: chars::count(data),
        }
    }

    fn grow(&mut self, slice: &str) {
        let new_capacity = {
            let pre_gap = self.gap_start;
            let post_gap = self.data.len() - self.gap_end;
            pre_gap + slice.len() + Self::GAP_SIZE + post_gap
        };
        let new_storage = {
            let mut buffer = Vec::with_capacity(new_capacity);
            // pre-gap
            buffer.extend_from_slice(&self.data[..self.gap_start]);
            // new text
            buffer.extend_from_slice(slice.as_bytes());
            // gap
            buffer.resize(buffer.len() + Self::GAP_SIZE, 0);
            // post-gap
            buffer.extend_from_slice(&self.data[self.gap_end..]);
            buffer.into_boxed_slice()
        };
        assert_eq!(new_storage.len(), new_capacity);
        self.data = new_storage;
        self.gap_start += slice.len();
        self.gap_end = self.gap_start + Self::GAP_SIZE;
        self.cursor.byte = self.gap_end;
        let num_chars = chars::count(slice);
        self.gap_chars += num_chars;
        self.cursor.char = self.gap_chars;
        self.total_chars += num_chars;
    }

    pub(crate) fn insert_char(&mut self, chr: char) {
        let buf = &mut [0; 4];
        self.insert(chr.encode_utf8(buf));
    }

    pub fn insert(&mut self, slice: &str) {
        // if gap is not at cursor, move it there
        if self.gap_chars != self.cursor.char {
            // TODO: we don't need to recalculate the position
            self.move_gap(self.cursor.char);
        }
        if (self.gap_end - self.gap_start) < slice.len() {
            // TODO: grow the gap and move the cursor in one go
            self.grow(slice);
        } else {
            let new_slice = &mut self.data[self.gap_start..(self.gap_start + slice.len())];
            new_slice.copy_from_slice(slice.as_bytes());
            self.gap_start += slice.len();
            let num_chars = chars::count(slice);
            self.gap_chars += num_chars;
            self.cursor.char += num_chars;
            self.total_chars += num_chars;
        }
    }

    fn delete_backwards(&mut self, size: usize) {
        self.delete_region(self.gap_chars - size, self.gap_chars);
    }

    fn delete_forwards(&mut self, size: usize) {
        self.delete_region(self.gap_chars, self.gap_chars + size);
    }

    pub fn delete_region(&mut self, beg: usize, end: usize) {
        let (mut beg, mut end) = (beg, end);
        if beg > end {
            (beg, end) = (end, beg);
        }
        let end = self.char_to_byte(end.min(self.total_chars));
        let beg = self.char_to_byte(beg.min(self.total_chars));
        if end != beg {
            self.delete_byte_region(beg, end);
        }
    }

    fn delete_byte_region(&mut self, beg: usize, end: usize) {
        // TODO: optimize this so that we count the chars deleted when calculating position
        assert!(beg <= end, "beg ({beg}) is greater then end ({end})");
        assert!(end <= self.data.len(), "end out of bounds");
        self.assert_char_boundary(beg);
        self.assert_char_boundary(end);
        if end < self.gap_start {
            // delete before gap
            //
            // hello New York City||||||||||
            //     ^      ^       ^         ^
            //     beg    end     gap_start gap_end
            //
            // shift end..gap_start to the right
            //
            // hell|||||||||||||||||ork City
            //     ^                ^
            //     gap_start        gap_end

            // update character count
            self.gap_chars -= num_chars(&self.data[beg..self.gap_start]);
            let deleted_chars = num_chars(&self.data[beg..end]);
            self.total_chars -= deleted_chars;
            let new_end = self.gap_end - (self.gap_start - end);
            // shift data
            self.data.copy_within(end..self.gap_start, new_end);
            // update cursor
            self.update_cursor_chars(beg, end, deleted_chars);
            if self.cursor.byte < self.gap_start {
                if self.cursor.byte > end {
                    self.cursor.byte += self.gap_len();
                } else if self.cursor.byte >= beg {
                    self.cursor.byte = new_end;
                }
            }
            // update gap position
            self.gap_end = new_end;
            self.gap_start = beg;
        } else if beg >= self.gap_end {
            // delete after gap
            //
            // ||||||||||hello New York City
            // ^         ^         ^   ^
            // gap_start gap_end   beg end
            //
            // shift gap_end..beg to the left
            //
            // hello New |||||||||||||| City
            //           ^             ^
            //           gap_start     gap_end

            // update character count
            let deleted_chars = num_chars(&self.data[beg..end]);
            self.total_chars -= deleted_chars;
            self.gap_chars += num_chars(&self.data[self.gap_end..beg]);
            // shift data
            self.data.copy_within(self.gap_end..beg, self.gap_start);
            // update cursor
            self.update_cursor_chars(beg, end, deleted_chars);
            if self.cursor.byte >= self.gap_end {
                if self.cursor.byte < beg {
                    self.cursor.byte -= self.gap_len();
                } else if self.cursor.byte < end {
                    self.cursor.byte = end;
                }
            }
            // update gap position
            self.gap_start += beg - self.gap_end;
            self.gap_end = end;
        } else if beg < self.gap_start && end >= self.gap_end {
            // delete spans gap
            //
            // hello|||||||||| New York City
            //  ^   ^         ^       ^
            //  beg gap_start gap_end end
            //
            // update start and end of gap
            //
            // h||||||||||||||||||||||k City
            //  ^                     ^
            //  gap_start             gap_end

            // update character count
            let chars_before = num_chars(&self.data[beg..self.gap_start]);
            let chars_after = num_chars(&self.data[self.gap_end..end]);
            let total_chars = chars_before + chars_after;
            self.gap_chars -= chars_before;
            self.total_chars -= total_chars;
            // update gap position
            self.gap_start = beg;
            self.gap_end = end;
            self.update_cursor_chars(beg, end, total_chars);
            if (beg..end).contains(&self.cursor.byte) {
                self.cursor.byte = end;
            }
        } else {
            panic!(
                "delete region inside gap -- gap: {}-{}, span: {beg}-{end}",
                self.gap_start, self.gap_end
            );
        }

        // update cursor chars
    }

    fn update_cursor_chars(&mut self, beg: usize, end: usize, size: usize) {
        if self.cursor.byte > beg {
            if self.cursor.byte > end {
                self.cursor.char -= size;
            } else {
                self.cursor.char = self.gap_chars;
            }
        }
    }

    fn move_gap_out_of(&mut self, range: impl RangeBounds<usize>) {
        if !range.contains(&self.gap_chars)
            || range.start_bound() == Bound::Included(&self.gap_chars)
        {
            return;
        }

        let start = match range.start_bound() {
            Bound::Included(x) => *x,
            Bound::Excluded(_) => unreachable!(),
            Bound::Unbounded => 0,
        };

        let end = match range.end_bound() {
            Bound::Included(_) => panic!("inclusive end bound not supported"),
            Bound::Excluded(x) => *x,
            Bound::Unbounded => self.total_chars,
        };

        if self.gap_chars - start < end - self.gap_chars {
            self.move_gap(start);
        } else {
            self.move_gap(end);
        }
    }

    fn move_gap(&mut self, pos: usize) {
        let pos = self.char_to_byte(pos);
        assert!(pos <= self.data.len(), "attempt to move gap out of bounds");
        self.assert_char_boundary(pos);
        if pos < self.gap_start {
            // move gap backwards
            let size = self.gap_start - pos;
            self.gap_chars -= num_chars(&self.data[pos..self.gap_start]);

            self.data
                .copy_within(pos..self.gap_start, self.gap_end - size);
            // if gap moves across cursor, update cursor position
            if self.cursor.byte < self.gap_start && self.cursor.byte >= pos {
                self.cursor.byte += self.gap_len();
            }
            self.gap_start = pos;
            self.gap_end -= size;
        } else if pos >= self.gap_end {
            // move gap forwards
            self.gap_chars += num_chars(&self.data[self.gap_end..pos]);
            self.data.copy_within(self.gap_end..pos, self.gap_start);
            let size = pos - self.gap_end;
            // if gap moves across cursor, update cursor position
            if self.cursor.byte >= self.gap_end && self.cursor.byte < pos {
                self.cursor.byte -= self.gap_len();
            }
            self.gap_start += size;
            self.gap_end = pos;
        } else {
            panic!(
                "move gap position byte: ({pos}) inside gap ({}-{})",
                self.gap_start, self.gap_end
            );
        }
    }

    pub fn set_cursor(&mut self, pos: usize) {
        let pos = pos.min(self.total_chars);
        let byte_pos = self.char_to_byte(pos);
        self.cursor = Point {
            byte: byte_pos,
            char: pos,
        };
    }

    pub fn len(&self) -> usize {
        self.data.len() - self.gap_len()
    }

    pub fn is_empty(&self) -> bool {
        self.total_chars == 0
    }

    fn gap_len(&self) -> usize {
        self.gap_end - self.gap_start
    }

    fn update_point_delete(point: &mut Point, beg: &Point, end: &Point) {
        if end.char < point.char {
            // move point back by the number of chars deleted
            point.char -= end.char - beg.char;
            point.byte -= end.byte - beg.byte;
        } else if beg.char < point.char {
            *point = *beg;
        }
        // If we didn't hit either case, then the point is before the deleted
        // region
    }

    pub(crate) fn as_str(&self) -> &str {
        if self.gap_start == 0 {
            self.to_str(self.gap_end..)
        } else if self.gap_end == self.data.len() {
            self.to_str(..self.gap_start)
        } else {
            panic!(
                "gap ({}-{}) not at start or end",
                self.gap_start, self.gap_end
            );
        }
    }

    fn char_to_byte(&self, pos: usize) -> usize {
        if pos == 0 {
            return if self.gap_start == 0 { self.gap_end } else { 0 };
        }
        if pos == self.total_chars {
            return self.data.len();
        }
        // (byte position, char positions) pairs sorted in ascending order
        let positions = {
            let start = (self.gap_start, self.gap_chars);
            let end = (self.gap_end, self.gap_chars);
            let total = (self.data.len(), self.total_chars);
            let cursor = (self.cursor.byte, self.cursor.char);
            if self.cursor.char < self.gap_chars {
                [(0, 0), cursor, start, end, total]
            } else {
                [(0, 0), start, end, cursor, total]
            }
        };

        // find which positions window the char position falls into
        let window = positions
            .windows(2)
            .find(|slice| slice[0].1 <= pos && pos < slice[1].1);

        let Some([(beg_byte, beg_char), (end_byte, end_char)]) = window else {
            unreachable!("char position ({pos}) did not fall into any window");
        };

        self.assert_char_boundary(*beg_byte);
        self.assert_char_boundary(*end_byte);

        let num_chars = end_char - beg_char;
        if end_byte - beg_byte == num_chars {
            // the slice is ascii text, so we can just index into it
            beg_byte + (pos - beg_char)
        } else {
            let string = self.to_str(*beg_byte..*end_byte);
            let byte_idx = chars::to_byte_idx(string, pos - beg_char);
            beg_byte + byte_idx
        }
    }

    fn to_str(&self, range: impl std::slice::SliceIndex<[u8], Output = [u8]>) -> &str {
        // TODO: remove this check once we are confident the code is correct
        std::str::from_utf8(&self.data[range]).unwrap()
    }

    fn assert_char_boundary(&self, pos: usize) {
        if pos == self.gap_start {
            return;
        }
        let is_boundary = match self.data.get(pos) {
            Some(byte) => Self::is_char_boundary(*byte),
            None => pos == self.data.len(),
        };
        assert!(is_boundary, "position ({pos}) not on utf8 boundary");
    }

    #[allow(clippy::cast_possible_wrap)]
    const fn is_char_boundary(byte: u8) -> bool {
        // This is bit magic equivalent to: b < 128 || b >= 192
        (byte as i8) >= -0x40
    }
}

#[cfg(test)]
impl Buffer {
    // used to convert indexes from the fuzzer
    fn wrap(&self, pos: usize) -> usize {
        let pos = pos % (self.len() + 2);
        println!("pos: {:?}", pos);
        pos
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn create() {
        let string = "hello buffer";
        let buffer = Buffer::new(string);
        assert_eq!(buffer.data.len(), string.len() + Buffer::GAP_SIZE);
        assert_eq!(buffer.gap_end, Buffer::GAP_SIZE);
        assert_eq!(buffer.gap_start, 0);
    }

    #[test]
    fn insert() {
        let string = "hello buffer";
        let mut buffer = Buffer::new(string);
        buffer.insert_char('x');
        assert_eq!(buffer.data.len(), string.len() + Buffer::GAP_SIZE);
        assert_eq!(buffer.gap_end, Buffer::GAP_SIZE);
        assert_eq!(buffer.gap_start, 1);
        assert_eq!(buffer.to_string(), "xhello buffer");
    }

    #[test]
    fn insert_slice() {
        let string = "world";
        let new_string = "hi ";
        let mut buffer = Buffer::new(string);
        buffer.insert(new_string);
        buffer.move_gap_out_of(..);
        assert_eq!(buffer.to_string(), "hi world");
        buffer.insert("starting Θ text ");
        assert_eq!(buffer.to_string(), "hi starting Θ text world");
        buffer.set_cursor(21);
        buffer.insert("x");
        assert_eq!(buffer.to_string(), "hi starting Θ text woxrld");
    }

    #[test]
    fn empty() {
        let mut buffer = Buffer::new("");
        assert_eq!(buffer.to_string(), "");
        buffer.delete_region(1, 2);
        assert_eq!(buffer.to_string(), "");
    }

    #[test]
    fn delete() {
        let world = "world";
        let hello = "hello ";
        let mut buffer = Buffer::new(world);
        buffer.insert(hello);
        buffer.delete_backwards(4);
        assert_eq!(buffer.gap_start, hello.len() - 4);
        assert_eq!(buffer.gap_end, hello.len() + Buffer::GAP_SIZE);
        buffer.move_gap_out_of(..);
        buffer.move_gap_out_of(..);
        buffer.move_gap(7);
        buffer.move_gap_out_of(..);
        assert_eq!(buffer.to_string(), "heworld");
    }

    #[test]
    fn delete_forwards() {
        let world = "world";
        let hello = "hello ";
        let mut buffer = Buffer::new(world);
        buffer.insert(hello);
        buffer.delete_forwards(4);
        buffer.move_gap_out_of(..);
        assert_eq!(buffer.to_string(), "hello d");
    }

    #[test]
    fn test_delete_region() {
        let mut buffer = Buffer::new("world");
        buffer.insert("hello ");
        buffer.delete_region(1, 3);
        buffer.move_gap_out_of(..);
        assert_eq!(buffer.to_string(), "hlo world");
        buffer.delete_region(4, 6);
        buffer.move_gap_out_of(..);
        assert_eq!(buffer.to_string(), "hlo rld");
    }

    // cases found during fuzzing
    #[test]
    fn edge_cases() {
        let mut buffer = Buffer::new(":?abdix7");
        assert_eq!(buffer.len(), 8);
        buffer.delete_region(2, 5);
        assert_eq!(buffer.len(), 5);
        buffer.delete_region(5, 4);
        assert_eq!(buffer.len(), 4);
        buffer.delete_region(0, 3);

        let mut buffer = Buffer::new("xyz");
        buffer.insert("abc");
        buffer.set_cursor(2);
        buffer.delete_region(1, 4);
        assert_eq!(buffer.to_string(), "ayz");
        buffer.insert("b");
        assert_eq!(buffer.to_string(), "abyz");

        let mut buffer = Buffer::new("ƽaejcoeuz");
        buffer.delete_region(5, 6);
        buffer.delete_region(1, 8);
        assert_eq!(buffer.to_string(), "ƽ");
    }

    #[test]
    fn test_bounds() {
        let mut buffer = Buffer::new("world");
        buffer.insert("hello ");
        buffer.delete_region(3, 100);
        assert_eq!(buffer.to_string(), "hel");
        buffer.delete_region(10, 1);
        assert_eq!(buffer.to_string(), "h");

        let mut buffer = Buffer::new(",\0\0\0\0\0\0 \0");
        buffer.delete_region(10, 10);
        assert_eq!(buffer.gap_len(), 5);

        let mut buffer = Buffer::new("+\0\0\u{1}\0\0\0\0\0'\0\0\0\0");
        buffer.delete_region(30, 6);
        assert_eq!(buffer.gap_len(), 13);
    }

    #[test]
    fn resize() {
        let world = "world";
        let hello = "hello ";
        let mut buffer = Buffer::new(world);
        buffer.insert(hello);
        assert_eq!(
            buffer.data.len(),
            hello.len() + world.len() + Buffer::GAP_SIZE
        );
        assert_eq!(buffer.gap_end, hello.len() + Buffer::GAP_SIZE);
        assert_eq!(buffer.gap_start, hello.len());
        assert_eq!(buffer.to_string(), "hello world");
    }

    #[test]
    fn cursor() {
        let string = "world";
        let new_string = "hi ";
        let mut buffer = Buffer::new(string);
        buffer.insert(new_string);
        assert_eq!(buffer.gap_chars, new_string.len());
    }
}
