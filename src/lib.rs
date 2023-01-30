#![allow(dead_code)]
use std::ops::{Bound, RangeBounds};

use bytecount::num_chars;

/// A Gap buffer. This represents the text of a buffer, and allows for
/// efficient insertion and deletion of text.
#[derive(Debug)]
pub(crate) struct Buffer {
    /// The buffer data
    storage: Box<[u8]>,
    /// start of the gap. From a elisp perspective, both gap_start and gap_end
    /// are the same point. But gap_start is never a valid byte index, and
    /// gap_end is always used instead.
    gap_start: usize,
    /// The end of the gap. This also represents the point.
    gap_end: usize,
    /// The number of characters until the gap
    gap_chars: usize,
    /// The current point. The first field is the byte index, the second is the
    /// character count.
    point: (usize, usize),
}

impl Buffer {
    const GAP_SIZE: usize = 5;

    pub(crate) fn new(data: &str) -> Self {
        let storage = {
            let capacity = data.len() + Self::GAP_SIZE;
            let mut storage = Vec::with_capacity(capacity);
            storage.resize(Self::GAP_SIZE, 0);
            storage.extend_from_slice(data.as_bytes());
            assert_eq!(storage.len(), capacity);
            storage.into_boxed_slice()
        };
        Self {
            storage,
            gap_start: 0,
            gap_end: Self::GAP_SIZE,
            gap_chars: 0,
            point: (0, 0),
        }
    }

    fn grow(&mut self, slice: &str) {
        let new_capacity = {
            let pre_gap = self.gap_start;
            let post_gap = self.storage.len() - self.gap_end;
            pre_gap + slice.len() + Self::GAP_SIZE + post_gap
        };
        let new_storage = {
            let mut buffer = Vec::with_capacity(new_capacity);
            // pre-gap
            buffer.extend_from_slice(&self.storage[..self.gap_start]);
            // new text
            buffer.extend_from_slice(slice.as_bytes());
            // gap
            buffer.resize(buffer.len() + Self::GAP_SIZE, 0);
            // post-gap
            buffer.extend_from_slice(&self.storage[self.gap_end..]);
            buffer.into_boxed_slice()
        };
        assert_eq!(new_storage.len(), new_capacity);
        self.storage = new_storage;
        self.gap_start += slice.len();
        self.gap_end = self.gap_start + Self::GAP_SIZE;
        self.gap_chars += num_chars(slice.as_bytes());
    }

    pub(crate) fn insert_char(&mut self, chr: char) {
        let buf = &mut [0; 4];
        self.insert_string(chr.encode_utf8(buf));
    }

    pub(crate) fn insert_string(&mut self, slice: &str) {
        if (self.gap_end - self.gap_start) < slice.len() {
            self.grow(slice);
        } else {
            let new_slice = &mut self.storage[self.gap_start..(self.gap_start + slice.len())];
            new_slice.copy_from_slice(slice.as_bytes());
            self.gap_start += slice.len();
            self.gap_chars += num_chars(slice.as_bytes());
        }
    }

    fn delete_backwards(&mut self, size: usize) {
        self.delete_region(self.gap_chars - size, self.gap_chars);
    }

    fn delete_forwards(&mut self, size: usize) {
        self.delete_region(self.gap_chars, self.gap_chars + size);
    }

    fn delete_region(&mut self, beg: usize, end: usize) {
        let beg = self.char_to_byte(beg);
        let end = self.char_to_byte(end);
        self.delete_byte_region(beg, end);
    }

    fn delete_byte_region(&mut self, beg: usize, end: usize) {
        assert!(beg <= end, "beg ({beg}) is greater then end ({end})");
        assert!(end <= self.storage.len(), "end out of bounds");
        self.assert_char_boundary(beg);
        self.assert_char_boundary(end);
        if end < self.gap_start {
            // delete before gap
            // ++++|||||||*********
            // ^   ^      ^       ^
            // 0   beg    end     gap_start
            //
            // copy end.. to beg
            //
            // ++++*********.......
            //             ^
            //             gap_start
            let size = end - beg;
            self.gap_chars -= num_chars(&self.storage[beg..end]);
            self.storage[..self.gap_start].copy_within(end.., beg);
            self.gap_start -= size;
        } else if beg >= self.gap_end {
            // delete after gap
            //       size (end - beg)
            //       v
            // *********|||||||++++
            // ^        ^
            // gap_end  beg
            //
            // copy ..beg to size
            //
            // .......*********++++
            //        ^
            //        gap_end
            let size = end - beg;
            let beg = beg - self.gap_end;
            self.storage[self.gap_end..].copy_within(..beg, size);
            self.gap_end += size;
        } else if beg < self.gap_start && end >= self.gap_end {
            // delete spans gap
            self.gap_chars -= num_chars(&self.storage[beg..self.gap_start]);
            self.gap_start = beg;
            self.gap_end = end;
        } else {
            // error
            panic!(
                "delete region inside gap -- gap: {}-{}, span: {beg}-{end}",
                self.gap_start, self.gap_end
            );
        }
    }

    fn move_gap_out_of(&mut self, range: impl RangeBounds<usize>) {
        if !range.contains(&self.gap_chars) {
            return;
        }

        let start = match range.start_bound() {
            Bound::Included(x) => *x,
            Bound::Excluded(_) => unreachable!(),
            Bound::Unbounded => 0,
        };

        let _end = match range.end_bound() {
            Bound::Included(_) => panic!("inclusive end bound not supported"),
            Bound::Excluded(x) => Some(*x),
            Bound::Unbounded => None,
        };

        self.move_gap(start);
    }

    fn move_gap(&mut self, pos: usize) {
        let pos = self.char_to_byte(pos);
        assert!(
            pos <= self.storage.len(),
            "attempt to move gap out of bounds"
        );
        self.assert_char_boundary(pos);
        if pos < self.gap_start {
            // move gap backwards
            let size = self.gap_start - pos;
            self.gap_chars -= num_chars(&self.storage[pos..self.gap_start]);

            self.storage
                .copy_within(pos..self.gap_start, self.gap_end - size);
            self.gap_start = pos;
            self.gap_end -= size;
        } else if pos >= self.gap_end {
            // move gap forwards
            self.gap_chars += num_chars(&self.storage[self.gap_end..pos]);
            self.storage.copy_within(self.gap_end..pos, self.gap_start);
            let size = pos - self.gap_end;
            self.gap_start += size;
            self.gap_end = pos;
        } else {
            panic!("move gap position inside gap");
        }
    }

    pub(crate) fn as_str(&self) -> &str {
        if self.gap_start == 0 {
            unsafe { std::str::from_utf8_unchecked(&self.storage[self.gap_end..]) }
        } else if self.gap_end == self.storage.len() {
            unsafe { std::str::from_utf8_unchecked(&self.storage[..self.gap_start]) }
        } else {
            panic!("gap not at start or end");
        }
    }

    fn char_to_byte(&self, pos: usize) -> usize {
        // (byte position, char positions) pairs sorted in ascending order
        #[rustfmt::skip]
        let positions = if self.point.1 <= self.gap_chars {
            [(0, 0), self.point, (self.gap_start, self.gap_chars), (self.gap_end, self.gap_chars)]
        } else {
            [(0, 0), (self.gap_start, self.gap_chars), (self.gap_end, self.gap_chars), self.point]
        };

        // find which positions window the char position falls into
        let window = positions
            .windows(2)
            .find(|slice| slice[0].1 <= pos && pos < slice[1].1);

        let Some([(beg_byte, beg_char), (end_byte, end_char)]) = window else {
            // char pos is past the last char we have cached. Search for it from the end.
            let (idx, chr) = positions.last().unwrap();
            self.assert_char_boundary(*idx);
            let string = unsafe { std::str::from_utf8_unchecked(&self.storage[*idx..]) };
            let count = pos - chr;
            return Self::nth_char(string, count) + idx;
        };

        self.assert_char_boundary(*beg_byte);
        self.assert_char_boundary(*end_byte);

        let num_chars = end_char - beg_char;
        if end_byte - beg_byte == num_chars {
            // the slice is ascii text, so we can just index into it
            beg_byte + (pos - beg_char)
        } else {
            let string =
                unsafe { std::str::from_utf8_unchecked(&self.storage[*beg_byte..*end_byte]) };
            let byte_idx = if pos - beg_char <= num_chars / 2 {
                Self::nth_char(string, pos - beg_char)
            } else {
                Self::nth_char_from_end(string, end_char - pos)
            };
            beg_byte + byte_idx
        }
    }

    fn assert_char_boundary(&self, pos: usize) {
        let is_boundary = match self.storage.get(pos) {
            Some(byte) => Self::is_char_boundary(*byte),
            None => pos == self.storage.len(),
        };
        assert!(is_boundary, "position {pos} not on utf8 boundary");
    }

    // Return the byte index of the nth character. Can potentially be one past
    // the end of the string. Note that this will not error if n is greater than
    // the number of characters in the string, instead just returning the string length.
    fn nth_char(string: &str, n: usize) -> usize {
        let mut count = 0;
        let mut byte_idx = 0;
        for byte in string.as_bytes() {
            if Self::is_char_boundary(*byte) {
                if count == n {
                    break;
                }
                count += 1;
            }
            byte_idx += 1;
        }
        assert!(
            count == n,
            "n {n} is greater than the number of characters in the string"
        );
        byte_idx
    }

    fn nth_char_from_end(string: &str, n: usize) -> usize {
        let mut count = 0;
        let mut byte_idx = string.len();
        if n == 0 {
            return byte_idx;
        }
        for byte in string.as_bytes().iter().rev() {
            if Self::is_char_boundary(*byte) {
                if count == n {
                    break;
                }
                count += 1;
            }
            byte_idx -= 1;
        }
        assert!(
            count == n,
            "n is greater than the number of characters in the string"
        );
        byte_idx
    }

    #[inline(always)]
    const fn is_char_boundary(byte: u8) -> bool {
        // This is bit magic equivalent to: b < 128 || b >= 192
        (byte as i8) >= -0x40
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn create() {
        let string = "hello buffer";
        let buffer = Buffer::new(string);
        assert_eq!(buffer.storage.len(), string.len() + Buffer::GAP_SIZE);
        assert_eq!(buffer.gap_end, Buffer::GAP_SIZE);
        assert_eq!(buffer.gap_start, 0);
    }

    #[test]
    fn insert() {
        let string = "hello buffer";
        let mut buffer = Buffer::new(string);
        buffer.insert_char('x');
        assert_eq!(buffer.storage.len(), string.len() + Buffer::GAP_SIZE);
        assert_eq!(buffer.gap_end, Buffer::GAP_SIZE);
        assert_eq!(buffer.gap_start, 1);
        buffer.move_gap_out_of(..);
        assert_eq!(buffer.as_str(), "xhello buffer");
    }

    #[test]
    fn insert_slice() {
        let string = "world";
        let new_string = "hi ";
        let mut buffer = Buffer::new(string);
        buffer.insert_string(new_string);
        buffer.move_gap_out_of(..);
        assert_eq!(buffer.as_str(), "hi world");
        buffer.insert_string("starting Θ text ");
        buffer.move_gap(19);
        buffer.insert_string("x");
        buffer.move_gap_out_of(..);
        assert_eq!(buffer.as_str(), "starting Θ text hi xworld");
    }

    #[test]
    fn delete() {
        let world = "world";
        let hello = "hello ";
        let mut buffer = Buffer::new(world);
        buffer.insert_string(hello);
        buffer.delete_backwards(4);
        assert_eq!(buffer.gap_start, hello.len() - 4);
        assert_eq!(buffer.gap_end, hello.len() + Buffer::GAP_SIZE);
        buffer.move_gap_out_of(..);
        buffer.move_gap_out_of(..);
        buffer.move_gap(7);
        assert_eq!(buffer.as_str(), "heworld");
    }

    #[test]
    fn delete_forwards() {
        let world = "world";
        let hello = "hello ";
        let mut buffer = Buffer::new(world);
        buffer.insert_string(hello);
        buffer.delete_forwards(4);
        assert_eq!(buffer.gap_start, hello.len());
        assert_eq!(buffer.gap_end, hello.len() + Buffer::GAP_SIZE + 4);
        buffer.move_gap_out_of(..);
        assert_eq!(buffer.as_str(), "hello d");
    }

    #[test]
    fn delete_region() {
        let world = "world";
        let hello = "hello ";
        let mut buffer = Buffer::new(world);
        buffer.insert_string(hello);
        buffer.delete_region(1, 3);
        buffer.move_gap_out_of(..);
        assert_eq!(buffer.as_str(), "hlo world");
        buffer.delete_region(4, 6);
        buffer.move_gap_out_of(..);
        assert_eq!(buffer.as_str(), "hlo rld");
    }

    #[test]
    fn resize() {
        let world = "world";
        let hello = "hello ";
        let mut buffer = Buffer::new(world);
        buffer.insert_string(hello);
        assert_eq!(
            buffer.storage.len(),
            hello.len() + world.len() + Buffer::GAP_SIZE
        );
        assert_eq!(buffer.gap_end, hello.len() + Buffer::GAP_SIZE);
        assert_eq!(buffer.gap_start, hello.len());
        buffer.move_gap_out_of(..);
        assert_eq!(buffer.as_str(), "hello world");
    }

    #[test]
    fn point() {
        let string = "world";
        let new_string = "hi ";
        let mut buffer = Buffer::new(string);
        buffer.insert_string(new_string);
        assert_eq!(buffer.gap_chars, new_string.len());
    }
}
