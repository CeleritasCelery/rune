#![allow(dead_code)]
use bstr::ByteSlice;

/// A Gap buffer. This represents the text of a buffer, and allows for
/// efficient insertion and deletion of text.
pub(crate) struct Buffer {
    /// The buffer data
    storage: Box<[u8]>,
    /// start of the gap. From a elisp perspective, both gap_start and gap_end
    /// are the same point. But gap_start is never a valid byte index, and
    /// gap_end is always used instead.
    gap_start: usize,
    /// The end of the gap. This also represents the point.
    gap_end: usize,
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
        }
    }

    fn delete_backwards(&mut self, size: usize) {
        self.delete_region(self.gap_start - size, self.gap_end);
    }

    fn delete_forwards(&mut self, size: usize) {
        self.delete_region(self.gap_end, self.gap_end + size);
    }

    fn delete_region(&mut self, beg: usize, end: usize) {
        assert!(beg <= end, "beg is greater then end");
        assert!(end <= self.storage.len(), "end out of bounds");
        self.assert_char_boundary(beg);
        self.assert_char_boundary(end);
        if end < self.gap_start {
            // delete before gap
            let size = end - beg;
            // ++++|||||||*********
            // ^   ^      ^       ^
            // 0   beg    end     gap_start
            //
            // copy end.. to beg
            //
            // ++++*********.......
            //             ^
            //             gap_start
            self.storage[..self.gap_start].copy_within(end.., beg);
            self.gap_start -= size;
        } else if beg >= self.gap_end {
            // delete after gap
            //
            // make beg/end relative to gap_end
            let beg = beg - self.gap_end;
            let end = end - self.gap_end;
            let size = end - beg;
            //       size
            //       v
            // *********|||||||++++
            // ^        ^      ^
            // gap_end  beg    end
            //
            // copy ..beg to size
            //
            // .......*********++++
            //        ^
            //        gap_end
            self.storage[self.gap_end..].copy_within(..beg, size);
            self.gap_end += size;
        } else if beg < self.gap_start && end >= self.gap_end {
            // delete spans gap
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

    fn move_gap(&mut self, pos: usize) {
        assert!(pos <= self.storage.len(), "attempt to move gap out of bounds");
        self.assert_char_boundary(pos);
        if pos < self.gap_start {
            // move gap backwards
            let size = self.gap_start - pos;
            self.storage
                .copy_within(pos..self.gap_start, self.gap_end - size);
            self.gap_start = pos;
            self.gap_end -= size;
        } else if pos >= self.gap_end {
            // move gap forwards
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
            self.storage[self.gap_end..].to_str().unwrap()
        } else if self.gap_end == self.storage.len() {
            self.storage[..self.gap_start].to_str().unwrap()
        } else {
            panic!("gap not at start or end");
        }
    }

    fn assert_char_boundary(&self, pos: usize) {
        let is_boundary = match self.storage.get(pos) {
            // This is bit magic equivalent to: b < 128 || b >= 192
            Some(byte) => (*byte as i8) >= -0x40,
            None => pos == self.storage.len(),
        };
        assert!(is_boundary, "position {pos} not on utf8 boundary");
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
        buffer.move_gap(0);
        assert_eq!(buffer.as_str(), "xhello buffer");
    }

    #[test]
    fn insert_slice() {
        let string = "world";
        let new_string = "hi ";
        let mut buffer = Buffer::new(string);
        buffer.insert_string(new_string);
        buffer.move_gap(0);
        assert_eq!(buffer.as_str(), "hi world");
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
        buffer.move_gap(0);
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
        // assert_eq!(buffer.gap_end, hello.len() + Buffer::GAP_SIZE + 4);
        buffer.move_gap(0);
        assert_eq!(buffer.as_str(), "hello d");
    }

    #[test]
    fn delete_region() {
        let world = "world";
        let hello = "hello ";
        let mut buffer = Buffer::new(world);
        buffer.insert_string(hello);
        buffer.delete_region(2, 3);
        assert_eq!(buffer.gap_start, hello.len() - 1);
        assert_eq!(buffer.gap_end, hello.len() + Buffer::GAP_SIZE);
        buffer.delete_region(11, 13);
        buffer.delete_region(3, 13);
        buffer.move_gap(0);
        assert_eq!(buffer.as_str(), "helrld");
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
        buffer.move_gap(0);
        assert_eq!(buffer.as_str(), "hello world");
    }
}
