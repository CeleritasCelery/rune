#![allow(dead_code)]
use bstr::ByteSlice;

pub(crate) struct Buffer {
    /// The pointer to the start of the buffer.
    storage: Box<[u8]>,
    gap_start: usize,
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
        // TODO: gap_end should work here
        self.delete_region(self.gap_end, self.gap_end + size);
    }

    pub(crate) fn delete_region(&mut self, beg: usize, end: usize) {
        assert!(beg <= end, "beg is greater then end");
        assert!(end <= self.storage.len(), "end out of bounds");
        if end < self.gap_start {
            // delete before gap
            let string = unsafe { self.storage[..self.gap_start].to_str_unchecked() };
            assert!(string.is_char_boundary(beg), "beg not on utf8 boundary");
            assert!(string.is_char_boundary(end), "end not on utf8 boundary");
            self.storage[..self.gap_start].copy_within(end.., beg);
            self.gap_start -= end - beg;
        } else if beg >= self.gap_end {
            // delete after gap
            //
            // make beg/end relative to gap_end
            let beg = beg - self.gap_end;
            let end = end - self.gap_end;
            let string = unsafe { self.storage[self.gap_end..].to_str_unchecked() };
            assert!(string.is_char_boundary(beg), "beg not on utf8 boundary");
            assert!(string.is_char_boundary(end), "end not on utf8 boundary");
            let dst = end - beg;
            self.storage[self.gap_end..].copy_within(..beg, dst);
            self.gap_end += dst;
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

    pub(crate) fn move_gap(&mut self, pos: usize) {
        assert!(pos <= self.storage.len(), "move_gap out of bounds");
        if pos < self.gap_start {
            // move gap backwards
            let string = unsafe { self.storage[..self.gap_start].to_str_unchecked() };
            assert!(
                string.is_char_boundary(pos),
                "move_gap not on utf8 boundary"
            );
            let size = self.gap_start - pos;
            self.storage
                .copy_within(pos..self.gap_start, self.gap_end - size);
            self.gap_start = pos;
            self.gap_end -= size;
        } else if pos >= self.gap_end {
            // move gap forwards
            let string = unsafe { self.storage[self.gap_end..].to_str_unchecked() };
            assert!(
                string.is_char_boundary(pos - self.gap_end),
                "move_gap not on utf8 boundary"
            );
            self.storage.copy_within(self.gap_end..pos, self.gap_start);
            let size = pos - self.gap_end;
            self.gap_start += size;
            self.gap_end = pos;
        } else {
            panic!("move-gap position inside gap");
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
