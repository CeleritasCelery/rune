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
            storage.extend_from_slice(&[0; Self::GAP_SIZE]);
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

    fn grow(&mut self, slice: &[u8]) {
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
            buffer.extend_from_slice(slice);
            // gap
            buffer.extend_from_slice(&[0; Self::GAP_SIZE]);
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
            self.grow(slice.as_bytes());
        } else {
            let new_slice = &mut self.storage[self.gap_start..(self.gap_start + slice.len())];
            new_slice.copy_from_slice(slice.as_bytes());
            self.gap_start += slice.len();
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
    }

    #[test]
    fn insert_slice() {
        let string = "hello buffer";
        let new_string = "world";
        let mut buffer = Buffer::new(string);
        buffer.insert_string(new_string);
        assert_eq!(buffer.storage.len(), string.len() + Buffer::GAP_SIZE);
        assert_eq!(buffer.gap_end, Buffer::GAP_SIZE);
        assert_eq!(buffer.gap_start, new_string.len());
    }

    #[test]
    fn grow() {
        let string = "hello";
        let mut buffer = Buffer::new(string);
        buffer.grow(&[]);
        assert_eq!(buffer.storage.len(), string.len() + Buffer::GAP_SIZE);
        assert_eq!(buffer.gap_end, Buffer::GAP_SIZE);
        assert_eq!(buffer.gap_start, 0);
    }

    #[test]
    fn resize() {
        let hello = "hello";
        let mut buffer = Buffer::new(hello);
        let world = " world";
        buffer.insert_string(world);
        assert_eq!(
            buffer.storage.len(),
            hello.len() + world.len() + Buffer::GAP_SIZE
        );
        assert_eq!(buffer.gap_end, world.len() + Buffer::GAP_SIZE);
        assert_eq!(buffer.gap_start, world.len());
    }
}
