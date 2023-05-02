#![no_main]

use gap_buffer::Buffer;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|transactions: Vec<Result<(usize, &str), (usize, usize)>>| {
    let mut buffer = Buffer::new();
    for trans in transactions {
        match trans {
            // Insert
            Ok((pos, text)) => {
                let pos = pos % (buffer.len() + 2);
                buffer.set_cursor(pos);
                buffer.insert(text)
            }
            // Delete
            Err((start, end)) => {
                let start = start % (buffer.len() + 2);
                let end = end % (buffer.len() + 2);
                buffer.delete_region(start, end)
            }
        }
    }
});
