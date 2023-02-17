#![no_main]

use libfuzzer_sys::fuzz_target;
use gap_buffer::Buffer;

fuzz_target!(|data: (&str, Vec<Result<(usize, &str), (usize, usize)>>)| {
    let (init, trns) = data;
    // fuzzed code goes here
    let mut buffer = Buffer::new(init);
    for trn in trns {
        match trn {
            Ok((pos, text)) => {
                let pos = pos % (buffer.len() + 2);
                buffer.set_cursor(pos);
                buffer.insert(text)
            },
            Err((start, end)) => {
                let start = start % (buffer.len() + 2);
                let end = end % (buffer.len() + 2);
                buffer.delete_region(start, end)
            },
        }
    }
});
