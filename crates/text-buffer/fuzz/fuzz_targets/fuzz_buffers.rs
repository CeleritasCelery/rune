#![no_main]

use libfuzzer_sys::fuzz_target;
use text_buffer::Buffer;

fuzz_target!(|transactions: Vec<Result<(usize, &str), (usize, usize)>>| {
    let mut buffer = Buffer::new();
    for trans in transactions {
        match trans {
            // Insert
            Ok((pos, text)) => {
                let pos = pos % (buffer.len_bytes() + 2);
                if (pos <= buffer.len_chars()) {
                    assert_eq!(pos, buffer.byte_to_char(buffer.char_to_byte(pos)));
                }
                buffer.set_cursor(pos);
                buffer.insert(text)
            }
            // Delete
            Err((start, end)) => {
                let start = start % (buffer.len_bytes() + 2);
                let end = end % (buffer.len_bytes() + 2);
                if (start <= buffer.len_chars()) {
                    assert_eq!(start, buffer.byte_to_char(buffer.char_to_byte(start)));
                }
                if (end <= buffer.len_chars()) {
                    assert_eq!(end, buffer.byte_to_char(buffer.char_to_byte(end)));
                }
                buffer.delete_range(start, end)
            }
        }
    }
});

#[allow(dead_code)]
fn create_repo(transactions: &[Result<(usize, &str), (usize, usize)>]) {
    let mut buffer = Buffer::new();
    println!("let mut buffer = Buffer::new();");
    for trans in transactions {
        match trans {
            // Insert
            Ok((pos, text)) => {
                let pos = pos % (buffer.len_bytes() + 2);
                println!("buffer.set_cursor({pos});");
                buffer.set_cursor(pos);
                println!("buffer.insert({text:?});");
                buffer.insert(text)
            }
            // Delete
            Err((start, end)) => {
                let start = start % (buffer.len_bytes() + 2);
                let end = end % (buffer.len_bytes() + 2);
                println!("buffer.delete_range({start}, {end});");
                buffer.delete_range(start, end)
            }
        }
    }
}

#[allow(dead_code)]
fn main() {
    let data = [];

    create_repo(&data);
}
