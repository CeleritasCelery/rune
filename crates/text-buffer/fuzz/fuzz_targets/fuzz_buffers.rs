#![no_main]

use libfuzzer_sys::fuzz_target;
use text_buffer::Buffer;

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
                let pos = pos % (buffer.len() + 2);
                println!("buffer.set_cursor({pos});");
                buffer.set_cursor(pos);
                println!("buffer.insert({text:?});");
                buffer.insert(text)
            }
            // Delete
            Err((start, end)) => {
                let start = start % (buffer.len() + 2);
                let end = end % (buffer.len() + 2);
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
