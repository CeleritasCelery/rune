use crdt_testdata::TestPatch;
use gap_buffer::Buffer;
use std::fs::File;
use std::io::Write as _;

fn main() -> Result<(), usize> {
    let mut args = std::env::args();
    args.next();
    let filename = args.next().expect("missing file arg");
    let steps = args
        .next()
        .map(|s| s.parse::<usize>().expect("invalid steps arg"));

    if let Some(steps) = steps {
        println!("running {steps} steps");
    }
    let test_data = crdt_testdata::load_testing_data(&filename);
    let mut debug_txn = String::new();
    let mut debug_buffer = String::new();
    let mut debug_internals = String::new();

    println!("applying {} tokens", test_data.txns.len());
    let mut buffer = Buffer::new(&test_data.start_content);
    for (i, txn) in test_data.txns.iter().enumerate() {
        if i % 10000 == 0 {
            println!("step {i}");
        }
        if let Some(step) = steps {
            if i == step {
                let mut file = File::create("rust_output.txt").unwrap();
                file.write_all(buffer.to_string().as_bytes()).unwrap();
                let mut file = File::create("old_output.txt").unwrap();
                file.write_all(debug_buffer.as_bytes()).unwrap();
                let mut file = File::create("token.txt").unwrap();
                file.write_all(debug_txn.as_bytes()).unwrap();
                let mut file = File::create("old_buffer.txt").unwrap();
                file.write_all(debug_internals.as_bytes()).unwrap();
                return Ok(());
            } else if i + 1 == step {
                debug_txn = format!("{txn:?}");
                debug_buffer = format!("{buffer}");
                debug_internals = format!("{buffer:?}");
            }
        }
        for TestPatch(pos, del, ins) in &txn.patches {
            buffer.set_cursor(*pos);
            buffer.delete_char(*del);
            buffer.insert(&ins);
        }
    }

    assert_eq!(buffer.to_string(), test_data.end_content);
    println!("success!");
    if let Some(step) = steps {
        Err(step)
    } else {
        Ok(())
    }
}
