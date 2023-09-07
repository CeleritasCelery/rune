use crdt_testdata::TestPatch;
use std::fs::File;
use std::io::Write as _;
use text_buffer::Buffer;

fn main() -> Result<(), usize> {
    let mut args = std::env::args();
    args.next();
    let filename = args.next().expect("missing file arg");
    let steps = args.next().map(|s| s.parse::<usize>().expect("invalid steps arg"));

    if let Some(steps) = steps {
        println!("running {steps} steps");
    }
    let test_data = crdt_testdata::load_testing_data(&filename);
    let mut debug_txn = String::new();
    let mut debug_buffer = String::new();
    let mut debug_internals = String::new();

    println!("applying {} tokens", test_data.txns.len());
    let mut buffer = Buffer::from(test_data.start_content.as_str());
    for (i, txn) in test_data.txns.iter().enumerate() {
        if i % 10000 == 0 {
            println!("step {i}");
        }
        if let Some(step) = steps {
            // dump out debug info
            if i == step {
                let mut file = File::create("output/rust_output.txt").unwrap();
                file.write_all(buffer.to_string().as_bytes()).unwrap();
                let mut file = File::create("output/old_output.txt").unwrap();
                file.write_all(debug_buffer.as_bytes()).unwrap();
                let mut file = File::create("output/token.txt").unwrap();
                file.write_all(debug_txn.as_bytes()).unwrap();
                let mut file = File::create("output/old_buffer.txt").unwrap();
                file.write_all(debug_internals.as_bytes()).unwrap();
                return Ok(());
            } else if i == step - 1 {
                debug_txn = format!("{txn:?}");
                debug_buffer = format!("{}", buffer.to_string());
                debug_internals = format!("{buffer:?}");
            }
        }
        for TestPatch(pos, del, ins) in &txn.patches {
            buffer.set_cursor(*pos);
            buffer.delete_forwards(*del);
            buffer.insert(ins);
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

#[cfg(test)]
mod test {
    use super::*;
    fn apply_tokens(name: &str) {
        let path = format!("{}/crdt-testdata/data/{}.json.gz", env!("CARGO_MANIFEST_DIR"), name);
        let test_data = crdt_testdata::load_testing_data(&path);
        let mut buffer = Buffer::from(test_data.start_content.as_str());
        for txn in test_data.txns.iter() {
            for TestPatch(pos, del, ins) in &txn.patches {
                buffer.set_cursor(*pos);
                buffer.delete_forwards(*del);
                buffer.insert(ins);
            }
        }
        assert_eq!(buffer, test_data.end_content);
    }

    #[test]
    fn test_seph() {
        apply_tokens("seph-blog1");
    }

    #[test]
    fn test_rustcode() {
        apply_tokens("rustcode");
    }

    #[test]
    fn test_automerge() {
        apply_tokens("automerge-paper");
    }

    #[test]
    fn test_svelte() {
        apply_tokens("sveltecomponent");
    }
}
