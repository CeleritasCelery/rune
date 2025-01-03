mod code;
use code::output::{Output, Status};
use proptest::prelude::TestCaseError;
use proptest::test_runner::{Config, TestError, TestRunner};
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::{BufRead, BufReader, Write};
use std::process::Stdio;
use std::{fs, path::PathBuf};

const START_TAG: &str = ";; ELPROP_START:";
const END_TAG: &str = "\n;; ELPROP_END\n";

fn main() {
    let crate_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace_root = crate_root.parent().unwrap();
    let target = workspace_root.join("target/elprop");

    let json = target.join("functions.json");
    // read the file to a string
    let json_string = fs::read_to_string(json).expect("Unable to read file");

    let config: code::data::Config =
        serde_json::from_str(&json_string).expect("Unable to deserialize json");

    let mut runner = TestRunner::new(Config {
        cases: config.test_count,
        failure_persistence: None,
        ..Config::default()
    });

    let cmd = crate_root.parent().unwrap().join("target/debug/rune");
    #[expect(clippy::zombie_processes)]
    let mut child = std::process::Command::new(cmd)
        .arg("--eval-stdin")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("Failed to start rune");

    let rune_stdin = RefCell::new(child.stdin.take().unwrap());
    let rune_stdout = RefCell::new(child.stdout.take().unwrap());
    let rune_panicked = RefCell::new(false);
    let master_count = RefCell::new(0);

    let arguments = RefCell::new(HashMap::<String, Vec<String>>::new());
    let outputs = RefCell::new(Vec::new());
    for func in config.functions {
        let name = func.name.clone();
        arguments.borrow_mut().entry(name.clone()).or_default();

        let result = runner.run(&func.strategy(), |input| {
            if *rune_panicked.borrow() {
                return Err(TestCaseError::Reject("Rune panicked".into()));
            }
            let body = code::data::print_args(&input);
            arguments.borrow_mut().entry(name.clone()).and_modify(|v| v.push(body.clone()));

            // send to emacs
            println!(";; sending to Emacs");
            let test_str = format!(";; ELPROP_START\n({name} {body})\n;; ELPROP_END");
            println!("{test_str}");
            // send to rune
            println!(";; sending to rune");
            match writeln!(rune_stdin.borrow_mut(), "{test_str}") {
                Ok(()) => (),
                Err(e) => {
                    *rune_panicked.borrow_mut() = true;
                    return Err(TestCaseError::Reject(format!("Rune panicked {e}").into()));
                }
            }

            let mut reader = BufReader::new(std::io::stdin());
            println!(";; reading from Emacs");
            let emacs_output =
                process_eval_result("Emacs", *master_count.borrow(), &mut reader, |text| {
                    let regex = regex::Regex::new("^\\([a-zA-Z0-9-]*-?error ").unwrap();
                    regex.is_match(text)
                });

            let mut rune_stdout = rune_stdout.borrow_mut();
            let mut reader = BufReader::new(&mut *rune_stdout);
            println!(";; reading from Rune");
            let rune_output =
                process_eval_result("Rune", *master_count.borrow(), &mut reader, |text| {
                    text.starts_with("Error: ")
                });
            println!(";; done");

            *master_count.borrow_mut() += 1;

            match (emacs_output, rune_output) {
                (Ok(e), Ok(r)) if e == r => Ok(()),
                (Err(_), Err(_)) => Ok(()),
                (Ok(e) | Err(e), Ok(r)) | (Ok(e), Err(r)) => {
                    println!("\"Emacs: '{e}', Rune: '{r}'\"");
                    Err(TestCaseError::Fail(format!("Emacs: {e}, Rune: {r}").into()))
                }
            }
        });

        // send the output of "result" to a file
        // open the file in write mode

        println!(";; sending output");
        let status = match result {
            Err(TestError::Fail(reason, value)) => Status::Fail(reason.to_string(), value),
            Err(TestError::Abort(reason)) => Status::Abort(reason.to_string()),
            Ok(()) => Status::Pass,
        };
        let output = Output { function: func.name.clone(), status };
        outputs.borrow_mut().push(output);
    }

    let args_file = target.join("arguments.json");
    let json = serde_json::to_string(&*arguments.borrow()).expect("Malformed Arguments JSON");
    fs::write(args_file, json).unwrap();

    let _ = child.kill();
    let json = serde_json::to_string(&*outputs.borrow()).expect("Malformed Output JSON");
    let output_file = target.join("output.json");
    fs::write(output_file, json).unwrap();
    println!(";; exit process");
}

fn process_eval_result(
    name: &str,
    master_count: usize,
    reader: &mut impl BufRead,
    test_fail: impl Fn(&str) -> bool,
) -> Result<String, String> {
    let mut line = String::new();
    reader.read_line(&mut line).unwrap();
    if line.contains("thread 'main' panicked") {
        return Err("Rune panicked".to_owned());
    }
    let count = line.strip_prefix(START_TAG).unwrap().trim().parse::<usize>().unwrap();
    assert_eq!(
        master_count, count,
        "Count from {name} was off. actual {count}, expected {master_count}",
    );
    line.clear();
    while !line.contains(END_TAG) {
        reader.read_line(&mut line).unwrap();
    }
    let text = line.strip_suffix(END_TAG).unwrap().trim().to_string();
    if test_fail(&text) {
        Err(text)
    } else {
        Ok(text)
    }
}
