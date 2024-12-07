mod code;
use clap::Parser;
use code::data::Config;
use code::output::{Output, Status};
use std::path::Path;
use std::process::ExitCode;
use std::{fs, path::PathBuf};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// The pattern to match rune functions against
    pattern: String,
    /// The number of tests to run per function (default 100)
    #[arg(short, long)]
    test_count: Option<u32>,
}

fn main() -> ExitCode {
    let cli = Cli::parse();

    let crate_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace_root = crate_root.parent().unwrap();
    // go to the source directory
    let rust_src = workspace_root.join("src");
    eprintln!("Generating Functions");
    let regex = regex::Regex::new(&cli.pattern).unwrap();
    let functions = get_all_functions(&rust_src)
        .into_iter()
        .filter(|x| regex.is_match(&x.name))
        .collect::<Vec<_>>();

    let config = Config { test_count: cli.test_count.unwrap_or(100), functions: functions.clone() };

    let json = serde_json::to_string(&config);
    let elprop_target = workspace_root.join("target/elprop");
    fs::create_dir_all(&elprop_target).unwrap();
    let function_file = elprop_target.join("functions.json");
    fs::write(function_file, json.expect("Malformed JSON")).unwrap();
    #[allow(unused_variables)]
    let emacs_cmd_file = crate_root.join("src/elprop.el");
    let runner = workspace_root.join("target/debug/runner");
    let lisp_cmd = format!("(load-file \"{}\")", emacs_cmd_file.to_str().unwrap());
    let child = std::process::Command::new("emacs")
        .args(["-Q", "--batch", "--eval", &lisp_cmd])
        .env("ELPROP_RUNNER", runner)
        .output()
        .expect("Failed to run emacs");
    println!("{}", String::from_utf8_lossy(&child.stdout));
    let code = child.status.code().unwrap();
    if code != 0 {
        eprintln!("Emacs exited with code: {code}");
        return ExitCode::from(code as u8);
    }

    let output_file = elprop_target.join("output.json");
    let json_string = fs::read_to_string(output_file).expect("Unable to read output file");
    let outputs: Vec<Output> =
        serde_json::from_str(&json_string).expect("Unable to deserialize Output json");

    let mut passed = true;
    for output in outputs {
        println!("====================");
        let func = output.function;
        match output.status {
            Status::Fail(reason, args) => {
                passed = false;
                println!("{func}: Failed");

                let body = code::data::print_args(&args);
                println!("Input: ({func} {body})");
                println!("Output: {reason}");
            }
            Status::Abort(reason) => {
                passed = false;
                println!("{func}: Aborted\nReason: {reason}");
            }
            Status::Pass => println!("{func}: Passed"),
        }
    }
    if passed {
        println!("All tests passed");
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

fn get_all_functions(pathbuf: &Path) -> Vec<code::data::Function> {
    let mut functions = Vec::new();
    for entry in fs::read_dir(pathbuf).unwrap() {
        let path = entry.unwrap().path();
        if path.extension().is_some_and(|ex| ex == "rs") {
            let contents = fs::read_to_string(&path).unwrap();
            let names = code::generate::generate_sigs(&contents, None);
            functions.extend(names);
        }
    }
    functions
}
