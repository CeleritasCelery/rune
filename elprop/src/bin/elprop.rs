mod code;
use clap::Parser;
use code::data::{Config, Function};
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
    eprintln!("Generating Functions...");
    let regex = regex::Regex::new(&cli.pattern).unwrap();
    let functions = get_all_functions(&rust_src)
        .into_iter()
        .filter(|x| {
            let rust_name = x.name.replace(['-'], "_");
            regex.is_match(&x.name) || regex.is_match(&rust_name)
        })
        .collect::<Vec<_>>();

    let config = Config { test_count: cli.test_count.unwrap_or(200), functions: functions.clone() };

    let json = serde_json::to_string(&config);
    let elprop_target = workspace_root.join("target/elprop");
    fs::create_dir_all(&elprop_target).unwrap();
    let function_file = elprop_target.join("functions.json");
    fs::write(function_file, json.expect("Malformed JSON")).unwrap();
    #[allow(unused_variables)]
    let emacs_cmd_file = crate_root.join("src/elprop.el");
    let runner = workspace_root.join("target/debug/runner");
    eprintln!("Launching Proptest...");
    let lisp_cmd = format!("(load-file \"{}\")", emacs_cmd_file.to_str().unwrap());
    let child = std::process::Command::new("emacs")
        .args(["-Q", "--batch", "--eval", &lisp_cmd])
        .env("ELPROP_RUNNER", runner)
        .output()
        .expect("Failed to run emacs");
    let output = String::from_utf8_lossy(&child.stdout);
    // find the start of the text mapbacktrace if it exists in output
    if let Some(panic_idx) = output.find("thread 'main' panicked") {
        // get the text between START and END tags
        let start_text = ";; ELPROP_START";
        let end_text = ";; ELPROP_END";
        let start = match output.find(start_text) {
            Some(i) => i + start_text.len(),
            None => 0,
        };
        let end = output.find(end_text).unwrap_or(output.len());
        let source = &output[start..end];

        let backtrace_end = match output[panic_idx..].find(";; ") {
            Some(i) => i + panic_idx,
            None => output.len(),
        };
        let backtrace = &output[panic_idx..backtrace_end];

        println!("====================");
        println!("Status: Failed (Rune Panicked)");
        println!("Failing Input: {source}");
        println!("{backtrace}");
        return ExitCode::FAILURE;
    }

    println!("{output}");

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
    let count = outputs.len();
    for output in outputs {
        println!("====================");
        let func = output.function;
        println!("Testing: {func} [{}]", output.count);
        match output.status {
            Status::Fail(reason, args) => {
                passed = false;
                println!("Status: Failed");

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
        println!();
    }
    if count == 0 {
        println!("No tests run");
        ExitCode::SUCCESS
    } else if passed {
        println!("All tests passed");
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}

fn get_all_functions(pathbuf: &Path) -> Vec<Function> {
    let mut functions = Vec::new();
    for entry in fs::read_dir(pathbuf).unwrap() {
        let path = entry.unwrap().path();
        if path.extension().is_some_and(|ex| ex == "rs") {
            let contents = fs::read_to_string(&path).unwrap();
            let names = get_fn_signatures(&contents);
            functions.extend(names);
        }
    }
    functions
}

fn get_fn_signatures(string: &str) -> Vec<Function> {
    syn::parse_file(string)
        .unwrap()
        .items
        .iter()
        .filter_map(|x| match x {
            syn::Item::Fn(x) => Some(x),
            _ => None,
        })
        .filter(is_defun)
        .filter_map(|x| Function::from_item(x).ok())
        .collect()
}

#[expect(clippy::trivially_copy_pass_by_ref)]
fn is_defun(func: &&syn::ItemFn) -> bool {
    use syn::Meta;
    use syn::MetaList;
    for attr in &func.attrs {
        match &attr.meta {
            Meta::Path(path) | Meta::List(MetaList { path, .. })
                if path.get_ident().unwrap() == "defun" =>
            {
                return true;
            }
            _ => {}
        }
    }
    false
}
