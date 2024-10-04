mod data;
mod generate;
mod output;
use clap::Parser;
use std::io::Write;
use std::path::Path;
use std::{fs, path::PathBuf};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    file: String,
    #[arg(short, long)]
    test_count: Option<usize>,
    #[arg(short, long)]
    output: Option<PathBuf>,
    #[arg(short, long)]
    function: String,
}

fn main() {
    let cli = Cli::parse();

    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let root = root.parent().unwrap();
    // go to the source directory
    let rust_src = root.join("src");
    eprintln!("Generating tests for {}", cli.file);
    eprintln!("Generating Functions");
    let functions = get_all_functions(&rust_src)
        .into_iter()
        .filter(|x| x.name == cli.function)
        .collect::<Vec<_>>();
    eprintln!("Generating Tests");
    let test_count = cli.test_count.unwrap_or(10);
    let tests = functions
        .iter()
        .flat_map(|function| generate::generate_tests(function, test_count))
        .collect();

    eprintln!("Generating Test String");
    let mut output = output::Output::new(cli.output);
    generate::generate_test_string(&mut output, tests);

    eprintln!("Outputting Test String");
    output.flush().unwrap();
}

fn get_all_functions(pathbuf: &Path) -> Vec<data::Function> {
    let mut functions = Vec::new();
    for entry in fs::read_dir(pathbuf).unwrap() {
        let path = entry.unwrap().path();
        if path.extension().is_some_and(|ex| ex == "rs") {
            let contents = fs::read_to_string(&path).unwrap();
            let names = generate::generate_sigs(&contents, &None);
            functions.extend(names);
        }
    }
    functions
}
