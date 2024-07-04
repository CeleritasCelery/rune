mod data;
mod generate;
mod output;
use clap::Parser;
use std::io::Write;
use std::{fs, path::PathBuf};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    file: String,
    #[arg(short, long)]
    test_count: Option<usize>,
    #[arg(short, long)]
    output: Option<PathBuf>,
}

fn main() {
    let cli = Cli::parse();

    eprintln!("Generating tests for {}", cli.file);
    eprintln!("Generating Functions");
    let functions = generate::generate_sigs(&fs::read_to_string(cli.file).unwrap());
    eprintln!("Generating Tests");
    let test_count = cli.test_count.unwrap_or(10);
    let tests = functions.iter().flat_map(|function| generate::generate_tests(function, test_count)).collect();

    eprintln!("Generating Test String");
    let mut output = output::Output::new(cli.output);
    generate::generate_test_string(&mut output, tests);

    eprintln!("Outputting Test String");
    output.flush().unwrap();
}
