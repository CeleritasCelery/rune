mod data;
mod generate;
use clap::{Parser, Subcommand};
use std::io::Write;
use std::path::Path;
use std::{fs, path::PathBuf};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(short, long)]
    command: String,
}

fn main() {
    let cli = Cli::parse();

    let functions = generate::generate_sigs(&fs::read_to_string(cli.command).unwrap());
    let tests = functions.iter().flat_map(|function| generate::generate_tests(function, 10)).collect();

    let test_string = generate::generate_test_string(tests);

    println!("{}", test_string);


}
