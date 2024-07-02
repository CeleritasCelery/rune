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

    println!("Generating tests for {}", cli.command);
    println!("Generating Functions");
    let functions = generate::generate_sigs(&fs::read_to_string(cli.command).unwrap());
    println!("Generating Tests");
    let tests = functions.iter().flat_map(|function| generate::generate_tests(function, 10)).collect();

    println!("Generating Test String");
    let test_string = generate::generate_test_string(tests);

    println!("Printing Test String");
    println!("{}", test_string);


}
