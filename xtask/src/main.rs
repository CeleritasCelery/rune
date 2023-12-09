use std::{path::PathBuf, fs};

use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    Test,
}

fn main() {
    let cli = Cli::parse();

    if matches!(cli.command, Some(Commands::Test)) {
        let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        // go up one directory
        let pathbuf = root.parent().unwrap();
        // go to the source directory
        let pathbuf = pathbuf.join("src");

        for entry in fs::read_dir(pathbuf).unwrap() {
            let path = entry.unwrap().path();
            let Some(ext) = path.extension() else {continue};
            if ext == "rs"  {
                let contents = fs::read_to_string(&path).unwrap();
                // parse contents with syn
                let syntax = syn::parse_file(&contents).unwrap();
            }
        }
    } else {
        println!("no command");
    }
}
