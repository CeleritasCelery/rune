mod data;
mod generate;
use clap::Parser;
use proptest::test_runner::{Config, TestError, TestRunner};
use std::path::Path;
use std::{fs, path::PathBuf};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    pattern: String,
    #[arg(short, long)]
    test_count: Option<usize>,
    #[arg(short, long)]
    output: Option<PathBuf>,
    // #[arg(short, long)]
    // function: String,
}

fn main() {
    let cli = Cli::parse();

    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let root = root.parent().unwrap();
    // go to the source directory
    let rust_src = root.join("src");
    eprintln!("Generating Functions");
    let regex = regex::Regex::new(&cli.pattern).unwrap();
    let functions = get_all_functions(&rust_src)
        .into_iter()
        .filter(|x| regex.is_match(&x.name))
        .collect::<Vec<_>>();
    eprintln!("Generating Tests");
    let test_count = cli.test_count.unwrap_or(20);

    let mut runner = TestRunner::new(Config {
        cases: test_count as u32,
        failure_persistence: None,
        ..Config::default()
    });
    for func in functions {
        let name = func.name.clone();
        let result = runner.run(&func.strategy(), |input| {
            let body = input
                .iter()
                .map(|x| match x {
                    Some(x) => format!("{x} "),
                    None => "nil".to_owned(),
                })
                .collect::<Vec<_>>()
                .join(" ");
            println!("({name} {body})");
            Ok(())
        });

        match result {
            Err(TestError::Fail(_, value)) => {
                println!("Found minimal failing case: {value:?}");
            }

            Err(TestError::Abort(reason)) => {
                println!("Testing for {name} aborted due to {reason}");
            }
            Ok(()) => println!("{name} Passed"),
        }
    }
}

fn get_all_functions(pathbuf: &Path) -> Vec<data::Function> {
    let mut functions = Vec::new();
    for entry in fs::read_dir(pathbuf).unwrap() {
        let path = entry.unwrap().path();
        if path.extension().is_some_and(|ex| ex == "rs") {
            let contents = fs::read_to_string(&path).unwrap();
            let names = generate::generate_sigs(&contents, None);
            functions.extend(names);
        }
    }
    functions
}
