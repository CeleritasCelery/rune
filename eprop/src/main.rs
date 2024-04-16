use clap::{Parser, Subcommand};
use std::io::Write;
use std::path::Path;
use std::{fs, path::PathBuf};

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
        let root = root.parent().unwrap();
        // go to the source directory
        let rust_src = root.join("src");
        let functions = get_all_functions(&rust_src);

        // write out a file with the function names
        let run_dir = root.join("target/eprop");
        fs::create_dir_all(&run_dir).unwrap();
        std::env::set_current_dir(&run_dir).unwrap();

        let mut file = fs::File::create("eprop-in.el").unwrap();
        writeln!(file, ";;; -*- lexical-binding: t; -*-").unwrap();
        for name in functions {
            writeln!(file, "(eprop-runner \"{name}\" (functionp '{name}))").unwrap();
        }
        writeln!(file, "(with-current-buffer eprop-output-buffer (write-region (point-min) (point-max) \"eprop-out.el\"))")
            .unwrap();

        fs::create_dir_all("emacs").unwrap();
        std::env::set_current_dir("emacs").unwrap();
        run_gnu_emacs();
        std::env::set_current_dir(&run_dir).unwrap();

        fs::create_dir_all("rune").unwrap();
        std::env::set_current_dir("rune").unwrap();
        run_rune(root);
        std::env::set_current_dir(&run_dir).unwrap();

        compare_output();
    } else {
        println!("no command");
    }
}

fn compare_output() {
    // go through eprop-out.el and eprop-ref.el and find any differences
    let ref_file = fs::read_to_string("emacs/eprop-out.el").unwrap();
    let out_file = fs::read_to_string("rune/eprop-out.el").unwrap();

    let ref_lines = ref_file.lines();
    let out_lines = out_file.lines();

    let mut tag = String::new();
    for (ref_line, out_line) in ref_lines.zip(out_lines) {
        if let Some(this_tag) = ref_line.strip_prefix(";; ") {
            tag = this_tag.to_string();
        }
        if ref_line != out_line {
            eprintln!("mismatch for {tag}:\nref: {ref_line}\nout: {out_line}");
        }
    }
}

fn run_gnu_emacs() {
    // check that the version is 29.1
    let output = std::process::Command::new("emacs").arg("--version").output().unwrap();

    let text = String::from_utf8_lossy(&output.stdout);
    if !text.contains("GNU Emacs 29.1") {
        // TODO: provide a better error message that has the path and real version
        eprintln!("WARNING: GNU Emacs version is not 29.1");
        std::thread::sleep(std::time::Duration::from_secs(1));
    }

    let runner = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/runner.el");

    let output = std::process::Command::new("emacs")
        .arg("--batch")
        .arg("--load")
        .arg(runner)
        .arg("--load")
        .arg("../eprop-in.el")
        .output()
        .unwrap();
    if !output.status.success() {
        eprintln!("emacs failed: {}", String::from_utf8_lossy(&output.stderr));
    }
}

fn run_rune(root: &Path) {
    let runner = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/runner.el");
    let exe = root.join("target/release/rune");
    let min_bootstrap = root.join("lisp/min-bootstrap.el");

    let output = std::process::Command::new(exe)
        .arg("--no-bootstrap")
        .arg("--load")
        .arg(min_bootstrap)
        .arg("--load")
        .arg(runner)
        .arg("--load")
        .arg("../eprop-in.el")
        .output()
        .unwrap();
    if !output.status.success() {
        eprintln!("rune failed: {}", String::from_utf8_lossy(&output.stderr));
    }
}

fn get_all_functions(pathbuf: &Path) -> Vec<String> {
    let mut functions = Vec::new();
    for entry in fs::read_dir(pathbuf).unwrap() {
        let path = entry.unwrap().path();
        let Some(ext) = path.extension() else { continue };
        if ext == "rs" {
            let contents = fs::read_to_string(&path).unwrap();
            // parse contents with syn
            let file = syn::parse_file(&contents).unwrap();
            let names = get_functions_in_file(&file);
            functions.extend(names.into_iter().map(map_function_name));
        }
    }
    functions
}

fn get_functions_in_file(file: &syn::File) -> Vec<&syn::Ident> {
    let is_defun = |attr: &syn::Attribute| {
        if let syn::Meta::Path(path) = &attr.meta {
            if let Some(ident) = path.get_ident() {
                if ident == "defun" {
                    return true;
                }
            }
        }
        false
    };

    let mut functions = Vec::new();
    for item in &file.items {
        if let syn::Item::Fn(func) = item {
            if func.attrs.iter().any(is_defun) {
                functions.push(&func.sig.ident);
            }
        }
    }
    functions
}

fn map_function_name(name: &syn::Ident) -> String {
    name.to_string().replace('_', "-")
}
