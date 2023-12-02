use std::ffi::OsStr;
use std::io::Write as _;
use std::path::MAIN_SEPARATOR;
use std::{
    fs::{self, File},
    path::Path,
};

// take an input str and parse it with syn::ExprCall and return the args as strings
#[must_use]
pub fn parse_args(input: &str) -> Vec<String> {
    use quote::ToTokens;
    let pseudo_fn = format!("X{input}");
    let expr = syn::parse_str::<syn::ExprCall>(&pseudo_fn).unwrap();
    expr.args.into_iter().map(|e| e.to_token_stream().to_string()).collect()
}

// get the substring between two string
fn get_substring_between<'a>(contents: &'a str, start: &str, end: &str) -> &'a str {
    let start_idx = contents.find(start).unwrap() + start.len();
    let end_idx = contents[start_idx..].find(end).unwrap() + start_idx;
    &contents[start_idx..end_idx]
}

// get substring between string and predicate function
fn get_substring_between_predicate<'a, F>(contents: &'a str, start: &str, predicate: F) -> &'a str
where
    F: Fn(char) -> bool,
{
    let start_idx = contents.find(start).unwrap() + start.len();
    let end_idx = contents[start_idx..].find(predicate).unwrap() + start_idx;
    &contents[start_idx..end_idx]
}

fn map_varname(ident: &str) -> String {
    if let Some(stripped) = ident.strip_prefix("KW_") {
        convert(':', stripped)
    } else {
        convert("", ident)
    }
}

fn convert(lisp_name: impl Into<String>, rust_name: &str) -> String {
    let mut lisp_name = lisp_name.into();
    lisp_name.extend(rust_name.chars().map(|c| match c {
        '_' => '-',
        c => c.to_ascii_lowercase(),
    }));
    lisp_name
}

#[derive(PartialEq)]
enum DefvarType {
    Bool,
    Other,
}

#[allow(clippy::too_many_lines)]
fn main() {
    let mut all_defun = Vec::new();
    let mut all_defvar = Vec::new();
    let mut all_defsym = Vec::new();

    // rerun for all top level files
    for entry in fs::read_dir("src").unwrap() {
        let path = entry.unwrap().path();
        if path.extension().and_then(OsStr::to_str) == Some("rs") {
            println!("cargo:rerun-if-changed={}", path.display());
        }
    }

    for entry in fs::read_dir("src").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_file() {
            let contents = fs::read_to_string(&path).unwrap();
            for (start, end) in contents.match_indices("\n#[defun") {
                let non_symbol = |c: char| !(c.is_alphanumeric() || c == '_');
                let name = get_substring_between_predicate(&contents[start..], "fn ", non_symbol);

                // check if the substring immdiately after end is name
                let rest = &contents[(start + end.len())..];
                let lisp_name = if rest.starts_with("(name") {
                    get_substring_between(rest, "name = \"", "\"").to_string()
                } else {
                    name.replace('_', "-")
                };
                // convert the path name to a hierarchy name with slashes replaced with colons
                let struct_name = {
                    let basename = path.strip_prefix("src/").unwrap().file_stem().unwrap();
                    let import_path = basename.to_str().unwrap().replace(MAIN_SEPARATOR, ":");
                    format!("crate::{import_path}::__subr_{name}")
                };
                all_defun.push((struct_name, name.to_string(), lisp_name));
            }
            // process all strings starting with defvar
            for (start, _) in contents.match_indices("\ndefvar") {
                let defvar_type = if contents[start..].starts_with("\ndefvar_bool!") {
                    DefvarType::Bool
                } else if contents[start..].starts_with("\ndefvar!") {
                    DefvarType::Other
                } else {
                    continue;
                };
                let body = get_substring_between(&contents[start..], "!", ";");
                let args = parse_args(body);
                let len = args.len();
                let mut fields = args.into_iter();
                let ident = fields.next().unwrap();
                let (ident, name, value) = match len {
                    1 => {
                        let name = map_varname(&ident);
                        (ident, name, None)
                    }
                    2 => {
                        let name = map_varname(&ident);
                        let value = fields.next().unwrap();
                        (ident, name, Some(value))
                    }
                    3 => {
                        let name = fields.next().unwrap();
                        let value = fields.next().unwrap();
                        (ident, name, Some(value))
                    }
                    _ => panic!("defvar form was too long {path:?}"),
                };
                all_defvar.push((ident, name, value, defvar_type));
            }

            // process all strings starting with defsym
            for (start, _) in contents.match_indices("\ndefsym!") {
                let body = get_substring_between(&contents[start..], "defsym!", ";");
                let args = parse_args(body);
                let mut fields = args.into_iter();
                let name = fields.next().unwrap();
                let value = fields.next();
                all_defsym.push((name, value));
            }
        }
    }

    let out_dir = std::env::var("OUT_DIR").unwrap();
    // println!("cargo:warning={out_dir}/sym.rs");
    let dest_path = Path::new(&out_dir).join("sym.rs");
    let mut f = File::create(dest_path).unwrap();

    let symbol_len = all_defsym.len() + all_defun.len() + all_defvar.len() + 2;
    writeln!(
        f,
        "
#[allow(non_upper_case_globals)]
#[allow(non_snake_case)]
#[allow(dead_code)]
pub(crate) mod sym {{
use crate::core::env::SymbolCell;
use crate::core::env::Symbol;

pub(super) static BUILTIN_SYMBOLS: [SymbolCell; {symbol_len}] = [
    SymbolCell::new_const(\"nil\"),
    SymbolCell::new_const(\"t\"),",
    )
    .unwrap();

    // write the list of all defsym to the file
    for (sym, name) in &all_defsym {
        let sym_name = match name {
            Some(name) => name.trim_matches('"').to_string(),
            None => map_varname(sym),
        };
        writeln!(f, "    SymbolCell::new(\"{sym_name}\"),").unwrap();
    }

    for (_, name, _, _) in &all_defvar {
        #[rustfmt::skip]
        writeln!(f, "    SymbolCell::new_special(\"{name}\"),").unwrap();
    }

    // write the list of all defun to a file in out_dir
    for (_, _, lisp_name) in &all_defun {
        #[rustfmt::skip]
        writeln!(f, "    SymbolCell::new(\"{lisp_name}\"),").unwrap();
    }

    // End BUILTIN_SYMBOLS
    writeln!(f, "];\n").unwrap();

    let special = ["nil".to_owned(), "true".to_owned()];
    let all_elements = special
        .iter()
        .chain(all_defsym.iter().map(|x| &x.0))
        .chain(all_defvar.iter().map(|x| &x.0))
        .chain(all_defun.iter().map(|x| &x.1))
        .enumerate();
    for (idx, element) in all_elements {
        let sym_name = element.to_ascii_uppercase();
        #[rustfmt::skip]
        writeln!(f, "pub(crate) const {sym_name}: Symbol = Symbol::new_builtin({idx});").unwrap();
    }

    // all SubrFn
    let subr_len = all_defun.len();
    writeln!(f, "static SUBR_DEFS: [&crate::core::object::SubrFn; {subr_len}] = [",).unwrap();
    for (subr_name, _, _) in &all_defun {
        writeln!(f, "    &{subr_name},",).unwrap();
    }
    // End SUBR_DEFS
    writeln!(f, "];\n").unwrap();

    let defun_start = symbol_len - subr_len;
    writeln!(
        f,
        "
pub(crate) fn init_symbols() {{
    for (sym, func) in BUILTIN_SYMBOLS[{defun_start}..].iter().zip(SUBR_DEFS.iter()) {{
        unsafe {{ sym.set_func((*func).into()).unwrap(); }}
    }}
}}
"
    )
    .unwrap();
    // End mod sym
    writeln!(f, "}}").unwrap();

    writeln!(
        f,
        "
/// TODO: Use `LazyLock`: https://github.com/CeleritasCelery/rune/issues/34
use std::sync::OnceLock;
static INTERNED_SYMBOLS: OnceLock<Mutex<ObjectMap>> = OnceLock::new();

pub(crate) fn interned_symbols() -> &'static Mutex<ObjectMap> {{
    INTERNED_SYMBOLS.get_or_init(|| Mutex::new({{
        let size: usize = {symbol_len};
        let mut map = SymbolMap::with_capacity(size);
        for sym in &sym::BUILTIN_SYMBOLS {{
            map.pre_init(Symbol::new(sym));
        }}
        ObjectMap {{
            map,
            block: Block::new_global(),
        }}
    }}))
}}
"
    )
    .unwrap();

    writeln!(
        f,
        "
#[allow(unused_qualifications)]
pub(crate) fn init_variables(
    cx: &crate::core::gc::Context,
    env: &mut crate::core::gc::Rt<crate::core::env::Env>,
) {{
use crate::core::object::Object;
"
    )
    .unwrap();

    let mut bool_vars = Vec::new();

    // write out the value of each defvar
    for (ident, _, value, ty) in all_defvar {
        // handled below
        if ident == "BYTE_BOOLEAN_VARS" {
            continue;
        }
        let mut value = value.unwrap_or_else(|| "Object::NIL".to_string());

        // if value starts with list! then insert cx before that last character
        if value.starts_with("list") {
            let len = value.len();
            value.insert_str(len - 1, "; cx");
        }
        writeln!(f, "env.vars.insert(sym::{ident}, cx.add({value}));").unwrap();
        if DefvarType::Bool == ty {
            bool_vars.push(ident);
        }
    }

    // write out the boolean vars
    writeln!(f, "let bool_vars = list![").unwrap();
    for ident in bool_vars {
        writeln!(f, "    sym::{ident},").unwrap();
    }
    writeln!(f, "; cx];").unwrap();
    writeln!(f, "env.vars.insert(sym::BYTE_BOOLEAN_VARS, bool_vars);").unwrap();

    writeln!(f, "}}").unwrap();
}
