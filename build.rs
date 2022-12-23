use std::borrow::Cow;
use std::ffi::OsStr;
use std::io::Write as _;
use std::{
    fs::{self, File},
    path::Path,
};

// take an input str and parse it with syn::ExprCall and return the args as strings
pub fn parse_args(input: &str) -> Vec<String> {
    let pseudo_fn = format!("X{input}");
    let expr = syn::parse_str::<syn::ExprCall>(&pseudo_fn).unwrap();
    use quote::ToTokens;
    expr.args
        .into_iter()
        .map(|e| e.to_token_stream().to_string())
        .collect()
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

enum DefvarType {
    Bool,
    Other,
}

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
            for (start, end) in contents.match_indices("#[defun") {
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
                    let path_name = path.to_str().unwrap();
                    let basename = path_name.strip_prefix("src/").unwrap().strip_suffix(".rs");
                    let import_path = basename.unwrap().replace('/', ":");
                    format!("crate::{import_path}::S{name}")
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
    println!("cargo:warning={out_dir}/sym.rs");
    let dest_path = Path::new(&out_dir).join("sym.rs");
    let mut f = File::create(dest_path).unwrap();

    let symbol_len = all_defsym.len() + all_defun.len() + all_defvar.len() + 2;
    writeln!(
        f,
        "
#[allow(unused_imports)]
#[allow(non_snake_case)]
#[allow(dead_code)]
pub(crate) mod sym {{
use crate::core::env::Symbol;
use crate::core::env::ConstSymbol;

static __RUNTIME_SYMBOL_GLOBAL: Symbol = Symbol::new(\"_dummy_runtime_symbol\", RUNTIME_SYMBOL);
fn __RUNTIME_SYMBOL_FN () -> crate::core::env::SymbolX<'static> {{&__RUNTIME_SYMBOL_GLOBAL}}
pub(crate) const RUNTIME_SYMBOL: ConstSymbol = ConstSymbol::new(__RUNTIME_SYMBOL_FN);

static BUILTIN_SYMBOLS: [Symbol; {symbol_len}] = [
    Symbol::new_const(\"nil\", NIL),
    Symbol::new_const(\"t\", TRUE),",
    )
    .unwrap();

    // write the list of all defsym to the file
    for (sym, name) in &all_defsym {
        let sym_name = match name {
            Some(name) => name.trim_matches('"').to_string(),
            None => map_varname(sym),
        };
        writeln!(f, "    Symbol::new(\"{sym_name}\", {sym}),").unwrap();
    }

    for (ident, name, _, _) in &all_defvar {
        #[rustfmt::skip]
        writeln!(f, "    Symbol::new(\"{name}\", {ident}),").unwrap();
    }

    // write the list of all defun to a file in out_dir
    for (_, name, lisp_name) in &all_defun {
        let const_name = name.to_ascii_uppercase();
        #[rustfmt::skip]
        writeln!(f, "    Symbol::new(\"{lisp_name}\", {const_name}),").unwrap();
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
        let matcher_name = format!("__FN_PTR_{sym_name}");
        #[rustfmt::skip]
        writeln!(f, "fn {matcher_name}() -> &'static Symbol {{ &BUILTIN_SYMBOLS[{idx}] }}").unwrap();
        #[rustfmt::skip]
        writeln!(f, "pub(crate) const {sym_name}: ConstSymbol = ConstSymbol::new({matcher_name});").unwrap();
    }

    // all SubrFn
    let subr_len = all_defun.len();
    writeln!(
        f,
        "static SUBR_DEFS: [&crate::core::object::SubrFn; {subr_len}] = [",
    )
    .unwrap();
    for (subr_name, _, _) in &all_defun {
        writeln!(f, "    &{subr_name},",).unwrap();
    }
    // End SUBR_DEFS
    writeln!(f, "];\n").unwrap();

    let defun_start = symbol_len - subr_len;
    writeln!(
        f,
        "
pub(super) fn init_symbols(map: &mut super::SymbolMap) {{
    for sym in BUILTIN_SYMBOLS[..{defun_start}].iter() {{
        map.pre_init(sym);
    }}
    for (sym, func) in BUILTIN_SYMBOLS[{defun_start}..].iter().zip(SUBR_DEFS.iter()) {{
        unsafe {{ sym.set_func((*func).into()).unwrap(); }}
        map.pre_init(sym);
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
lazy_static::lazy_static! {{
    pub(crate) static ref INTERNED_SYMBOLS: Mutex<ObjectMap> = Mutex::new({{
        let size: usize = {symbol_len};
        let mut map = SymbolMap::with_capacity(size);
        sym::init_symbols(&mut map);
        ObjectMap {{
            map,
            block: Block::new_global(),
        }}
    }});
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
) {{"
    )
    .unwrap();

    // write out the value of each defvar
    for (ident, _, value, ty) in all_defvar {
        let nil = "crate::core::object::nil()";
        let mut value = match value {
            Some(value) => Cow::from(value),
            None => Cow::from(nil),
        };

        // if value starts with list! then insert cx before that last character
        if value.starts_with("list") {
            let len = value.len();
            value.to_mut().insert_str(len - 1, "; cx");
        }
        writeln!(f, "env.vars.insert(sym::{ident}, cx.add({value}));").unwrap();
        writeln!(f, "env.special_variables.insert(sym::{ident});").unwrap();
        match ty {
            DefvarType::Bool => {
                writeln!(
                    f,
                    "{{
    let bool_vars = env.vars.get_mut(sym::BYTE_BOOLEAN_VARS).unwrap();
    bool_vars.set(crate::cons!(sym::{ident}, bool_vars.bind(cx); cx));
}}"
                )
                .unwrap();
            }
            DefvarType::Other => {}
        }
    }

    writeln!(f, "}}").unwrap();
}
