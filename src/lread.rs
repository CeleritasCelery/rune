//! Loading elisp from files and strings.
use crate::core::env::Symbol;
use crate::core::env::{sym, Env};
use crate::core::error::{Type, TypeError};
use crate::core::gc::Context;
use crate::core::gc::Rt;
use crate::core::object::{nil, Gc, GcObj, LispString, Object, WithLifetime};
use crate::interpreter;
use crate::reader;
use anyhow::{anyhow, Context as _};
use anyhow::{bail, ensure, Result};
use rune_core::macros::{cons, root};
use rune_macros::defun;
use std::fs;
use std::path::{Path, PathBuf};

fn check_lower_bounds(idx: Option<i64>, len: usize) -> Result<usize> {
    let len = len as i64;
    let idx = idx.unwrap_or(0);
    ensure!(
        -len <= idx && idx < len,
        "start index of {idx} is out of bounds for string of length {len}"
    );
    let idx = if idx < 0 { len + idx } else { idx };
    Ok(idx as usize)
}

fn check_upper_bounds(idx: Option<i64>, len: usize) -> Result<usize> {
    let len = len as i64;
    let idx = idx.unwrap_or(len);
    ensure!(
        -len <= idx && idx <= len,
        "end index of {idx} is out of bounds for string of length {len}"
    );
    let idx = if idx < 0 { len + idx } else { idx };
    Ok(idx as usize)
}

#[defun]
pub(crate) fn read_from_string<'ob>(
    string: &str,
    start: Option<i64>,
    end: Option<i64>,
    cx: &'ob Context,
) -> Result<GcObj<'ob>> {
    let len = string.len();
    let start = check_lower_bounds(start, len)?;
    let end = check_upper_bounds(end, len)?;

    let (obj, new_pos) = match reader::read(&string[start..end], cx) {
        Ok((obj, pos)) => (obj, pos),
        Err(mut e) => {
            e.update_pos(start);
            bail!(e);
        }
    };
    Ok(cons!(obj, new_pos as i64; cx))
}

pub(crate) fn load_internal(contents: &str, cx: &mut Context, env: &mut Rt<Env>) -> Result<bool> {
    let mut pos = 0;
    loop {
        let (obj, new_pos) = match reader::read(&contents[pos..], cx) {
            Ok((obj, pos)) => (obj, pos),
            Err(reader::Error::EmptyStream) => return Ok(true),
            Err(mut e) => {
                e.update_pos(pos);
                bail!(e);
            }
        };
        if crate::debug::debug_enabled() {
            let content = &contents[pos..(new_pos + pos)];
            println!("-----READ START-----\n {content}");
            println!("-----READ END-----");
        }
        root!(obj, cx);
        interpreter::eval(obj, None, env, cx)?;
        assert_ne!(new_pos, 0);
        pos += new_pos;
    }
}

fn file_in_path(file: &str, path: &str) -> Option<PathBuf> {
    let path = Path::new(path).join(file);
    if path.exists() {
        Some(path)
    } else {
        let with_ext = path.with_extension("el");
        with_ext.exists().then_some(with_ext)
    }
}

fn find_file_in_load_path(file: &str, cx: &Context, env: &Rt<Env>) -> Result<PathBuf> {
    let load_path = env.vars.get(sym::LOAD_PATH).unwrap();
    let paths = load_path.bind(cx).as_list().context("`load-path' was not a list")?;
    let mut final_file = None;
    for path in paths {
        match path?.untag() {
            Object::String(path) => {
                if let Some(x) = file_in_path(file, path.try_into()?) {
                    final_file = Some(x);
                    break;
                }
            }
            x => {
                return Err(TypeError::new(Type::String, x))
                    .context("Found non-string in `load-path'");
            }
        }
    }
    final_file.ok_or_else(|| anyhow!("Unable to find file `{file}' in load-path"))
}

#[defun]
pub(crate) fn load(
    file: &Rt<Gc<&LispString>>,
    noerror: Option<()>,
    nomessage: Option<()>,
    cx: &mut Context,
    env: &mut Rt<Env>,
) -> Result<bool> {
    let noerror = noerror.is_some();
    let nomessage = nomessage.is_some();
    let file: &str = file.get(cx).try_into()?;
    let final_file = if Path::new(file).exists() {
        PathBuf::from(file)
    } else {
        match find_file_in_load_path(file, cx, env) {
            Ok(x) => x,
            Err(e) => {
                return match noerror {
                    true => Ok(false),
                    false => Err(e),
                };
            }
        }
    };

    let filename = String::from(file);
    if !nomessage {
        println!("Loading {filename}...");
    }
    let new_load_file = cx.add(final_file.to_string_lossy().to_string());
    let prev_load_file = match env.vars.get_mut(sym::LOAD_FILE_NAME) {
        Some(val) => {
            let prev = val.bind(cx);
            val.set(new_load_file);
            prev
        }
        None => nil(),
    };
    root!(prev_load_file, cx);
    let result = match fs::read_to_string(&final_file)
        .with_context(|| format!("Couldn't open file {:?}", final_file.as_os_str()))
    {
        Ok(content) => load_internal(&content, cx, env),
        Err(e) => match noerror {
            true => Ok(false),
            false => Err(e),
        },
    };

    if !nomessage && result.is_ok() {
        println!("Loading {filename} Done");
    }
    env.vars.insert(sym::LOAD_FILE_NAME, &*prev_load_file);
    result
}

#[defun]
pub(crate) fn intern<'ob>(string: &str, cx: &'ob Context) -> Symbol<'ob> {
    crate::core::env::intern(string, cx)
}

#[defun]
pub(crate) fn intern_soft(string: GcObj, obarray: Option<()>) -> Result<Symbol> {
    ensure!(obarray.is_none(), "intern-soft obarray not implemented");
    match string.untag() {
        Object::Symbol(sym) => {
            if sym.interned() {
                Ok(sym)
            } else {
                Ok(sym::NIL)
            }
        }
        Object::String(string) => {
            let map = crate::core::env::interned_symbols().lock().unwrap();
            match map.get(string.try_into()?) {
                Some(sym) => Ok(unsafe { sym.with_lifetime() }),
                None => Ok(sym::NIL),
            }
        }
        x => Err(TypeError::new(Type::String, x).into()),
    }
}

defvar!(LEXICAL_BINDING, true);
defvar!(CURRENT_LOAD_LIST);
defvar!(LOAD_HISTORY);
defvar!(LOAD_PATH, list![format!("{}/lisp", env!("CARGO_MANIFEST_DIR"))]);
defvar!(LOAD_FILE_NAME);
defvar!(BYTE_BOOLEAN_VARS);
defvar!(MACROEXP__DYNVARS);
defvar!(AFTER_LOAD_ALIST);

#[cfg(test)]
mod test {

    use super::*;
    use crate::core::gc::RootSet;
    use rune_core::macros::root;

    #[test]
    #[allow(clippy::float_cmp)] // Bug in Clippy
    fn test_load() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        root!(env, Env::default(), cx);
        load_internal("(setq foo 1) (setq bar 2) (setq baz 1.5)", cx, env).unwrap();

        let obj = reader::read("(+ foo bar baz)", cx).unwrap().0;
        root!(obj, cx);
        let val = interpreter::eval(obj, None, env, cx).unwrap();
        assert_eq!(val, 4.5);
    }
}
