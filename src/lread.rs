//! Loading elisp from files and strings.
use crate::core::cons::Cons;
use crate::core::env::{Env, sym};
use crate::core::error::{Type, TypeError};
use crate::core::gc::{Context, Rt, Rto};
use crate::core::object::{
    Function, Gc, LispString, NIL, Object, ObjectType, OptionalFlag, Symbol, TRUE, TagType,
    WithLifetime,
};
use crate::reader;
use crate::{interpreter, rooted_iter};
use anyhow::{Context as _, anyhow};
use anyhow::{Result, bail, ensure};
use fallible_streaming_iterator::FallibleStreamingIterator;
use rune_core::macros::{call, rebind, root};
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
) -> Result<Object<'ob>> {
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
    Ok(Cons::new(obj, new_pos as i64, cx).into())
}

pub(crate) fn load_internal(contents: &str, cx: &mut Context, env: &mut Rt<Env>) -> Result<bool> {
    let mut pos = 0;
    let macroexpand: Option<Function> = None;
    root!(macroexpand, cx);
    if let Some(fun) = sym::INTERNAL_MACROEXPAND_FOR_LOAD.func(cx) {
        macroexpand.set(Some(fun));
    }
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
        let result = if let Some(fun) = macroexpand.as_ref() {
            eager_expand(obj, fun, env, cx)
        } else {
            interpreter::eval(obj, None, env, cx)
        };
        if let Err(e) = result {
            let content = &contents[pos..(new_pos + pos)];
            println!("-----LOAD ERROR START-----\n {content}");
            println!("-----LOAD ERROR END-----");
            return Err(e);
        }
        assert_ne!(new_pos, 0);
        pos += new_pos;
    }
}

fn eager_expand<'ob>(
    obj: &Rto<Object>,
    macroexpand: &Rto<Function>,
    env: &mut Rt<Env>,
    cx: &'ob mut Context,
) -> Result<Object<'ob>, anyhow::Error> {
    let name = "internal-macroexpand-for-load";
    let val = call!(macroexpand, obj, NIL; name, env, cx)?;
    let val = rebind!(val, cx);
    if let Ok((sym::PROGN, forms)) = val.as_cons_pair() {
        root!(val, NIL, cx);
        rooted_iter!(forms, forms.tag(), cx);
        while let Some(form) = forms.next()? {
            let result = eager_expand(form, macroexpand, env, cx)?;
            val.set(result);
        }
        return Ok(val.bind(cx));
    }
    let result = call!(macroexpand, val, TRUE; name, env, cx)?;
    root!(result, cx);
    interpreter::eval(result, None, env, cx)
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
            ObjectType::String(path) => {
                if let Some(x) = file_in_path(file, path) {
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
    file: &Rto<Gc<&LispString>>,
    noerror: OptionalFlag,
    nomessage: OptionalFlag,
    cx: &mut Context,
    env: &mut Rt<Env>,
) -> Result<bool> {
    let noerror = noerror.is_some();
    let nomessage = nomessage.is_some();
    let file: &str = file.untag(cx);
    let final_file = if Path::new(file).exists() {
        PathBuf::from(file)
    } else {
        match find_file_in_load_path(file, cx, env) {
            Ok(x) => x,
            Err(e) => {
                return if noerror { Ok(false) } else { Err(e) };
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
        None => NIL,
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
pub(crate) fn intern_soft(string: Object, obarray: OptionalFlag) -> Result<Symbol> {
    ensure!(obarray.is_none(), "intern-soft obarray not implemented");
    match string.untag() {
        ObjectType::Symbol(sym) => {
            if sym.interned() {
                Ok(sym)
            } else {
                Ok(sym::NIL)
            }
        }
        ObjectType::String(string) => {
            let map = crate::core::env::INTERNED_SYMBOLS.lock().unwrap();
            match map.get(string) {
                Some(sym) => Ok(unsafe { sym.with_lifetime() }),
                None => Ok(sym::NIL),
            }
        }
        x => Err(TypeError::new(Type::String, x).into()),
    }
}

defsym!(INTERNAL_MACROEXPAND_FOR_LOAD);
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
    fn test_load() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        sym::init_symbols();
        root!(env, new(Env), cx);
        load_internal("(setq foo 1) (setq bar 2) (setq baz 1.5)", cx, env).unwrap();

        let obj = reader::read("(+ foo bar baz)", cx).unwrap().0;
        root!(obj, cx);
        let val = interpreter::eval(obj, None, env, cx).unwrap();
        assert_eq!(val, 4.5);
    }
}
