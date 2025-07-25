//! File I/O.
use crate::{
    core::{
        cons::Cons,
        env::{Env, sym},
        error::{Type, TypeError},
        gc::{Context, Rt},
        object::{Number, Object, ObjectType, OptionalFlag, Symbol},
    },
    data::symbol_value,
    lisp::maybe_quit,
    search::lisp_regex_to_rust,
    sym::{FILE_NAME_HANDLER_ALIST, INHIBIT_FILE_NAME_HANDLERS, NIL},
};
use anyhow::{Result, bail, ensure};
use fancy_regex::Regex;
use rune_macros::defun;
use std::path::{Component, MAIN_SEPARATOR, Path};

#[defun]
pub(crate) fn expand_file_name(
    name: &str,
    default_directory: Option<&str>,
    env: &Rt<Env>,
    cx: &Context,
) -> Result<String> {
    // TODO: this needs to be tested to ensure it has the same behavior as GNU
    // Emacs. It doesn't do any normalization for one thing.
    if Path::new(name).is_absolute() {
        Ok(name.to_owned())
    } else if let Some(dir) = default_directory {
        let path = Path::new(dir);
        Ok(path.join(name).to_string_lossy().to_string())
    } else {
        let dir = env.vars.get(sym::DEFAULT_DIRECTORY).unwrap();
        match dir.untag(cx) {
            ObjectType::String(dir) => {
                let path = Path::new(dir.as_ref());
                Ok(path.join(name).to_string_lossy().to_string())
            }
            _ => unreachable!("`default-directory' should be a string"),
        }
    }
}

#[defun]
fn car_less_than_car(a: &Cons, b: &Cons) -> Result<bool> {
    let a: Number = a.car().try_into()?;
    let b: Number = b.car().try_into()?;
    Ok(a.val() < b.val())
}

#[defun]
fn file_name_as_directory(filename: &str) -> String {
    if filename.ends_with(MAIN_SEPARATOR) {
        filename.to_owned()
    } else {
        format!("{filename}{MAIN_SEPARATOR}")
    }
}

#[defun]
fn file_directory_p(filename: &str) -> bool {
    if filename.is_empty() { true } else { Path::new(filename).is_dir() }
}

/// Return dirname sans final path separator, unless the string consists entirely of separators.
#[defun]
fn directory_file_name(dirname: &str) -> &str {
    let path = Path::new(dirname);
    let mut path_components = path.components();
    if path_components.clone().next().is_none() {
        return "";
    }

    if path_components.all(|c| c == Component::RootDir || c == Component::Normal("".as_ref())) {
        return "/";
    }

    dirname.strip_suffix(MAIN_SEPARATOR).unwrap_or(dirname)
}

/// Returns true if the path is absolute
#[defun]
fn file_name_absolute_p(filename: &str) -> bool {
    let path = Path::new(filename);
    // TODO: GNU Emacs has special handling for ~user directories, where the user exists.
    //   so as per example in the manual, ~rms/foo is considered absolute if user `rms` exists
    //   doing this here would require "knowing" the list of valid users and looking for ~path
    //   components.
    path.is_absolute()
}

/// Returns the directory part of `filename`, as a directory name, or nil if filename does not include a directory part.
#[defun]
fn file_name_directory(filename: &str) -> Option<String> {
    // TODO: GNU Emacs docs stipulate that "On MS-DOS [ed: presumably windows,
    // too] it can also end in a colon."
    if !filename.contains(MAIN_SEPARATOR) {
        return None;
    }

    if filename.ends_with(MAIN_SEPARATOR) {
        return Some(filename.into());
    }

    let path = Path::new(filename);
    let parent = path.parent()?;

    // Special case for root path so we don't end up returning '//'
    if parent.parent().is_none() {
        return Some(format!("{MAIN_SEPARATOR}"));
    }
    let parent_path = parent.to_str()?;
    Some(format!("{parent_path}{MAIN_SEPARATOR}"))
}

/// Returns the non-directory part of `filename`
#[defun]
fn file_name_nondirectory(filename: &str) -> &str {
    if filename.ends_with(MAIN_SEPARATOR) {
        return "";
    }

    let path = Path::new(filename);
    let Some(file_name) = path.file_name() else {
        return "";
    };

    file_name.to_str().unwrap()
}

/// Return non-nil if NAME ends with a directory separator character.
#[defun]
fn directory_name_p(name: &str) -> bool {
    name.ends_with(MAIN_SEPARATOR)
}

defvar!(FILE_NAME_HANDLER_ALIST);
defvar!(INHIBIT_FILE_NAME_HANDLERS);
defsym!(INHIBIT_FILE_NAME_OPERATION);

/// Return FILENAME's handler function for OPERATION, if it has one.
/// Otherwise, return nil.
/// A file name is handled if one of the regular expressions in
/// `file-name-handler-alist' matches it.
///
/// If OPERATION equals `inhibit-file-name-operation', then ignore
/// any handlers that are members of `inhibit-file-name-handlers',
/// but still do run any other handlers.  This lets handlers
/// use the standard functions without calling themselves recursively.
#[defun]
fn find_file_name_handler<'ob>(
    filename: &str,
    operation: Symbol,
    env: &mut Rt<Env>,
    cx: &'ob Context,
) -> Symbol<'ob> {
    find_file_name_handler_internal(filename, operation, env, cx).unwrap_or(NIL)
}

fn find_file_name_handler_internal<'ob>(
    filename: &str,
    operation: Symbol,
    env: &mut Rt<Env>,
    cx: &'ob Context,
) -> Option<Symbol<'ob>> {
    let file_name_handler_alist = symbol_value(FILE_NAME_HANDLER_ALIST, env, cx)?.as_list().ok()?;
    let inhibit_file_name_handlers =
        symbol_value(INHIBIT_FILE_NAME_HANDLERS, env, cx).and_then(|sym| sym.as_list().ok());

    file_name_handler_alist
        .filter_map(|elem| {
            let cons = elem.ok()?.cons()?;
            let regexp = cons.car().string()?;
            let re = Regex::new(&lisp_regex_to_rust(regexp))
                .unwrap_or_else(|err| panic!("Invalid regexp '{regexp}': {err}"));
            if re.is_match(filename).ok()? {
                let handler = cons.cdr().symbol()?;
                if operation != sym::INHIBIT_FILE_NAME_OPERATION
                    || !inhibit_file_name_handlers
                        .clone()
                        .is_some_and(|mut ifnh| ifnh.any(|h| h.is_ok_and(|h| h == handler)))
                {
                    return Some(handler);
                }
            }
            maybe_quit();
            None
        })
        .next()
}

#[defun]
fn file_symlink_p(filename: &str) -> bool {
    Path::new(filename).is_symlink()
}

#[defun]
fn file_name_case_insensitive_p(filename: &str) -> bool {
    if !Path::new(filename).exists() {
        return false;
    }
    case_insensitive(filename)
}

#[cfg(target_os = "macos")]
fn case_insensitive(filename: &str) -> bool {
    // https://github.com/phracker/MacOSX-SDKs/blob/master/MacOSX10.1.5.sdk/usr/include/sys/unistd.h#L127
    const _PC_CASE_SENSITIVE: libc::c_int = 11;
    let result = unsafe {
        let filename = std::ffi::CString::new(filename).unwrap();
        libc::pathconf(filename.as_ptr(), _PC_CASE_SENSITIVE)
    };
    if result == -1 {
        panic!(
            "file-name-case-insensitive-p pathconf failed: {}",
            std::io::Error::last_os_error()
        )
    }
    result == 0
}

#[cfg(windows)]
fn case_insensitive(filename: &str) -> bool {
    // https://learn.microsoft.com/en-us/windows/wsl/case-sensitivity#inspect-current-case-sensitivity
    let output = std::process::Command::new("fsutil.exe")
        .arg("file")
        .arg("queryCaseSensitiveInfo")
        .arg(filename)
        .output()
        .unwrap()
        .stdout;
    std::str::from_utf8(&output).unwrap().contains("disabled")
}

#[cfg(target_os = "linux")]
fn case_insensitive(_filename: &str) -> bool {
    false
}

#[test]
#[cfg(not(miri))]
fn test_case_sensative_call() {
    let _ = file_name_case_insensitive_p("/");
}

#[defun]
#[expect(clippy::too_many_arguments)]
fn write_region(
    start: i64,
    end: i64,
    filename: &str,
    append: OptionalFlag,
    visit: OptionalFlag,
    lockname: OptionalFlag,
    mustbenew: OptionalFlag,
    env: &Rt<Env>,
) -> Result<()> {
    use std::io::Write;
    ensure!(append.is_none(), "append not implemented");
    ensure!(visit.is_none(), "visit not implemented");
    ensure!(lockname.is_none(), "lockname not implemented");
    ensure!(mustbenew.is_none(), "mustbenew not implemented");
    // Open filename for writing
    let mut file = std::fs::OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(filename)
        .unwrap();
    let b = env.current_buffer.get();
    let (s1, s2) = b.slice_with_gap(start as usize, end as usize)?;
    write!(file, "{s1}")?;
    write!(file, "{s2}")?;
    Ok(())
}

/// Concatenate components to directory, inserting path separators as required.
#[defun]
fn file_name_concat(directory: &str, rest_components: &[Object]) -> Result<String> {
    let mut path = String::from(directory);

    // All components must be stringp...
    for r_c in rest_components {
        let ObjectType::String(s) = r_c.untag() else {
            bail!(TypeError::new(Type::String, r_c));
        };

        // Append separator before adding the new element, but only if the
        // existing path isn't already terminated with a "/"
        if !path.ends_with(MAIN_SEPARATOR) {
            path.push(MAIN_SEPARATOR)
        }

        path.push_str(s.as_ref());
    }

    Ok(path)
}

// TODO: file-relative-name -- requires knowing the current buffer's default directory
// TODO: file-name-sans-versions
// TODO: find-file-name-handler: https://www.gnu.org/software/emacs/manual/html_node/elisp/Magic-File-Names.html
//   required by file-name-extension  & file-name-sans-extension library & file-relative-name functions (among others)
#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        assert_elprop,
        core::{env::sym, gc::RootSet},
        data::set,
        sym::INHIBIT_FILE_NAME_HANDLERS,
    };
    use rune_core::macros::{list, root};

    #[test]
    fn test_find_file_name_handler_matching() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        root!(env, new(Env), cx);

        let handler = Symbol::new_uninterned("matching-handler", cx);
        let not_matching_handler = Symbol::new_uninterned("not-matching-handler", cx);
        let file_name_handler_alist = list![
            Cons::new(r"test\.txt", handler, cx),
            Cons::new("NEVER-MATCH", not_matching_handler, cx);
            cx
        ];
        set(FILE_NAME_HANDLER_ALIST, file_name_handler_alist, env).unwrap();

        let test_txt_handler = find_file_name_handler("test.txt", NIL, env, cx);
        assert_eq!(test_txt_handler, handler);
        let not_matching_handler = find_file_name_handler("not-matching.el", NIL, env, cx);
        assert_eq!(not_matching_handler, NIL);
    }

    #[test]
    fn test_find_file_name_handler_inhibit_handlers() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        root!(env, new(Env), cx);

        let handler = Symbol::new_uninterned("matching-handler", cx);
        let inhibit_handler = Symbol::new_uninterned("inhibit-handler", cx);
        let file_name_handler_alist = list![
            Cons::new(r"test\.txt", inhibit_handler, cx),
            Cons::new(r"test\.txt", handler, cx);
            cx
        ];
        set(FILE_NAME_HANDLER_ALIST, file_name_handler_alist, env).unwrap();
        set(INHIBIT_FILE_NAME_HANDLERS, list![inhibit_handler; cx], env).unwrap();

        let result = find_file_name_handler("test.txt", NIL, env, cx);
        assert_eq!(result, inhibit_handler);

        let result = find_file_name_handler("test.txt", sym::INHIBIT_FILE_NAME_OPERATION, env, cx);
        assert_eq!(result, handler);
    }

    #[test]
    fn test_find_file_name_handler_no_panic() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        let handler = Symbol::new_uninterned("handler", cx);
        root!(env, new(Env), cx);

        set(FILE_NAME_HANDLER_ALIST, list![Cons::new(NIL, handler, cx); cx], env).unwrap();
        find_file_name_handler("example", NIL, env, cx);

        set(FILE_NAME_HANDLER_ALIST, list![Cons::new(".*", NIL , cx); cx], env).unwrap();
        find_file_name_handler("example", NIL, env, cx);
    }

    #[test]
    #[should_panic(
        expected = "Invalid regexp '\\\\(incorrect-regexp': Parsing error at position 17: Opening parenthesis without closing parenthesis"
    )]
    fn test_find_file_name_handler_panic_on_invalid_regex() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        let handler = Symbol::new_uninterned("handler", cx);
        root!(env, new(Env), cx);

        set(
            FILE_NAME_HANDLER_ALIST,
            list![Cons::new(r#"\(incorrect-regexp"#, handler, cx); cx],
            env,
        )
        .unwrap();
        find_file_name_handler("example", NIL, env, cx);
    }

    #[test]
    #[ignore = "TODO: Handle ~ in expand-file-name"]
    fn test_expand_file_name() {
        assert_elprop![r#"(expand-file-name "~/test.txt" "/")"#];
    }
}
