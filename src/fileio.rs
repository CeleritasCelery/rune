//! File I/O.
use crate::core::{
    cons::Cons,
    env::{sym, Env},
    error::{Type, TypeError},
    gc::{Context, Rt},
    object::{Number, Object, ObjectType},
};
use anyhow::{bail, Result};
use rune_macros::defun;
use std::path::{Component, Path, MAIN_SEPARATOR};

defvar!(FILE_NAME_HANDLER_ALIST);

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
    if filename.is_empty() {
        true
    } else {
        Path::new(filename).is_dir()
    }
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
