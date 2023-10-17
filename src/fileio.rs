use crate::core::{
    env::{sym, Env},
    gc::{Context, Rt},
    object::Object,
};
use anyhow::Result;
use fn_macros::defun;
use std::path::Path;

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
        match dir.get(cx) {
            Object::String(s) => {
                let name: &str = s.try_into()?;
                let path = Path::new(name);
                Ok(path.join(name).to_string_lossy().to_string())
            }
            _ => unreachable!("`default-directory' should be a string"),
        }
    }
}

#[defun]
fn file_name_as_directory(filename: &str) -> String {
    use std::path::MAIN_SEPARATOR as SEPARATOR;
    if filename.ends_with(SEPARATOR) {
        filename.to_owned()
    } else {
        format!("{filename}{SEPARATOR}")
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
