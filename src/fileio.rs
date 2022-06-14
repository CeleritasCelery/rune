use std::path::Path;

use fn_macros::defun;

use crate::core::{
    arena::{Arena, Root},
    env::{sym, Environment},
    object::Object,
};

#[defun]
pub(crate) fn expand_file_name(
    name: &str,
    default_directory: Option<&str>,
    env: &Root<Environment>,
    gc: &Arena,
) -> String {
    // TODO: this needs to be tested to ensure it has the same behavior as GNU
    // Emacs. It doesn't do any normalization for one thing.
    if Path::new(name).is_absolute() {
        name.to_owned()
    } else if let Some(dir) = default_directory {
        let path = Path::new(dir);
        path.join(name).to_string_lossy().to_string()
    } else {
        let dir = env.vars.get(&sym::DEFAULT_DIRECTORY).unwrap();
        match dir.bind(gc).get() {
            Object::String(s) => {
                let path = Path::new(s);
                path.join(name).to_string_lossy().to_string()
            }
            _ => unreachable!("`default-directory' should be a string"),
        }
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

defsubr!(expand_file_name, file_directory_p);
