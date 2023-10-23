use fn_macros::defun;
use titlecase::titlecase;

#[defun]
fn capitalize(s: &str) -> String {
    titlecase(s)
}

#[defun]
fn upcase(s: &str) -> String {
    // TODO: use unicode
    s.to_uppercase()
}
