use rune_macros::defun;

#[defun]
fn capitalize(s: &str) -> String {
    titlecase::titlecase(s)
}

#[defun]
fn upcase(s: &str) -> String {
    // TODO: use unicode
    s.to_uppercase()
}
