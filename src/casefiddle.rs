use fn_macros::defun;
use titlecase::titlecase;

#[defun]
fn capitalize(s: &str) -> String {
    titlecase(s)
}
