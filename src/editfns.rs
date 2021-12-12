use fn_macros::defun;

use crate::object::Object;

#[defun]
fn message(format_string: &str, args: &[Object]) -> String {
    println!("MESSAGE: {} -> {:?}", format_string, args);
    format_string.to_owned()
}

defsubr!(message);
