use fn_macros::defun;

use crate::core::object::GcObj;

#[defun]
fn error_message_string(obj: GcObj) -> String {
    // TODO: implement
    format!("Error: {obj}")
}

define_symbols!(FUNCS => {error_message_string});
