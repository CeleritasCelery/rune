use fn_macros::defun;

use crate::core::object::GcObj;

#[defun]
fn error_message_string(obj: GcObj) -> String {
    // TODO: implement
    format!("Error: {obj}")
}

defvar!(PRINT_LENGTH);
defvar!(PRINT_LEVEL);
defvar_bool!(PRINT_ESCAPE_NEWLINES, false);
