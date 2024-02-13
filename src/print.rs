//! Printing utilities.
use crate::core::object::Object;
use rune_macros::defun;

#[defun]
fn error_message_string(obj: Object) -> String {
    // TODO: implement
    format!("Error: {obj}")
}

defvar!(PRINT_LENGTH);
defvar!(PRINT_LEVEL);
defvar_bool!(PRINT_ESCAPE_NEWLINES, false);
