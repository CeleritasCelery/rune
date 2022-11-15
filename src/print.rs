use fn_macros::defun;

use crate::core::object::GcObj;

#[defun]
fn error_message_string(obj: GcObj) -> String {
    // TODO: implement
    format!("Error: {obj}")
}

defvar!(PRINT_LENGTH);
defvar!(PRINT_LEVEL);
defvar!(PRINT_ESCAPE_NEWLINES);

define_symbols!(
    FUNCS => {error_message_string}
    VARS => {PRINT_LENGTH, PRINT_LEVEL, PRINT_ESCAPE_NEWLINES}
);
