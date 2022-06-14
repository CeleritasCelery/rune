

use fn_macros::defun;

use crate::core::object::GcObj;

#[defun]
fn set_buffer(buffer_or_name: GcObj) -> GcObj {
    // TODO: implement
    buffer_or_name
}

defsubr!(set_buffer);
