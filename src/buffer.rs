use crate::core::object::GcObj;
use fn_macros::defun;

#[defun]
fn set_buffer(buffer_or_name: GcObj) -> GcObj {
    // TODO: implement
    buffer_or_name
}

#[defun]
fn set_buffer_modified_p(flag: GcObj) -> GcObj {
    // TODO: implement
    flag
}
