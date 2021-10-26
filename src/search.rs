use fn_macros::defun;
use crate::object::Object;


#[defun]
fn string_match<'ob>(_regexp: &str, _string: &str, _start: Option<i64>) -> Object<'ob> {
    // TODO: implement
    Object::NIL
}

defsubr!(string_match);
