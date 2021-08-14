use crate::arena::Arena;
use crate::cons::Cons;
use crate::object::{IntoObject, Object};
use fn_macros::defun;

#[defun]
pub(crate) fn list<'ob>(objects: &[Object<'ob>], arena: &'ob Arena) -> Object<'ob> {
    let mut head = Object::Nil;
    for object in objects.iter().rev() {
        head = Cons::new(*object, head).into_obj(arena);
    }
    head
}

#[defun]
pub(crate) fn progn<'ob>(forms: &[Object<'ob>]) -> Object<'ob> {
    match forms.last() {
        Some(form) => *form,
        None => Object::Nil,
    }
}

defsubr!(progn, list);
