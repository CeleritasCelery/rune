use crate::arena::Arena;
use crate::data::Environment;
use crate::eval;
use crate::object::{Function, List, Object};
use anyhow::Result;
use fn_macros::defun;

pub(crate) fn vector_into_list<'ob>(
    vec: Vec<Object<'ob>>,
    tail: Option<Object<'ob>>,
    arena: &'ob Arena,
) -> Object<'ob> {
    if vec.is_empty() {
        Object::Nil
    } else {
        let from_end = vec.into_iter().rev();
        from_end.fold(tail.into(), |acc, obj| cons!(obj, acc; arena))
    }
}

impl<'ob> Function<'ob> {
    pub(crate) fn call(
        self,
        args: Vec<Object<'ob>>,
        env: &mut Environment<'ob>,
        arena: &'ob Arena,
    ) -> Result<Object<'ob>> {
        match self {
            Function::LispFn(f) => eval::call_lisp(!f, args, env, arena),
            Function::SubrFn(f) => eval::call_subr(*!f, args, env, arena),
        }
    }
}

#[defun]
pub(crate) fn mapcar<'ob>(
    function: Function<'ob>,
    sequence: List<'ob>,
    env: &mut Environment<'ob>,
    arena: &'ob Arena,
) -> Result<Object<'ob>> {
    match sequence {
        List::Nil => Ok(Object::Nil),
        List::Cons(cons) => {
            let iter = cons.into_iter();
            let vec = iter
                .map(|x| match x {
                    Ok(obj) => function.call(vec![obj], env, arena),
                    err => err,
                })
                .collect::<Result<_>>()?;
            Ok(vector_into_list(vec, None, arena))
        }
    }
}

defsubr!(mapcar);
