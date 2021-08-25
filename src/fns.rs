use crate::arena::Arena;
use crate::data::Environment;
use crate::eval;
use crate::object::{Function, List, Object};
use anyhow::Result;
use fn_macros::defun;

pub(crate) fn slice_into_list<'ob>(
    slice: &[Object<'ob>],
    tail: Option<Object<'ob>>,
    arena: &'ob Arena,
) -> Object<'ob> {
    if slice.is_empty() {
        Object::Nil
    } else {
        let from_end = slice.iter().rev();
        from_end.fold(tail.into(), |acc, obj| cons!(*obj, acc; arena))
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
                .collect::<Result<Vec<_>>>()?;
            Ok(slice_into_list(&vec, None, arena))
        }
    }
}

#[defun]
pub(crate) fn assq<'ob>(key: Object<'ob>, alist: List<'ob>) -> Object<'ob> {
    match alist {
        List::Nil => Object::Nil,
        List::Cons(cons) => cons
            .into_iter()
            .find(|x| match x {
                Ok(Object::Cons(cons)) => crate::data::eq(key, cons.car()),
                _ => false,
            })
            .transpose()
            .expect("should never return error value")
            .unwrap_or_default(),
    }
}

#[defun]
pub(crate) fn make_hash_table<'ob>(_keyword_args: &[Object<'ob>]) -> Object<'ob> {
    // TODO: Implement
    Object::Nil
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_mapcar() {
        let arena = &Arena::new();
        let element = cons!(5, 6; arena);
        let list = list![cons!(1, 2; arena), cons!(3, 4; arena), element; arena];
        if let Object::Cons(cons) = list {
            let result = assq(5.into(), List::Cons(!cons));
            assert_eq!(result, element);
        }
    }
}

defsubr!(mapcar, assq, make_hash_table);
