use std::convert::TryInto;

use crate::arena::Arena;
use crate::cons::Cons;
use crate::data;
use crate::data::Environment;
use crate::eval;
use crate::object::{Function, List, Object};
use crate::symbol::Symbol;
use anyhow::{bail, Result};
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
            Function::LispFn(f) => eval::call_lisp(&f, args, env, arena),
            Function::SubrFn(f) => eval::call_subr(*f, args, env, arena),
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
pub(crate) fn nreverse(seq: Object) -> Result<Object> {
    let mut iter = next_mut(seq)?;
    let mut prev = Object::Nil;
    while let Some(tail) = iter {
        let next = tail.cdr();
        tail.set_cdr(prev);
        prev = tail.into();
        iter = next_mut(next)?;
    }
    Ok(prev)
}

#[defun]
pub(crate) fn assq<'ob>(key: Object<'ob>, alist: List<'ob>) -> Object<'ob> {
    match alist {
        List::Nil => Object::Nil,
        List::Cons(cons) => cons
            .into_iter()
            .find(|x| match x {
                Ok(Object::Cons(cons)) => data::eq(key, cons.car()),
                _ => false,
            })
            .transpose()
            .expect("should never return error value")
            .unwrap_or_default(),
    }
}

fn next_mut(obj: Object) -> Result<Option<&mut Cons>> {
    match obj {
        Object::Nil => Ok(None),
        x => Ok(Some(x.try_into()?)),
    }
}

#[defun]
pub(crate) fn delq<'ob>(elt: Object<'ob>, list: Object<'ob>) -> Result<Object<'ob>> {
    let mut head = list;
    let mut iter = next_mut(head)?;
    let mut prev: Option<&mut Cons> = None;
    while let Some(tail) = iter {
        let next = tail.cdr();
        if data::eq(tail.car(), elt) {
            if let Some(ref mut prev) = prev {
                prev.set_cdr(tail.cdr());
            } else {
                head = tail.cdr();
            }
        } else {
            prev = Some(tail);
        }
        iter = next_mut(next)?;
    }
    Ok(head)
}

#[defun]
pub(crate) fn memq<'ob>(elt: Object<'ob>, list: List<'ob>) -> Result<List<'ob>> {
    let val = list.into_iter().by_cons().find(|x| match x {
        Ok(obj) => data::eq(obj.car(), elt),
        Err(_) => true,
    });
    match val {
        Some(elem) => elem.map(List::Cons),
        None => Ok(List::Nil),
    }
}

// eval.c
#[defun]
pub(crate) fn apply<'ob>(
    function: Function<'ob>,
    arguments: &[Object<'ob>],
    env: &mut Environment<'ob>,
    arena: &'ob Arena,
) -> Result<Object<'ob>> {
    let args = match arguments.len() {
        0 => Vec::new(),
        len => {
            let end = len - 1;
            let last = arguments[end];
            let mut args = arguments[..end].to_vec();
            let list: List = last.try_into()?;
            for element in list {
                args.push(element?);
            }
            args
        }
    };
    function.call(args, env, arena)
}

// eval.c
#[defun]
pub(crate) fn funcall<'ob>(
    function: Function<'ob>,
    arguments: &[Object<'ob>],
    env: &mut Environment<'ob>,
    arena: &'ob Arena,
) -> Result<Object<'ob>> {
    function.call(arguments.to_vec(), env, arena)
}

#[defun]
pub(crate) fn defvaralias(
    new_alias: Symbol,
    _base_variable: Symbol,
    _docstring: Option<&str>,
) -> Symbol {
    // TODO: implement
    new_alias
}

#[defun]
pub(crate) fn featurep(_feature: Symbol, _subfeature: Option<Symbol>) -> bool {
    // TODO: implement
    false
}

#[defun]
fn require<'ob>(
    feature: Symbol,
    _filename: Option<&str>,
    _noerror: Option<Object<'ob>>,
    env: &mut Environment<'ob>,
    arena: &'ob Arena,
) -> Result<Symbol> {
    if feature.name() == "macroexp" {
        crate::lread::load("/home/foco/remac/lisp/macroexp.el", arena, env)?;
        Ok(feature)
    } else {
        bail!("require is only implemented for macroexp");
    }
}

#[defun]
pub(crate) fn concat(sequences: &[Object]) -> Result<String> {
    let mut concat = String::new();
    for elt in sequences {
        match elt {
            Object::String(string) => concat.push_str(string),
            _ => bail!("Currently only concatenating strings are supported"),
        }
    }
    Ok(concat)
}

#[defun]
pub(crate) fn length(sequence: Object) -> Result<i64> {
    use crate::error::{Error, Type};
    let size = match sequence {
        Object::Cons(x) => x.into_iter().len(),
        Object::Vec(x) => x.len(),
        Object::String(x) => x.len(),
        obj => bail!(Error::from_object(Type::Sequence, obj)),
    };
    Ok(size
        .try_into()
        .expect("conversion from usize to isize should never fail"))
}

#[defun]
pub(crate) fn nth(n: usize, list: List) -> Result<Object> {
    list.into_iter().nth(n).unwrap_or(Ok(Object::Nil))
}

#[defun]
pub(crate) fn make_hash_table<'ob>(_keyword_args: &[Object<'ob>]) -> Object<'ob> {
    // TODO: Implement
    Object::Nil
}

#[defun]
pub(crate) fn puthash<'ob>(
    _key: Object<'ob>,
    value: Object<'ob>,
    _table: Object<'ob>,
) -> Object<'ob> {
    // TODO: Implement
    value
}

#[cfg(test)]
mod test {
    use crate::object::IntoObject;

    use super::*;

    #[test]
    fn test_delq() {
        let arena = &Arena::new();
        let list = list![1, 2, 3, 1, 4, 1; arena];
        let res = delq(1.into(), list).unwrap();
        assert_eq!(res, list![2, 3, 4; arena]);

        let list = list![true, true, true; arena];
        let res = delq(Object::True, list).unwrap();
        assert_eq!(res, Object::Nil);
    }

    #[test]
    fn test_nreverse() {
        let arena = &Arena::new();
        let list = list![1, 2, 3, 4; arena];
        let res = nreverse(list).unwrap().into_obj(arena);
        assert_eq!(res, list![4, 3, 2, 1; arena]);

        let list = list![1; arena];
        let res = nreverse(list).unwrap().into_obj(arena);
        assert_eq!(res, list![1; arena]);
    }

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

defsubr!(
    mapcar,
    nreverse,
    assq,
    make_hash_table,
    puthash,
    length,
    nth,
    concat,
    delq,
    memq,
    apply,
    funcall,
    defvaralias,
    featurep,
    require,
);
