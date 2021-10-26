use crate::arena::Arena;
use crate::bytecode;
use crate::cons::Cons;
use crate::data;
use crate::data::Environment;
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
        Object::NIL
    } else {
        let from_end = slice.iter().rev();
        from_end.fold(tail.into(), |acc, obj| cons!(*obj, acc; arena))
    }
}

#[defun]
pub(crate) fn prin1_to_string(object: Object, _noescape: Option<Object>) -> String {
    format!("{}", object)
}

impl<'ob> Function<'ob> {
    pub(crate) fn call(
        self,
        args: Vec<Object<'ob>>,
        env: &mut Environment<'ob>,
        arena: &'ob Arena,
    ) -> Result<Object<'ob>> {
        match self {
            Function::LispFn(f) => bytecode::call_lisp(&f, args, env, arena),
            Function::SubrFn(f) => bytecode::call_subr(*f, args, env, arena),
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
        List::Nil => Ok(Object::NIL),
        List::Cons(cons) => {
            let vec = cons
                .into_iter()
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
pub(crate) fn mapc<'ob>(
    function: Function<'ob>,
    sequence: List<'ob>,
    env: &mut Environment<'ob>,
    arena: &'ob Arena,
) -> Result<Object<'ob>> {
    match sequence {
        List::Nil => Ok(Object::NIL),
        List::Cons(cons) => {
            for elem in !cons {
                function.call(vec![elem?], env, arena)?;
            }
            Ok(sequence.into())
        }
    }
}

#[defun]
pub(crate) fn nreverse(seq: List) -> Result<Object> {
    let mut prev = Object::NIL;
    for tail in seq.conses_mut() {
        let tail = tail?;
        tail.set_cdr(prev);
        prev = tail.into();
    }
    Ok(prev)
}

#[defun]
pub(crate) fn assq<'ob>(key: Object<'ob>, alist: List<'ob>) -> Object<'ob> {
    match alist {
        List::Nil => Object::NIL,
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

#[defun]
pub(crate) fn delq<'ob>(elt: Object<'ob>, list: List<'ob>) -> Result<Object<'ob>> {
    let mut head = list.into();
    let mut prev: Option<&mut Cons> = None;
    for tail in list.conses_mut() {
        let tail = tail?;
        if data::eq(tail.car(), elt) {
            if let Some(prev) = &mut prev {
                prev.set_cdr(tail.cdr());
            } else {
                head = tail.cdr();
            }
        } else {
            prev = Some(tail);
        }
    }
    Ok(head)
}

fn member_of_list<'ob>(
    elt: Object<'ob>,
    list: List<'ob>,
    eq_fn: fn(Object<'ob>, Object<'ob>) -> bool,
) -> Result<Object<'ob>> {
    let val = list.conses().find(|x| match x {
        Ok(obj) => eq_fn(obj.car(), elt),
        Err(_) => true,
    });
    match val {
        Some(Ok(elem)) => Ok(Object::Cons(elem)),
        Some(Err(e)) => Err(e),
        None => Ok(Object::NIL),
    }
}

#[defun]
pub(crate) fn memq<'ob>(elt: Object<'ob>, list: List<'ob>) -> Result<Object<'ob>> {
    member_of_list(elt, list, data::eq)
}

#[defun]
pub(crate) fn member<'ob>(elt: Object<'ob>, list: List<'ob>) -> Result<Object<'ob>> {
    member_of_list(elt, list, data::equal)
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
    if feature.name == "macroexp" {
        crate::lread::load("lisp/macroexp.el", arena, env)?;
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
    list.elements().nth(n).unwrap_or(Ok(Object::NIL))
}

#[defun]
pub(crate) fn make_hash_table<'ob>(_keyword_args: &[Object<'ob>]) -> Object<'ob> {
    // TODO: Implement
    Object::NIL
}

#[defun]
pub(crate) fn hash_table_p<'ob>(_obj: Object) -> Object<'ob> {
    // TODO: Implement
    Object::NIL
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
        let res = delq(1.into(), list.try_into().unwrap()).unwrap();
        assert_eq!(res, list![2, 3, 4; arena]);

        let list = list![true, true, true; arena];
        let res = delq(Object::TRUE, list.try_into().unwrap()).unwrap();
        assert_eq!(res, Object::NIL);
    }

    #[test]
    fn test_nreverse() {
        let arena = &Arena::new();
        let list = list![1, 2, 3, 4; arena];
        let res = nreverse(list.try_into().unwrap()).unwrap().into_obj(arena);
        assert_eq!(res, list![4, 3, 2, 1; arena]);

        let list = list![1; arena];
        let res = nreverse(list.try_into().unwrap()).unwrap().into_obj(arena);
        assert_eq!(res, list![1; arena]);
    }

    #[test]
    fn test_mapcar() {
        let arena = &Arena::new();
        let element = cons!(5, 6; arena);
        let list = list![cons!(1, 2; arena), cons!(3, 4; arena), element; arena];
        if let Object::Cons(cons) = list {
            let result = assq(5.into(), List::Cons(cons));
            assert_eq!(result, element);
        }
    }
}

defsubr!(
    mapcar,
    mapc,
    nreverse,
    assq,
    make_hash_table,
    hash_table_p,
    puthash,
    length,
    nth,
    concat,
    delq,
    memq,
    member,
    defvaralias,
    featurep,
    require,
    prin1_to_string,
);
