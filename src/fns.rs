use crate::arena::Arena;
use crate::cons::Cons;
use crate::data;
use crate::data::Environment;
use crate::error::{Error, Type};
use crate::object::{Function, List, Object};
use crate::symbol::Symbol;
use anyhow::{bail, Result};
use fn_macros::defun;

pub(crate) fn slice_into_list<'ob>(
    slice: &[Object<'ob>],
    tail: Option<Object<'ob>>,
    arena: &'ob Arena,
) -> Object<'ob> {
    let from_end = slice.iter().rev();
    from_end.fold(tail.into(), |acc, obj| cons!(*obj, acc; arena))
}

#[defun]
pub(crate) fn prin1_to_string(object: Object, _noescape: Option<Object>) -> String {
    format!("{}", object)
}

impl<'ob> Function<'ob> {
    pub(crate) fn call(
        self,
        args: Vec<Object<'ob>>,
        env: &mut Environment,
        arena: &'ob Arena,
    ) -> Result<Object<'ob>> {
        match self {
            Function::LispFn(_) => todo!("call lisp functions"),
            Function::SubrFn(f) => (*f).call(args, env, arena),
            Function::Uncompiled(f) => crate::interpreter::call(Object::Cons(f), args, env, arena),
        }
    }
}

#[defun]
pub(crate) fn mapcar<'ob>(
    function: Function<'ob>,
    sequence: List<'ob>,
    env: &mut Environment,
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
    env: &mut Environment,
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
    for tail in seq.conses() {
        let tail = tail?;
        tail.set_cdr(prev)?;
        prev = Object::Cons(tail.into());
    }
    Ok(prev)
}

fn join<'ob>(list: &mut Vec<Object<'ob>>, seq: List<'ob>) -> Result<()> {
    match seq {
        List::Cons(cons) => {
            for elt in !cons {
                list.push(elt?);
            }
        }
        List::Nil => {}
    }
    Ok(())
}

#[defun]
pub(crate) fn append<'ob>(
    append: Object<'ob>,
    sequences: &[Object<'ob>],
    arena: &'ob Arena,
) -> Result<Object<'ob>> {
    let mut list = Vec::new();
    join(&mut list, append.try_into()?)?;
    for seq in sequences {
        join(&mut list, (*seq).try_into()?)?;
    }
    Ok(slice_into_list(&list, None, arena))
}

#[defun]
pub(crate) fn assq<'ob>(key: Object<'ob>, alist: List<'ob>) -> Object<'ob> {
    match alist {
        List::Nil => Object::NIL,
        List::Cons(cons) => cons
            .into_iter()
            .find(|x| match x {
                Ok(Object::Cons(elem)) => data::eq(key, elem.car()),
                _ => false,
            })
            .transpose()
            .expect("should never return error value")
            .unwrap_or_default(),
    }
}

type EqFunc = for<'ob> fn(Object<'ob>, Object<'ob>) -> bool;

fn delete_from_list<'ob>(elt: Object<'ob>, list: List<'ob>, eq_fn: EqFunc) -> Result<Object<'ob>> {
    let mut head = list.into();
    let mut prev: Option<&'ob Cons> = None;
    for tail in list.conses() {
        let tail = tail?;
        if eq_fn(tail.car(), elt) {
            if let Some(prev_tail) = &mut prev {
                prev_tail.set_cdr(tail.cdr())?;
            } else {
                head = tail.cdr();
            }
        } else {
            prev = Some(tail);
        }
    }
    Ok(head)
}

#[defun]
pub(crate) fn delete<'ob>(elt: Object<'ob>, list: List<'ob>) -> Result<Object<'ob>> {
    delete_from_list(elt, list, data::equal)
}

#[defun]
pub(crate) fn delq<'ob>(elt: Object<'ob>, list: List<'ob>) -> Result<Object<'ob>> {
    delete_from_list(elt, list, data::eq)
}

fn member_of_list<'ob>(elt: Object<'ob>, list: List<'ob>, eq_fn: EqFunc) -> Result<Object<'ob>> {
    let val = list.conses().find(|x| match x {
        Ok(obj) => eq_fn(obj.car(), elt),
        Err(_) => true,
    });
    match val {
        Some(Ok(elem)) => Ok(Object::Cons(elem.into())),
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
    filename: Option<&str>,
    noerror: Option<bool>,
    env: &mut Environment,
    arena: &'ob Arena,
) -> Result<Object<'ob>> {
    if crate::data::FEATURES.lock().unwrap().contains(feature) {
        return Ok(feature.into());
    }
    let file = match filename {
        Some(file) => file.to_owned(),
        None => {
            format!("lisp/{}.el", feature.name)
        }
    };
    match crate::lread::load(&file, None, None, None, None, arena, env) {
        Ok(_) => Ok(feature.into()),
        Err(e) => match noerror {
            Some(_) => Ok(Object::NIL),
            None => Err(e),
        },
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
    let size = match sequence {
        Object::Cons(x) => x.into_iter().len(),
        Object::Vec(x) => x.borrow().len(),
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

#[defun]
fn copy_sequence<'ob>(arg: Object<'ob>, arena: &'ob Arena) -> Result<Object<'ob>> {
    match arg {
        Object::String(_) | Object::Vec(_) | Object::Nil(_) | Object::Cons(_) => {
            Ok(arg.clone_in(arena))
        }
        _ => Err(Error::from_object(Type::Sequence, arg).into()),
    }
}

#[defun]
fn substring(string: &str, from: Option<usize>, to: Option<usize>) -> String {
    let new_string = match (from, to) {
        (None, None) => string,
        (None, Some(t)) => &string[..t],
        (Some(f), None) => &string[f..],
        (Some(f), Some(t)) => &string[f..t],
    };
    new_string.to_owned()
}

#[cfg(test)]
mod test {
    use crate::object::IntoObject;

    use super::*;

    #[test]
    fn test_delq() {
        let arena = &Arena::new();
        {
            let list = list![1, 2, 3, 1, 4, 1; arena];
            let res = delq(1.into(), list.try_into().unwrap()).unwrap();
            assert_eq!(res, list![2, 3, 4; arena]);
        }
        {
            let list = list![true, true, true; arena];
            let res = delq(Object::TRUE, list.try_into().unwrap()).unwrap();
            assert_eq!(res, Object::NIL);
        }
    }

    #[test]
    fn test_nreverse() {
        let arena = &Arena::new();
        {
            let list = list![1, 2, 3, 4; arena];
            let res = nreverse(list.try_into().unwrap()).unwrap().into_obj(arena);
            assert_eq!(res, list![4, 3, 2, 1; arena]);
        }
        {
            let list = list![1; arena];
            let res = nreverse(list.try_into().unwrap()).unwrap().into_obj(arena);
            assert_eq!(res, list![1; arena]);
        }
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
    append,
    delq,
    delete,
    memq,
    member,
    defvaralias,
    featurep,
    require,
    prin1_to_string,
    substring,
    copy_sequence,
);
