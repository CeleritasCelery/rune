use crate::arena::RootOwner;
use crate::arena::{Arena, Root, RootObj};
use crate::cons::Cons;
use crate::data::Environment;
use crate::error::{Error, Type};
use crate::object::{CallableX, FunctionX, Gc, ListX, Object, ObjectX};
use crate::symbol::Symbol;
use crate::{data, element_iter, rebind, root, root_struct};
use anyhow::anyhow;
use anyhow::{bail, Result};
use fn_macros::defun;
use streaming_iterator::StreamingIterator;

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
    format!("{object}")
}

impl<'ob> Gc<FunctionX<'ob>> {
    pub(crate) fn call<'gc, 'id>(
        self,
        args: &Root<'id, Vec<RootObj>>,
        env: &Root<'id, Environment>,
        arena: &'gc mut Arena,
        owner: &mut RootOwner<'id>,
    ) -> Result<Object<'gc>> {
        use crate::interpreter;
        match self.get() {
            FunctionX::LispFn(_) => todo!("call lisp functions"),
            FunctionX::SubrFn(f) => (*f).call(args, env, arena, owner),
            FunctionX::Cons(_) => interpreter::call(self.into(), args, env, arena, owner),
            FunctionX::Symbol(s) => {
                if let Some(resolved) = s.resolve_callable(arena) {
                    let tmp: Object = resolved.into();
                    root!(tmp, arena); // root callable
                    let callable: Gc<CallableX> = tmp.try_into().unwrap();
                    match callable.get() {
                        CallableX::LispFn(_) => todo!("call lisp functions"),
                        CallableX::SubrFn(f) => (*f).call(args, env, arena, owner),
                        CallableX::Cons(cons) => {
                            match cons.try_as_macro(arena) {
                                Ok(_) => Err(anyhow!("Macro's are invalid as functions")),
                                Err(_) => {
                                    let tmp: Object = callable.into();
                                    root!(tmp, arena); // root callable
                                    if let ObjectX::Cons(_) = tmp.get() {
                                        interpreter::call(tmp, args, env, arena, owner)
                                    } else {
                                        unreachable!();
                                    }
                                }
                            }
                        }
                    }
                } else {
                    Err(anyhow!("Void FunctionX: {}", s))
                }
            }
        }
    }
}

#[defun]
pub(crate) fn mapcar<'ob, 'id>(
    function: Gc<FunctionX<'ob>>,
    sequence: Gc<ListX<'ob>>,
    env: &Root<'id, Environment>,
    owner: &mut RootOwner<'id>,
    gc: &'ob mut Arena,
) -> Result<Object<'ob>> {
    match sequence.get() {
        ListX::Nil => Ok(Object::NIL),
        ListX::Cons(cons) => {
            root_struct!(outputs, Vec::new(), gc);
            root_struct!(call_arg, Vec::new(), gc);
            element_iter!(iter, cons, gc);
            while let Some(x) = iter.next() {
                let obj = x.obj();
                call_arg.borrow_mut(owner, gc).push(obj);
                let output = function.call(call_arg, env, gc, owner)?;
                rebind!(output, gc);
                outputs.borrow_mut(owner, gc).push(output);
                call_arg.borrow_mut(owner, gc).clear();
            }
            // TODO: remove this intermediate vector
            let slice = outputs.borrow(owner).as_gc().as_ref();
            Ok(slice_into_list(slice, None, gc))
        }
    }
}

#[defun]
pub(crate) fn mapc<'ob, 'id>(
    function: Gc<FunctionX<'ob>>,
    sequence: Gc<ListX<'ob>>,
    env: &Root<'id, Environment>,
    owner: &mut RootOwner<'id>,
    gc: &'ob mut Arena,
) -> Result<Object<'ob>> {
    match sequence.get() {
        ListX::Nil => Ok(Object::NIL),
        ListX::Cons(cons) => {
            root_struct!(call_arg, Vec::new(), gc);
            element_iter!(elements, cons, gc);
            while let Some(elem) = elements.next() {
                call_arg.borrow_mut(owner, gc).push(elem.obj());
                function.call(call_arg, env, gc, owner)?;
                call_arg.borrow_mut(owner, gc).clear();
            }
            Ok(sequence.into())
        }
    }
}

#[defun]
pub(crate) fn nreverse<'ob>(seq: Gc<ListX<'ob>>, arena: &'ob Arena) -> Result<Object<'ob>> {
    let mut prev = Object::NIL;
    for tail in seq.conses(arena) {
        let tail = tail?;
        tail.set_cdr(prev);
        prev = tail.into();
    }
    Ok(prev)
}

fn join<'ob>(list: &mut Vec<Object<'ob>>, seq: Gc<ListX<'ob>>, arena: &'ob Arena) -> Result<()> {
    match seq.get() {
        ListX::Cons(cons) => {
            for elt in cons.elements(arena) {
                list.push(elt?);
            }
        }
        ListX::Nil => {}
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
    join(&mut list, append.try_into()?, arena)?;
    for seq in sequences {
        join(&mut list, (*seq).try_into()?, arena)?;
    }
    Ok(slice_into_list(&list, None, arena))
}

#[defun]
pub(crate) fn assq<'ob>(key: Object<'ob>, alist: Gc<ListX<'ob>>, arena: &'ob Arena) -> Object<'ob> {
    match alist.get() {
        ListX::Nil => Object::NIL,
        ListX::Cons(cons) => cons
            .elements(arena)
            .find(|x| match x {
                Ok(elem) => match elem.get() {
                    ObjectX::Cons(cons) => data::eq(key, cons.car(arena)),
                    _ => false,
                },
                _ => false,
            })
            .transpose()
            .expect("should never return error value")
            .unwrap_or_default(),
    }
}

type EqFunc = for<'ob> fn(Object<'ob>, Object<'ob>) -> bool;

fn delete_from_list<'ob>(
    elt: Object<'ob>,
    list: Gc<ListX<'ob>>,
    eq_fn: EqFunc,
    arena: &'ob Arena,
) -> Result<Object<'ob>> {
    let mut head = list.into();
    let mut prev: Option<&'ob Cons> = None;
    for tail in list.conses(arena) {
        let tail = tail?;
        if eq_fn(tail.car(arena), elt) {
            if let Some(prev_tail) = &mut prev {
                prev_tail.set_cdr(tail.cdr(arena));
            } else {
                head = tail.cdr(arena);
            }
        } else {
            prev = Some(tail);
        }
    }
    Ok(head)
}

#[defun]
pub(crate) fn delete<'ob>(
    elt: Object<'ob>,
    list: Gc<ListX<'ob>>,
    arena: &'ob Arena,
) -> Result<Object<'ob>> {
    delete_from_list(elt, list, data::equal, arena)
}

#[defun]
pub(crate) fn delq<'ob>(
    elt: Object<'ob>,
    list: Gc<ListX<'ob>>,
    arena: &'ob Arena,
) -> Result<Object<'ob>> {
    delete_from_list(elt, list, data::eq, arena)
}

fn member_of_list<'ob>(
    elt: Object<'ob>,
    list: Gc<ListX<'ob>>,
    eq_fn: EqFunc,
    arena: &'ob Arena,
) -> Result<Object<'ob>> {
    let val = list.conses(arena).find(|x| match x {
        Ok(obj) => eq_fn(obj.car(arena), elt),
        Err(_) => true,
    });
    match val {
        Some(Ok(elem)) => Ok(elem.into()),
        Some(Err(e)) => Err(e),
        None => Ok(Object::NIL),
    }
}

#[defun]
pub(crate) fn memq<'ob>(
    elt: Object<'ob>,
    list: Gc<ListX<'ob>>,
    arena: &'ob Arena,
) -> Result<Object<'ob>> {
    member_of_list(elt, list, data::eq, arena)
}

#[defun]
pub(crate) fn member<'ob>(
    elt: Object<'ob>,
    list: Gc<ListX<'ob>>,
    arena: &'ob Arena,
) -> Result<Object<'ob>> {
    member_of_list(elt, list, data::equal, arena)
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
fn require<'ob, 'id>(
    feature: Symbol,
    filename: Option<&str>,
    noerror: Option<bool>,
    env: &Root<'id, Environment>,
    owner: &mut RootOwner<'id>,
    arena: &'ob mut Arena,
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
    match crate::lread::load(&file, None, None, None, None, arena, env, owner) {
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
        match elt.get() {
            ObjectX::String(string) => concat.push_str(string),
            _ => bail!("Currently only concatenating strings are supported"),
        }
    }
    Ok(concat)
}

#[defun]
pub(crate) fn length<'ob>(sequence: Object<'ob>, arena: &'ob Arena) -> Result<i64> {
    let size = match sequence.get() {
        ObjectX::Cons(x) => x.elements(arena).len(),
        ObjectX::Vec(x) => x.borrow().len(),
        ObjectX::String(x) => x.len(),
        obj => bail!(Error::from_object(Type::Sequence, obj)),
    };
    Ok(size
        .try_into()
        .expect("conversion from usize to isize should never fail"))
}

#[defun]
pub(crate) fn nth<'ob>(n: usize, list: Gc<ListX<'ob>>, arena: &'ob Arena) -> Result<Object<'ob>> {
    list.elements(arena).nth(n).unwrap_or(Ok(Object::NIL))
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
    match arg.get() {
        ObjectX::String(_) | ObjectX::Vec(_) | ObjectX::Nil | ObjectX::Cons(_) => {
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
    use crate::{arena::RootSet, object::IntoObject};

    use super::*;

    #[test]
    fn test_delq() {
        let roots = &RootSet::default();
        let arena = &Arena::new(roots);
        {
            let list: Object = list![1, 2, 3, 1, 4, 1; arena];
            let res = delq(1.into(), list.try_into().unwrap(), arena).unwrap();
            assert_eq!(res, list![2, 3, 4; arena]);
        }
        {
            let list: Object = list![true, true, true; arena];
            let res = delq(Object::TRUE, list.try_into().unwrap(), arena).unwrap();
            assert_eq!(res, Object::NIL);
        }
    }

    #[test]
    fn test_nreverse() {
        let roots = &RootSet::default();
        let arena = &Arena::new(roots);
        {
            let list: Object = list![1, 2, 3, 4; arena];
            let res: Object = nreverse(list.try_into().unwrap(), arena)
                .unwrap()
                .into_obj(arena);
            assert_eq!(res, list![4, 3, 2, 1; arena]);
        }
        {
            let list: Object = list![1; arena];
            let res: Object = nreverse(list.try_into().unwrap(), arena)
                .unwrap()
                .into_obj(arena);
            assert_eq!(res, list![1; arena]);
        }
    }

    #[test]
    fn test_mapcar() {
        let roots = &RootSet::default();
        let arena = &Arena::new(roots);
        let element = cons!(5, 6; arena);
        let list: Object = list![cons!(1, 2; arena), cons!(3, 4; arena), element; arena];
        let list = list.try_into().unwrap();
        let result = assq(5.into(), list, arena);
        assert_eq!(result, element);
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
