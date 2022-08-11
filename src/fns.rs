use std::cell::RefCell;

use crate::core::{
    cons::Cons,
    env::{Env, Symbol},
    error::{Type, TypeError},
    gc::{Context, Root, Rt},
    object::{nil, Function, Gc, GcObj, HashTable, List, Object},
};
use crate::{data, root, rooted_iter};
use anyhow::{bail, ensure, Result};
use fn_macros::defun;
use streaming_iterator::StreamingIterator;

pub(crate) fn slice_into_list<'ob>(
    slice: &[GcObj<'ob>],
    tail: Option<GcObj<'ob>>,
    cx: &'ob Context,
) -> GcObj<'ob> {
    let from_end = slice.iter().rev();
    from_end.fold(tail.into(), |acc, obj| cons!(*obj, acc; cx))
}

#[defun]
pub(crate) fn prin1_to_string(object: GcObj, _noescape: Option<GcObj>) -> String {
    format!("{object}")
}

#[defun]
pub(crate) fn mapcar<'ob>(
    function: &Rt<Gc<Function>>,
    sequence: &Rt<Gc<List>>,
    env: &mut Root<Env>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>> {
    match sequence.bind(cx).get() {
        List::Nil => Ok(nil()),
        List::Cons(cons) => {
            root!(outputs, Vec::new(), cx);
            root!(call_arg, Vec::new(), cx);
            rooted_iter!(iter, cons, cx);
            while let Some(x) = iter.next() {
                let obj = x.bind(cx);
                call_arg.as_mut(cx).push(obj);
                let output = rebind!(function.call(call_arg, env, cx, None)?, cx);
                outputs.as_mut(cx).push(output);
                call_arg.as_mut(cx).clear();
            }
            // TODO: remove this intermediate vector
            let slice = outputs.as_slice().as_ref(cx);
            Ok(slice_into_list(slice, None, cx))
        }
    }
}

#[defun]
pub(crate) fn mapc<'ob>(
    function: &Rt<Gc<Function>>,
    sequence: &Rt<Gc<List>>,
    env: &mut Root<Env>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>> {
    match sequence.bind(cx).get() {
        List::Nil => Ok(nil()),
        List::Cons(cons) => {
            root!(call_arg, Vec::new(), cx);
            rooted_iter!(elements, cons, cx);
            while let Some(elem) = elements.next() {
                call_arg.as_mut(cx).push(elem);
                function.call(call_arg, env, cx, None)?;
                call_arg.as_mut(cx).clear();
            }
            Ok(sequence.bind(cx).into())
        }
    }
}

#[defun]
pub(crate) fn nreverse(seq: Gc<List>) -> Result<GcObj> {
    let mut prev = nil();
    for tail in seq.conses() {
        let tail = tail?;
        tail.set_cdr(prev)?;
        prev = tail.into();
    }
    Ok(prev)
}

#[defun]
pub(crate) fn reverse<'ob>(seq: Gc<List>, cx: &'ob Context) -> Result<GcObj<'ob>> {
    let mut tail = nil();
    for elem in seq.elements() {
        tail = cons!(elem?, tail; cx);
    }
    Ok(tail)
}

#[defun]
fn nconc<'ob>(lists: &[Gc<List<'ob>>]) -> Result<GcObj<'ob>> {
    let mut tail: Option<&Cons> = None;
    for list in lists {
        if let Some(cons) = tail {
            cons.set_cdr(list.into())?;
        }
        if let Some(x) = list.conses().last() {
            tail = Some(x?);
        };
    }

    Ok(match lists.iter().find(|&&x| x != List::empty()) {
        Some(x) => x.into(),
        None => nil(),
    })
}

fn join<'ob>(list: &mut Vec<GcObj<'ob>>, seq: Gc<List<'ob>>) -> Result<()> {
    if let List::Cons(cons) = seq.get() {
        for elt in cons.elements() {
            list.push(elt?);
        }
    }
    Ok(())
}

#[defun]
pub(crate) fn append<'ob>(
    append: GcObj<'ob>,
    sequences: &[GcObj<'ob>],
    cx: &'ob Context,
) -> Result<GcObj<'ob>> {
    let mut list = Vec::new();
    join(&mut list, append.try_into()?)?;
    for seq in sequences {
        join(&mut list, (*seq).try_into()?)?;
    }
    Ok(slice_into_list(&list, None, cx))
}

#[defun]
pub(crate) fn assq<'ob>(key: GcObj<'ob>, alist: Gc<List<'ob>>) -> GcObj<'ob> {
    alist_get(key, alist, data::eq)
}

#[defun]
pub(crate) fn assoc<'ob>(
    key: GcObj<'ob>,
    alist: Gc<List<'ob>>,
    testfn: Option<GcObj>,
) -> Result<GcObj<'ob>> {
    ensure!(
        testfn.is_none(),
        "test functions for assoc not yet supported"
    );
    Ok(alist_get(key, alist, data::equal))
}

fn alist_get<'ob>(key: GcObj<'ob>, alist: Gc<List<'ob>>, testfn: EqFunc) -> GcObj<'ob> {
    match alist.get() {
        List::Nil => nil(),
        List::Cons(cons) => cons
            .elements()
            .find(|x| match x {
                Ok(elem) => match elem.get() {
                    Object::Cons(cons) => testfn(key, cons.car()),
                    _ => false,
                },
                _ => false,
            })
            .transpose()
            .expect("should never return error value")
            .unwrap_or_default(),
    }
}

#[defun]
fn copy_alist<'ob>(alist: Gc<List<'ob>>, cx: &'ob Context) -> Result<GcObj<'ob>> {
    match alist.get() {
        List::Nil => Ok(nil()),
        List::Cons(cons) => {
            let first = copy_alist_elem(cons.car(), cx);
            let head = cons!(first; cx);
            let mut tail = head.as_cons();

            for elem in cons.cdr().as_list()? {
                let copy = copy_alist_elem(elem?, cx);
                tail.set_cdr(cons!(copy; cx)).unwrap();
                tail = tail.cdr().as_cons();
            }
            Ok(head)
        }
    }
}

fn copy_alist_elem<'ob>(elem: GcObj<'ob>, cx: &'ob Context) -> GcObj<'ob> {
    match elem.get() {
        Object::Cons(cons) => cons!(cons.car(), cons.cdr(); cx),
        _ => elem,
    }
}

type EqFunc = for<'ob> fn(GcObj<'ob>, GcObj<'ob>) -> bool;

fn delete_from_list<'ob>(
    elt: GcObj<'ob>,
    list: Gc<List<'ob>>,
    eq_fn: EqFunc,
) -> Result<GcObj<'ob>> {
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
pub(crate) fn delete<'ob>(elt: GcObj<'ob>, list: Gc<List<'ob>>) -> Result<GcObj<'ob>> {
    delete_from_list(elt, list, data::equal)
}

#[defun]
pub(crate) fn delq<'ob>(elt: GcObj<'ob>, list: Gc<List<'ob>>) -> Result<GcObj<'ob>> {
    delete_from_list(elt, list, data::eq)
}

fn member_of_list<'ob>(elt: GcObj<'ob>, list: Gc<List<'ob>>, eq_fn: EqFunc) -> Result<GcObj<'ob>> {
    let val = list.conses().find(|x| match x {
        Ok(obj) => eq_fn(obj.car(), elt),
        Err(_) => true,
    });
    match val {
        Some(Ok(elem)) => Ok(elem.into()),
        Some(Err(e)) => Err(e),
        None => Ok(nil()),
    }
}

#[defun]
pub(crate) fn memq<'ob>(elt: GcObj<'ob>, list: Gc<List<'ob>>) -> Result<GcObj<'ob>> {
    member_of_list(elt, list, data::eq)
}

#[defun]
pub(crate) fn member<'ob>(elt: GcObj<'ob>, list: Gc<List<'ob>>) -> Result<GcObj<'ob>> {
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
    filename: Option<&Rt<Gc<&String>>>,
    noerror: Option<()>,
    env: &mut Root<Env>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>> {
    if crate::data::FEATURES.lock().unwrap().contains(feature) {
        return Ok(feature.into());
    }
    let file = match filename {
        None => feature.name,
        Some(file) => file.bind(cx).get(),
    };
    let file: GcObj = cx.add(file);
    root!(file, cx);
    match crate::lread::load(file, None, None, cx, env) {
        Ok(_) => Ok(feature.into()),
        Err(e) => match noerror {
            Some(()) => Ok(nil()),
            None => Err(e),
        },
    }
}

#[defun]
pub(crate) fn concat(sequences: &[GcObj]) -> Result<String> {
    let mut concat = String::new();
    for elt in sequences {
        match elt.get() {
            Object::String(string) => concat.push_str(string),
            _ => bail!("Currently only concatenating strings are supported"),
        }
    }
    Ok(concat)
}

#[defun]
pub(crate) fn length(sequence: GcObj) -> Result<i64> {
    let size = match sequence.get() {
        Object::Cons(x) => x.elements().len(),
        Object::Vec(x) => x.borrow().len(),
        Object::String(x) => x.len(),
        Object::Symbol(s) if s.nil() => 0,
        obj => bail!(TypeError::new(Type::Sequence, obj)),
    };
    Ok(size
        .try_into()
        .expect("conversion from usize to isize should never fail"))
}

#[defun]
pub(crate) fn safe_length(sequence: GcObj) -> i64 {
    let size = match sequence.get() {
        Object::Cons(x) => x.elements().len(),
        Object::Vec(x) => x.borrow().len(),
        Object::String(x) => x.len(),
        _ => 0,
    };
    size.try_into()
        .expect("conversion from usize to isize should never fail")
}

#[defun]
pub(crate) fn nth(n: usize, list: Gc<List>) -> Result<GcObj> {
    list.elements().nth(n).unwrap_or_else(|| Ok(nil()))
}

#[defun]
pub(crate) fn nthcdr(n: usize, list: Gc<List>) -> Result<Gc<List>> {
    match list.conses().nth(n) {
        Some(x) => x.map(Into::into),
        None => Ok(List::empty()),
    }
}

#[defun]
pub(crate) fn make_hash_table<'ob>(
    keyword_args: &[GcObj<'ob>],
    cx: &'ob Context,
) -> Result<GcObj<'ob>> {
    use crate::core::env::sym;
    if let Some(i) = keyword_args
        .iter()
        .step_by(2)
        .position(|&x| x == sym::KW_TEST)
    {
        let val = match keyword_args.get((i * 2) + 1) {
            Some(x) => *x,
            None => bail!("Missing keyword value for :test"),
        };
        if val != *sym::EQ && val != *sym::EQUAL {
            // TODO: we are currently only using `equal', but eq should be okay
            bail!("only `eq' and `equal' keywords support for make-hash-table :test. Found {val}");
        }
    }
    // TODO, the rest of the keywords need to be supported here
    let map: HashTable = HashTable::with_hasher(std::hash::BuildHasherDefault::default());
    Ok(cx.add(map))
}

#[defun]
pub(crate) fn hash_table_p(obj: GcObj) -> bool {
    matches!(obj.get(), Object::HashTable(_))
}

#[defun]
pub(crate) fn puthash<'ob>(
    key: GcObj<'ob>,
    value: GcObj<'ob>,
    table: &RefCell<HashTable<'ob>>,
) -> GcObj<'ob> {
    table.borrow_mut().insert(key, value);
    value
}

#[defun]
pub(crate) fn gethash<'ob>(
    key: GcObj<'ob>,
    table: &RefCell<HashTable<'ob>>,
    dflt: Option<GcObj<'ob>>,
) -> Option<GcObj<'ob>> {
    match table.borrow().get(&key) {
        Some(x) => Some(*x),
        None => dflt,
    }
}

#[defun]
fn copy_sequence<'ob>(arg: GcObj<'ob>, cx: &'ob Context) -> Result<GcObj<'ob>> {
    match arg.get() {
        Object::Symbol(s) if s.nil() => Ok(arg.clone_in(cx)),
        Object::String(_) | Object::Vec(_) | Object::Cons(_) => Ok(arg.clone_in(cx)),
        _ => Err(TypeError::new(Type::Sequence, arg).into()),
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

#[defun]
fn enable_debug() -> bool {
    crate::debug::enable_debug();
    false
}

#[cfg(test)]
mod test {
    use crate::core::{gc::RootSet, object::qtrue};

    use super::*;

    #[test]
    fn test_delq() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        {
            let list = list![1, 2, 3, 1, 4, 1; cx];
            let res = delq(1.into(), list.try_into().unwrap()).unwrap();
            assert_eq!(res, list![2, 3, 4; cx]);
        }
        {
            let list = list![true, true, true; cx];
            let res = delq(qtrue(), list.try_into().unwrap()).unwrap();
            assert_eq!(res, nil());
        }
    }

    #[test]
    fn test_nthcdr() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        let list = list![1, 2, 3; cx];
        let res = nthcdr(1, list.try_into().unwrap()).unwrap();
        assert_eq!(res.get().car(), 2);
    }

    #[test]
    fn test_reverse() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        {
            let list = list![1, 2, 3, 4; cx];
            let res = nreverse(list.try_into().unwrap()).unwrap();
            assert_eq!(res, list![4, 3, 2, 1; cx]);
        }
        {
            let list = list![1; cx];
            let res = nreverse(list.try_into().unwrap()).unwrap();
            assert_eq!(res, list![1; cx]);
        }
        {
            let list = list![1, 2, 3; cx];
            let res = reverse(list.try_into().unwrap(), cx).unwrap();
            assert_eq!(res, list![3, 2, 1; cx]);
        }
    }

    #[test]
    fn test_nconc() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        {
            let res = nconc(&[List::empty()]).unwrap();
            assert!(res == nil());
        }
        {
            let list: Gc<List> = list![1, 2; cx].try_into().unwrap();
            let res = nconc(&[list]).unwrap();
            assert_eq!(res, list![1, 2; cx]);
        }
        {
            let list1: Gc<List> = list![1, 2; cx].try_into().unwrap();
            let list2: Gc<List> = list![3, 4; cx].try_into().unwrap();
            let res = nconc(&[list1, list2]).unwrap();
            assert_eq!(res, list![1, 2, 3, 4; cx]);
        }
        {
            let list1: Gc<List> = list![1, 2; cx].try_into().unwrap();
            let list2: Gc<List> = list![3, 4; cx].try_into().unwrap();
            let list3: Gc<List> = list![5, 6; cx].try_into().unwrap();
            let res = nconc(&[list1, list2, list3]).unwrap();
            assert_eq!(res, list![1, 2, 3, 4, 5, 6; cx]);
        }
        {
            let list1: Gc<List> = nil().try_into().unwrap();
            let list2: Gc<List> = list![1, 2; cx].try_into().unwrap();
            let res = nconc(&[list1, list2]).unwrap();
            assert_eq!(res, list![1, 2; cx]);
        }
        {
            let list1: Gc<List> = list![1, 2; cx].try_into().unwrap();
            let list2: Gc<List> = nil().try_into().unwrap();
            let res = nconc(&[list1, list2]).unwrap();
            assert_eq!(res, list![1, 2; cx]);
        }
    }

    #[test]
    fn test_mapcar() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        let element = cons!(5, 6; cx);
        let list = list![cons!(1, 2; cx), cons!(3, 4; cx), element; cx];
        let list = list.try_into().unwrap();
        let result = assq(5.into(), list);
        assert_eq!(result, element);
    }

    #[test]
    fn test_copy_alist() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        let alist = list![cons!(1, 2; cx), cons!(3, 4; cx), cons!(5, 6; cx); cx];
        let list = alist.try_into().unwrap();
        let result = copy_alist(list, cx).unwrap();
        assert_eq!(alist, result);
    }
}

define_symbols!(
    FUNCS => {
        mapcar,
        mapc,
        reverse,
        nreverse,
        nconc,
        assq,
        assoc,
        copy_alist,
        make_hash_table,
        hash_table_p,
        puthash,
        gethash,
        length,
        safe_length,
        nth,
        nthcdr,
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
        enable_debug,
    }
    SYMS => {
        KW_TEST,
    }
);
