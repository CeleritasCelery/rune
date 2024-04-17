//! General purpose lisp functions
use crate::{
    core::{
        cons::Cons,
        env::{sym, Env},
        error::{Type, TypeError},
        gc::{Context, Rt, Rto},
        object::{
            Function, Gc, HashTable, IntoObject, LispHashTable, LispString, LispVec, List,
            ListType, Object, ObjectType, Symbol, WithLifetime, NIL,
        },
    },
    data::aref,
    rooted_iter,
};
use anyhow::{bail, ensure, Result};
use fallible_iterator::FallibleIterator;
use fallible_streaming_iterator::FallibleStreamingIterator;
use rune_core::macros::{call, list, rebind, root};
use rune_macros::defun;

#[defun]
fn identity(arg: Object) -> Object {
    arg
}

pub(crate) fn slice_into_list<'ob>(
    slice: &[Object<'ob>],
    tail: Option<Object<'ob>>,
    cx: &'ob Context,
) -> Object<'ob> {
    let from_end = slice.iter().rev();
    from_end.fold(tail.into(), |acc, obj| Cons::new(*obj, acc, cx).into())
}

pub(crate) fn build_list<'ob, E>(
    mut iter: impl Iterator<Item = Result<Object<'ob>, E>>,
    cx: &'ob Context,
) -> Result<Object<'ob>, E> {
    let Some(first) = iter.next() else { return Ok(NIL) };
    let head = Cons::new1(first?, cx);
    let mut prev = head;
    for elem in iter {
        let new = Cons::new1(elem?, cx);
        prev.set_cdr(new.into()).unwrap();
        prev = new;
    }
    Ok(head.into())
}

#[defun]
pub(crate) fn eq(obj1: Object, obj2: Object) -> bool {
    obj1.ptr_eq(obj2)
}

#[defun]
pub(crate) fn equal<'ob>(obj1: Object<'ob>, obj2: Object<'ob>) -> bool {
    obj1 == obj2
}

#[defun]
pub(crate) fn eql<'ob>(obj1: Object<'ob>, obj2: Object<'ob>) -> bool {
    match (obj1.untag(), obj2.untag()) {
        (ObjectType::Float(f1), ObjectType::Float(f2)) => f1.to_bits() == f2.to_bits(),
        _ => obj1.ptr_eq(obj2),
    }
}

#[defun]
fn equal_including_properties<'ob>(o1: Object<'ob>, o2: Object<'ob>) -> bool {
    // TODO: implement text properties
    equal(o1, o2)
}

#[defun]
pub fn plist_get<'ob>(plist: Object<'ob>, prop: Object<'ob>) -> Result<Object<'ob>> {
    let Ok(plist) = Gc::<ListType>::try_from(plist) else { return Ok(NIL) };
    // TODO: this function should never fail. Need to implement safe iterator
    let mut iter = plist.elements();
    while let Some(cur_prop) = iter.next() {
        let Some(value) = iter.next() else { return Ok(NIL) };
        if eq(cur_prop?, prop) {
            return Ok(value?);
        }
    }
    Ok(NIL)
}

#[defun]
fn plist_member<'ob>(
    plist: Object<'ob>,
    prop: Object<'ob>,
    predicate: Option<Object>,
) -> Result<Object<'ob>> {
    ensure!(predicate.is_none(), "plist-member predicate support not implemented");
    let plist: List = plist.try_into()?;
    for (idx, value) in plist.conses().enumerate() {
        if idx % 2 != 0 {
            continue;
        }
        let value = value?;
        if eq(value.car(), prop) {
            return Ok(value.into());
        }
    }
    Ok(NIL)
}

#[defun]
pub(crate) fn prin1_to_string(object: Object, _noescape: Option<Object>) -> String {
    format!("{object}")
}

#[defun]
fn string_to_multibyte(string: &LispString) -> &LispString {
    // TODO: Handle the unibyte case
    string
}

#[defun]
fn string_search(needle: &str, haystack: &str, start_pos: Option<usize>) -> Option<usize> {
    let start = start_pos.unwrap_or(0);
    haystack[start..].find(needle).map(|x| x + start)
}

#[defun]
pub(crate) fn mapcar<'ob>(
    function: &Rto<Function>,
    sequence: &Rto<Object>,
    env: &mut Rt<Env>,
    cx: &'ob mut Context,
) -> Result<Object<'ob>> {
    let sequence = sequence.bind(cx);
    match sequence.untag() {
        ObjectType::NIL => Ok(NIL),
        ObjectType::Cons(cons) => {
            rooted_iter!(iter, cons, cx);
            root!(outputs, new(Vec), cx);
            while let Some(obj) = iter.next()? {
                let output = call!(function, obj; env, cx)?;
                outputs.push(output);
            }
            // TODO: remove this intermediate vector
            Ok(slice_into_list(Rt::bind_slice(outputs, cx), None, cx))
        }
        ObjectType::ByteFn(fun) => {
            let len = fun.len();
            root!(fun, cx);
            root!(outputs, new(Vec), cx);
            for i in 0..len {
                let val = fun.bind(cx).index(i, cx).unwrap();
                let output = call!(function, val; env, cx)?;
                outputs.push(output);
            }
            // TODO: remove this intermediate vector
            Ok(slice_into_list(Rt::bind_slice(outputs, cx), None, cx))
        }
        _ => Err(TypeError::new(Type::Sequence, sequence).into()),
    }
}

#[defun]
pub(crate) fn mapc<'ob>(
    function: &Rto<Function>,
    sequence: &Rto<List>,
    env: &mut Rt<Env>,
    cx: &'ob mut Context,
) -> Result<Object<'ob>> {
    match sequence.untag(cx) {
        ListType::Nil => Ok(NIL),
        ListType::Cons(cons) => {
            rooted_iter!(elements, cons, cx);
            while let Some(elem) = elements.next()? {
                call!(function, elem; env, cx)?;
            }
            Ok(sequence.bind(cx).into())
        }
    }
}

#[defun]
pub(crate) fn mapcan<'ob>(
    function: &Rto<Function>,
    sequence: &Rto<Object>,
    env: &mut Rt<Env>,
    cx: &'ob mut Context,
) -> Result<Object<'ob>> {
    let mapped = mapcar(function, sequence, env, cx)?;
    let mut lists = Vec::new();
    for list in mapped.as_list()? {
        lists.push(list?.try_into()?);
    }
    nconc(&lists)
}

#[defun]
pub(crate) fn mapconcat(
    function: &Rto<Function>,
    sequence: &Rto<Object>,
    seperator: Option<&Rto<Gc<&LispString>>>,
    env: &mut Rt<Env>,
    cx: &mut Context,
) -> Result<String> {
    let mapped = rebind!(mapcar(function, sequence, env, cx)?);
    let sep = match seperator {
        Some(sep) => sep.bind(cx).untag(),
        _ => "",
    };
    let mut string = String::new();
    let mut first = true;
    for element in mapped.as_list()? {
        if first {
            first = false;
        } else {
            string.push_str(sep);
        }
        let element: &str = element?.try_into()?;
        string.push_str(element);
    }
    Ok(string)
}

#[defun]
pub(crate) fn nreverse(seq: List) -> Result<Object> {
    let mut prev = NIL;
    for tail in seq.conses() {
        let tail = tail?;
        tail.set_cdr(prev)?;
        prev = tail.into();
    }
    Ok(prev)
}

#[defun]
pub(crate) fn reverse<'ob>(seq: List, cx: &'ob Context) -> Result<Object<'ob>> {
    let mut tail = NIL;
    for elem in seq {
        tail = Cons::new(elem?, tail, cx).into();
    }
    Ok(tail)
}

#[defun]
pub(crate) fn nconc<'ob>(lists: &[List<'ob>]) -> Result<Object<'ob>> {
    let mut tail: Option<&Cons> = None;
    for list in lists {
        if let Some(cons) = tail {
            cons.set_cdr((*list).into())?;
        }
        if let Some(last) = list.conses().last() {
            tail = Some(last?);
        }
    }

    Ok(match lists.iter().find(|&&x| x != ListType::empty()) {
        Some(x) => (*x).into(),
        None => NIL,
    })
}

fn join<'ob>(list: &mut Vec<Object<'ob>>, seq: List<'ob>) -> Result<()> {
    if let ListType::Cons(cons) = seq.untag() {
        for elt in cons {
            list.push(elt?);
        }
    }
    Ok(())
}

#[defun]
fn take<'ob>(n: i64, list: List<'ob>, cx: &'ob Context) -> Result<Object<'ob>> {
    let Ok(n) = usize::try_from(n) else { return Ok(NIL) };
    Ok(build_list(list.elements().take(n), cx)?)
}

#[defun]
pub(crate) fn append<'ob>(
    append: Object<'ob>,
    sequences: &[Object<'ob>],
    cx: &'ob Context,
) -> Result<Object<'ob>> {
    let mut list = Vec::new();
    match append.untag() {
        ObjectType::String(string) => {
            for ch in string.chars() {
                list.push((ch as i64).into());
            }
        }
        ObjectType::ByteString(string) => {
            for ch in string.iter() {
                list.push((*ch as i64).into());
            }
        }
        _ => join(&mut list, append.try_into()?)?,
    }
    for seq in sequences {
        join(&mut list, (*seq).try_into()?)?;
    }
    // TODO: Remove this temp vector
    Ok(slice_into_list(&list, None, cx))
}

#[defun]
pub(crate) fn assq<'ob>(key: Object<'ob>, alist: List<'ob>) -> Result<Object<'ob>> {
    for elem in alist {
        if let ObjectType::Cons(cons) = elem?.untag() {
            if eq(key, cons.car()) {
                return Ok(cons.into());
            }
        }
    }
    Ok(NIL)
}

#[defun]
fn rassq<'ob>(key: Object<'ob>, alist: List<'ob>) -> Result<Object<'ob>> {
    for elem in alist {
        if let ObjectType::Cons(cons) = elem?.untag() {
            if eq(key, cons.cdr()) {
                return Ok(cons.into());
            }
        }
    }
    Ok(NIL)
}

#[defun]
pub(crate) fn assoc<'ob>(
    key: &Rto<Object<'ob>>,
    alist: &Rto<List<'ob>>,
    testfn: Option<&Rto<Object>>,
    cx: &'ob mut Context,
    env: &mut Rt<Env>,
) -> Result<Object<'ob>> {
    match testfn {
        Some(x) => {
            let func: Function = x.bind(cx).try_into()?;
            root!(func, cx);
            rooted_iter!(iter, alist, cx);
            while let Some(elem) = iter.next()? {
                if let ObjectType::Cons(cons) = elem.bind(cx).untag() {
                    let val = cons.car();
                    root!(cons, cx);
                    let result = call!(func, key, val; env, cx)?;
                    if result != NIL {
                        return Ok(cons.bind(cx).into());
                    }
                }
            }
        }
        None => {
            let alist = alist.bind(cx);
            let key = key.bind(cx);
            for elem in alist {
                if let ObjectType::Cons(cons) = elem?.untag() {
                    if equal(key, cons.car()) {
                        return Ok(cons.into());
                    }
                }
            }
        }
    };
    Ok(NIL)
}

type EqFunc = for<'ob> fn(Object<'ob>, Object<'ob>) -> bool;

#[defun]
fn copy_alist<'ob>(alist: List<'ob>, cx: &'ob Context) -> Result<Object<'ob>> {
    match alist.untag() {
        ListType::Nil => Ok(NIL),
        ListType::Cons(cons) => {
            let first = copy_alist_elem(cons.car(), cx);
            let head = Cons::new1(first, cx);
            let mut tail = head;

            for elem in cons.cdr().as_list()? {
                let elem = copy_alist_elem(elem?, cx);
                let copy = Cons::new1(elem, cx);
                tail.set_cdr(copy.into()).unwrap();
                tail = copy;
            }
            Ok(head.into())
        }
    }
}

fn copy_alist_elem<'ob>(elem: Object<'ob>, cx: &'ob Context) -> Object<'ob> {
    match elem.untag() {
        ObjectType::Cons(cons) => Cons::new(cons.car(), cons.cdr(), cx).into(),
        _ => elem,
    }
}

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
    delete_from_list(elt, list, equal)
}

#[defun]
pub(crate) fn delq<'ob>(elt: Object<'ob>, list: List<'ob>) -> Result<Object<'ob>> {
    delete_from_list(elt, list, eq)
}

fn member_of_list<'ob>(elt: Object<'ob>, list: List<'ob>, eq_fn: EqFunc) -> Result<Object<'ob>> {
    let val = list.conses().fallible().find(|x| Ok(eq_fn(x.car(), elt)))?;
    match val {
        Some(elem) => Ok(elem.into()),
        None => Ok(NIL),
    }
}

#[defun]
pub(crate) fn memq<'ob>(elt: Object<'ob>, list: List<'ob>) -> Result<Object<'ob>> {
    member_of_list(elt, list, eq)
}

#[defun]
pub(crate) fn memql<'ob>(elt: Object<'ob>, list: List<'ob>) -> Result<Object<'ob>> {
    member_of_list(elt, list, eql)
}

#[defun]
pub(crate) fn member<'ob>(elt: Object<'ob>, list: List<'ob>) -> Result<Object<'ob>> {
    member_of_list(elt, list, equal)
}

// TODO: Handle sorting vectors
// TODO: Sort items in place
#[defun]
fn sort<'ob>(
    seq: &Rto<List>,
    predicate: &Rto<Function>,
    env: &mut Rt<Env>,
    cx: &'ob mut Context,
) -> Result<Object<'ob>> {
    let vec: Vec<_> = seq.bind(cx).elements().fallible().collect()?;
    let len = vec.len();
    if len <= 1 {
        return Ok(seq.bind(cx).into());
    }

    root!(tmp, NIL, cx);
    root!(vec, cx);
    // A simple insertion sort
    // TODO: use a better sort like tim sort
    for i in 1..len {
        tmp.set(&vec[i]);
        let mut j = i;
        while j > 0 {
            let result = call!(predicate, &*tmp, &vec[j - 1]; env, cx);
            // check if elements are out of order
            match result {
                Ok(cmp) => {
                    if cmp == NIL {
                        break;
                    }
                }
                Err(e) => {
                    vec[j].set(&*tmp);
                    return Err(e.into());
                }
            }
            let left = vec[j - 1].bind(cx);
            vec[j].set(left);
            j -= 1;
        }
        vec[j].set(&*tmp);
    }
    Ok(slice_into_list(Rt::bind_slice(vec, cx), None, cx))
}

#[defun]
pub(crate) fn defvaralias<'ob>(
    new_alias: Symbol<'ob>,
    _base_variable: Symbol,
    _docstring: Option<&str>,
) -> Symbol<'ob> {
    // TODO: implement
    new_alias
}

#[defun]
// TODO: implement
pub(crate) fn featurep(_feature: Symbol, _subfeature: Option<Symbol>) {}

#[defun]
pub(crate) fn require<'ob>(
    feature: &Rto<Gc<Symbol>>,
    filename: Option<&Rto<Gc<&LispString>>>,
    noerror: Option<()>,
    env: &mut Rt<Env>,
    cx: &'ob mut Context,
) -> Result<Symbol<'ob>> {
    // TODO: Fix this unsafe into_root
    let feat = unsafe { feature.untag(cx).with_lifetime() };
    if crate::data::features().lock().unwrap().contains(&feat) {
        return Ok(feature.untag(cx));
    }
    let file = match filename {
        Some(file) => file.untag(cx),
        None => feature.untag(cx).get().name(),
    };
    let file = file.into_obj(cx);
    root!(file, cx);
    match crate::lread::load(file, noerror, None, cx, env) {
        Ok(_) => Ok(feature.untag(cx)),
        Err(e) => Err(e),
    }
}

#[defun]
pub(crate) fn concat(sequences: &[Object]) -> Result<String> {
    let mut concat = String::new();
    for elt in sequences {
        match elt.untag() {
            ObjectType::String(string) => concat += string,
            ObjectType::NIL => continue,
            _ => bail!("Currently only concatenating strings are supported"),
        }
    }
    Ok(concat)
}

#[defun]
pub(crate) fn vconcat<'ob>(sequences: &[Object], cx: &'ob Context) -> Result<Gc<&'ob LispVec>> {
    let mut concated: Vec<Object> = Vec::new();
    for elt in sequences {
        match elt.untag() {
            // TODO: need to correctly handle unibyte strings (no unicode codepoints)
            ObjectType::String(string) => {
                for chr in string.chars() {
                    concated.push((chr as i64).into());
                }
            }
            ObjectType::Cons(cons) => {
                for x in cons {
                    concated.push(x?);
                }
            }
            ObjectType::Vec(vec) => {
                for x in vec.iter() {
                    concated.push(x.get());
                }
            }
            ObjectType::NIL => {}
            obj => bail!(TypeError::new(Type::Sequence, obj)),
        }
    }
    Ok(concated.into_obj(cx))
}

#[defun]
pub(crate) fn length(sequence: Object) -> Result<usize> {
    let size = match sequence.untag() {
        ObjectType::Cons(x) => x.elements().len()?,
        ObjectType::Vec(x) => x.len(),
        ObjectType::String(x) => x.len(),
        ObjectType::ByteString(x) => x.len(),
        ObjectType::ByteFn(x) => x.len(),
        ObjectType::NIL => 0,
        obj => bail!(TypeError::new(Type::Sequence, obj)),
    };
    Ok(size)
}

#[defun]
pub(crate) fn safe_length(sequence: Object) -> usize {
    length(sequence).unwrap_or(0)
}

#[defun]
pub(crate) fn proper_list_p(object: Object) -> Option<usize> {
    // TODO: Handle dotted list and circular
    match object.untag() {
        ObjectType::Cons(x) => x.elements().len().ok(),
        _ => None,
    }
}

#[defun]
pub(crate) fn nth(n: usize, list: List) -> Result<Object> {
    Ok(list.elements().fallible().nth(n)?.unwrap_or_default())
}

#[defun]
pub(crate) fn nthcdr(n: usize, list: List) -> Result<List> {
    match list.conses().fallible().nth(n)? {
        Some(x) => Ok(x.into()),
        None => Ok(ListType::empty()),
    }
}

#[defun]
pub(crate) fn elt<'ob>(sequence: Object<'ob>, n: usize, cx: &'ob Context) -> Result<Object<'ob>> {
    match sequence.untag() {
        ObjectType::Cons(x) => nth(n, x.into()),
        ObjectType::NIL => Ok(NIL),
        ObjectType::Vec(x) => aref(x.into(), n, cx),
        ObjectType::Record(x) => aref(x.into(), n, cx),
        ObjectType::String(x) => aref(x.into(), n, cx),
        ObjectType::ByteFn(x) => aref(x.into(), n, cx),
        other => Err(TypeError::new(Type::Sequence, other).into()),
    }
}

///////////////
// HashTable //
///////////////

defsym!(KW_TEST);
defsym!(KW_DOCUMENTATION);

#[defun]
pub(crate) fn make_hash_table<'ob>(
    keyword_args: &[Object<'ob>],
    cx: &'ob Context,
) -> Result<Object<'ob>> {
    let kw_test_pos = keyword_args.iter().step_by(2).position(|&x| x == sym::KW_TEST);
    if let Some(i) = kw_test_pos {
        let Some(val) = keyword_args.get((i * 2) + 1) else {
            bail!("Missing keyword value for :test")
        };
        if *val != sym::EQ && *val != sym::EQUAL && *val != sym::EQL {
            // TODO: we are currently only using `equal', but eq should be okay
            bail!("only `eq' and `equal' keywords support for make-hash-table :test. Found {val}");
        }
    }
    // TODO, the rest of the keywords need to be supported here
    let map = HashTable::with_hasher(std::hash::BuildHasherDefault::default());
    Ok(cx.add(map))
}

#[defun]
pub(crate) fn hash_table_p(obj: Object) -> bool {
    matches!(obj.untag(), ObjectType::HashTable(_))
}

#[defun]
pub(crate) fn gethash<'ob>(
    key: Object<'ob>,
    table: &'ob LispHashTable,
    dflt: Option<Object<'ob>>,
) -> Option<Object<'ob>> {
    match table.get(key) {
        Some(x) => Some(x),
        None => dflt,
    }
}

#[defun]
pub(crate) fn puthash<'ob>(
    key: Object<'ob>,
    value: Object<'ob>,
    table: &'ob LispHashTable,
) -> Object<'ob> {
    table.insert(key, value);
    value
}

#[defun]
fn remhash(key: Object, table: &LispHashTable) -> Result<()> {
    let Some(idx) = table.get_index_of(key) else { return Ok(()) };
    // If the removed element is before our iterator, then we need to shift the
    // iterator back one because the whole map get's shifted when something is
    // removed.
    let iter_idx = table.get_iter_index();
    if idx < iter_idx {
        table.set_iter_index(iter_idx - 1);
    }
    // TODO: can we use swap_remove?
    table.shift_remove(key);
    Ok(())
}

#[defun]
fn maphash(
    function: &Rto<Function>,
    table: &Rto<Gc<&LispHashTable>>,
    env: &mut Rt<Env>,
    cx: &mut Context,
) -> Result<bool> {
    let loop_idx = |table: &LispHashTable| {
        let end = table.len();
        let idx = table.get_iter_index();
        table.set_iter_index(idx + 1);
        if idx >= end {
            None
        } else {
            Some(idx)
        }
    };

    loop {
        let (key, val) = {
            let table = table.untag(cx);
            let Some(idx) = loop_idx(table) else { break };
            table.get_index(idx).unwrap()
        };
        let result = call!(function, key, val; env, cx);
        if let Err(e) = result {
            table.untag(cx).set_iter_index(0);
            return Err(e.into());
        }
    }
    table.untag(cx).set_iter_index(0);
    Ok(false)
}

#[defun]
fn copy_sequence<'ob>(arg: Object<'ob>, cx: &'ob Context) -> Result<Object<'ob>> {
    match arg.untag() {
        ObjectType::Vec(x) => Ok(cx.add(x.to_vec())),
        ObjectType::Cons(x) => {
            // TODO: remove this temp vector
            let mut elements = Vec::new();
            let mut tail = None;
            for cons in x.conses() {
                let cons = cons?;
                elements.push(cons.car());
                if !matches!(cons.cdr().untag(), ObjectType::Cons(_)) {
                    tail = Some(cons.cdr());
                }
            }
            Ok(slice_into_list(&elements, tail, cx))
        }
        ObjectType::String(x) => Ok(cx.add(x.to_owned())),
        ObjectType::NIL => Ok(NIL),
        _ => Err(TypeError::new(Type::Sequence, arg).into()),
    }
}

#[defun]
fn substring(string: &str, from: Option<usize>, to: Option<usize>) -> Result<String> {
    if from.unwrap_or_default() > string.len() || to.unwrap_or_default() > string.len() {
        bail!("substring args out of range for {string} : {from:?} {to:?}");
    }
    let new_string = match (from, to) {
        (None, None) => string,
        (None, Some(t)) => &string[..t],
        (Some(f), None) => &string[f..],
        (Some(f), Some(t)) => {
            let range = if f > t { t..f } else { f..t };
            &string[range]
        }
    };
    Ok(new_string.to_owned())
}

defsym!(MD5);
defsym!(SHA1);
defsym!(SHA224);
defsym!(SHA256);
defsym!(SHA384);
defsym!(SHA512);

#[defun]
fn secure_hash_algorithms<'ob>(cx: &'ob Context) -> Object<'ob> {
    // https://crates.io/crates/md-5
    // https://crates.io/crates/sha1
    // https://crates.io/crates/sha2
    // https://crates.io/crates/digest ?
    list![sym::MD5, sym::SHA1, sym::SHA224, sym::SHA256, sym::SHA384, sym::SHA512; cx]
}

#[defun]
fn enable_debug() -> bool {
    crate::debug::enable_debug();
    true
}

#[defun]
fn debug_enabled() -> bool {
    crate::debug::debug_enabled()
}

#[defun]
fn disable_debug() -> bool {
    crate::debug::disable_debug();
    false
}

#[cfg(test)]
mod test {
    use crate::core::{gc::RootSet, object::TRUE};
    use rune_core::macros::root;

    use super::*;

    #[test]
    fn test_take() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        let list = list![1, 2, 3, 4; cx];
        let res = take(2, list.try_into().unwrap(), cx).unwrap();
        assert_eq!(res, list![1, 2; cx]);
    }

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
            let res = delq(TRUE, list.try_into().unwrap()).unwrap();
            assert_eq!(res, NIL);
        }
    }

    #[test]
    fn test_nthcdr() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        let list = list![1, 2, 3; cx];
        let res = nthcdr(1, list.try_into().unwrap()).unwrap();
        assert_eq!(res.untag().car(), 2);
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
            let list = list![1, 2,; cx];
            let res = nreverse(list.try_into().unwrap()).unwrap();
            assert_eq!(res, list![2, 1; cx]);
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
            let res = nconc(&[ListType::empty()]).unwrap();
            assert!(res == NIL);
        }
        {
            let list: List = list![1, 2; cx].try_into().unwrap();
            let res = nconc(&[list]).unwrap();
            assert_eq!(res, list![1, 2; cx]);
        }
        {
            let list1: List = list![1, 2; cx].try_into().unwrap();
            let list2: List = list![3, 4; cx].try_into().unwrap();
            let res = nconc(&[list1, list2]).unwrap();
            assert_eq!(res, list![1, 2, 3, 4; cx]);
        }
        {
            let list1: List = list![1, 2; cx].try_into().unwrap();
            let list2: List = list![3, 4; cx].try_into().unwrap();
            let list3: List = list![5, 6; cx].try_into().unwrap();
            let res = nconc(&[list1, list2, list3]).unwrap();
            assert_eq!(res, list![1, 2, 3, 4, 5, 6; cx]);
        }
        {
            let list1: List = NIL.try_into().unwrap();
            let list2: List = list![1, 2; cx].try_into().unwrap();
            let res = nconc(&[list1, list2]).unwrap();
            assert_eq!(res, list![1, 2; cx]);
        }
        {
            let list1: List = list![1, 2; cx].try_into().unwrap();
            let list2: List = NIL.try_into().unwrap();
            let res = nconc(&[list1, list2]).unwrap();
            assert_eq!(res, list![1, 2; cx]);
        }
    }

    #[test]
    fn test_append() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        let expect = list![104, 101, 108, 108, 111; cx];
        let result = append(cx.add("hello"), &[], cx).unwrap();
        assert_eq!(result, expect);
    }

    #[test]
    fn test_assq() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        let element = Object::from(Cons::new(5, 6, cx));
        let list = list![Cons::new(1, 2, cx), Cons::new(3, 4, cx), element; cx];
        let list = list.try_into().unwrap();
        let result = assq(5.into(), list).unwrap();
        assert_eq!(result, element);
    }

    #[test]
    fn test_maphash() {
        sym::init_symbols();
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        let mut table = HashTable::default();
        table.insert(1.into(), 6.into());
        table.insert(2.into(), 8.into());
        table.insert(3.into(), 10.into());
        let table = table.into_obj(cx);
        let func = sym::EQ.func(cx).unwrap();
        root!(env, new(Env), cx);
        root!(table, cx);
        root!(func, cx);
        // This test does not assert anything, but allows this to be checked by
        // miri
        maphash(func, table, env, cx).unwrap();
    }

    #[test]
    #[allow(clippy::needless_borrow)]
    fn test_sort() {
        sym::init_symbols();
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        root!(env, new(Env), cx);
        let func = sym::LESS_THAN.func(cx).unwrap();
        root!(func, cx);
        {
            root!(list, ListType::empty(), cx);
            let res = rebind!(sort(list, func, env, cx).unwrap());
            assert_eq!(res, NIL);
        }
        {
            let list: List = list![1; cx].try_into().unwrap();
            root!(list, cx);
            let res = rebind!(sort(list, func, env, cx).unwrap());
            assert_eq!(res, list![1; cx]);
        }
        {
            let list: List = list![2, 1; cx].try_into().unwrap();
            root!(list, cx);
            let res = rebind!(sort(list, func, env, cx).unwrap());
            assert_eq!(res, list![1, 2; cx]);
        }
        {
            let list: List = list![1, 2, 3; cx].try_into().unwrap();
            root!(list, cx);
            let res = rebind!(sort(list, func, env, cx).unwrap());
            assert_eq!(res, list![1, 2, 3; cx]);
        }
        {
            let list: List = list![3, 2, 1; cx].try_into().unwrap();
            root!(list, cx);
            let res = rebind!(sort(list, func, env, cx).unwrap());
            assert_eq!(res, list![1, 2, 3; cx]);
        }

        {
            let list: List = list![3, 1, 2; cx].try_into().unwrap();
            root!(list, cx);
            let res = rebind!(sort(list, func, env, cx).unwrap());
            assert_eq!(res, list![1, 2, 3; cx]);
        }
        {
            let func = sym::GREATER_THAN.func(cx).unwrap();
            let list: List = list![1, 2, 3, 4, 5; cx].try_into().unwrap();
            root!(list, cx);
            root!(func, cx);
            let res = rebind!(sort(list, func, env, cx).unwrap());
            assert_eq!(res, list![5, 4, 3, 2, 1; cx]);
        }
        {
            // check stable sorting
            let func = sym::CAR_LESS_THAN_CAR.func(cx).unwrap();
            let list: List =
                list![Cons::new(1, 1, cx), Cons::new(1, 2, cx), Cons::new(1, 3, cx); cx]
                    .try_into()
                    .unwrap();
            root!(list, cx);
            root!(func, cx);
            let res = rebind!(sort(list, func, env, cx).unwrap());
            assert_eq!(
                res,
                list![Cons::new(1, 1, cx), Cons::new(1, 2, cx), Cons::new(1, 3, cx); cx]
            );
        }
    }

    #[test]
    fn test_copy_alist() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        let alist = list![Cons::new(1, 2, cx), Cons::new(3, 4, cx), Cons::new(5, 6, cx); cx];
        let list = alist.try_into().unwrap();
        let result = copy_alist(list, cx).unwrap();
        assert_eq!(alist, result);
    }
}
