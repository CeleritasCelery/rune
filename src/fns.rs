use crate::{
    core::{
        cons::Cons,
        env::{sym, Env, Symbol},
        error::{Type, TypeError},
        gc::{Context, IntoRoot, Rt},
        object::{
            nil, Function, Gc, GcObj, HashTable, HashTableView, IntoObject, LispHashTable,
            LispString, LispVec, List, Object,
        },
    },
    data::aref,
};
use crate::{root, rooted_iter};
use anyhow::{bail, Result};
use bstr::ByteSlice;
use fallible_iterator::FallibleIterator;
use fallible_streaming_iterator::FallibleStreamingIterator;
use rune_macros::defun;

#[defun]
fn identity(arg: GcObj) -> GcObj {
    arg
}

pub(crate) fn slice_into_list<'ob>(
    slice: &[GcObj<'ob>],
    tail: Option<GcObj<'ob>>,
    cx: &'ob Context,
) -> GcObj<'ob> {
    let from_end = slice.iter().rev();
    from_end.fold(tail.into(), |acc, obj| cons!(*obj, acc; cx))
}

pub(crate) fn build_list<'ob, E>(
    mut iter: impl Iterator<Item = Result<GcObj<'ob>, E>>,
    cx: &'ob Context,
) -> Result<GcObj<'ob>, E> {
    let Some(first) = iter.next() else { return Ok(nil()) };
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
pub(crate) fn eq(obj1: GcObj, obj2: GcObj) -> bool {
    obj1.ptr_eq(obj2)
}

#[defun]
pub(crate) fn equal<'ob>(obj1: GcObj<'ob>, obj2: GcObj<'ob>) -> bool {
    obj1 == obj2
}

#[defun]
pub(crate) fn eql<'ob>(obj1: GcObj<'ob>, obj2: GcObj<'ob>) -> bool {
    match (obj1.untag(), obj2.untag()) {
        (Object::Float(f1), Object::Float(f2)) => f1.to_bits() == f2.to_bits(),
        _ => obj1.ptr_eq(obj2),
    }
}

#[defun]
fn equal_including_properties<'ob>(o1: GcObj<'ob>, o2: GcObj<'ob>) -> bool {
    // TODO: implement text properties
    equal(o1, o2)
}

#[defun]
fn plist_get<'ob>(plist: GcObj<'ob>, prop: GcObj<'ob>) -> Result<GcObj<'ob>> {
    let plist: Result<Gc<List>, _> = plist.try_into();
    let Ok(plist) = plist else { return Ok(nil()) };
    // TODO: this function should never fail. Need to implement safe iterator
    let iter = plist.elements().zip(plist.elements().skip(1));

    for (cur_prop, value) in iter {
        if eq(cur_prop?, prop) {
            return Ok(value?);
        }
    }
    Ok(nil())
}

#[defun]
pub(crate) fn prin1_to_string(object: GcObj, _noescape: Option<GcObj>) -> String {
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
    function: &Rt<Gc<Function>>,
    sequence: &Rt<GcObj>,
    env: &mut Rt<Env>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>> {
    let sequence = sequence.bind(cx);
    match sequence.untag() {
        Object::NIL => Ok(nil()),
        Object::Cons(cons) => {
            rooted_iter!(iter, cons, cx);
            root!(outputs, Vec::new(), cx);
            root!(call_arg, Vec::new(), cx);
            while let Some(obj) = iter.next()? {
                call_arg.push(obj);
                let output = function.call(call_arg, None, env, cx)?;
                outputs.push(output);
                call_arg.clear();
            }
            // TODO: remove this intermediate vector
            Ok(slice_into_list(outputs.bind_ref(cx), None, cx))
        }
        Object::ByteFn(fun) => {
            let len = fun.len();
            root!(fun, cx);
            root!(outputs, Vec::new(), cx);
            root!(call_arg, Vec::new(), cx);
            for i in 0..len {
                let item = fun.bind(cx).index(i).unwrap();
                call_arg.push(item);
                let output = function.call(call_arg, None, env, cx)?;
                outputs.push(output);
                call_arg.clear();
            }
            // TODO: remove this intermediate vector
            Ok(slice_into_list(outputs.bind_ref(cx), None, cx))
        }
        _ => Err(TypeError::new(Type::Sequence, sequence).into()),
    }
}

#[defun]
pub(crate) fn mapc<'ob>(
    function: &Rt<Gc<Function>>,
    sequence: &Rt<Gc<List>>,
    env: &mut Rt<Env>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>> {
    match sequence.get(cx) {
        List::Nil => Ok(nil()),
        List::Cons(cons) => {
            root!(call_arg, Vec::new(), cx);
            rooted_iter!(elements, cons, cx);
            while let Some(elem) = elements.next()? {
                call_arg.push(elem);
                function.call(call_arg, None, env, cx)?;
                call_arg.clear();
            }
            Ok(sequence.bind(cx).into())
        }
    }
}

#[defun]
pub(crate) fn mapcan<'ob>(
    function: &Rt<Gc<Function>>,
    sequence: &Rt<GcObj>,
    env: &mut Rt<Env>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>> {
    let mapped = mapcar(function, sequence, env, cx)?;
    let mut lists = Vec::new();
    for list in mapped.as_list()? {
        lists.push(list?.try_into()?);
    }
    nconc(&lists)
}

#[defun]
pub(crate) fn mapconcat(
    function: &Rt<Gc<Function>>,
    sequence: &Rt<GcObj>,
    seperator: Option<&Rt<Gc<&LispString>>>,
    env: &mut Rt<Env>,
    cx: &mut Context,
) -> Result<String> {
    let mapped = rebind!(mapcar(function, sequence, env, cx)?);
    let sep = match seperator {
        Some(sep) => sep.bind(cx).untag().try_into()?,
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
    for elem in seq {
        tail = cons!(elem?, tail; cx);
    }
    Ok(tail)
}

#[defun]
pub(crate) fn nconc<'ob>(lists: &[Gc<List<'ob>>]) -> Result<GcObj<'ob>> {
    let mut tail: Option<&Cons> = None;
    for list in lists {
        if let Some(cons) = tail {
            cons.set_cdr((*list).into())?;
        }
        if let Some(last) = list.conses().last() {
            tail = Some(last?);
        }
    }

    Ok(match lists.iter().find(|&&x| x != List::empty()) {
        Some(x) => (*x).into(),
        None => nil(),
    })
}

fn join<'ob>(list: &mut Vec<GcObj<'ob>>, seq: Gc<List<'ob>>) -> Result<()> {
    if let List::Cons(cons) = seq.untag() {
        for elt in cons {
            list.push(elt?);
        }
    }
    Ok(())
}

#[defun]
fn take<'ob>(n: i64, list: Gc<List<'ob>>, cx: &'ob Context) -> Result<GcObj<'ob>> {
    let Ok(n) = usize::try_from(n) else { return Ok(nil()) };
    Ok(build_list(list.elements().take(n), cx)?)
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
pub(crate) fn assq<'ob>(key: GcObj<'ob>, alist: Gc<List<'ob>>) -> Result<GcObj<'ob>> {
    for elem in alist {
        if let Object::Cons(cons) = elem?.untag() {
            if eq(key, cons.car()) {
                return Ok(cons.into());
            }
        }
    }
    Ok(nil())
}

#[defun]
fn rassq<'ob>(key: GcObj<'ob>, alist: Gc<List<'ob>>) -> Result<GcObj<'ob>> {
    for elem in alist {
        if let Object::Cons(cons) = elem?.untag() {
            if eq(key, cons.cdr()) {
                return Ok(cons.into());
            }
        }
    }
    Ok(nil())
}

#[defun]
pub(crate) fn assoc<'ob>(
    key: &Rt<GcObj<'ob>>,
    alist: &Rt<Gc<List<'ob>>>,
    testfn: Option<&Rt<GcObj>>,
    cx: &'ob mut Context,
    env: &mut Rt<Env>,
) -> Result<GcObj<'ob>> {
    match testfn {
        Some(x) => {
            let func: Gc<Function> = x.bind(cx).try_into()?;
            root!(func, cx);
            rooted_iter!(iter, alist, cx);
            while let Some(elem) = iter.next()? {
                if let Object::Cons(cons) = elem.bind(cx).untag() {
                    root!(cons, cx);
                    root!(call_arg, Vec::new(), cx);
                    call_arg.push(key);
                    call_arg.push(cons.car(cx));
                    let result = func.call(call_arg, None, env, cx)?;
                    if result != nil() {
                        return Ok(cons.bind(cx).into());
                    }
                    call_arg.clear();
                }
            }
        }
        None => {
            let alist = alist.bind(cx);
            let key = key.bind(cx);
            for elem in alist {
                if let Object::Cons(cons) = elem?.untag() {
                    if equal(key, cons.car()) {
                        return Ok(cons.into());
                    }
                }
            }
        }
    };
    Ok(nil())
}

type EqFunc = for<'ob> fn(GcObj<'ob>, GcObj<'ob>) -> bool;

#[defun]
fn copy_alist<'ob>(alist: Gc<List<'ob>>, cx: &'ob Context) -> Result<GcObj<'ob>> {
    match alist.untag() {
        List::Nil => Ok(nil()),
        List::Cons(cons) => {
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

fn copy_alist_elem<'ob>(elem: GcObj<'ob>, cx: &'ob Context) -> GcObj<'ob> {
    match elem.untag() {
        Object::Cons(cons) => cons!(cons.car(), cons.cdr(); cx),
        _ => elem,
    }
}

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
    delete_from_list(elt, list, equal)
}

#[defun]
pub(crate) fn delq<'ob>(elt: GcObj<'ob>, list: Gc<List<'ob>>) -> Result<GcObj<'ob>> {
    delete_from_list(elt, list, eq)
}

fn member_of_list<'ob>(elt: GcObj<'ob>, list: Gc<List<'ob>>, eq_fn: EqFunc) -> Result<GcObj<'ob>> {
    let val = list.conses().fallible().find(|x| Ok(eq_fn(x.car(), elt)))?;
    match val {
        Some(elem) => Ok(elem.into()),
        None => Ok(nil()),
    }
}

#[defun]
pub(crate) fn memq<'ob>(elt: GcObj<'ob>, list: Gc<List<'ob>>) -> Result<GcObj<'ob>> {
    member_of_list(elt, list, eq)
}

#[defun]
pub(crate) fn memql<'ob>(elt: GcObj<'ob>, list: Gc<List<'ob>>) -> Result<GcObj<'ob>> {
    member_of_list(elt, list, eql)
}

#[defun]
pub(crate) fn member<'ob>(elt: GcObj<'ob>, list: Gc<List<'ob>>) -> Result<GcObj<'ob>> {
    member_of_list(elt, list, equal)
}

// TODO: Handle sorting vectors
#[defun]
fn sort<'ob>(
    seq: &Rt<Gc<List>>,
    predicate: &Rt<Gc<Function>>,
    env: &mut Rt<Env>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>> {
    let vec: Vec<_> = seq.bind(cx).elements().fallible().collect()?;
    let len = vec.len();
    if len <= 1 {
        return Ok(seq.bind(cx).into());
    }

    root!(tmp, nil(), cx);
    root!(vec, cx);
    root!(call_args, Vec::new(), cx);
    // A simple insertion sort
    // TODO: use a better sort like tim sort
    for i in 1..len {
        tmp.set(&vec[i]);
        let mut j = i;
        while j > 0 {
            call_args.clear();
            call_args.push(&vec[j - 1]);
            call_args.push(&*tmp);
            match predicate.call(call_args, None, env, cx) {
                Ok(cmp) => {
                    if cmp != nil() {
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
    Ok(slice_into_list(vec.bind_ref(cx), None, cx))
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
    feature: &Rt<Gc<Symbol>>,
    filename: Option<&Rt<Gc<&LispString>>>,
    noerror: Option<()>,
    env: &mut Rt<Env>,
    cx: &'ob mut Context,
) -> Result<Symbol<'ob>> {
    // TODO: Fix this unsafe into_root
    let feat = unsafe { feature.get(cx).into_root() };
    if crate::data::features().lock().unwrap().contains(&feat) {
        return Ok(feature.get(cx));
    }
    let file = match filename {
        Some(file) => file.get(cx).try_into()?,
        None => feature.get(cx).get().name(),
    };
    let file = file.into_obj(cx);
    root!(file, cx);
    match crate::lread::load(file, None, None, cx, env) {
        Ok(_) => Ok(feature.get(cx)),
        Err(e) => match noerror {
            Some(()) => Ok(sym::NIL),
            None => Err(e),
        },
    }
}

#[defun]
pub(crate) fn concat(sequences: &[GcObj]) -> Result<String> {
    let mut concat = String::new();
    for elt in sequences {
        match elt.untag() {
            Object::String(string) => concat += string.try_into()?,
            Object::NIL => continue,
            _ => bail!("Currently only concatenating strings are supported"),
        }
    }
    Ok(concat)
}

#[defun]
pub(crate) fn vconcat<'ob>(sequences: &[GcObj], cx: &'ob Context) -> Result<Gc<&'ob LispVec>> {
    let mut concated: Vec<GcObj> = Vec::new();
    for elt in sequences {
        match elt.untag() {
            // TODO: need to correctly handle unibyte strings (no unicode codepoints)
            Object::String(string) => {
                for chr in string.chars() {
                    concated.push((chr as i64).into());
                }
            }
            Object::Cons(cons) => {
                for x in cons {
                    concated.push(x?);
                }
            }
            Object::Vec(vec) => {
                for x in vec.iter() {
                    concated.push(x.get());
                }
            }
            Object::NIL => {}
            obj => bail!(TypeError::new(Type::Sequence, obj)),
        }
    }
    Ok(concated.into_obj(cx))
}

#[defun]
pub(crate) fn length(sequence: GcObj) -> Result<usize> {
    let size = match sequence.untag() {
        Object::Cons(x) => x.elements().len()?,
        Object::Vec(x) => x.len(),
        Object::String(x) => x.len(),
        Object::ByteFn(x) => x.len(),
        Object::NIL => 0,
        obj => bail!(TypeError::new(Type::Sequence, obj)),
    };
    Ok(size)
}

#[defun]
pub(crate) fn safe_length(sequence: GcObj) -> usize {
    length(sequence).unwrap_or(0)
}

#[defun]
pub(crate) fn proper_list_p(object: GcObj) -> Option<usize> {
    // TODO: Handle dotted list and circular
    match object.untag() {
        Object::Cons(x) => x.elements().len().ok(),
        _ => None,
    }
}

#[defun]
pub(crate) fn nth(n: usize, list: Gc<List>) -> Result<GcObj> {
    Ok(list.elements().fallible().nth(n)?.unwrap_or_default())
}

#[defun]
pub(crate) fn nthcdr(n: usize, list: Gc<List>) -> Result<Gc<List>> {
    match list.conses().fallible().nth(n)? {
        Some(x) => Ok(x.into()),
        None => Ok(List::empty()),
    }
}

#[defun]
pub(crate) fn elt(sequence: GcObj, n: usize) -> Result<GcObj> {
    match sequence.untag() {
        Object::Cons(x) => nth(n, x.into()),
        Object::NIL => Ok(nil()),
        Object::Vec(x) => aref(x.into(), n),
        Object::Record(x) => aref(x.into(), n),
        Object::String(x) => aref(x.into(), n),
        Object::ByteFn(x) => aref(x.into(), n),
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
    keyword_args: &[GcObj<'ob>],
    cx: &'ob Context,
) -> Result<GcObj<'ob>> {
    let kw_test_pos = keyword_args.iter().step_by(2).position(|&x| x == sym::KW_TEST);
    if let Some(i) = kw_test_pos {
        let Some(val) = keyword_args.get((i * 2) + 1) else {
            bail!("Missing keyword value for :test")
        };
        if *val != sym::EQ && *val != sym::EQUAL {
            // TODO: we are currently only using `equal', but eq should be okay
            bail!("only `eq' and `equal' keywords support for make-hash-table :test. Found {val}");
        }
    }
    // TODO, the rest of the keywords need to be supported here
    let map = HashTable::with_hasher(std::hash::BuildHasherDefault::default());
    Ok(cx.add(map))
}

#[defun]
pub(crate) fn hash_table_p(obj: GcObj) -> bool {
    matches!(obj.untag(), Object::HashTable(_))
}

#[defun]
pub(crate) fn gethash<'ob>(
    key: GcObj<'ob>,
    table: &'ob LispHashTable,
    dflt: Option<GcObj<'ob>>,
    cx: &'ob Context,
) -> Option<GcObj<'ob>> {
    match table.borrow().get(&key) {
        Some(x) => Some(cx.bind(x.get())),
        None => dflt,
    }
}

#[defun]
pub(crate) fn puthash<'ob>(
    key: GcObj<'ob>,
    value: GcObj<'ob>,
    table: &'ob LispHashTable,
) -> Result<GcObj<'ob>> {
    table.try_borrow_mut()?.insert(key, value);
    Ok(value)
}

#[defun]
fn remhash(key: GcObj, table: &LispHashTable) -> Result<()> {
    let mut table = table.try_borrow_mut()?;
    let Some(idx) = table.get_index_of(&key) else { return Ok(()) };
    // If the removed element is before our iterator, then we need to shift the
    // iterator back one because the whole map get's shifted when something is
    // removed.
    if idx < table.iter_next {
        table.iter_next -= 1;
    }
    table.shift_remove(&key);
    Ok(())
}

#[defun]
fn maphash(
    function: &Rt<Gc<Function>>,
    table: &Rt<Gc<&'static LispHashTable>>,
    env: &mut Rt<Env>,
    cx: &mut Context,
) -> Result<bool> {
    let get_idx = |table: &mut HashTableView<'_, GcObj>| {
        let end = table.len();
        let idx = table.iter_next;
        table.iter_next += 1;
        if idx >= end {
            None
        } else {
            Some(idx)
        }
    };

    root!(call_arg, Vec::new(), cx);
    loop {
        {
            let mut view = table.bind(cx).untag().try_borrow_mut()?;
            let Some(idx) = get_idx(&mut view) else { break };
            let (key, val) = view.get_index(idx).unwrap();
            call_arg.push(*key);
            call_arg.push(*val);
        }
        if let Err(e) = function.call(call_arg, None, env, cx) {
            table.bind(cx).untag().try_borrow_mut().unwrap().iter_next = 0;
            return Err(e.into());
        }
        call_arg.clear();
    }
    table.bind(cx).untag().try_borrow_mut().unwrap().iter_next = 0;
    Ok(false)
}

#[defun]
fn copy_sequence<'ob>(arg: GcObj<'ob>, cx: &'ob Context) -> Result<GcObj<'ob>> {
    match arg.untag() {
        Object::Vec(x) => Ok(cx.add(x.to_vec())),
        Object::Cons(x) => {
            // TODO: remove this temp vector
            let mut elements = Vec::new();
            let mut tail = None;
            for cons in x.conses() {
                let cons = cons?;
                elements.push(cons.car());
                if !matches!(cons.cdr().untag(), Object::Cons(_)) {
                    tail = Some(cons.cdr());
                }
            }
            Ok(slice_into_list(&elements, tail, cx))
        }
        Object::String(x) => {
            let string: Result<&str, _> = x.try_into();
            match string {
                Ok(s) => Ok(cx.add(s)),
                Err(_) => Ok(cx.add(x.to_vec())),
            }
        }
        Object::NIL => Ok(nil()),
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
fn secure_hash_algorithms<'ob>(cx: &'ob Context) -> GcObj<'ob> {
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
    use crate::core::{gc::RootSet, object::qtrue};

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
    fn test_assq() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        let element = cons!(5, 6; cx);
        let list = list![Cons::new(1, 2, cx), Cons::new(3, 4, cx), element; cx];
        let list = list.try_into().unwrap();
        let result = assq(5.into(), list).unwrap();
        assert_eq!(result, element);
    }

    #[test]
    fn test_maphash() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        let mut table = HashTable::default();
        table.insert(1.into(), 6.into());
        table.insert(2.into(), 8.into());
        table.insert(3.into(), 10.into());
        let table = table.into_obj(cx);
        let func = sym::EQ.func(cx).unwrap();
        root!(env, Env::default(), cx);
        root!(table, cx);
        root!(func, cx);
        // This test does not assert anything, but allows this to be checked by
        // miri
        maphash(func, table, env, cx).unwrap();
    }

    #[test]
    #[allow(clippy::needless_borrow)]
    fn test_sort() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        root!(env, Env::default(), cx);
        let func = sym::LESS_THAN.func(cx).unwrap();
        root!(func, cx);
        {
            root!(list, List::empty(), cx);
            let res = rebind!(sort(list, func, env, cx).unwrap());
            assert_eq!(res, nil());
        }
        {
            let list: Gc<List> = list![1; cx].try_into().unwrap();
            root!(list, cx);
            let res = rebind!(sort(list, func, env, cx).unwrap());
            assert_eq!(res, list![1; cx]);
        }
        {
            let list: Gc<List> = list![2, 1; cx].try_into().unwrap();
            root!(list, cx);
            let res = rebind!(sort(list, func, env, cx).unwrap());
            assert_eq!(res, list![1, 2; cx]);
        }
        {
            let list: Gc<List> = list![1, 2, 3; cx].try_into().unwrap();
            root!(list, cx);
            let res = rebind!(sort(list, func, env, cx).unwrap());
            assert_eq!(res, list![1, 2, 3; cx]);
        }
        {
            let list: Gc<List> = list![3, 2, 1; cx].try_into().unwrap();
            root!(list, cx);
            let res = rebind!(sort(list, func, env, cx).unwrap());
            assert_eq!(res, list![1, 2, 3; cx]);
        }

        {
            let list: Gc<List> = list![3, 1, 2; cx].try_into().unwrap();
            root!(list, cx);
            let res = rebind!(sort(list, func, env, cx).unwrap());
            assert_eq!(res, list![1, 2, 3; cx]);
        }
        {
            let func = sym::GREATER_THAN.func(cx).unwrap();
            let list: Gc<List> = list![1, 2, 3, 4, 5; cx].try_into().unwrap();
            root!(list, cx);
            root!(func, cx);
            let res = rebind!(sort(list, func, env, cx).unwrap());
            assert_eq!(res, list![5, 4, 3, 2, 1; cx]);
        }
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
