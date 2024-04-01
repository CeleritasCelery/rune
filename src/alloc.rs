//! builtin lisp data structures.
use crate::core::cons::Cons;
use crate::core::gc::Context;
use crate::core::object::{
    ByteFn, ByteString, FnArgs, Gc, IntoObject, LispVec, Object, RecordBuilder, Symbol, NIL,
};
use anyhow::{ensure, Result};
use rune_macros::defun;

#[defun]
pub(crate) fn list<'ob>(objects: &[Object<'ob>], cx: &'ob Context) -> Object<'ob> {
    let mut head = NIL;
    for object in objects.iter().rev() {
        head = Cons::new(*object, head, cx).into();
    }
    head
}

/// Convert a function to closure by replacing the first N elements with their
/// closure values.
#[defun]
pub(crate) fn make_closure<'ob>(
    prototype: &ByteFn,
    closure_vars: &[Object<'ob>],
    cx: &'ob Context,
) -> Result<Gc<&'ob ByteFn>> {
    let const_len = prototype.consts().len();
    let vars = closure_vars.len();
    ensure!(vars <= const_len, "Closure vars do not fit in const vec");
    let mut constants = prototype.consts().to_vec();
    let zipped = constants.iter_mut().zip(closure_vars.iter());
    for (cnst, var) in zipped {
        *cnst = *var;
    }

    unsafe {
        Ok(ByteFn::make(
            prototype.codes(),
            constants.into_obj(cx).untag(),
            prototype.args,
            prototype.depth,
        )
        .into_obj(cx))
    }
}

#[defun]
#[allow(clippy::too_many_arguments)]
pub(crate) fn make_byte_code<'ob>(
    arglist: u64,
    byte_code: &'ob ByteString,
    constants: &'ob LispVec,
    depth: usize,
    _docstring: Option<Object>,
    _interactive_spec: Option<Object>,
    _elements: &[Object],
    cx: &'ob Context,
) -> Result<&'ob ByteFn> {
    unsafe {
        let bytefn = ByteFn::make(byte_code, constants, FnArgs::from_arg_spec(arglist)?, depth);
        Ok(bytefn.into_obj(cx).untag())
    }
}

#[defun]
fn make_vector(length: usize, init: Object) -> Vec<Object> {
    vec![init; length]
}

#[defun]
fn vector<'ob>(objects: &[Object<'ob>]) -> Vec<Object<'ob>> {
    objects.into()
}

#[defun]
fn record<'ob>(type_: Object<'ob>, slots: &[Object<'ob>], cx: &'ob Context) -> RecordBuilder<'ob> {
    let mut record = cx.vec_with_capacity(1 + slots.len());
    record.push(type_);
    record.extend_from_slice(slots);
    RecordBuilder(record)
}

#[defun]
fn purecopy(obj: Object) -> Object {
    obj
}

#[defun]
fn make_symbol<'ob>(name: &str, cx: &'ob Context) -> Symbol<'ob> {
    Symbol::new_uninterned(name, cx)
}

#[defun]
fn garbage_collect(cx: &mut Context) -> bool {
    cx.garbage_collect(true);
    true
}

#[cfg(test)]
mod test {
    use rune_core::macros::root;

    use crate::core::{env::intern, gc::RootSet, object::ObjectType};

    use super::*;

    #[test]
    fn build_record() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        let type_ = intern("dummy-type", cx);
        let slots = vec![cx.add("slot1"), cx.add("slot2")];
        let record = record(type_.into(), &slots, cx);
        assert_eq!(record.0.len(), 3);
        assert_eq!(record.0[0], type_);
        assert_eq!(record.0[1], slots[0]);
        assert_eq!(record.0[2], slots[1]);
        let x = cx.add(record);
        root!(x, cx);
        cx.garbage_collect(true);
        let ObjectType::Record(record) = x.bind(cx).untag() else { unreachable!() };
        assert_eq!(record.len(), 3);
        assert_eq!(record[1].get(), "slot1");
        assert_eq!(record[2].get(), "slot2");
    }
}
