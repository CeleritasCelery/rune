use crate::lisp_object::{Function, LispObj, Object, Symbol};

#[lisp_fn]
pub fn defalias(symbol: Symbol, definition: Function) -> Symbol {
    symbol.set_func(definition);
    symbol
}


#[lisp_fn]
pub fn progn<'obj>(forms: &[Object<'obj>]) -> Object<'obj> {
    match forms.last() {
        Some(form) => *form,
        None => LispObj::nil(),
    }
}

defsubr!(defalias, progn);
