use crate::lisp_object::{LispObj, Symbol, Function};

#[lisp_fn]
pub fn defalias(symbol: Symbol, definition: Function) -> Symbol {
    symbol.set_func(definition);
    symbol
}

#[lisp_fn]
pub fn progn(forms: &[LispObj]) -> LispObj {
    match forms.last() {
        Some(form) => *form,
        None => LispObj::nil(),
    }
}

defsubr!(defalias, progn);
