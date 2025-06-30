use anyhow::Result;
use std::cell::RefCell;

mod strategies;
pub(crate) use strategies::*;

use crate::library::elprop::inf_emacs::InferiorEmacs;
mod inf_emacs;

thread_local! {
    static INFERIOR_EMACS: RefCell<InferiorEmacs> = RefCell::new(InferiorEmacs::default());
}

pub fn start_emacs() -> Result<()> {
    INFERIOR_EMACS.with_borrow_mut(|inf_emacs| inf_emacs.start())
}

pub fn eval(lisp: &str) -> Result<String> {
    INFERIOR_EMACS.with_borrow_mut(|inf_emacs| inf_emacs.eval(lisp))
}

#[macro_export]
macro_rules! assert_elprop {
    ($($form:tt)*) => {{
        let lisp_form = format!($($form)*);
        $crate::library::elprop::start_emacs().unwrap();
        let emacs_result = $crate::library::elprop::eval(&lisp_form).unwrap();

        let roots = &$crate::core::gc::RootSet::default();
        let cx = &mut $crate::core::gc::Context::new(roots);
        $crate::core::env::sym::init_symbols();
        rune_core::macros::root!(env, init($crate::core::env::Env::default()), cx);
        let rune_result = {
            let obj = $crate::reader::read(&lisp_form, cx).unwrap().0;
            rune_core::macros::root!(obj, cx);
            match $crate::interpreter::eval(obj, None, env, cx) {
                Ok(val) => format!("{val}"),
                Err(e) => format!("Error: {e}"),
            }
        };
        if !(rune_result == emacs_result) {
            panic!(
                r#"assertion `Rune vs Emacs` failed:
  Rune: {rune_result}
 Emacs: {emacs_result}"#
            )
        }
    };};
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_eval() {
        start_emacs().unwrap();
        let result = eval("(+ 40 2)").unwrap();
        assert_eq!("42", result);
        assert_elprop!["(+ 40 3)"];
    }
}
