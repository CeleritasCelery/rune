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

/// Asserts Elisp form evaluates identically in Emacs and Rune.
///
/// This macro is particularly useful for property-based testing, allowing
/// you to ensure consistency between Emacs Lisp's behavior and Rune's
/// interpretation across various inputs.
///
/// The Lisp `FORM` can contain placeholders for injecting Rust values, just like `format!`
///
/// ## Examples
///
/// Verify that the addition of 40 and 2 yields the same result in both Emacs and Rune:
/// ```rust
/// assert_elprop!["(+ 40 2)"];
/// ```
///
/// Test that `make-vector` works consistently for any arbitrary byte size,
/// demonstrating integration with property testing tools like `proptest`.
/// Here, `size` is a Rust value injected into the Lisp form, while `?A`
/// represents an arbitrary value managed internally by the macro for comparison:
/// ```rust
/// # use proptest::prelude::*;
/// # fn arb_byte() -> impl Strategy<Value = u8> { any::<u8>() } // Mock `arb_byte` for example to compile
/// proptest! {
///     #[test]
///     fn check_make_vector_consistency(size in arb_byte()) {
///         assert_elprop!["(make-vector {} ?A)", size];
///     }
/// }
/// ```
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
