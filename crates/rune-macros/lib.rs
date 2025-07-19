//! # rune-macros
//!
//! `rune-macros` is the crate that generates two proc macros for use in other parts of the `rune` architecture:
//!
//! * [`macro@defun`]: Functions hydrated to emacs lisp.
//! * [Trace](`macro@Trace`): Implement Trace for a struct.
use darling::{Error, FromMeta, ast::NestedMeta};
use proc_macro::TokenStream;
use syn::parse_macro_input;

mod defun;
mod trace;
mod variantly;

/// ## `#[defun]`
///
/// Represents the functions that are going to be hydrated to emacs lisp, through the `rune` VM execution. As of
/// today, they are not documented with the GNU Emacs documentation, though definitely is a point of improvement.
/// Following Rust convention, `defun` names are written in `snake_case`, though if you search them in GNU Emacs,
/// you'll find them in `kebab-case`.
///
/// Arguments of the `defun`s follow the arguments on its corresponding documentation, or rather, definition on
/// the C Emacs core.
///
/// ### Examples
///
/// For the `make-vector` function, here's its signature:
///
/// > make-vector is a function defined in `alloc.c`. Signature `(make-vector LENGTH INIT)`
///
/// Its corresponding `rune` `#[defun]` signature would be:
///
/// ```ignore
/// #[defun]
/// fn make_vector(length: usize, init: GcObj) -> Vec<GcObj> {}
/// ```
///
/// The return object is interesting, as it's not so easily inferrable from the signature, but rather from documentation.
/// In this case, the `make-vector` defun returns a *newly created vector*.
#[proc_macro_attribute]
pub fn defun(attr_ts: TokenStream, fn_ts: TokenStream) -> TokenStream {
    let function = parse_macro_input!(fn_ts as defun::Function);
    match NestedMeta::parse_meta_list(attr_ts.into()) {
        Ok(args) => match defun::Spec::from_list(&args) {
            Ok(spec) => defun::expand(function, spec).into(),
            Err(e) => TokenStream::from(e.write_errors()),
        },
        Err(e) => TokenStream::from(Error::from(e).write_errors()),
    }
}

/// ## `Trace`
///
/// TODO: Document `Trace` macro.
#[proc_macro_derive(Trace, attributes(no_trace))]
pub fn trace_derive(stream: TokenStream) -> TokenStream {
    let derived = parse_macro_input!(stream as syn::DeriveInput);
    trace::expand(&derived).into()
}

/// The generated methods assume that `TargetType` provides an appropriate `untag()` method
/// (and `untag_mut()` if mutable access methods are used) that allows access to the
/// underlying enum (`MyEnum` in the example).
///
/// For methods that construct a new instance of `TargetType` (like `and_then`, `or_else`),
/// it's assumed that the enum variant (e.g., `MyEnum::Tuple(...)`) can be converted into
/// `TargetType` via `.into()`. This means `TargetType` must implement `From<MyEnum>`.
#[proc_macro_attribute]
pub fn enum_methods(attr: TokenStream, item: TokenStream) -> TokenStream {
    match variantly::variantly_attribute_macro_impl(attr.into(), item.into()) {
        Ok(ts) => ts.into(),
        Err(e) => e.into_compile_error(),
    }
}
