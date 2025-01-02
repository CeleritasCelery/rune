//! # rune-macros
//!
//! `rune-macros` is the crate that generates two proc macros for use in other parts of the `rune` architecture:
//!
//! * [`macro@defun`]: Functions hydrated to emacs lisp.
//! * [Trace](`macro@Trace`): TODO
use darling::{ast::NestedMeta, Error, FromMeta};
use proc_macro::TokenStream;
use syn::parse_macro_input;

mod defun;
mod trace;

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

// dummy macro for elprop
#[proc_macro_attribute]
pub fn elprop(_: TokenStream, fn_ts: TokenStream) -> TokenStream {
    fn_ts
}
