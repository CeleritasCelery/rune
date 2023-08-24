#![warn(rust_2018_idioms)]
#![allow(elided_lifetimes_in_paths)]
#![warn(
    clippy::all,
    clippy::as_ptr_cast_mut,
    clippy::equatable_if_let,
    clippy::nonstandard_macro_braces,
    clippy::or_fun_call,
    unused_qualifications,
    meta_variable_misuse,
    explicit_outlives_requirements,
    missing_copy_implementations,
    noop_method_call,
    semicolon_in_expressions_from_macros,
    trivial_numeric_casts,
    unreachable_pub,
    unused_lifetimes
)]
use darling::FromMeta;
use proc_macro::TokenStream;
use syn::parse_macro_input;

mod defun;
mod trace;

#[proc_macro_attribute]
pub fn defun(attr_ts: TokenStream, fn_ts: TokenStream) -> TokenStream {
    let attr_args = parse_macro_input!(attr_ts as syn::AttributeArgs);
    let function = parse_macro_input!(fn_ts as defun::Function);

    match defun::Spec::from_list(&attr_args) {
        Ok(spec) => defun::expand(function, spec).into(),
        Err(e) => TokenStream::from(e.write_errors()),
    }
}

#[proc_macro_derive(Trace, attributes(no_trace))]
pub fn trace_derive(stream: TokenStream) -> TokenStream {
    let derived = parse_macro_input!(stream as syn::DeriveInput);
    trace::expand(&derived).into()
}
