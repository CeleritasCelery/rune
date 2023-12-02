use darling::{ast::NestedMeta, Error, FromMeta};
use proc_macro::TokenStream;
use syn::parse_macro_input;

mod defun;
mod trace;

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

#[proc_macro_derive(Trace, attributes(no_trace))]
pub fn trace_derive(stream: TokenStream) -> TokenStream {
    let derived = parse_macro_input!(stream as syn::DeriveInput);
    trace::expand(&derived).into()
}
