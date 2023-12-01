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
