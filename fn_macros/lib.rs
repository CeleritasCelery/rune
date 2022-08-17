use darling::FromMeta;
use proc_macro::TokenStream;
use syn::parse_macro_input;

mod defun;
mod trace;
mod varname;

#[proc_macro_attribute]
pub fn defun(attr_ts: TokenStream, fn_ts: TokenStream) -> TokenStream {
    let attr_args = parse_macro_input!(attr_ts as syn::AttributeArgs);
    let function = parse_macro_input!(fn_ts as defun::Function);

    match defun::Spec::from_list(&attr_args) {
        Ok(spec) => defun::expand(function, spec).into(),
        Err(e) => TokenStream::from(e.write_errors()),
    }
}

#[proc_macro]
pub fn varname(stream: TokenStream) -> TokenStream {
    let ident = parse_macro_input!(stream as syn::Ident);
    varname::expand(ident).into()
}

#[proc_macro_derive(Trace)]
pub fn trace(stream: TokenStream) -> TokenStream {
    let derived = parse_macro_input!(stream as syn::DeriveInput);
    trace::expand(&derived).into()
}
