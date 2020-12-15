use proc_macro::TokenStream;
use darling::FromMeta;
use quote::{quote, format_ident};
use syn;
use syn::{parse_macro_input, Error};

#[proc_macro_attribute]
pub fn lisp_fn(attr_ts: TokenStream, fn_ts: TokenStream) -> TokenStream {
    let attr_args = parse_macro_input!(attr_ts as syn::AttributeArgs);
    let function = parse_macro_input!(fn_ts as Function);

    let spec = match Spec::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => { return TokenStream::from(e.write_errors()); }
    };

    let body = function.body;
    let subr = function.name;
    let subr_name = subr.to_string();
    let struct_name = format_ident!("S{}", &subr_name);
    let func_name = format_ident!("F{}", &subr_name);
    let args = function.args.len();
    let name = match spec.name {
        Some(x) => x,
        None => subr_name,
    };
    let (required, optional) = match spec.required {
        None => (args as u16, 0),
        Some(req) => (req, args as u16 - req),
    };
    let rest = function.rest;
    let slice = if rest { quote! {&args[#args..]} } else { quote! {} };
    let indices: Vec<usize> = (0..args).collect();
    let tokens = quote!{
        #[allow(non_upper_case_globals)]
        const #struct_name: crate::lisp_object::BuiltInFn = crate::lisp_object::BuiltInFn{
            name: #name,
            subr: #func_name,
            args: crate::lisp_object::FnArgs {
                required: #required,
                optional: #optional,
                rest: #rest,
                max_stack_usage: 0,
                advice: false,
            }
        };

        #[allow(non_snake_case)]
        pub fn #func_name(args: &[crate::lisp_object::LispObj]) ->
            crate::lisp_object::LispObj {
            #subr(#(args[#indices]),* #slice)
        }

        #body
    };
    tokens.into()
}

struct Function {
    name: syn::Ident,
    body: syn::Item,
    args: Vec<syn::FnArg>,
    rest: bool,
}

impl<'a> syn::parse::Parse for Function {
    fn parse(input: syn::parse::ParseStream) -> Result<Self, Error> {
        let item: syn::Item = input.parse()?;
        parse_fn(item)
    }
}

fn parse_fn(item: syn::Item) -> Result<Function, Error> {
    match item {
        syn::Item::Fn(syn::ItemFn {ref sig, ..}) => {
            if sig.unsafety.is_some() {
                Err(Error::new_spanned(sig, "lisp functions cannot be `unsafe`"))
            } else if sig.constness.is_some() {
                Err(Error::new_spanned(sig, "lisp functions cannot be `const`"))
            } else {
                let mut args: Vec<syn::FnArg> = vec![];
                let mut rest = false;
                for x in sig.inputs.iter() {
                    if is_slice(x)? {
                        rest = true;
                        break;
                    } else {
                        args.push(x.clone());
                    }
                }
                Ok(Function{name: sig.ident.clone(), body: item, args, rest})
            }
        }
        _ => Err(Error::new_spanned(item, "`lisp_fn` attribute can only be used on functions"))
    }
}

fn is_slice(arg: &syn::FnArg) -> Result<bool, Error> {
    match arg {
        syn::FnArg::Receiver(x) => Err(Error::new_spanned(x, "Self is not valid in lisp functions")),
        syn::FnArg::Typed(pat_type) => {
            match pat_type.ty.as_ref() {
                syn::Type::Reference(x) => {
                    Ok(matches!(x.elem.as_ref(), syn::Type::Slice(_)))
                },
                _ => Ok(false)
            }
        }
    }
}

#[derive(Default, PartialEq, Debug, FromMeta)]
struct Spec {
    #[darling(default)]
    name: Option<String>,
    #[darling(default)]
    required: Option<u16>,
    #[darling(default)]
    intspec: Option<String>,
}

#[cfg(test)]
mod test {
    use super::*;
    use proc_macro2::TokenStream;
    #[test]
    fn test() {
        let stream: TokenStream = quote!{fn foo() {}}.into();
        let function: Function = syn::parse2(stream).unwrap();
        assert_eq!("foo", function.name.to_string());

        let stream: TokenStream = quote! {pub fn foo(vars: &[u8]) -> u8 {0}}.into();
        let function: Function = syn::parse2(stream).unwrap();
        assert!(function.rest);

        let stream: TokenStream = quote! {pub fn foo(var: u8) -> u8 {0}}.into();
        let function: Function = syn::parse2(stream).unwrap();
        assert!(!function.rest);

        let stream: TokenStream = quote! {pub fn foo(var0: u8, var1: u8, vars: &[u8]) -> u8 {0}}.into();
        let function: Function = syn::parse2(stream).unwrap();
        assert!(function.rest);
        assert_eq!(function.args.len(), 2);
    }
}
