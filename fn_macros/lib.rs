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

    expand(function, spec).into()
}

fn expand(function: Function, spec: Spec) -> proc_macro2::TokenStream {
    let body = function.body;
    let subr = function.name;
    let subr_name = subr.to_string();
    let struct_name = format_ident!("S{}", &subr_name);
    let func_name = format_ident!("F{}", &subr_name);
    let args = function.args.len() - if function.rest {1} else {0};
    let name = match spec.name {
        Some(x) => x,
        None => subr_name,
    };
    let (required, optional) = match spec.required {
        None => (args as u16, 0),
        Some(req) => (req, args as u16 - req),
    };
    let rest = function.rest;
    let arg_conversion = get_arg_conversion(function.args);
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
        pub fn #func_name(args: &[crate::lisp_object::LispObj]) -> Result<crate::lisp_object::LispObj, crate::error::Error> {
            Ok(#subr(#(#arg_conversion),*).into())
        }

        #body
    };
    tokens.into()
}

fn get_arg_conversion(args: Vec<syn::Type>) -> Vec<proc_macro2::TokenStream> {
    args.iter().enumerate().map(|(idx, ty)| {
        let call = get_call(idx, ty);
        if convert_type(ty) {
            quote! {std::convert::TryFrom::try_from(#call)?}
        } else {
            quote! {#call}
        }
    }).collect()
}

fn get_call(idx: usize, ty: &syn::Type) -> proc_macro2::TokenStream {
    match ty {
        syn::Type::Reference(refer) => {
            let elem = get_call(idx, refer.elem.as_ref());
            quote! {&#elem}
        }
        syn::Type::Slice(_) => quote! {args[#idx..]},
        _ => quote! {args[#idx]}
    }
}

fn convert_type(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Reference(refer) => convert_type(refer.elem.as_ref()),
        syn::Type::Path(path) => {
            "LispObj" != path.path.segments.last().unwrap().ident.to_string().as_str()
        }
        _ => false,
    }
}

struct Function {
    name: syn::Ident,
    body: syn::Item,
    args: Vec<syn::Type>,
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
                let (args, rest) = parse_signature(sig)?;
                Ok(Function{name: sig.ident.clone(), body: item, args, rest})
            }
        }
        _ => Err(Error::new_spanned(item, "`lisp_fn` attribute can only be used on functions"))
    }
}

fn parse_signature(sig: &syn::Signature) -> Result<(Vec<syn::Type>, bool), Error> {
    let mut args: Vec<syn::Type> = vec![];
    let mut rest = false;
    for input in sig.inputs.iter() {
        match input {
            syn::FnArg::Receiver(x) => return Err(Error::new_spanned(x, "Self is not valid in lisp functions")),
            syn::FnArg::Typed(pat_type) => {
                if is_slice(pat_type) {
                    rest = true;
                }
                args.push(pat_type.ty.as_ref().clone());
            }
        }
    }
    Ok((args, rest))
}

fn is_slice(arg: &syn::PatType) -> bool {
    match arg.ty.as_ref() {
        syn::Type::Reference(x) => matches!(x.elem.as_ref(), syn::Type::Slice(_)),
        _ => false
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

#[proc_macro]
pub fn concat_ident(input: TokenStream) -> TokenStream {
    let ident = {
        let mut iter = input.into_iter();
        let lhs: TokenStream = iter.next().unwrap().into();
        let rhs: TokenStream = iter.next().unwrap().into();
        concat_ident_impl(
            parse_macro_input!(lhs),
            parse_macro_input!(rhs),
        )
    };
    let result = quote!{#ident};
    result.into()
}

fn concat_ident_impl(lhs: syn::Ident, rhs: syn::Ident) -> syn::Ident {
    format_ident!("{}{}", lhs, rhs)
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test() {
        let stream = quote!{fn foo() {}}.into();
        let function: Function = syn::parse2(stream).unwrap();
        assert_eq!("foo", function.name.to_string());

        let stream = quote! {pub fn foo(vars: &[u8]) -> u8 {0}}.into();
        let function: Function = syn::parse2(stream).unwrap();
        assert!(function.rest);

        let stream = quote! {pub fn foo(var: u8) -> u8 {0}}.into();
        let function: Function = syn::parse2(stream).unwrap();
        assert!(!function.rest);

        let stream = quote! {pub fn foo(var0: u8, var1: u8, vars: &[u8]) -> u8 {0}}.into();
        let function: Function = syn::parse2(stream).unwrap();
        assert!(function.rest);
        assert_eq!(function.args.len(), 3);
    }

    #[test]
    fn concat() {
        let stream: proc_macro2::TokenStream = quote!{S add}.into();
        let mut iter = stream.into_iter();
        let lhs = syn::parse2(iter.next().unwrap().into()).unwrap();
        let rhs = syn::parse2(iter.next().unwrap().into()).unwrap();
        let ident = concat_ident_impl(lhs, rhs);
        assert_eq!("Sadd", ident.to_string());
    }

    #[test]
    fn test_expand() {
        // let stream = quote! {pub fn foo(var0: u8, var1: &u8) -> u8 {0}}.into();
        let stream = quote! {pub fn foo(var0: u8, var1: u8, vars: &[u8]) -> u8 {0}}.into();
        let function: Function = syn::parse2(stream).unwrap();
        let spec = Spec{name: Some("bar".into()), required: Some(1), intspec: None};
        let result = expand(function, spec);
        println!("{}", result.to_string());
    }
}
