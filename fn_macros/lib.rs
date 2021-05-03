use darling::FromMeta;
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Error};

#[proc_macro_attribute]
pub fn lisp_fn(attr_ts: TokenStream, fn_ts: TokenStream) -> TokenStream {
    let attr_args = parse_macro_input!(attr_ts as syn::AttributeArgs);
    let function = parse_macro_input!(fn_ts as Function);

    let spec = match Spec::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => {
            return TokenStream::from(e.write_errors());
        }
    };

    expand(function, spec).into()
}

fn expand(function: Function, spec: Spec) -> proc_macro2::TokenStream {
    let body = function.body;
    let subr = function.name;
    let subr_name = subr.to_string();
    let struct_name = format_ident!("S{}", &subr_name);
    let func_name = format_ident!("F{}", &subr_name);
    let name = match spec.name {
        Some(x) => x,
        None => subr_name,
    };
    let (required, optional, rest) = get_call_signature(&function.args, spec.required);
    let arg_conversion = get_arg_conversion(function.args);
    let returns_result = match function.output {
        syn::Type::Path(path) => "Result" == get_path_ident_name(&path),
        _ => false,
    };

    let err = if returns_result {
        quote! {?}
    } else {
        quote! {}
    };
    let subr_call = quote! {Ok(crate::lisp_object::IntoObject::into_obj(#subr(#(#arg_conversion),*)#err, arena))};

    quote! {
        #[allow(non_upper_case_globals)]
        const #struct_name: crate::lisp_object::SubrFn = crate::lisp_object::SubrFn {
            name: #name,
            subr: #func_name,
            args: crate::lisp_object::FnArgs {
                required: #required,
                optional: #optional,
                rest: #rest,
                max_stack_usage: 0_u16,
                advice: false,
            }
        };

        #[allow(non_snake_case)]
        pub fn #func_name<'obj>(
            args: &[crate::lisp_object::Object<'obj>],
            vars: &mut crate::hashmap::HashMap<crate::lisp_object::Symbol,
                                               crate::lisp_object::Object<'obj>>,
            arena: &'obj crate::arena::Arena,
        ) -> crate::error::Result<crate::lisp_object::Object<'obj>> {
            #subr_call
        }

        #body
    }
}

fn get_arg_conversion(args: Vec<syn::Type>) -> Vec<proc_macro2::TokenStream> {
    args.iter()
        .enumerate()
        .map(|(idx, ty)| {
            if is_var_hashmap(ty) {
                quote! {vars}
            } else if is_arena(ty) {
                quote! {arena}
            } else {
                let call = get_call(idx, ty);
                if convert_type(ty) {
                    if is_slice(ty) {
                        quote! {crate::lisp_object::try_from_slice(#call)?}
                    } else {
                        quote! {std::convert::TryFrom::try_from(#call)?}
                    }
                } else {
                    quote! {#call}
                }
            }
        })
        .collect()
}

fn is_var_hashmap(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Reference(refer) => match refer.elem.as_ref() {
            syn::Type::Path(path) => get_path_ident_name(path) == "HashMap",
            _ => false,
        },
        _ => false,
    }
}

fn is_arena(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Reference(refer) => match refer.elem.as_ref() {
            syn::Type::Path(path) => get_path_ident_name(path) == "Arena",
            _ => false,
        },
        _ => false,
    }
}

fn get_call(idx: usize, ty: &syn::Type) -> proc_macro2::TokenStream {
    match ty {
        syn::Type::Reference(refer) => {
            let elem = get_call(idx, refer.elem.as_ref());
            quote! {&#elem}
        }
        syn::Type::Slice(_) => quote! {args[#idx..]},
        _ => quote! {args[#idx]},
    }
}

fn convert_type(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Reference(refer) => convert_type(refer.elem.as_ref()),
        syn::Type::Slice(slice) => convert_type(slice.elem.as_ref()),
        syn::Type::Path(path) => "Object" != get_path_ident_name(path),
        _ => false,
    }
}

fn get_call_signature(args: &[syn::Type], spec_min: Option<u16>) -> (u16, u16, bool) {
    let min = match spec_min {
        Some(x) => x as usize,
        None => 0,
    };

    let args: Vec<&syn::Type> = args
        .iter()
        .filter(|x| !(is_var_hashmap(x) || is_arena(x)))
        .collect();

    let rest = match args.last() {
        Some(x) => is_slice(x),
        _ => false,
    };

    let required = {
        let last_req = args.iter().rposition(|x| match x {
            syn::Type::Path(path) => "Option" != get_path_ident_name(path),
            syn::Type::Reference(_) => !is_slice(x),
            _ => false,
        });
        match last_req {
            Some(x) => std::cmp::max(x + 1, min),
            None => 0,
        }
    };

    let len = args.len() - rest as usize;
    if min > len {
        panic!("`min` is larger then arguments provided");
    }

    let optional = len - required;

    (required as u16, optional as u16, rest)
}

fn is_slice(arg: &syn::Type) -> bool {
    match arg {
        syn::Type::Reference(x) => matches!(x.elem.as_ref(), syn::Type::Slice(_)),
        _ => false,
    }
}

fn get_path_ident_name(type_path: &syn::TypePath) -> String {
    type_path.path.segments.last().unwrap().ident.to_string()
}

struct Function {
    name: syn::Ident,
    body: syn::Item,
    args: Vec<syn::Type>,
    output: syn::Type,
}

impl<'a> syn::parse::Parse for Function {
    fn parse(input: syn::parse::ParseStream) -> Result<Self, Error> {
        let item: syn::Item = input.parse()?;
        parse_fn(item)
    }
}

fn parse_fn(item: syn::Item) -> Result<Function, Error> {
    match item {
        syn::Item::Fn(syn::ItemFn { ref sig, .. }) => {
            if sig.unsafety.is_some() {
                Err(Error::new_spanned(sig, "lisp functions cannot be `unsafe`"))
            } else if sig.constness.is_some() {
                Err(Error::new_spanned(sig, "lisp functions cannot be `const`"))
            } else {
                let (args, output) = parse_signature(sig)?;
                Ok(Function {
                    name: sig.ident.clone(),
                    body: item,
                    args,
                    output,
                })
            }
        }
        _ => Err(Error::new_spanned(
            item,
            "`lisp_fn` attribute can only be used on functions",
        )),
    }
}

fn parse_signature(sig: &syn::Signature) -> Result<(Vec<syn::Type>, syn::Type), Error> {
    let mut args: Vec<syn::Type> = vec![];
    for input in sig.inputs.iter() {
        match input {
            syn::FnArg::Receiver(x) => {
                return Err(Error::new_spanned(x, "Self is not valid in lisp functions"))
            }
            syn::FnArg::Typed(pat_type) => args.push(pat_type.ty.as_ref().clone()),
        }
    }
    Ok((args, get_signature_return_type(&sig.output)?))
}

fn get_signature_return_type(output: &syn::ReturnType) -> Result<syn::Type, Error> {
    match output {
        syn::ReturnType::Type(_, ty) => Ok(ty.as_ref().clone()),
        syn::ReturnType::Default => Err(Error::new_spanned(
            output,
            "Lisp Function must return a value",
        )),
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

    fn test_sig(stream: proc_macro2::TokenStream, min: Option<u16>, expect: (u16, u16, bool)) {
        let function: Function = syn::parse2(stream).unwrap();
        assert_eq!(expect, get_call_signature(&function.args, min));
    }

    #[test]
    fn sig() {
        test_sig(quote! {fn foo() -> u8 {}}, None, (0, 0, false));
        test_sig(quote! {fn foo(vars: &[u8]) -> u8 {0}}, None, (0, 0, true));
        test_sig(quote! {fn foo(var: u8) -> u8 {0}}, None, (1, 0, false));
        test_sig(
            quote! {fn foo(var0: u8, var1: u8, vars: &[u8]) -> u8 {0}},
            None,
            (2, 0, true),
        );
        test_sig(
            quote! {fn foo(var0: u8, var1: Option<u8>, vars: &[u8]) -> u8 {0}},
            None,
            (1, 1, true),
        );
        test_sig(
            quote! {fn foo(var0: u8, var1: Option<u8>, var2: Option<u8>) -> u8 {0}},
            Some(2),
            (2, 1, false),
        );
    }

    #[test]
    fn test_expand() {
        let stream = quote! {
            pub fn add<'obj>(vars: &[Number], arena: &'obj Arena) -> Number<'obj> {
                use std::ops::Add;
                vars.iter()
                    .fold(0.into(), |acc, x| {
                        NumberFold::acc(acc, x, Add::add, Add::add)
                    })
                    .into_number(arena)
            }
        };
        let function: Function = syn::parse2(stream).unwrap();
        let spec = Spec {
            name: Some("+".into()),
            required: None,
            intspec: None,
        };
        let result = expand(function, spec);
        println!("{}", result.to_string());
        panic!();
    }
}
