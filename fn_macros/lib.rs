use darling::FromMeta;
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Error};

#[proc_macro_attribute]
pub fn defun(attr_ts: TokenStream, fn_ts: TokenStream) -> TokenStream {
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
    let symbol_name = format_ident!("{}", subr_name.to_ascii_uppercase());
    let lisp_name = spec.name.unwrap_or_else(|| map_function_name(subr_name));
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
    let subr_call =
        // Create the arena from a pointer to get around the issue that the
        // return val is bound to the mutable borrow, meaning we can use them
        // both in the into_obj function.
        quote! {
            let ptr = arena as *mut crate::core::arena::Arena;
            let val = #subr(#(#arg_conversion),*)#err;
            let arena: &'ob mut crate::core::arena::Arena = unsafe {&mut *ptr};
            Ok(crate::core::object::IntoObject::into_obj(val, arena).into())
        };

    quote! {
        #[doc(hidden)]
        #[allow(non_upper_case_globals)]
        const #struct_name: crate::core::object::SubrFn = crate::core::object::SubrFn {
            name: #lisp_name,
            subr: #func_name,
            args: crate::core::object::FnArgs {
                required: #required,
                optional: #optional,
                rest: #rest,
                advice: false,
            }
        };

        #[doc(hidden)]
        #[allow(non_snake_case)]
        pub(crate) fn #func_name<'ob, 'id>(
            args: &[crate::core::object::GcObj<'ob>],
            env: &crate::core::arena::Root<'id, crate::data::Environment>,
            arena: &'ob mut crate::core::arena::Arena,
            owner: &mut crate::core::arena::RootOwner<'id>,
        ) -> anyhow::Result<crate::core::object::GcObj<'ob>> {
            #subr_call
        }

        #[doc(hidden)]
        pub(crate) static #symbol_name: crate::core::symbol::GlobalSymbol = crate::core::symbol::GlobalSymbol::new(#lisp_name);

        #body
    }
}

enum ArgType {
    Arena,
    Env,
    Owner,
    Other,
}

fn get_arg_conversion(args: Vec<syn::Type>) -> Vec<proc_macro2::TokenStream> {
    args.iter()
        .enumerate()
        .map(|(idx, ty)| {
            match get_arg_type(ty) {
                ArgType::Arena => quote!{arena},
                ArgType::Env => quote!{env},
                ArgType::Owner => quote!{owner},
                ArgType::Other => {
                    let call = get_call(idx, ty);
                    if type_needs_conversion(ty) {
                        if is_slice(ty) {
                            quote! {crate::core::object::try_from_slice(#call)?}
                        } else {
                            quote! {std::convert::TryFrom::try_from(#call)?}
                        }
                    } else {
                        quote! {#call}
                    }
                }
            }
        })
        .collect()
}

fn get_arg_type(ty: &syn::Type) -> ArgType {
    match ty {
        syn::Type::Reference(refer) => match refer.elem.as_ref() {
            syn::Type::Path(path) => match get_path_ident_name(path).as_str() {
                "Arena" => ArgType::Arena,
                "RootOwner" => ArgType::Owner,
                "Root" => ArgType::Env,
                _ => ArgType::Other,
            }
            _ => ArgType::Other,
        }
        _ => ArgType::Other,
    }
}

fn get_call(idx: usize, ty: &syn::Type) -> proc_macro2::TokenStream {
    match ty {
        syn::Type::Reference(refer) => get_call(idx, refer.elem.as_ref()),
        syn::Type::Slice(_) => quote! {&args[#idx..]},
        _ => quote! {args[#idx]},
    }
}

fn type_needs_conversion(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Reference(refer) => type_needs_conversion(refer.elem.as_ref()),
        syn::Type::Slice(slice) => type_needs_conversion(slice.elem.as_ref()),
        syn::Type::Path(path) => "Object" != get_path_ident_name(path),
        _ => false,
    }
}

fn get_call_signature(args: &[syn::Type], spec_min: Option<u16>) -> (u16, u16, bool) {
    let min = match spec_min {
        Some(x) => x as usize,
        None => 0,
    };

    let args: Vec<_> = args
        .iter()
        .filter(|x| matches!(get_arg_type(x), ArgType::Other))
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

fn map_function_name(name: String) -> String {
    name.chars()
        .map(|c| match c {
            '_' => '-',
            c => c,
        })
        .collect()
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
            fn car<'ob, 'id>(list: &Cons<'ob>, env: &Root<'id, Environment>, owner: &RootOwner<'id>) -> Object<'ob> {
                list.car()
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
    }
}
