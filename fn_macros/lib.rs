use darling::FromMeta;
use proc_macro::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{parse_macro_input, spanned::Spanned, Error};

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
        // both in the into_obj function. Similar to the rebind! macro.
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
            args: &[crate::core::arena::Rt<crate::core::object::GcObj<'static>>],
            env: &crate::core::arena::Root<'id, crate::core::env::Environment>,
            arena: &'ob mut crate::core::arena::Arena,
            owner: &mut crate::core::arena::RootOwner<'id>,
        ) -> anyhow::Result<crate::core::object::GcObj<'ob>> {
            #subr_call
        }

        #[doc(hidden)]
        pub(crate) static #symbol_name: crate::core::env::GlobalSymbol = crate::core::env::GlobalSymbol::new(#lisp_name);

        #body
    }
}

fn get_arg_conversion(args: Vec<(ArgType, syn::Type)>) -> Vec<proc_macro2::TokenStream> {
    let is_mut = args.iter().any(|(ty, _)| matches!(ty, ArgType::Arena(MUT)));
    args.iter()
        .enumerate()
        .map(|(idx, (arg_type, ty))| {match arg_type {
            ArgType::Arena(_) => quote! {arena},
            ArgType::Env => quote! {env},
            ArgType::Owner => quote! {owner},
            ArgType::Rt(gc) => match gc {
                Gc::Obj => quote! {&args[#idx]},
                Gc::Other => quote! {crate::core::arena::Rt::try_as(&args[#idx])?},
            },
            ArgType::Gc(gc) => {
                if is_mut {
                    quote_spanned! {
                        ty.span() => compile_error!("Can't have raw Gc pointer {ty} is function with mutable Arena");
                    }
                } else {
                    let bind = quote! {crate::core::arena::Rt::bind(&args[#idx], arena)};
                    match gc {
                        Gc::Obj => bind,
                        Gc::Other => quote! { std::convert::TryFrom::try_from(#bind)? },
                    }

                }
            },
            ArgType::Slice(gc) => {
                let bind = quote! {crate::core::arena::Rt::bind_slice(&args[#idx..], arena)};
                match gc {
                    Gc::Obj => bind,
                    Gc::Other => quote! {crate::core::object::try_from_slice(#bind)?},
                }
            },
            ArgType::SliceRt(gc) => {
                match gc {
                    Gc::Obj => quote! {&args[#idx..]},
                    Gc::Other => quote_spanned!{
                        ty.span() => compile_error!("Converting to {ty} is unimplemented")
                    },
                }
            },
            ArgType::Other => {
                if is_mut {
                    quote! { std::convert::TryFrom::try_from(&args[#idx])? }
                } else {
                    let bind = quote! {crate::core::arena::Rt::bind(&args[#idx], arena)};
                    quote! { std::convert::TryFrom::try_from(#bind)? }
                }
            },
        }})
        .collect()
}

fn get_call_signature(args: &[(ArgType, syn::Type)], spec_min: Option<u16>) -> (u16, u16, bool) {
    let min = match spec_min {
        Some(x) => x as usize,
        None => 0,
    };

    let args: Vec<_> = args
        .iter()
        .filter(|(x, _)| !x.is_supporting_arg())
        .collect();

    let rest = match args.last() {
        Some((_, x)) => is_slice(x),
        _ => false,
    };

    let required = {
        let last_req = args.iter().rposition(|(_, x)| match x {
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
    args: Vec<(ArgType, syn::Type)>,
    output: syn::Type,
}

impl syn::parse::Parse for Function {
    fn parse(input: syn::parse::ParseStream) -> Result<Self, Error> {
        let item: syn::Item = input.parse()?;
        parse_fn(item)
    }
}

const MUT: bool = true;

#[derive(PartialEq, Debug, Copy, Clone)]
enum Gc {
    Obj,
    Other,
}

#[derive(PartialEq, Debug, Copy, Clone)]
enum ArgType {
    Arena(bool),
    Env,
    Owner,
    Rt(Gc),
    Gc(Gc),
    Slice(Gc),
    SliceRt(Gc),
    Other,
}

impl ArgType {
    fn is_supporting_arg(&self) -> bool {
        matches!(self, ArgType::Arena(_) | ArgType::Owner | ArgType::Env)
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

fn parse_signature(sig: &syn::Signature) -> Result<(Vec<(ArgType, syn::Type)>, syn::Type), Error> {
    let mut args = Vec::new();
    for input in sig.inputs.iter() {
        match input {
            syn::FnArg::Receiver(x) => {
                return Err(Error::new_spanned(x, "Self is not valid in lisp functions"))
            }
            syn::FnArg::Typed(pat_type) => {
                let ty = pat_type.ty.as_ref().clone();
                let arg = (get_arg_type(&ty)?, ty);
                args.push(arg)
            }
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

fn get_arg_type(ty: &syn::Type) -> Result<ArgType, Error> {
    Ok(match ty {
        syn::Type::Reference(syn::TypeReference {
            elem, mutability, ..
        }) => match elem.as_ref() {
            syn::Type::Path(path) => match get_path_ident_name(path).as_str() {
                "Arena" => ArgType::Arena(mutability.is_some()),
                "RootOwner" => ArgType::Owner,
                "Root" => ArgType::Env,
                "Rt" => get_rt_type(path)?,
                _ => ArgType::Other,
            },
            syn::Type::Slice(slice) => match get_arg_type(slice.elem.as_ref())? {
                ArgType::Rt(rt) => ArgType::SliceRt(rt),
                ArgType::Gc(gc) => ArgType::Slice(gc),
                _ => ArgType::Slice(Gc::Other),
            },
            _ => ArgType::Other,
        },
        syn::Type::Path(path) => {
            if get_path_ident_name(path) == "Rt" {
                get_rt_type(path)?
            } else {
                get_object_type(path)
            }
        }
        _ => ArgType::Other,
    })
}

fn get_object_type(type_path: &syn::TypePath) -> ArgType {
    let outer_type = type_path.path.segments.last().unwrap();
    if outer_type.ident == "GcObj" {
        ArgType::Gc(Gc::Obj)
    } else if outer_type.ident == "Gc" {
        let inner = match get_generic_param(outer_type) {
            Some(generic) if get_path_ident_name(generic) == "Object" => Gc::Obj,
            _ => Gc::Other,
        };
        ArgType::Gc(inner)
    } else {
        ArgType::Other
    }
}

fn get_rt_type(type_path: &syn::TypePath) -> Result<ArgType, Error> {
    let segment = type_path.path.segments.last().unwrap();
    match get_generic_param(segment) {
        Some(inner) => match get_object_type(inner) {
            ArgType::Gc(gc) => Ok(ArgType::Rt(gc)),
            _ => Err(Error::new_spanned(inner, "Found Rt of non-Gc type")),
        },
        None => Ok(ArgType::Other),
    }
}

fn get_generic_param(outer_type: &syn::PathSegment) -> Option<&syn::TypePath> {
    match &outer_type.arguments {
        syn::PathArguments::AngleBracketed(generic) => match generic.args.first().unwrap() {
            syn::GenericArgument::Type(syn::Type::Path(path)) => Some(path),
            _ => None,
        },
        _ => None,
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
    use std::iter::zip;

    use super::*;

    fn test_sig(stream: proc_macro2::TokenStream, min: Option<u16>, expect: (u16, u16, bool)) {
        let function: Function = syn::parse2(stream).unwrap();
        assert_eq!(get_call_signature(&function.args, min), expect);
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

    fn test_args(args: proc_macro2::TokenStream, expect: &[ArgType]) {
        let stream = quote! {fn foo(#args) -> u8 {0}};
        let function: Function = syn::parse2(stream).unwrap();
        let iter = zip(function.args, expect);
        for (cmp, exp) in iter {
            assert_eq!(cmp.0, *exp);
        }
    }

    #[test]
    fn test_arguments() {
        test_args(quote! {x: Gc<Object>}, &[ArgType::Gc(Gc::Obj)]);
        test_args(quote! {x: GcObj}, &[ArgType::Gc(Gc::Obj)]);
        test_args(quote! {x: Gc<T>}, &[ArgType::Gc(Gc::Other)]);
        test_args(quote! {x: &Rt<GcObj>}, &[ArgType::Rt(Gc::Obj)]);
        test_args(quote! {x: &Rt<Gc<Object>>}, &[ArgType::Rt(Gc::Obj)]);
        test_args(quote! {x: &Rt<Gc<T>>}, &[ArgType::Rt(Gc::Other)]);
        test_args(quote! {x: u8}, &[ArgType::Other]);
        test_args(quote! {x: Option<u8>}, &[ArgType::Other]);
        test_args(quote! {x: &[GcObj]}, &[ArgType::Slice(Gc::Obj)]);
        test_args(quote! {x: &[Gc<T>]}, &[ArgType::Slice(Gc::Other)]);
        test_args(quote! {x: &[Gc<T>]}, &[ArgType::Slice(Gc::Other)]);
        test_args(quote! {x: &[u8]}, &[ArgType::Slice(Gc::Other)]);
        test_args(quote! {x: &[Rt<GcObj>]}, &[ArgType::SliceRt(Gc::Obj)]);
        test_args(quote! {x: &[Rt<Gc<T>>]}, &[ArgType::SliceRt(Gc::Other)]);
        test_args(quote! {x: &mut Arena}, &[ArgType::Arena(MUT)]);
        test_args(quote! {x: &Arena}, &[ArgType::Arena(false)]);
        test_args(quote! {x: &Root<'id, Environment>}, &[ArgType::Env]);
        test_args(quote! {x: &RootOwner}, &[ArgType::Owner]);
        test_args(
            quote! {x: u8, s: &[Rt<GcObj>], y: &Arena, z: &Root<'id, Environment>, u: &RootOwner},
            &[
                ArgType::Other,
                ArgType::SliceRt(Gc::Obj),
                ArgType::Arena(false),
                ArgType::Env,
                ArgType::Owner,
            ],
        );
    }

    #[test]
    fn test_expand() {
        let stream = quote! {
            fn car<'ob>(list: Gc<List>, arena: &'ob Arena) -> GcObj<'ob> {
                match list.get() {
                    List::Cons(cons) => cons.car(arena),
                    List::Nil => GcObj::NIL,
                }
            }
        };
        let function: Function = syn::parse2(stream).unwrap();
        let spec = Spec {
            name: Some("+".into()),
            required: None,
            intspec: None,
        };
        let result = expand(function, spec);
        println!("{}", result);
    }
}
