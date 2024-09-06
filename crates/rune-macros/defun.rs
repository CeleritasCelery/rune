#![allow(clippy::manual_unwrap_or_default)]
use darling::FromMeta;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Error;

pub(crate) fn expand(function: Function, spec: Spec) -> TokenStream {
    if let Some(required) = spec.required {
        let actual_required = function.args.iter().filter(|x| x.is_positional_arg()).count();
        if required as usize > actual_required {
            return quote! { compile_error!("Spec `required` is larger then the number of arguments provided"); };
        }
    }

    let body = function.body;
    let subr = function.name;
    let subr_name = subr.to_string();
    let struct_name = format_ident!("__subr_{}", &subr_name);
    let func_name = format_ident!("__wrapper_fn_{}", &subr_name);
    let lisp_name = spec.name.unwrap_or_else(|| map_function_name(&subr_name));
    let (required, optional, rest) = parse_call_signature(&function.args, spec.required);

    let arg_conversion = get_arg_conversion(&function.args);

    let create_args = if !function.args.iter().any(|x| matches!(x, ArgType::Env(MUT))) {
        // If mut Env is not needed, then we can just pass a slice from the
        // stack directly. This is the cheapest option
        quote! { let args = &env.stack[..arg_cnt]; }
    } else if !rest || function.args.iter().any(|x| matches!(x, ArgType::ArgSlice)) {
        // If number of arguments are known, we can allocate them on the stack
        // as an array to avoid allocation. This is the purpose of the ArgSlice
        // type, to give us a know set of arguments when the callee is expecting
        // a slice.
        let positional_args = (required + optional) as usize;
        quote! {
            let init: [crate::core::gc::Slot<crate::core::object::Object>; #positional_args] = core::array::from_fn(|x| crate::core::gc::Slot::new(crate::core::object::NIL));
            rune_core::macros::root!(args, init(init), cx);
            let pos_count = std::cmp::min(arg_cnt, #positional_args);
            let stack_slice = &env.stack[..arg_cnt];
            for i in 0..pos_count {
                args[i].set(&stack_slice[i]);
            }
            let args = &args[..pos_count];
        }
    } else {
        // If the function requires a mutable env and arguments are unbounded,
        // we should use the ArgSlice type to avoid allocating
        quote! { compile_error!("Can't use an argument slice with a mutable enviroment. Use `ArgSlice` instead."); }
    };

    let err = if function.fallible {
        quote! {?}
    } else {
        quote! {}
    };

    // Create the context from a pointer to get around the issue that the
    // return val is bound to the mutable borrow, meaning we can use them
    // both in the into_obj function. Similar to the rebind! macro.
    let subr_call = quote! {
        let ptr = cx as *mut crate::core::gc::Context;
        let val = #subr(#(#arg_conversion),*)#err;
        let cx: &'ob mut crate::core::gc::Context = unsafe {&mut *ptr};
        Ok(crate::core::object::IntoObject::into_obj(val, cx).into())
    };

    let arg_count_guard = match (required as usize, optional as usize, rest) {
        (r, 0, false) => quote! {arg_cnt != #r},
        (0, o, false) => quote! {#o < arg_cnt},
        (r, o, false) => quote! {arg_cnt < #r || #r + #o < arg_cnt},
        (r, _, true) => quote! {arg_cnt < #r},
    };

    quote! {
        #[automatically_derived]
        #[doc(hidden)]
        fn #func_name<'ob>(
            arg_cnt: usize,
            env: &mut crate::core::gc::Rt<crate::core::env::Env>,
            cx: &'ob mut crate::core::gc::Context,
        ) -> anyhow::Result<crate::core::object::Object<'ob>> {
            #[allow(clippy::manual_range_contains)]
            if #arg_count_guard {
                let upper = #required + #optional;
                let expected = if arg_cnt > upper as usize {upper} else {#required};
                return Err(crate::data::arg_error(#lisp_name, expected, arg_cnt as u16, cx).into());
            }
            #create_args
            #subr_call
        }

        #[automatically_derived]
        #[doc(hidden)]
        #[allow(non_upper_case_globals)]
        pub(crate) const #struct_name: crate::core::object::SubrFn = crate::core::object::SubrFn {
            name: #lisp_name,
            subr: #func_name,
            args: crate::core::object::FnArgs {
                required: #required,
                optional: #optional,
                rest: #rest,
                advice: false,
            }
        };

        #body
    }
}

fn get_arg_conversion(args: &[ArgType]) -> Vec<TokenStream> {
    let is_mut = args.iter().any(|ty| matches!(ty, ArgType::Context(MUT)));
    args.iter()
        .enumerate()
        .map(|(idx, arg_type)| match arg_type {
            ArgType::Context(_) => quote! {cx},
            ArgType::Env(_) => quote! {env},
            // Rt<Gc<..>>
            ArgType::Rt(gc) => match gc {
                Gc::Obj => quote! {&args[#idx]},
                Gc::Other => quote! {args[#idx].try_as()?},
            },
            // Gc<..>
            ArgType::Gc(gc) => {
                let bind = quote! {args[#idx].bind(cx)};
                match gc {
                    Gc::Obj => bind,
                    Gc::Other => quote! { std::convert::TryFrom::try_from(#bind)? },
                }
            }
            // &[Gc<..>]
            ArgType::Slice(gc) => {
                let bind =
                    quote! {crate::core::gc::Rt::bind_slice(&args[(#idx).min(args.len())..], cx)};
                match gc {
                    Gc::Obj => bind,
                    Gc::Other => quote! {crate::core::object::try_from_slice(#bind)?},
                }
            }
            // &[Rt<Gc<..>>]
            ArgType::SliceRt(gc) => match gc {
                Gc::Obj => quote! {&args[(#idx).min(args.len())..]},
                Gc::Other => unreachable!(),
            },
            // ArgSlice
            ArgType::ArgSlice => {
                let positional = args.iter().filter(|x| x.is_positional_arg()).count();
                quote! {crate::core::env::ArgSlice::new(arg_cnt.saturating_sub(#positional))}
            }
            // Option<Rt<Gc<..>>>
            ArgType::OptionRt => {
                quote! {
                    match args.get(#idx) {
                        Some(x) => crate::core::gc::Rt::try_as_option(x)?,
                        None => None,
                    }
                }
            }
            // Option<T>
            ArgType::Option => {
                let bind = quote! {x.bind(cx)};
                quote! {
                    match args.get(#idx) {
                        Some(x) => crate::core::object::Gc::try_from_option(#bind)?,
                        None => None,
                    }
                }
            }
            ArgType::Other => {
                if is_mut {
                    quote! { std::convert::TryFrom::try_from(&args[#idx])? }
                } else {
                    let bind = quote! {args[#idx].bind(cx)};
                    quote! { std::convert::TryFrom::try_from(#bind)? }
                }
            }
        })
        .collect()
}

fn parse_call_signature(args: &[ArgType], spec_required: Option<u16>) -> (u16, u16, bool) {
    let required = {
        let actual_required = args.iter().filter(|x| x.is_required_arg()).count();
        let spec_required = match spec_required {
            Some(x) => x as usize,
            None => 0,
        };
        std::cmp::max(actual_required, spec_required)
    };

    let optional = {
        let pos_args = args.iter().filter(|x| x.is_positional_arg()).count();
        pos_args - required
    };

    let rest = args.iter().any(|x| x.is_rest_arg());

    let required = u16::try_from(required).unwrap();
    let optional = u16::try_from(optional).unwrap();
    (required, optional, rest)
}

fn get_path_ident_name(type_path: &syn::TypePath) -> String {
    type_path.path.segments.last().unwrap().ident.to_string()
}

fn map_function_name(name: &str) -> String {
    name.replace('_', "-")
}

const MUT: bool = true;

#[derive(PartialEq, Debug, Copy, Clone)]
enum Gc {
    Obj,
    Other,
}

#[derive(PartialEq, Debug, Copy, Clone)]
enum ArgType {
    Context(bool),
    Env(bool),
    Rt(Gc),
    Gc(Gc),
    Slice(Gc),
    SliceRt(Gc),
    ArgSlice,
    Option,
    OptionRt,
    Other,
}

impl ArgType {
    fn is_required_arg(self) -> bool {
        use ArgType as A;
        matches!(self, A::Rt(_) | A::Gc(_) | A::Other)
    }

    fn is_positional_arg(self) -> bool {
        use ArgType as A;
        matches!(self, A::Rt(_) | A::Gc(_) | A::Other | A::Option | A::OptionRt)
    }

    fn is_rest_arg(self) -> bool {
        use ArgType as A;
        matches!(self, A::SliceRt(_) | A::Slice(_) | A::ArgSlice)
    }
}

pub(crate) struct Function {
    name: syn::Ident,
    body: syn::Item,
    args: Vec<ArgType>,
    fallible: bool,
}

impl syn::parse::Parse for Function {
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
                let args = parse_signature(sig)?;
                check_invariants(&args, sig)?;
                let fallible = return_type_is_result(&sig.output);
                Ok(Function { name: sig.ident.clone(), body: item, args, fallible })
            }
        }
        _ => Err(Error::new_spanned(item, "`lisp_fn` attribute can only be used on functions")),
    }
}

fn check_invariants(args: &[ArgType], sig: &syn::Signature) -> Result<(), Error> {
    let is_mut = args.iter().any(|x| matches!(x, ArgType::Context(MUT)));
    if is_mut {
        let mut iter = sig.inputs.iter().zip(args.iter());
        if let Some((arg, _)) = iter.find(|(_, ty)| matches!(ty, ArgType::Gc(_))) {
            return Err(Error::new_spanned(
                arg,
                "Can't have raw Gc pointer in function with mutable Context",
            ));
        }
    }
    let mut iter = sig.inputs.iter().zip(args.iter());
    if let Some((arg, _)) = iter.find(|(_, ty)| matches!(ty, ArgType::SliceRt(Gc::Other))) {
        return Err(Error::new_spanned(arg, "Converting an Rt slice to is unimplemented"));
    }

    for (arg, ty) in sig.inputs.iter().zip(args.iter()) {
        if matches!(ty, ArgType::Rt(_)) {
            if let syn::FnArg::Typed(ty) = arg {
                if !matches!(ty.ty.as_ref(), syn::Type::Reference(_)) {
                    return Err(Error::new_spanned(arg, "Can't take Rt by value"));
                }
            }
        }
    }

    let rest_args = args.iter().filter(|x| x.is_rest_arg()).count();
    if rest_args > 1 {
        return Err(Error::new_spanned(sig, "Found duplicate argument slice in signature"));
    }

    let first_opt = args.iter().position(|x| matches!(x, ArgType::Option));
    let last_required = args.iter().rposition(|x| x.is_required_arg()).unwrap_or_default();
    if let Some(first_optional) = first_opt {
        if last_required > first_optional {
            let arg = sig.inputs.iter().nth(last_required).unwrap();
            return Err(Error::new_spanned(
                arg,
                "Required argument is after the first optional argument",
            ));
        }
    }
    Ok(())
}

fn parse_signature(sig: &syn::Signature) -> Result<Vec<ArgType>, Error> {
    let mut args = Vec::new();
    for input in &sig.inputs {
        match input {
            syn::FnArg::Receiver(x) => {
                return Err(Error::new_spanned(x, "Self is not valid in lisp functions"))
            }
            syn::FnArg::Typed(pat_type) => {
                let ty = pat_type.ty.as_ref().clone();
                let arg = get_arg_type(&ty)?;
                args.push(arg);
            }
        }
    }
    Ok(args)
}

fn return_type_is_result(output: &syn::ReturnType) -> bool {
    match output {
        syn::ReturnType::Type(_, ty) => match ty.as_ref() {
            syn::Type::Path(path) => get_path_ident_name(path) == "Result",
            _ => false,
        },
        syn::ReturnType::Default => false,
    }
}

fn get_arg_type(ty: &syn::Type) -> Result<ArgType, Error> {
    Ok(match ty {
        syn::Type::Reference(syn::TypeReference { elem, mutability, .. }) => match elem.as_ref() {
            syn::Type::Path(path) => match &*get_path_ident_name(path) {
                "Context" => ArgType::Context(mutability.is_some()),
                "Rt" | "Rto" => get_rt_type(path, mutability.is_some())?,
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
            let name = get_path_ident_name(path);
            match &*name {
                "ArgSlice" => ArgType::ArgSlice,
                "Rt" | "Rto" => get_rt_type(path, false)?,
                "Option" => {
                    let outer = path.path.segments.last().unwrap();
                    match get_generic_param(outer) {
                        Some(syn::Type::Reference(inner)) => match inner.elem.as_ref() {
                            syn::Type::Path(path) => match &*get_path_ident_name(path) {
                                "Rt" | "Rto" => ArgType::OptionRt,
                                _ => ArgType::Option,
                            },
                            _ => ArgType::Option,
                        },
                        _ => ArgType::Option,
                    }
                }
                "OptionalFlag" => ArgType::Option,
                _ => get_object_type(path),
            }
        }
        _ => ArgType::Other,
    })
}

fn get_object_type(type_path: &syn::TypePath) -> ArgType {
    let outer_type = type_path.path.segments.last().unwrap();
    if outer_type.ident == "Object" {
        ArgType::Gc(Gc::Obj)
    } else if outer_type.ident == "Function"
        || outer_type.ident == "Number"
        || outer_type.ident == "List"
    {
        ArgType::Gc(Gc::Other)
    } else if outer_type.ident == "Slot" {
        match get_generic_param(outer_type) {
            Some(syn::Type::Path(inner)) => get_object_type(inner),
            _ => ArgType::Gc(Gc::Other),
        }
    } else if outer_type.ident == "Gc" {
        let inner = match get_generic_param(outer_type) {
            Some(syn::Type::Path(generic)) if get_path_ident_name(generic) == "Object" => Gc::Obj,
            _ => Gc::Other,
        };
        ArgType::Gc(inner)
    } else if outer_type.ident == "Env" {
        ArgType::Env(false)
    } else {
        ArgType::Other
    }
}

fn get_rt_type(type_path: &syn::TypePath, mutable: bool) -> Result<ArgType, Error> {
    let segment = type_path.path.segments.last().unwrap();
    match get_generic_param(segment) {
        Some(syn::Type::Path(inner)) => match get_object_type(inner) {
            ArgType::Gc(gc) => Ok(ArgType::Rt(gc)),
            ArgType::Env(_) => Ok(ArgType::Env(mutable)),
            _ => Err(Error::new_spanned(inner, "Found Rt of non-Gc type")),
        },
        _ => Ok(ArgType::Other),
    }
}

fn get_generic_param(outer_type: &syn::PathSegment) -> Option<&syn::Type> {
    match &outer_type.arguments {
        syn::PathArguments::AngleBracketed(generic) => match generic.args.first().unwrap() {
            syn::GenericArgument::Type(ty) => Some(ty),
            _ => None,
        },
        _ => None,
    }
}

#[derive(Default, PartialEq, Debug, FromMeta)]
pub(crate) struct Spec {
    #[darling(default)]
    name: Option<String>,
    #[darling(default)]
    required: Option<u16>,
}

#[cfg(test)]
mod test {
    use super::*;

    fn test_sig(stream: TokenStream, min: Option<u16>, expect: (u16, u16, bool)) {
        let function: Function = syn::parse2(stream).unwrap();
        let sig = parse_call_signature(&function.args, min);
        assert_eq!(sig, expect);
    }

    #[test]
    fn sig() {
        test_sig(quote! {fn foo() -> u8 {}}, None, (0, 0, false));
        test_sig(quote! {fn foo(vars: &[u8]) -> u8 {0}}, None, (0, 0, true));
        test_sig(quote! {fn foo(var: u8) -> u8 {0}}, None, (1, 0, false));
        test_sig(quote! {fn foo(var0: u8, var1: u8, vars: &[u8]) -> u8 {0}}, None, (2, 0, true));
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
        test_sig(
            quote! { fn foo(a: &Rt<Slot<Gc<foo>>>, b: &[Rt<Slot<Object>>], env: &Rt<Env>, cx: &mut Context) -> u8 {0} },
            None,
            (1, 0, true),
        );
        test_sig(
            quote! { fn foo(env: &Rt<Env>, a: &Rt<Slot<Gc<foo>>>, x: Option<u8>, cx: &mut Context, b: &[Rt<Slot<Object>>]) -> u8 {0} },
            None,
            (1, 1, true),
        );
    }

    #[expect(clippy::needless_pass_by_value)]
    fn test_args(args: TokenStream, expect: &[ArgType]) {
        let stream = quote! {fn foo(#args) -> u8 {0}};
        let function: Function = syn::parse2(stream).unwrap();
        let iter = std::iter::zip(function.args, expect);
        for (cmp, exp) in iter {
            assert_eq!(cmp, *exp, "input: `{args}`");
        }
    }

    #[test]
    fn test_arguments() {
        test_args(quote! {x: Object}, &[ArgType::Gc(Gc::Obj)]);
        test_args(quote! {x: Gc<T>}, &[ArgType::Gc(Gc::Other)]);
        test_args(quote! {x: &Rt<Slot<Object>>}, &[ArgType::Rt(Gc::Obj)]);
        test_args(quote! {x: &Rt<Slot<Gc<Object>>>}, &[ArgType::Rt(Gc::Obj)]);
        test_args(quote! {x: &Rt<Slot<Gc<T>>>}, &[ArgType::Rt(Gc::Other)]);
        test_args(quote! {x: &Rto<Object>}, &[ArgType::Rt(Gc::Obj)]);
        test_args(quote! {x: &Rto<Gc<Object>>}, &[ArgType::Rt(Gc::Obj)]);
        test_args(quote! {x: &Rto<Gc<T>>}, &[ArgType::Rt(Gc::Other)]);
        test_args(quote! {x: u8}, &[ArgType::Other]);
        test_args(quote! {x: Option<u8>}, &[ArgType::Option]);
        test_args(quote! {x: Option<()>}, &[ArgType::Option]);
        test_args(quote! {x: OptionalFlag}, &[ArgType::Option]);
        test_args(quote! {x: Option<&Rt<Slot<Object>>>}, &[ArgType::OptionRt]);
        test_args(quote! {x: Option<&Rto<Object>>}, &[ArgType::OptionRt]);
        test_args(quote! {x: &[Object]}, &[ArgType::Slice(Gc::Obj)]);
        test_args(quote! {x: &[Gc<T>]}, &[ArgType::Slice(Gc::Other)]);
        test_args(quote! {x: &[Gc<T>]}, &[ArgType::Slice(Gc::Other)]);
        test_args(quote! {x: &[u8]}, &[ArgType::Slice(Gc::Other)]);
        test_args(quote! {x: ArgSlice}, &[ArgType::ArgSlice]);
        test_args(quote! {x: &[Rt<Slot<Object>>]}, &[ArgType::SliceRt(Gc::Obj)]);
        test_args(quote! {x: &[Rto<Object>]}, &[ArgType::SliceRt(Gc::Obj)]);
        test_args(quote! {x: &mut Context}, &[ArgType::Context(MUT)]);
        test_args(quote! {x: &Context}, &[ArgType::Context(false)]);
        test_args(quote! {x: &Rt<Env>}, &[ArgType::Env(false)]);
        test_args(quote! {x: &mut Rt<Env>}, &[ArgType::Env(MUT)]);
        test_args(
            quote! {x: u8, s: &[Rt<Slot<Object>>], y: &Context, z: &Rt<Env>},
            &[
                ArgType::Other,
                ArgType::SliceRt(Gc::Obj),
                ArgType::Context(false),
                ArgType::Env(false),
            ],
        );

        test_args(
            quote! {x: u8, s: &[Rto<Object>], y: &Context, z: &Rt<Env>},
            &[
                ArgType::Other,
                ArgType::SliceRt(Gc::Obj),
                ArgType::Context(false),
                ArgType::Env(false),
            ],
        );
    }

    fn check_error(stream: TokenStream) {
        println!("signature: {stream}");
        let function: Result<Function, _> = syn::parse2(stream);
        assert!(function.is_err());
    }

    #[test]
    fn test_error() {
        check_error(quote! {fn foo(a: Object, a: &mut Context) {}});
        check_error(quote! {fn foo(a: &[Rt<T>]) {}});
        check_error(quote! {fn foo(a: Rt<Slot<Object>>) {}});
        check_error(quote! {fn foo(a: u8, b: &[Object], c: &[Object]) {}});
        check_error(quote! {fn foo(a: u8, b: Option<u8>, c: u8) {}});
    }

    #[test]
    fn test_expand() {
        let stream = quote! {
            fn car<'ob>(list: Gc<List>, cx: &'ob Context) -> Object<'ob> {
                match list.get() {
                    List::Cons(cons) => cons.car(),
                    List::Nil => NIL,
                }
            }
        };
        let function: Function = syn::parse2(stream).unwrap();
        let spec = Spec { name: Some("+".into()), ..Default::default() };
        let result = expand(function, spec);
        println!("{result}");
    }
}
