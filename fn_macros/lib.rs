use proc_macro::TokenStream;
use quote::quote;
use syn;

#[proc_macro_attribute]
pub fn lisp_fn(attr_ts: TokenStream, fn_ts: TokenStream) -> TokenStream {
    // let fn_item = syn::parse(fn_ts.clone()).unwrap();
    // let attr = syn::parse_macro_input!(attr_ts as syn::AttributeArgs);
    // let lisp_fn_name = parse_attr(attr).unwrap();
    // let function = parse_fn(&fn_item).unwrap();

    // let tokens = quote! {
    //     let x = 5;
    // };

    fn_ts
    // let mut cargs = proc_macro2::TokenStream::new();
    // let mut rargs = proc_macro2::TokenStream::new();
    // let mut body = proc_macro2::TokenStream::new();

    // let cname = lisp_fn_args.c_name;
    // let sname = concat_idents("S", &cname);
    // let fname = concat_idents("F", &cname);
    // let rname = function.name;
    // let min_args = lisp_fn_args.min;
    // let mut windows_header = quote! {};

    // let max_args = if lisp_fn_args.unevalled {
    //     quote! { -1 }
    // } else {
    //     match function.fntype {
    //         function::LispFnType::Normal(_) => quote! { #max_args },
    //         function::LispFnType::Many => quote! { crate::lisp::MANY  },
    //     }
    // };
    // let symbol_name = CByteLiteral(&lisp_fn_args.name);

    // if cfg!(windows) {
    //     windows_header = quote! {
    //         | (std::mem::size_of::<crate::remacs_sys::Lisp_Subr>()
    //            / std::mem::size_of::<crate::remacs_sys::EmacsInt>()) as libc::ptrdiff_t
    //     };
    // }

    // let tokens = quote! {
    //     #[no_mangle]
    //     #[allow(clippy::not_unsafe_ptr_arg_deref)]
    //     #[allow(clippy::transmute_ptr_to_ptr)]
    //     #[allow(clippy::diverging_sub_expression)]
    //     pub extern "C" fn #fname(#cargs) -> crate::lisp::LispObject {
    //         #body

    //         let ret = #rname(#rargs);
    //         #[allow(unreachable_code)]
    //         crate::lisp::LispObject::from(ret)
    //     }
    // };

    // // we could put #fn_item into the quoted code above, but doing so
    // // drops all of the line numbers on the floor and causes the
    // // compiler to attribute any errors in the function to the macro
    // // invocation instead.
    // let tokens: TokenStream = tokens.into();
    // tokens.into_iter().chain(fn_ts.into_iter()).collect()
}

// struct CByteLiteral<'a>(&'a str);

// impl<'a> quote::ToTokens for CByteLiteral<'a> {
//     fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
//         let s = RE.replace_all(self.0, |caps: &regex::Captures| {
//             format!("\\x{:x}", u32::from(caps[0].chars().next().unwrap()))
//         });
//         let identifier = format!(r#"b"{}\0""#, s);
//         let expr = syn::parse_str::<syn::Expr>(&identifier).unwrap();
//         tokens.extend(quote! { #expr });
//     }
// }

// fn concat_idents(lhs: &str, rhs: &str) -> syn::Ident {
//     syn::Ident::new(
//         format!("{}{}", lhs, rhs).as_str(),
//         quote::__rt::Span::call_site(),
//     )
// }

fn parse_fn(item: &syn::Item) -> Result<&syn::Ident, &'static str> {
    match item {
        syn::Item::Fn(syn::ItemFn {ref sig, ..}) => {
            if sig.unsafety.is_some() {
                Err("lisp functions cannot be `unsafe`")
            } else if sig.constness.is_some() {
                Err("lisp functions cannot be `const`")
            } else {
                Ok(&sig.ident)
            }
        }
        _ => Err("`lisp_fn` attribute can only be used on functions"),
    }
}

#[derive(Default, PartialEq, Debug)]
struct Spec {
    name: Option<String>,
    min: Option<u32>,
    intspec: Option<String>,
}

impl Spec {
    fn add_attr(&mut self, key: &syn::Ident, value: &syn::Lit) -> Result<(), syn::Error> {
        match key.to_string().as_str() {
            "name" => {
                match value {
                    syn::Lit::Str(x) => {
                        self.name = Some(x.value());
                        Ok(())
                    }
                    _ => Err(syn::Error::new(key.span(), "name attribute must be a string")),
                }
            }
            "min" => {
                match value {
                    syn::Lit::Int(x) => {
                        self.min = Some(x.base10_parse()?);
                        Ok(())
                    }
                    _ => Err(syn::Error::new(key.span(), "min attribute must be a integer")),
                }
            }
            "intspec" => {
                match value {
                    syn::Lit::Str(x) => {
                        self.intspec = Some(x.value());
                        Ok(())
                    }
                    _ => Err(syn::Error::new(key.span(), "intspec attribute must be a string")),
                }
            }
            _ => Err(syn::Error::new(key.span(), "Valid attributes are name, min, and intspec")),
        }
    }
}

impl syn::parse::Parse for Spec {
    fn parse(input: syn::parse::ParseStream) -> Result<Self, syn::Error> {
        let attr = input.call(syn::Attribute::parse_outer)?;
        parse_attr(&attr[0])
    }
}

fn parse_attr(attr: &syn::Attribute) -> Result<Spec, syn::Error> {
    let mut spec = Spec::default();
    match attr.parse_meta()? {
        syn::Meta::Path(_) => Ok(spec),
        syn::Meta::List(list) => {
            for i in list.nested.iter() {
                let (key, value) = parse_meta(i)?;
                spec.add_attr(key, value)?;
            }
            Ok(spec)
        }
        _ => Err(syn::Error::new_spanned(attr, "Expected attribute list")),
    }
}

fn parse_meta(meta: &syn::NestedMeta) -> Result<(&syn::Ident, &syn::Lit), syn::Error> {
    match meta {
        syn::NestedMeta::Meta(syn::Meta::NameValue(name_value)) => parse_name_value_pair(name_value),
        _ => Err(syn::Error::new_spanned(meta, "Expected Meta item")),
    }
}

fn parse_name_value_pair(pair: &syn::MetaNameValue) -> Result<(&syn::Ident, &syn::Lit), syn::Error> {
    let syn::MetaNameValue{ref path, ref lit, ..} = pair;
    match path.get_ident() {
        Some(ident) => Ok((ident, lit)),
        None => Err(syn::Error::new_spanned(path, "Expected Identifier")),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test() {
        let stream: proc_macro2::TokenStream = quote!{fn foo() {}}.into();
        let item: syn::Item = syn::parse2(stream).unwrap();
        println!("{}", parse_fn(&item).unwrap());
        assert!(parse_fn(&item).is_ok());
    }

    #[test]
    fn attr() {
        let attr: proc_macro2::TokenStream = quote!{#[lisp_fn(min = 1, name = "bobby", intspec = "bbuffer:")]}.into();
        let compare: Spec = syn::parse2(attr).unwrap();
        let expect = Spec{name: Some("bobby".into()), min: Some(1), intspec: Some("bbuffer:".into())};
        assert_eq!(expect, compare);

        let attr: proc_macro2::TokenStream = quote!{#[lisp_fn]}.into();
        let compare: Spec = syn::parse2(attr).unwrap();
        let expect = Spec::default();
        assert_eq!(expect, compare);

        let attr: proc_macro2::TokenStream = quote!{#[lisp_fn(min = "1")]}.into();
        let compare: Result<Spec, syn::Error> = syn::parse2(attr);
        assert!(compare.is_err());
    }
}
