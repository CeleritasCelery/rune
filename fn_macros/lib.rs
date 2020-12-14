use proc_macro::TokenStream;
use quote::quote;
use syn;

#[proc_macro_attribute]
pub fn lisp_fn(attr_ts: TokenStream, fn_ts: TokenStream) -> TokenStream {
    let spec = syn::parse_macro_input!(attr_ts as Spec);
    let function = syn::parse_macro_input!(fn_ts as Function);

    let body = function.body;
    let subr = function.name;
    let struct_name = concat_idents("S", &subr.to_string());
    let name = match spec.name {
        Some(x) => x,
        None => subr.to_string(),
    };
    let tokens = quote!{
        #[allow(non_upper_case_globals)]
        const #struct_name: BuiltInFn = BuiltInFn{
            name: #name,
            subr: #subr,
            args: FnArgs {
                required: 2,
                optional: 0,
                rest: false,
                max_stack_usage: 0,
                advice: false,
            }
        };

        #body
    };
    tokens.into()
}

fn concat_idents(lhs: &str, rhs: &str) -> syn::Ident {
    syn::Ident::new(
        format!("{}{}", lhs, rhs).as_str(),
        proc_macro2::Span::call_site(),
    )
}

fn parse_fn(item: syn::Item) -> Result<Function, syn::Error> {
    match item {
        syn::Item::Fn(syn::ItemFn {ref sig, ..}) => {
            if sig.unsafety.is_some() {
                Err(syn::Error::new_spanned(sig, "lisp functions cannot be `unsafe`"))
            } else if sig.constness.is_some() {
                Err(syn::Error::new_spanned(sig, "lisp functions cannot be `const`"))
            } else {
                Ok(Function{name: sig.ident.clone(), body: item})
            }
        }
        _ => Err(syn::Error::new_spanned(item, "`lisp_fn` attribute can only be used on functions"))
    }
}

struct Function {
    name: syn::Ident,
    body: syn::Item,
}

impl<'a> syn::parse::Parse for Function {
    fn parse(input: syn::parse::ParseStream) -> Result<Self, syn::Error> {
        let item: syn::Item = input.parse()?;
        parse_fn(item)
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
        match &attr.get(0) {
            Some(x) => parse_attr(x),
            None => Ok(Spec::default()),
        }
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
        let function: Function = syn::parse2(stream).unwrap();
        assert_eq!("foo", function.name.to_string());
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
