use proc_macro::TokenStream;
use darling::FromMeta;
use quote::quote;
use syn;

#[proc_macro_attribute]
pub fn lisp_fn(attr_ts: TokenStream, fn_ts: TokenStream) -> TokenStream {
    let attr_args = syn::parse_macro_input!(attr_ts as syn::AttributeArgs);
    let function = syn::parse_macro_input!(fn_ts as Function);

    let spec = match Spec::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => { return TokenStream::from(e.write_errors()); }
    };

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

#[derive(Default, PartialEq, Debug, FromMeta)]
struct Spec {
    #[darling(default)]
    name: Option<String>,
    #[darling(default)]
    min: Option<u32>,
    #[darling(default)]
    intspec: Option<String>,
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
}
