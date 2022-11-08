use proc_macro2::TokenStream;
use quote::quote;

pub(crate) fn expand(ident: syn::Ident) -> TokenStream {
    let lisp_name = expand_string(&ident.to_string());
    quote! {#lisp_name}
}

fn expand_string(ident: &str) -> String {
    if let Some(stripped) = ident.strip_prefix("KW_") {
        convert(':', stripped)
    } else {
        convert("", ident)
    }
}

fn convert(lisp_name: impl Into<String>, rust_name: &str) -> String {
    let mut lisp_name = lisp_name.into();
    lisp_name.extend(rust_name.chars().map(|c| match c {
        '_' => '-',
        c => c.to_ascii_lowercase(),
    }));
    lisp_name
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn varnames() {
        assert_eq!(expand_string("FOO"), "foo");
        assert_eq!(expand_string("FOO_BAR"), "foo-bar");
        assert_eq!(expand_string("FOO__BAR"), "foo--bar");
        assert_eq!(expand_string("KW_FOO_BAR"), ":foo-bar");
        assert_eq!(expand_string("FOO_KW_BAR"), "foo-kw-bar");
    }
}
