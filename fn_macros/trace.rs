use proc_macro2::TokenStream;
use quote::{format_ident, quote};

pub(crate) fn expand(orig: &syn::DeriveInput) -> TokenStream {
    let rt = quote!(crate::core::gc::Rt);
    let vis = &orig.vis;
    let orig_name = &orig.ident;
    let rooted_name = format_ident!("__Rooted_{orig_name}");
    let derive = match &orig.data {
        syn::Data::Struct(strct) => {
            let mut new_fields = TokenStream::new();
            let mut mark_fields = TokenStream::new();
            match &strct.fields {
                syn::Fields::Named(fields) => {
                    for x in &fields.named {
                        #[rustfmt::skip]
                        let syn::Field { vis, ident, ty, attrs, .. } = &x;
                        let ident = ident
                            .as_ref()
                            .expect("named fields should have an identifer");
                        if no_trace(attrs) {
                            new_fields.extend(quote! {#vis #ident: #ty,});
                            // Remove dead_code warnings
                            mark_fields.extend(quote! {let _ = &self.#ident;});
                        } else {
                            new_fields.extend(quote! {#vis #ident: #rt<#ty>,});
                            mark_fields.extend(
                                quote! {crate::core::gc::Trace::trace(&self.#ident, stack);},
                            );
                        }
                    }
                    new_fields = quote! {{#new_fields}};
                }
                syn::Fields::Unnamed(fields) => {
                    for (i, x) in fields.unnamed.iter().enumerate() {
                        let syn::Field { vis, ty, attrs, .. } = &x;
                        let idx = syn::Index::from(i);
                        if no_trace(attrs) {
                            new_fields.extend(quote! {#vis #ty,});
                            // Remove dead_code warnings
                            mark_fields.extend(quote! {let _ = &self.#idx;});
                        } else {
                            new_fields.extend(quote! {#vis #rt<#ty>,});
                            mark_fields
                                .extend(quote! {crate::core::gc::Trace::trace(&self.#idx, stack);});
                        }
                    }
                    new_fields = quote! {(#new_fields);};
                }
                syn::Fields::Unit => panic!("fieldless structs don't need tracing"),
            }
            quote! {
                impl crate::core::gc::Trace for #orig_name {
                    fn trace(&self, stack: &mut Vec<crate::core::object::RawObj>) {
                        #mark_fields
                    }
                }

                #[allow(non_camel_case_types)]
                #[doc(hidden)]
                #vis struct #rooted_name #new_fields
            }
        }
        _ => todo!(),
    };

    quote! {
        #derive

        impl std::ops::Deref for #rt<#orig_name> {
            type Target = #rooted_name;
            fn deref(&self) -> &Self::Target {
                unsafe { &*(self as *const Self).cast::<Self::Target>() }
            }
        }

        impl std::ops::DerefMut for #rt<#orig_name> {
            fn deref_mut(&mut self) -> &mut Self::Target {
                unsafe { &mut *(self as *mut Self).cast::<Self::Target>() }
            }
        }
    }
}

fn no_trace(attrs: &[syn::Attribute]) -> bool {
    for attr in attrs {
        if let Ok(syn::Meta::Path(meta)) = attr.parse_meta() {
            if meta.is_ident("no_trace") {
                return true;
            }
        }
    }
    false
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_expand() {
        let stream = quote!(
            struct LispStack(Vec<GcObj<'static>>);
        );
        let input: syn::DeriveInput = syn::parse2(stream).unwrap();
        let result = expand(&input);
        println!("{result}");
    }
}
