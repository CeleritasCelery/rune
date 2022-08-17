use proc_macro2::TokenStream;
use quote::{format_ident, quote};

pub(crate) fn expand(orig: &syn::Item) -> TokenStream {
    match orig {
        syn::Item::Struct(strct) => {
            let orig_name = &strct.ident;
            let vis = &strct.vis;
            let new_name = format_ident!("__Rooted_{orig_name}");

            let mut new_fields = TokenStream::new();
            match &strct.fields {
                syn::Fields::Named(fields) => {
                    for x in &fields.named {
                        let syn::Field { vis, ident, ty, .. } = &x;
                        let ident = ident
                            .as_ref()
                            .expect("named fields should have an identifer");
                        new_fields.extend(quote!(#vis #ident: crate::core::gc::Rt<#ty>,))
                    }
                }
                syn::Fields::Unnamed(fields) => {
                    for x in &fields.unnamed {
                        let syn::Field { vis, ty, .. } = &x;
                        new_fields.extend(quote!(#vis crate::core::gc::Rt<#ty>,))
                    }
                }
                syn::Fields::Unit => panic!("fieldless structs don't need tracing"),
            }
            let rt = quote!(crate::core::gc::Rt);
            quote! {
                #orig

                #[allow(non_camel_case_types)]
                #[doc(hidden)]
                #vis struct #new_name {
                    #new_fields
                }

                impl std::ops::Deref for #rt<#orig_name> {
                    type Target = #new_name;
                    fn deref(&self) -> &Self::Target {
                        unsafe { &*(self as *const Self).cast::<#new_name>() }
                    }
                }

                impl std::ops::DerefMut for #rt<#orig_name> {
                    fn deref_mut(&mut self) -> &mut Self::Target {
                        unsafe { &mut *(self as *mut Self).cast::<#new_name>() }
                    }
                }
            }
        }
        _ => todo!(),
    }
}
