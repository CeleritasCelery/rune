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
                        let syn::Field { vis, ident, ty, .. } = &x;
                        let ident = ident
                            .as_ref()
                            .expect("named fields should have an identifer");
                        new_fields.extend(quote! {#vis #ident: #rt<#ty>,});
                        mark_fields.extend(quote! {self.#ident.mark(stack);});
                    }
                }
                syn::Fields::Unnamed(fields) => {
                    for (i, x) in fields.unnamed.iter().enumerate() {
                        let syn::Field { vis, ty, .. } = &x;
                        new_fields.extend(quote! {#vis #rt<#ty>,});
                        mark_fields.extend(quote! {self.#i.mark(stack);});
                    }
                }
                syn::Fields::Unit => panic!("fieldless structs don't need tracing"),
            }
            quote! {
                impl crate::core::gc::Trace for #orig_name {
                    fn mark(&self, stack: &mut Vec<crate::core::object::RawObj>) {
                        #mark_fields
                    }
                }

                #[allow(non_camel_case_types)]
                #[doc(hidden)]
                #vis struct #rooted_name {
                    #new_fields
                }
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
