use proc_macro2::TokenStream;
use quote::{format_ident, quote};

#[allow(clippy::too_many_lines)]
pub(crate) fn expand(orig: &syn::DeriveInput) -> TokenStream {
    let rt = quote!(crate::core::gc::Rt);
    let vis = &orig.vis;
    let orig_name = &orig.ident;
    let rooted_name = format_ident!("Rooted{orig_name}");
    let generic_params = &orig.generics;

    let derive = match &orig.data {
        syn::Data::Struct(strct) => {
            let mut new_fields = TokenStream::new();
            let mut mark_fields = TokenStream::new();
            let mut test_fields = TokenStream::new();
            match &strct.fields {
                syn::Fields::Named(fields) => {
                    for x in &fields.named {
                        #[rustfmt::skip]
                        let syn::Field { vis, ident, ty, attrs, .. } = &x;
                        let ident = ident.as_ref().expect("named fields should have an identifer");
                        test_fields.extend(quote! {
                            assert_eq!(memoffset::offset_of!(super::#orig_name, #ident),
                                       memoffset::offset_of!(super::#rooted_name, #ident));
                        });
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
                        test_fields.extend(quote! {
                            assert_eq!(memoffset::offset_of!(super::#orig_name, #idx),
                                       memoffset::offset_of!(super::#rooted_name, #idx));
                        });
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
            let test_mod = format_ident!("derive_trace_{orig_name}");
            quote! {
                impl #generic_params crate::core::gc::Trace for #orig_name #generic_params {
                    fn trace(&self, stack: &mut Vec<crate::core::object::RawObj>) {
                        #mark_fields
                    }
                }

                #[automatically_derived]
                #[allow(non_camel_case_types)]
                #vis struct #rooted_name #generic_params #new_fields

                // This makes sure that the offsets of the fields are the same
                // between the derived and orignal structs. Once
                // https://github.com/rust-lang/rust/issues/80384 is stabilized
                // or we switch to nightly, this can be moved into a const
                // assertion.
                #[cfg(test)]
                #[allow(non_snake_case_types)]
                mod #test_mod {
                    #[test]
                    #[doc(hidden)]
                    fn offsets() {
                        #test_fields
                    }
                }
            }
        }
        _ => todo!(),
    };

    quote! {
        #derive

        impl #generic_params crate::core::gc::RootedDeref for #orig_name #generic_params {
            type Target = #rooted_name #generic_params;

            fn rooted_deref(rooted: &crate::core::gc::Rt<Self>) -> &Self::Target {
                unsafe { &*(rooted as *const crate::core::gc::Rt<Self>).cast::<Self::Target>() }            }

            fn rooted_derefmut(rooted: &mut crate::core::gc::Rt<Self>) -> &mut Self::Target {
                unsafe { &mut *(rooted as *mut crate::core::gc::Rt<Self>).cast::<Self::Target>() }
            }
        }
    }
}

fn no_trace(attrs: &[syn::Attribute]) -> bool {
    for attr in attrs {
        if let syn::Meta::Path(path) = &attr.meta {
            if path.is_ident("no_trace") {
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

        let stream = quote!(
            struct Foo {
                a: A,
                #[no_trace]
                b: B,
            }
        );
        let input: syn::DeriveInput = syn::parse2(stream).unwrap();
        let result = expand(&input);
        println!("{result}");
    }
}
