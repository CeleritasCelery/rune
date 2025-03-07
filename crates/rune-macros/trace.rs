use proc_macro2::TokenStream;
use quote::{format_ident, quote};

pub(crate) fn expand(orig: &syn::DeriveInput) -> TokenStream {
    let orig_name = &orig.ident;
    let rooted_name = format_ident!("Rooted{orig_name}");
    let generic_params = &orig.generics;

    let derive = match &orig.data {
        syn::Data::Struct(s) => derive_struct(orig, s),
        syn::Data::Enum(data_enum) => derive_enum(orig, data_enum),
        syn::Data::Union(_) => panic!("Derive Trace for Unions is not supported"),
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

fn derive_enum(orig: &syn::DeriveInput, data_enum: &syn::DataEnum) -> TokenStream {
    let rt = quote!(crate::core::gc::Rt);
    let vis = &orig.vis;
    let orig_name = &orig.ident;
    let rooted_name = format_ident!("Rooted{orig_name}");
    let generic_params = &orig.generics;
    let repr = get_repr(&orig.attrs);

    let mut new_fields = TokenStream::new();
    let mut mark_fields = TokenStream::new();

    for x in &data_enum.variants {
        let no_trace = no_trace(&x.attrs);
        let ident = &x.ident;
        match &x.fields {
            syn::Fields::Unit => {
                new_fields.extend(quote! { #ident, });
                mark_fields.extend(quote! { #orig_name::#ident => {}, });
            }
            syn::Fields::Unnamed(unnamed_fields) => {
                if no_trace {
                    new_fields.extend(quote! { #ident #unnamed_fields, });

                    let num_fields = unnamed_fields.unnamed.iter().count();
                    let mut punctuated = syn::punctuated::Punctuated::<_, syn::Token![,]>::new();
                    for _ in 0..num_fields {
                        punctuated.push(syn::Ident::new("_", proc_macro2::Span::call_site()));
                    }

                    mark_fields.extend(quote! { #orig_name::#ident (#punctuated) => {}, });
                } else {
                    let mut rooted_fields = TokenStream::new();
                    let mut trace_fields = TokenStream::new();
                    let mut bind_fields = TokenStream::new();
                    for (i, field) in unnamed_fields.unnamed.iter().enumerate() {
                        let binding = format_ident!("x{i}");
                        bind_fields.extend(quote! {#binding,});
                        rooted_fields.extend(quote! {#rt<#field>,});
                        trace_fields
                            .extend(quote! {crate::core::gc::Trace::trace(#binding, state);});
                    }
                    new_fields.extend(quote! { #ident (#rooted_fields), });
                    mark_fields
                        .extend(quote! { #orig_name::#ident(#bind_fields) => {#trace_fields}, });
                }
            }
            syn::Fields::Named(_) => unreachable!(),
        }
    }

    let doc_string = format!("Automatically derived from [{orig_name}] via `#[derive(Trace)]`");
    quote! {
        impl #generic_params crate::core::gc::Trace for #orig_name #generic_params {
            fn trace(&self, state: &mut crate::core::gc::GcState) {
                match self {
                    #mark_fields
                }
            }
        }

        #[automatically_derived]
        #[allow(non_camel_case_types)]
        #[doc = #doc_string]
        #repr
        #vis enum #rooted_name #generic_params {#new_fields}
    }
}

fn derive_struct(orig: &syn::DeriveInput, data_struct: &syn::DataStruct) -> TokenStream {
    let rt = quote!(crate::core::gc::Rt);
    let vis = &orig.vis;
    let orig_name = &orig.ident;
    let rooted_name = format_ident!("Rooted{orig_name}");
    let generic_params = &orig.generics;
    let repr = get_repr(&orig.attrs);

    let mut new_fields = TokenStream::new();
    let mut mark_fields = TokenStream::new();
    let mut test_fields = TokenStream::new();

    match &data_struct.fields {
        syn::Fields::Named(fields) => {
            for x in &fields.named {
                #[rustfmt::skip]
                let syn::Field { vis, ident, ty, attrs, .. } = &x;
                let ident = ident.as_ref().expect("named fields should have an identifer");
                let panic_string = format!(
                    "Field '{}' of struct '{}' is incorrectly aligned in #[derive(Trace)]",
                    stringify!(#ident),
                    stringify!(#rooted_name),
                );
                test_fields.extend(quote! {
                    if std::mem::offset_of!(#orig_name, #ident) != std::mem::offset_of!(#rooted_name, #ident) {
                        panic!(#panic_string);
                    }
                });
                if no_trace(attrs) {
                    new_fields.extend(quote! {#vis #ident: #ty,});
                    // Remove dead_code warnings
                    mark_fields.extend(quote! {let _ = &self.#ident;});
                } else {
                    new_fields.extend(quote! {#vis #ident: #rt<#ty>,});
                    mark_fields
                        .extend(quote! {crate::core::gc::Trace::trace(&self.#ident, state);});
                }
            }
            new_fields = quote! {{#new_fields}};
        }
        syn::Fields::Unnamed(fields) => {
            for (i, x) in fields.unnamed.iter().enumerate() {
                let syn::Field { vis, ty, attrs, .. } = &x;
                let idx = syn::Index::from(i);
                let panic_string = format!(
                    "Field '{}' of struct '{}' is incorrectly aligned in #[derive(Trace)]",
                    stringify!(#idx),
                    stringify!(#rooted_name),
                );
                test_fields.extend(quote! {
                    if std::mem::offset_of!(#orig_name, #idx) != std::mem::offset_of!(#rooted_name, #idx) {
                        panic!(#panic_string);
                    }
                });
                if no_trace(attrs) {
                    new_fields.extend(quote! {#vis #ty,});
                    // Remove dead_code warnings
                    mark_fields.extend(quote! {let _ = &self.#idx;});
                } else {
                    new_fields.extend(quote! {#vis #rt<#ty>,});
                    mark_fields.extend(quote! {crate::core::gc::Trace::trace(&self.#idx, state);});
                }
            }
            new_fields = quote! {(#new_fields);};
        }
        syn::Fields::Unit => panic!("fieldless structs don't need tracing"),
    }
    let test_mod = format_ident!("derive_trace_{orig_name}");
    let doc_string = format!("Automatically derived from [{orig_name}] via `#[derive(Trace)]`");
    quote! {
        impl #generic_params crate::core::gc::Trace for #orig_name #generic_params {
            fn trace(&self, state: &mut crate::core::gc::GcState) {
                #mark_fields
            }
        }

        #[automatically_derived]
        #[allow(non_camel_case_types)]
        #[doc = #doc_string]
        #repr
        #vis struct #rooted_name #generic_params #new_fields

        #[allow(dead_code)]
        #[allow(non_snake_case)]
        // Ensure at compile time that all fields are at the same offset
        const #test_mod: () = {
            #test_fields
        };
    }
}

fn get_repr(attrs: &[syn::Attribute]) -> TokenStream {
    for attr in attrs {
        if let syn::Meta::List(list) = &attr.meta {
            if list.path.is_ident("repr") {
                return quote! {#attr};
            }
        }
    }
    quote! {}
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
    fn test_expand_struct() {
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

    #[test]
    fn test_expand_enum() {
        let stream = quote!(
            enum LispStack {
                A,
                B(i32),
                C(String, usize),
                #[no_trace]
                D(i32, usize),
            }
        );
        let input: syn::DeriveInput = syn::parse2(stream).unwrap();
        let result = expand(&input);
        println!("{result}");
    }

    #[test]
    fn test_repr_c() {
        let stream = quote!(
            #[repr(C)]
            struct Foo {
                a: A,
                b: B,
            }
        );
        let input: syn::DeriveInput = syn::parse2(stream).unwrap();
        let result = expand(&input);
        println!("{result}");
    }
}
