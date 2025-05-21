#![allow(clippy::manual_unwrap_or_default)]
// MIT License
//
// Copyright (c) 2020 Luke Roberts
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

use darling::Error as DarlingError;
use darling::ast::Style::{Struct, Tuple, Unit};
use darling::{FromField, FromVariant, ast::Fields};
use inflector::cases::snakecase::to_snake_case;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::format_ident;
use quote::quote;
use syn::Error as SynError;
use syn::Ident;
use syn::parse::Error as SynParseError;
use syn::{ItemEnum, Type};

type Result<T> = std::result::Result<T, Error>;

/// Custom error type for wrapping syn & darling errors as well as any other custom errors that may become necessary.
pub(crate) enum Error {
    Syn(SynError),
    Darling(DarlingError),
}

impl Error {
    pub(crate) fn into_compile_error(self) -> TokenStream {
        match self {
            Error::Syn(err) => err.to_compile_error().into(),
            Error::Darling(err) => err.write_errors().into(),
        }
    }
}

impl From<DarlingError> for Error {
    fn from(err: DarlingError) -> Self {
        Error::Darling(err)
    }
}

impl From<SynError> for Error {
    fn from(err: SynError) -> Self {
        Error::Syn(err)
    }
}

/// Declare a series of vars named by `operation` that contain an ident created
/// by concatenating the stringified `operation`, and the passed in `ident`.
/// # Examples
/// ```ignore
/// # use quote::format_ident;
/// # use inflector::cases::snakecase::to_snake_case;
/// let foo = format_ident!("{}", "foo");
/// identify!(foo, [get, and]);
/// // Expands to:
/// // let get = format_ident!("{}_{}", stringify!(get), to_snake_case(&foo.to_string()));
/// // let and = format_ident!("{}_{}", stringify!(and), to_snake_case(&foo.to_string()));
/// // Which results in:
/// // assert_eq!(get.to_string(), "get_foo");
/// // assert_eq!(and.to_string(), "and_foo");
/// ```
macro_rules! identify {
    ($ident:expr, [$($operation:ident$(,)*)*]) => {
        $(
            let $operation = format_ident!(
                "{}_{}",
                stringify!($operation),
                $ident
            );
        )*
    };
}

/// Generate the given number of unique and random idents and collect them into a vec.
/// Struct for parsing relevant input to each variant of a variantly derived enum.
#[derive(FromVariant, Debug)]
#[darling(attributes(variantly))]
struct VariantInput {
    ident: Ident,
    #[darling(default)]
    rename: Option<Ident>,
    fields: Fields<FieldParsed>,
}

/// Struct for parsing relevant information from a variant field
#[derive(FromField, Debug)]
#[darling(forward_attrs)]
struct FieldParsed {
    ty: Type,
}

/// Parsed input to each variant of a variantly derived enum.
#[derive(Debug)]
struct VariantParsed {
    ident: Ident,
    used_name: Ident,
    fields: Fields<FieldParsed>,
}

impl From<VariantInput> for VariantParsed {
    fn from(variant: VariantInput) -> Self {
        let ident = &variant.ident;
        VariantParsed {
            used_name: format_ident!(
                "{}",
                to_snake_case(&variant.rename.unwrap_or_else(|| ident.clone()).to_string())
            ),
            ident: variant.ident,
            fields: variant.fields,
        }
    }
}

/// Attempt to parse an `ItemEnum` into a vec of parsed variants.
fn try_parse_variants(item_enum: &ItemEnum) -> Result<Vec<VariantParsed>> {
    item_enum
        .variants
        .iter()
        .map(|variant| {
            VariantInput::from_variant(variant).map(VariantInput::into).map_err(Error::from)
        })
        .collect()
}

/// Helper function for validation that requires comparing each variant with each other variant.
/// Visits each pair only once and early returns on the first failure.
fn validate_compare<F: Fn(&VariantParsed, &VariantParsed) -> Result<()>>(
    variants: &Vec<VariantParsed>,
    validations: Vec<F>,
) -> Result<()> {
    // Enumerate over the entire set.
    variants
        .as_slice()
        .iter()
        .enumerate()
        .try_for_each(|(index, variant_a)| -> Result<()> {
            // Iterate over variants not visited already by the primary iterator.
            variants[(index + 1)..variants.len()].iter().try_for_each(|variant_b| {
                // Run the current pair against all validation fns
                validations.iter().try_for_each(|validation| validation(variant_a, variant_b))
            })
        })
}

/// Validate that the used names for each variant will not cause naming conflicts.
fn compare_used_names(a: &VariantParsed, b: &VariantParsed) -> Result<()> {
    if a.used_name == b.used_name {
        let message = format!(
            "`{}` cannot be coerced into a unique & idiomatic snake_case function name as it would collide with the `{}` variant of the same Enum. \
            use the following attribute on this or the conflicting variant to resolve: `#[variantly(rename = \"some_other_name\")]`",
            &a.ident, &b.ident
        );
        Err(syn::Error::new(a.ident.span(), message).into())
    } else {
        Ok(())
    }
}

fn derive_variantly_fns(target_type: &Ident, item_enum: ItemEnum) -> Result<TokenStream2> {
    let enum_name = &item_enum.ident; // Name of the enum the attribute is on (e.g., FooEnum)

    // For collecting impl functions
    let mut functions = vec![];

    let variants = try_parse_variants(&item_enum)?;

    validate_compare(&variants, vec![compare_used_names])?;

    for variant in variants {
        let variant_ident = &variant.ident; // Original variant identifier (e.g., MyVariant)
        let enum_variant_path = quote! { #enum_name::#variant_ident };

        let ignore_pattern = match &variant.fields.style {
            Tuple => {
                handle_tuple_variant(&variant, &mut functions, enum_name, &enum_variant_path);
                quote!((..))
            }
            Struct => quote!({ .. }),
            Unit => quote!(),
        };

        identify!(variant.used_name, [is, is_not, and, or]);

        functions.push(quote! {
            pub fn #is(&self) -> bool {
                match self.untag() {
                    #enum_variant_path #ignore_pattern => true,
                    _ => false
                }
            }

            pub fn #is_not(&self) -> bool {
                !self.#is()
            }

            // Assumes `and` is also of `target_type` and has `.untag()`
            pub fn #and(self, and: Self) -> Self {
                match (self.untag(), and.untag()) {
                    (#enum_variant_path #ignore_pattern, #enum_variant_path #ignore_pattern) => and,
                    _ => self
                }
            }

            pub fn #or(self, or: Self) -> Self {
                match self.untag() {
                    #enum_variant_path #ignore_pattern => self,
                    _ => or
                }
            }
        });
    }

    // Generics of the enum are not directly applied to the impl of `target_type`.
    // If `target_type` is generic, the user must ensure its definition is available
    // and might need to adjust the generated `impl` line if generics are involved.
    let output = quote! {
        impl<'ob> #target_type<'ob> {
            #(#functions)*
        }
    };

    Ok(output)
}

/// Construct all impl functions related to variants with tuple style internal variables.
fn handle_tuple_variant(
    variant: &VariantParsed,
    functions: &mut Vec<TokenStream2>,
    _enum_name: &Ident,               // Name of the enum itself (e.g. FooEnum)
    enum_variant_path: &TokenStream2, // Full path to the enum variant (e.g. FooEnum::VariantName)
) {
    let types: Vec<&Type> = variant.fields.fields.iter().map(|field| &field.ty).collect();
    // let ret_type = types.first().unwrap();

    let return_types_owned = quote! { (#( #types ),*) };

    identify!(variant.used_name, [expect, unwrap_or_else, unwrap_or, unwrap]);

    let var_fn_name = &variant.used_name;
    let var_or_fn_name = format_ident!("{}_or", var_fn_name);
    let var_or_else_fn_name = format_ident!("{}_or_else", var_fn_name);

    // Path for matching: Enum::Variant(vars)
    let match_path_owned = quote! { #enum_variant_path(x) }; // Use vars_pattern for ref binding

    functions.push(quote! {
        pub fn #var_fn_name(self) -> std::option::Option<#return_types_owned> {
            match self.untag() {
                #match_path_owned => std::option::Option::Some(x),
                _ => std::option::Option::None,
            }
        }

        pub fn #var_or_fn_name<E>(self, err: E) -> std::result::Result<#return_types_owned, E> {
            self.#var_or_else_fn_name(|| err)
        }

        pub fn #var_or_else_fn_name<E, F: std::ops::FnOnce() -> E>(self, or_else: F) -> std::result::Result<#return_types_owned, E> {
            match self.untag() {
                #match_path_owned => std::result::Result::Ok(x),
                _ => std::result::Result::Err(or_else())
            }
        }

        pub fn #expect(self, msg: &str) -> #return_types_owned {
            self.#unwrap_or_else(|| panic!("{}", msg))
        }

        pub fn #unwrap(self) -> #return_types_owned {
            self.#unwrap_or_else(|| panic!("called `unwrap()` on a non-`{}` value", stringify!(#var_fn_name)))
        }

        pub fn #unwrap_or(self, default: #return_types_owned) -> #return_types_owned {
            self.#unwrap_or_else(|| default)
        }

        pub fn #unwrap_or_else<F: std::ops::FnOnce() -> #return_types_owned>(self, or_else: F) -> #return_types_owned {
            match self.untag() {
                #match_path_owned => x,
                _ => or_else()
            }
        }
    });
}

pub(crate) fn variantly_attribute_macro_impl(
    attr: TokenStream2,
    item: TokenStream2,
) -> Result<TokenStream2> {
    let target_type: Ident = syn::parse2(attr).map_err(|e: SynParseError| Error::Syn(e))?;

    let enum_item: ItemEnum =
        syn::parse2(item.clone()).map_err(|e: SynParseError| Error::Syn(e))?;

    let fns = derive_variantly_fns(&target_type, enum_item)?;
    Ok(quote! {#item #fns})
}
