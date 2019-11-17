extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[derive(Clone)]
enum DelegateImplementer {
    Enum { variant_idents: Vec<syn::Ident> },
}

#[proc_macro_derive(Delegate, attributes(delegate))]
pub fn delegate_macro(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let input = parse_macro_input!(input as DeriveInput);

    let delegate_attributes: Vec<syn::Attribute> = input
        .clone()
        .attrs
        .into_iter()
        .filter(|n| n.path.is_ident("delegate"))
        .collect();
    if delegate_attributes.len() == 0 {
        panic!("No #[delegate] attribute specified. If you want to delegate an implementation of trait `SomeTrait` add the attribute:\n#[delegate(SomeTrait)]")
    }

    let implementer_ident = input.ident.clone();
    let implementer = match input.clone().data {
        syn::Data::Enum(enum_data) => {
            let variant_idents = enum_data.variants.into_iter().map(|n| n.ident).collect();
            DelegateImplementer::Enum { variant_idents }
        }
        _ => panic!(
            "ambassador currently only supports #[derive(Delegate)] for: \n- single-field enums"
        ),
    };

    let mut impl_macros = vec![];

    for delegate_attr in delegate_attributes {
        let trait_path_full: syn::Path = delegate_attr.parse_args().unwrap();
        let trait_ident: syn::Ident = trait_path_full
            .segments
            .clone()
            .into_iter()
            .last()
            .unwrap()
            .ident;

        let (trait_path, trait_path_colon) = build_invocation_path(trait_path_full);
        let macro_name: syn::Ident = quote::format_ident!("ambassador_impl_{}", trait_ident);

        let impl_macro = match implementer.clone() {
            DelegateImplementer::Enum { variant_idents } => {
                quote! {
                    #trait_path#trait_path_colon#macro_name!{Enum; #implementer_ident; #(#implementer_ident::#variant_idents),*}
                }
            }
        };
        impl_macros.push(impl_macro);
    }

    // Build the output, possibly using quasi-quotation
    let expanded = quote! {
        #(#impl_macros)*
    };

    // Hand the output tokens back to the compiler
    TokenStream::from(expanded)
}

/// Build the invocation path prefix for the macro invocation.
///
/// Macros are always imported from the crate root
///
/// (None, None) -> ""
/// (Some(...), Some(...)) -> "foo_crate::"
fn build_invocation_path(
    trait_path_full: syn::Path,
) -> (Option<syn::Path>, Option<syn::token::Colon2>) {
    let trait_path: Vec<syn::PathSegment> = trait_path_full
        .segments
        .clone()
        .into_iter()
        .take(usize::max(trait_path_full.segments.len() - 1, 0))
        .collect();
    let trait_path_str = trait_path
        .into_iter()
        .map(|n| n.ident.to_string())
        .collect::<Vec<_>>()
        .join("::");

    let (trait_path, trait_path_colon): (Option<syn::Path>, Option<syn::token::Colon2>) =
        match trait_path_str.as_ref() {
            "" => (None, None),
            _ => (
                Some(syn::parse_str(&trait_path_str).unwrap()),
                Some(syn::parse_quote!(::)),
            ),
        };
    (trait_path, trait_path_colon)
}

#[proc_macro_attribute]
pub fn delegatable_trait(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let original_item: syn::ItemTrait = syn::parse(item).unwrap();
    let trait_ident = original_item.ident.clone();
    let macro_name: syn::Ident = quote::format_ident!("ambassador_impl_{}", trait_ident);

    let original_trait_methods: Vec<syn::TraitItemMethod> = original_item
        .items
        .clone()
        .into_iter()
        .map(|n| match n {
            syn::TraitItem::Method(method) => method,
            _ => unimplemented!(),
        })
        .collect();

    let enum_trait_methods = build_enum_trait_methods(original_trait_methods);

    let register_trait = quote! {
        #[macro_export]
        macro_rules! #macro_name {
            (Enum; $implementer:ty; $( $variants:path ),+) => {
                impl #trait_ident for $implementer {
                    #(#enum_trait_methods)*
                }
            }
        }
    };

    let expanded = quote! {
        #original_item

        #register_trait
    };
    TokenStream::from(expanded)
}

fn build_enum_trait_methods(
    original_trait_methods: Vec<syn::TraitItemMethod>,
) -> Vec<proc_macro2::TokenStream> {
    let mut enum_trait_methods = vec![];
    for original_method in original_trait_methods {
        let method_sig = original_method.sig.clone();
        let method_ident = method_sig.ident.clone();
        let argument_list: syn::punctuated::Punctuated<syn::Pat, syn::token::Comma> = method_sig
            .inputs
            .clone()
            .into_iter()
            .filter_map(|fn_arg| match fn_arg {
                syn::FnArg::Receiver(_) => None,
                syn::FnArg::Typed(pat_type) => Some(pat_type),
            })
            .map(|pat_type| *pat_type.pat.to_owned())
            .collect();
        let method_invocation = quote! { inner.#method_ident(#argument_list) };

        let method_impl = quote! {
            #method_sig {
                match self {
                    $($variants(inner) => #method_invocation),*
                }
            }
        };
        enum_trait_methods.push(method_impl);
    }
    enum_trait_methods
}
