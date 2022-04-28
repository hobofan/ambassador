use proc_macro2::Ident;
use quote::{quote};
use syn::{TraitItem, TraitItemConst, TraitItemType};

pub(crate) fn macro_name(trait_ident: &syn::Ident) -> syn::Ident {
    quote::format_ident!("ambassador_impl_{}", trait_ident)
}

pub(crate) fn match_name(trait_ident: &syn::Ident) -> syn::Ident {
    quote::format_ident!("Match{}", trait_ident)
}

pub fn build_register_trait(original_item: &syn::ItemTrait) -> proc_macro2::TokenStream {
    let trait_ident = &original_item.ident;
    let macro_name = macro_name(trait_ident);
    let match_name = match_name(trait_ident);

    let (struct_items, enum_items): (Vec<_>, Vec<_>) = original_item
        .items
        .iter()
        .map(|item| build_trait_items(item, trait_ident))
        .unzip();

    let assoc_ty_bounds = make_assoc_ty_bound(&original_item.items, trait_ident, &match_name);

    let mut register_trait = quote! {
        #[doc = concat!("A macro to be used by [`ambassador::Delegate`] to delegate [`", stringify!(#trait_ident), "`]")]
        #[macro_export]
        macro_rules! #macro_name {
            (body_struct($ty:ty, $field_ident:tt)) => {
                #(#struct_items)*
            };
            (body_enum($ty:ty, ($( $other_tys:ty ),+), ($( $variants:path ),+))) => {
                #(#enum_items)*
            };
            (use_assoc_ty_bounds) => {
                #assoc_ty_bounds
            };
        }


    };
    if cfg!(feature = "backward_compatible") {
        let enum_name = quote::format_ident!("{}_body_enum", macro_name);
        let struct_name = quote::format_ident!("{}_body_single_struct", macro_name);
        let legacy_macros = quote!{
            #[macro_export]
            macro_rules! #struct_name {
                ($field_ident:tt) => {#macro_name!{body_struct((), $field_ident)}};
            }
            #[macro_export]
            macro_rules! #enum_name {
                ($( $variants:path ),+) => {#macro_name!{body_enum((), (()), ($( $variants),*))}};
            }
        };
        register_trait.extend(legacy_macros);
    }
    register_trait
}

fn make_assoc_ty_bound(items: &[syn::TraitItem], trait_ident: &Ident, match_name: &Ident) -> proc_macro2::TokenStream {
    let assoc_type_bounds: Vec<_> = items.iter()
        .filter_map(|item| match item {
            TraitItem::Type(ty) => {
                let ty_ident = &ty.ident;
                Some(match ty.generics.params.len() {
                    0 => quote! {#ty_ident = <X as #trait_ident>::#ty_ident},
                    _ => quote! {compile_error!("Enums cannot currently delegate to traits with GATs")}
                })
            },
            _ => None,
        }).collect();
    let new_bound = match assoc_type_bounds.len() {
        0 => quote! {#trait_ident},
        _ => quote! {#trait_ident<#(#assoc_type_bounds,)*>}
    };

    quote! {
        #[doc(hidden)]
        pub trait #match_name<X: #trait_ident>: #new_bound {}
        impl<X: #trait_ident, Y: #new_bound> #match_name<X> for Y {} // Replace with trait alias when they become stable
    }
}

fn build_trait_items(
    original_item: &syn::TraitItem,
    trait_ident: &Ident,
) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
    match original_item {
        TraitItem::Const(TraitItemConst { ident, ty, .. }) => (
            quote! {
                const #ident : #ty = <$ty as #trait_ident>::#ident;
            },
            quote! {
                const #ident : #ty = {
                    $(assert!(<$ty as #trait_ident>::#ident == <$other_tys as #trait_ident>::#ident);)*
                    <$ty as #trait_ident>::#ident
                };
            },
        ),
        TraitItem::Type(TraitItemType {
            ident, generics, ..
        }) => {
            let item = quote! {
                type #ident #generics = <$ty as #trait_ident>::#ident #generics;
            };
            (item.clone(), item)
        }
        TraitItem::Method(original_method) => {
            let method_sig = &original_method.sig;
            (
                {
                    let method_invocation =
                        build_method_invocation(original_method, &quote!(self.$field_ident));
                    quote! {
                        #method_sig {
                            #method_invocation
                        }
                    }
                },
                {
                    let method_invocation =
                        build_method_invocation(&original_method, &quote!(inner));
                    quote! {
                        #method_sig {
                            match self {
                                $($variants(inner) => #method_invocation),*
                            }
                        }
                    }
                },
            )
        }
        _ => unimplemented!(),
    }
}

fn build_method_invocation(
    original_method: &syn::TraitItemMethod,
    field_ident: &proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let method_sig = &original_method.sig;
    let method_ident = &method_sig.ident;
    let argument_list: syn::punctuated::Punctuated<&Box<syn::Pat>, syn::token::Comma> = method_sig
        .inputs
        .iter()
        .filter_map(|fn_arg| match fn_arg {
            syn::FnArg::Receiver(_) => None,
            syn::FnArg::Typed(pat_type) => Some(&pat_type.pat),
        })
        .collect();

    let method_invocation = quote! { #field_ident.#method_ident(#argument_list) };
    method_invocation
}
