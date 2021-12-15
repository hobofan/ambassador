use proc_macro2::Ident;
use quote::quote;
use syn::{TraitItem, TraitItemConst, TraitItemType};

pub fn build_register_trait(original_item: &syn::ItemTrait) -> proc_macro2::TokenStream {
    let trait_ident = &original_item.ident;
    let macro_name: syn::Ident =
        quote::format_ident!("ambassador_impl_{}", trait_ident);


    let enum_items = build_trait_items(&*original_item.items, trait_ident, true);
    let struct_items = build_trait_items(&*original_item.items, trait_ident, false);

    let register_trait = quote! {
        #[doc = concat!("A macro to be used by [`ambassador::Delegate`] to delegate [`", stringify!(#trait_ident), "`]")]
        #[macro_export]
        macro_rules! #macro_name {
            (body_struct($ty:ty, $field_ident:tt)) => {
                #(#struct_items)*
            };
            (body_enum($ty:ty, $( $variants:path ),+)) => {
                #(#enum_items)*
            }
        }
    };
    register_trait
}

fn build_trait_items (
    original_trait_methods: &[syn::TraitItem],
    trait_ident: &Ident,
    is_enum: bool,
) -> Vec<proc_macro2::TokenStream> {
    let mut trait_items = vec![];
    for original_item in original_trait_methods {
        let item = match original_item {
            TraitItem::Const(TraitItemConst{ident, ty, ..}) => {
                quote! {
                    const #ident : #ty = <$ty as #trait_ident>::#ident;
                }
            }
            TraitItem::Type(TraitItemType{ident, generics, ..}) => {
                quote! {
                    type #ident #generics = <$ty as #trait_ident>::#ident #generics;
                }
            }
            TraitItem::Method(original_method) => {
                let method_sig = &original_method.sig;
                if is_enum {
                    let method_invocation = build_method_invocation(&original_method, &quote!(inner));
                    quote! {
                        #method_sig {
                            match self {
                                $($variants(inner) => #method_invocation),*
                            }
                        }
                    }
                } else {
                    let method_invocation =
                        build_method_invocation(original_method, &quote!(self.$field_ident));
                    quote! {
                        #method_sig {
                            #method_invocation
                        }
                    }
                }
            }
            _ => unimplemented!()
        };

        trait_items.push(item);
    }
    trait_items
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
