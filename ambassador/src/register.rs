use proc_macro2::{Ident};
use quote::quote;
use syn::{ConstParam, GenericParam, ItemTrait, LifetimeDef, TraitItem, TraitItemConst, TraitItemType, TypeParam};

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
        .map(|item| build_trait_items(item, trait_ident, &macro_name))
        .unzip();

    let assoc_ty_bounds = make_assoc_ty_bound(&original_item.items, &original_item, &match_name);

    let register_trait = quote! {
        #[doc = concat!("A macro to be used by [`ambassador::Delegate`] to delegate [`", stringify!(#trait_ident), "`]")]
        #[macro_export]
        macro_rules! #macro_name {
            (body_struct(<$($impl_gens:ty),*>, $ty:ty, $field_ident:tt)) => {
                #(#struct_items)*
            };
            (body_enum(<$($impl_gens:ty),*>, $ty:ty, ($( $other_tys:ty ),+), ($( $variants:path ),+))) => {
                #(#enum_items)*
            };
            (make_const_assertions($trait_ident:ident, $ty:ty, $ident:ident, ($($other_tys:ty,)*), $impl_gens:tt)) => {
              $(#macro_name!(make_const_assertion($trait_ident, $ty, $ident, $other_tys, $impl_gens)))*
            };
            (make_const_assertion($trait_ident:ident, $ty:ty, $ident:ident, $other_ty:ty, ($($impl_gens:ty,)*))) => {
              assert!(<$ty as $trait_ident<$($impl_gens,)*>>::$ident == <$other_ty as $trait_ident<$($impl_gens,)*>>::$ident);
            };
            (use_assoc_ty_bounds) => {
                #assoc_ty_bounds
            };
        }


    };
    register_trait
}

fn param_to_tokens(param: &GenericParam) -> proc_macro2::TokenStream{
    match param {
        GenericParam::Type(TypeParam{ident, ..}) => quote!(#ident,),
        GenericParam::Lifetime(LifetimeDef{lifetime, ..}) => quote!(#lifetime,),
        GenericParam::Const(ConstParam{ident, ..}) => quote!(#ident,)
    }
}

fn make_assoc_ty_bound(items: &[syn::TraitItem], item_trait: &ItemTrait, match_name: &Ident) -> proc_macro2::TokenStream {
    let trait_ident = &item_trait.ident;
    let gen_params = &item_trait.generics.params;
    
    let gen_tokens: proc_macro2::TokenStream = gen_params.iter().flat_map(param_to_tokens).collect();
    let assoc_type_bounds: Vec<_> = items.iter()
        .filter_map(|item| match item {
            TraitItem::Type(ty) => {
                let ty_ident = &ty.ident;
                Some(match ty.generics.params.len() {
                    0 => quote! {#ty_ident = <ambassador_X as #trait_ident<#gen_tokens>>::#ty_ident},
                    _ => quote! {compile_error!("Enums cannot currently delegate to traits with GATs")}
                })
            },
            _ => None,
        }).collect();
    let new_bound =  quote! {#trait_ident<#gen_tokens #(#assoc_type_bounds,)*>};

    quote! {
        #[doc(hidden)]
        #[allow(non_camel_case_types)]
        pub trait #match_name<#gen_params ambassador_X: #trait_ident>: #new_bound {}
        #[allow(non_camel_case_types)]
        impl<#gen_params ambassador_X: #trait_ident, ambassador_Y: #new_bound> #match_name<#gen_tokens ambassador_X> for ambassador_Y {} // Replace with trait alias when they become stable
    }
}

fn build_trait_items(
    original_item: &syn::TraitItem,
    trait_ident: &Ident,
    macro_name: &Ident,
) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
    match original_item {
        TraitItem::Const(TraitItemConst { ident, ty, .. }) => (
            quote! {
                const #ident : #ty = <$ty as #trait_ident<$($impl_gens,)*>>::#ident;
            },
            quote! {
                const #ident : #ty = {
                    //$(assert!(<$ty as #trait_ident<$($impl_gens,)*>>::#ident == <$other_tys as #trait_ident<$($impl_gens,)*>>::#ident);)*
                    #macro_name!(make_const_assertions(#trait_ident, $ty, #ident, ($($other_tys,)*), ($($impl_gens,)*)));
                    <$ty as #trait_ident<$($impl_gens,)*>>::#ident
                };
            },
        ),
        TraitItem::Type(TraitItemType {
            ident, generics, ..
        }) => {
            let where_clause = &generics.where_clause;
            let item = quote! {
                type #ident #generics = <$ty as #trait_ident<$($impl_gens,)*>>::#ident #generics;
                // TODO add #where_clase to appropriate place once it is decided
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
