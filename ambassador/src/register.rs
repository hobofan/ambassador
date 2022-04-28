use proc_macro2::{Ident, TokenStream, TokenTree};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{
    ConstParam, GenericParam, ItemTrait, LifetimeDef, TraitItem, TraitItemConst, TraitItemType,
    TypeParam,
};

pub(crate) fn macro_name(trait_ident: &Ident) -> Ident {
    quote::format_ident!("ambassador_impl_{}", trait_ident)
}

pub(crate) fn match_name(trait_ident: &Ident) -> Ident {
    quote::format_ident!("Match{}", trait_ident)
}

pub fn build_register_trait(original_item: &ItemTrait) -> TokenStream {
    let trait_ident = &original_item.ident;
    let macro_name = macro_name(trait_ident);
    let match_name = match_name(trait_ident);
    let gen_idents: Vec<_> = original_item
        .generics
        .params
        .iter()
        .map(param_to_ident)
        .collect();
    let gen_matcher: TokenStream = original_item
        .generics
        .params
        .iter()
        .map(param_to_matcher)
        .collect();

    let (mut struct_items, mut enum_items, mut self_items) = (vec![], vec![], vec![]);
    original_item
        .items
        .iter()
        .map(|item| build_trait_items(item, trait_ident, &gen_idents))
        .for_each(|(st, en, se)| {
            struct_items.push(st);
            enum_items.push(en);
            self_items.push(se);
        });

    let assoc_ty_bounds = make_assoc_ty_bound(&original_item.items, original_item, &match_name);

    let mut register_trait = quote! {
        #[doc = concat!("A macro to be used by [`ambassador::Delegate`] to delegate [`", stringify!(#trait_ident), "`]")]
        #[macro_export]
        macro_rules! #macro_name {
            (body_struct(<#gen_matcher>, $ty:ty, $field_ident:tt)) => {
                #(#struct_items)*
            };
            (body_enum(<#gen_matcher>, $ty:ty, ($( $other_tys:ty ),+), ($( $variants:path ),+))) => {
                #(#enum_items)*
            };
            (body_self(<#gen_matcher>)) => {
                #(#self_items)*
            };
            (use_assoc_ty_bounds) => {
                #assoc_ty_bounds
            };
        }


    };
    if cfg!(feature = "backward_compatible") {
        let enum_name = quote::format_ident!("{}_body_enum", macro_name);
        let struct_name = quote::format_ident!("{}_body_single_struct", macro_name);
        let legacy_macros = quote! {
            #[macro_export]
            macro_rules! #struct_name {
                ($field_ident:tt) => {#macro_name!{body_struct(<>, (), $field_ident)}};
            }
            #[macro_export]
            macro_rules! #enum_name {
                ($( $variants:path ),+) => {#macro_name!{body_enum(<>, (), (()), ($( $variants),*))}};
            }
        };
        register_trait.extend(legacy_macros);
    }
    register_trait
}

fn param_to_ident(param: &GenericParam) -> &Ident {
    match param {
        GenericParam::Type(TypeParam { ident, .. }) => ident,
        GenericParam::Lifetime(LifetimeDef { lifetime, .. }) => &lifetime.ident,
        GenericParam::Const(ConstParam { ident, .. }) => ident,
    }
}

fn param_to_matcher(param: &GenericParam) -> TokenStream {
    match param {
        GenericParam::Type(TypeParam { ident, .. }) => quote!($ #ident : ty,),
        GenericParam::Lifetime(LifetimeDef { lifetime, .. }) => {
            let ident = &lifetime.ident;
            quote!($ #ident : lifetime,)
        }
        GenericParam::Const(ConstParam { ident, .. }) => quote!($ #ident : ident,),
    }
}

fn param_to_tokens(param: &GenericParam) -> TokenStream {
    match param {
        GenericParam::Type(TypeParam { ident, .. }) => quote!(#ident,),
        GenericParam::Lifetime(LifetimeDef { lifetime, .. }) => quote!(#lifetime,),
        GenericParam::Const(ConstParam { ident, .. }) => quote!(#ident,),
    }
}

fn make_assoc_ty_bound(
    items: &[TraitItem],
    item_trait: &ItemTrait,
    match_name: &Ident,
) -> TokenStream {
    let trait_ident = &item_trait.ident;
    let gen_params = &item_trait.generics.params;
    let gen_params_t = super::util::TailingPunctuated::wrap_ref(gen_params);

    let gen_tokens: TokenStream = gen_params.iter().flat_map(param_to_tokens).collect();
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
    let new_bound = quote! {#trait_ident<#gen_tokens #(#assoc_type_bounds,)*>};

    quote! {
        #[doc(hidden)]
        #[allow(non_camel_case_types)]
        pub trait #match_name<#gen_params_t ambassador_X: #trait_ident<#gen_tokens>>: #new_bound {}
        #[allow(non_camel_case_types)]
        impl<#gen_params_t ambassador_X: #trait_ident<#gen_tokens>, ambassador_Y: #new_bound> #match_name<#gen_tokens ambassador_X> for ambassador_Y {} // Replace with trait alias when they become stable
    }
}

// Replaces the identifiers in gen_idents with there macro args (X => $X) ('a => $a)
fn replace_gen_idents(tokens: TokenStream, gen_idents: &[&Ident]) -> TokenStream {
    let mut res = TokenStream::new();
    let mut apostrophe: Option<proc_macro2::Punct> = None;
    for tt in tokens {
        match tt {
            TokenTree::Group(g) => res.append(proc_macro2::Group::new(
                g.delimiter(),
                replace_gen_idents(g.stream(), gen_idents),
            )),
            TokenTree::Ident(ref id) if gen_idents.contains(&id) => {
                apostrophe = None;
                res.append(TokenTree::Punct(proc_macro2::Punct::new(
                    '$',
                    proc_macro2::Spacing::Joint,
                )));
                res.append(tt)
            }
            TokenTree::Punct(p) if p.as_char() == '\'' => apostrophe = Some(p),
            _ => {
                if let Some(ap) = apostrophe.take() {
                    res.append(ap);
                }
                res.append(tt)
            }
        }
    }
    res
}

fn build_trait_items(
    original_item: &TraitItem,
    trait_ident: &Ident,
    gen_idents: &[&Ident],
) -> (TokenStream, TokenStream, TokenStream) {
    let gen_pat: TokenStream = gen_idents.iter().flat_map(|id| quote! {$#id,}).collect();
    match original_item {
        TraitItem::Const(TraitItemConst { ident, ty, .. }) => (
            quote! {
                const #ident : #ty = <$ty as #trait_ident<#gen_pat>>::#ident;
            },
            quote! {
                const #ident : #ty = {
                    $(assert!(<$ty as #trait_ident<#gen_pat>>::#ident == <$other_tys as #trait_ident<#gen_pat>>::#ident);)*
                    <$ty as #trait_ident<#gen_pat>>::#ident
                };
            },
            quote! {compile_error!("trg=\"self\" is not allowed with associated constants")},
        ),
        TraitItem::Type(TraitItemType {
            ident, generics, ..
        }) => {
            let _where_clause = &generics.where_clause;
            let item = quote! {
                type #ident #generics = <$ty as #trait_ident<#gen_pat>>::#ident #generics;
                // TODO add #where_clase to appropriate place once it is decided
            };
            (
                item.clone(),
                item,
                quote! {compile_error!("trg=\"self\" is not allowed with associated types")},
            )
        }
        TraitItem::Method(original_method) => {
            let method_sig = original_method.sig.to_token_stream();
            let method_sig = replace_gen_idents(method_sig, gen_idents);
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
                        build_method_invocation(original_method, &quote!(inner));
                    quote! {
                        #method_sig {
                            match self {
                                $($variants(inner) => #method_invocation),*
                            }
                        }
                    }
                },
                {
                    let method_invocation = build_method_invocation(original_method, &quote!(self));
                    quote! {
                        #[deny(unconditional_recursion)]
                        #method_sig {
                            #method_invocation
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
    field_ident: &TokenStream,
) -> TokenStream {
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
