use crate::util::{error, process_results, receiver_type, ReceiverType};
use itertools::Itertools;
use proc_macro2::{Ident, TokenStream, TokenTree};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::spanned::Spanned;
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

struct UsedReceivers {
    owned: bool,
    ref_r: bool,
    ref_mut: bool,
}

fn compile_error_or_none(message: &str, return_cmp_err: bool) -> Option<TokenStream> {
    if return_cmp_err {
        Some(quote!(compile_error! {#message}))
    } else {
        None
    }
}

pub fn build_register_trait(original_item: &ItemTrait) -> TokenStream {
    let trait_ident = &original_item.ident;
    let macro_name = macro_name(trait_ident);
    let match_name = match_name(trait_ident);
    let gen_params = &original_item.generics.params;
    let gen_idents: Vec<_> = gen_params.iter().map(param_to_ident).collect();
    let gen_matcher: TokenStream = gen_params.iter().map(param_to_matcher).collect();

    let mut used_recievers = UsedReceivers {
        owned: false,
        ref_r: false,
        ref_mut: false,
    };
    let iter = original_item
        .items
        .iter()
        .map(|item| build_trait_items(item, trait_ident, &gen_idents, &mut used_recievers));
    let (struct_items, enum_items, self_items): (Vec<_>, Vec<_>, Vec<_>) =
        match process_results(iter, |iter| iter.multiunzip()) {
            Ok(tup) => tup,
            Err(err) => return err.to_compile_error(),
        };
    let assoc_ty_bounds = make_assoc_ty_bound(&original_item.items, original_item, &match_name);
    let gen_idents_pat: TokenStream = gen_idents.into_iter().map(|id| quote! {$ #id ,}).collect();
    let check_owned = compile_error_or_none(
        "target_owned was not specified but was needed",
        used_recievers.owned,
    );
    let check_ref = compile_error_or_none(
        "target_ref was not specified but was needed",
        used_recievers.ref_r,
    );
    let check_ref_mut = compile_error_or_none(
        "target_mut was not specified but was needed",
        used_recievers.ref_mut,
    );
    let mut register_trait = quote! {
        #[doc = concat!("A macro to be used by [`ambassador::Delegate`] to delegate [`", stringify!(#trait_ident), "`]")]
        #[macro_export]
        macro_rules! #macro_name {
            (body_struct(<#gen_matcher>, $ty:ty, $field_ident:tt)) => {
                #macro_name!{body_struct(<#gen_idents_pat>, $ty, ($field_ident), ($field_ident), ($field_ident))}
            };
            (body_struct(<#gen_matcher>, $ty:ty, ($($ident_owned:tt)*), ($($ident_ref:tt)*), ($($ident_ref_mut:tt)*))) => {
                #macro_name!{check_owned($($ident_owned)*)}
                #macro_name!{check_ref($($ident_ref)*)}
                #macro_name!{check_ref_mut($($ident_ref_mut)*)}
                #(#struct_items)*
            };
            (check_owned()) => {
                #check_owned
            };
            (check_owned($($_:tt)+)) => {};
            (check_ref()) => {
                #check_ref
            };
            (check_ref($($_:tt)+)) => {};
            (check_ref_mut()) => {
                #check_ref_mut
            };
            (check_ref_mut($($_:tt)+)) => {};
            (body_enum(<#gen_matcher>, $ty:ty, ($( $other_tys:ty ),*), ($( $variants:path ),+))) => {
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
    let gen_params_t = super::util::TailingPunctuated(gen_params);

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
    used_recievers: &mut UsedReceivers,
) -> syn::Result<(TokenStream, TokenStream, TokenStream)> {
    let gen_pat: TokenStream = gen_idents.iter().flat_map(|id| quote! {$#id,}).collect();
    let res = match original_item {
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
            let where_clause = &generics.where_clause;
            let item = quote! {
                type #ident #generics = <$ty as #trait_ident<#gen_pat>>::#ident #generics #where_clause;
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
                    let field_ident = match receiver_type(&original_method.sig)? {
                        ReceiverType::Owned => {
                            used_recievers.owned = true;
                            quote!(self.$($ident_owned)*)
                        }
                        ReceiverType::Ref => {
                            used_recievers.ref_r = true;
                            quote!(self.$($ident_ref)*)
                        }
                        ReceiverType::MutRef => {
                            used_recievers.ref_mut = true;
                            quote!(self.$($ident_ref_mut)*)
                        }
                    };
                    let method_invocation = build_method_invocation(original_method, &field_ident);
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
        _ => return error!(original_item.span(), "unsupported trait item"),
    };
    Ok(res)
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
