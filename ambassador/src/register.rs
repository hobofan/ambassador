use crate::util::{error, process_results, receiver_type, ReceiverType};
use itertools::Itertools;
use proc_macro2::{Ident, TokenStream, TokenTree};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::spanned::Spanned;
use syn::{
    AttrStyle, Attribute, ConstParam, GenericParam, ItemTrait, LifetimeParam, TraitItem,
    TraitItemConst, TraitItemType, TypeParam, Visibility,
};

pub(crate) fn macro_name(trait_ident: &Ident) -> Ident {
    quote::format_ident!("ambassador_impl_{}", trait_ident)
}

pub(crate) fn match_name(trait_ident: &Ident) -> Ident {
    quote::format_ident!("Match{}", trait_ident)
}

#[derive(Default)]
struct CfgNames(Vec<(TokenStream, Ident)>);

impl CfgNames {
    fn get_macro(&mut self, pred: TokenStream) -> &Ident {
        let fresh = self.0.len();
        self.0.push((pred, quote::format_ident!("cfg{}", fresh)));
        &self.0.last().unwrap().1
    }

    fn definitions(&self, mod_name: &Ident, vis: &Visibility) -> TokenStream {
        let iter = self.0.iter().map(|(cfg, name)| {
            let base_macro_name = quote::format_ident!("_{}_{}", mod_name, name);
            quote! {
                #[cfg(#cfg)]
                #[doc(hidden)]
                #[macro_export]
                macro_rules! #base_macro_name {
                    ($($t:tt)*) => {$($t)*};
                }

                #[cfg(not(#cfg))]
                #[doc(hidden)]
                #[macro_export]
                macro_rules! #base_macro_name {
                    ($($t:tt)*) => {};
                }

                pub use #base_macro_name as #name;
            }
        });
        quote! {
            #[doc(hidden)]
            #[allow(non_snake_case)]
            #vis mod #mod_name {
                #(#iter)*
            }
        }
    }
}

pub fn build_register_trait(original_item: &ItemTrait) -> TokenStream {
    let trait_ident = &original_item.ident;
    let macro_name = macro_name(trait_ident);
    let macro_def = quote::format_ident!("_{}", macro_name);
    let match_name = match_name(trait_ident);
    let gen_params = &original_item.generics.params;
    let gen_idents: Vec<_> = gen_params.iter().map(param_to_ident).collect();
    let gen_matcher: TokenStream = gen_params.iter().map(param_to_matcher).collect();

    let mut cfg_names = CfgNames::default();
    let iter = original_item
        .items
        .iter()
        .map(|item| build_trait_items(item, trait_ident, &gen_idents, &mut cfg_names));
    let (struct_items, enum_items, self_items): (Vec<_>, Vec<_>, Vec<_>) =
        match process_results(iter, |iter| iter.multiunzip()) {
            Ok(tup) => tup,
            Err(err) => return err.to_compile_error(),
        };
    let assoc_ty_bounds = make_assoc_ty_bound(&original_item.items, original_item, &match_name);
    let gen_idents_pat: TokenStream = gen_idents.into_iter().map(|id| quote! {$ #id ,}).collect();
    let vis = &original_item.vis;
    let cfg_definitions = cfg_names.definitions(&macro_name, vis);
    quote! {
        #[macro_export]
        #[doc(hidden)]
        macro_rules! #macro_def {
            (body_struct(<#gen_matcher>, $ty:ty, $field_ident:tt)) => {
                #macro_name!{body_struct(<#gen_idents_pat>, $ty, ($field_ident), ($field_ident), ($field_ident))}
            };
            (body_struct(<#gen_matcher>, $ty:ty, ($($ident_owned:tt)*), ($($ident_ref:tt)*), ($($ident_ref_mut:tt)*))) => {
                #(#struct_items)*
            };
            (check_non_empty($err:literal, $s:ident.$($t:tt)+)) => {$s.$($t)*};
            (check_non_empty($err:literal, $s:ident.)) => {
                compile_error! {$err}
            };
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

        #[doc(inline)]
        #[doc = concat!("A macro to be used by [`ambassador::Delegate`] to delegate [`", stringify!(#trait_ident), "`]")]
        #vis use #macro_def as #macro_name;

        #cfg_definitions
    }
}

fn param_to_ident(param: &GenericParam) -> &Ident {
    match param {
        GenericParam::Type(TypeParam { ident, .. }) => ident,
        GenericParam::Lifetime(LifetimeParam { lifetime, .. }) => &lifetime.ident,
        GenericParam::Const(ConstParam { ident, .. }) => ident,
    }
}

fn param_to_matcher(param: &GenericParam) -> TokenStream {
    match param {
        GenericParam::Type(TypeParam { ident, .. }) => quote!($ #ident : ty,),
        GenericParam::Lifetime(LifetimeParam { lifetime, .. }) => {
            let ident = &lifetime.ident;
            quote!($ #ident : lifetime,)
        }
        GenericParam::Const(ConstParam { ident, .. }) => quote!($ #ident : expr,),
    }
}

fn param_to_tokens(param: &GenericParam) -> TokenStream {
    match param {
        GenericParam::Type(TypeParam { ident, .. }) => quote!(#ident,),
        GenericParam::Lifetime(LifetimeParam { lifetime, .. }) => quote!(#lifetime,),
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

// Replaces the identifiers in gen_idents with their macro args (X => $X) ('a => $a)
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
fn build_method(
    method_sig: &TokenStream,
    method_invocation: TokenStream,
    extr_attrs: TokenStream,
) -> TokenStream {
    quote! {
        #[inline]
        #[allow(unused_braces)]
        #extr_attrs
        #method_sig {
            #method_invocation
        }
    }
}

fn extract_cfg(attrs: &[Attribute]) -> Option<TokenStream> {
    let mut iter = attrs
        .iter()
        .filter(|attr| attr.style == AttrStyle::Outer && attr.path().is_ident("cfg"))
        .filter_map(|attr| match &attr.meta {
            syn::Meta::List(meta_list) => Some(meta_list.tokens.clone()),
            _ => None,
        });
    let e0 = iter.next()?;
    if let Some(e1) = iter.next() {
        let iter = IntoIterator::into_iter([e0, e1]).chain(iter);
        Some(quote!(all(#(#iter)*)))
    } else {
        Some(e0)
    }
}

fn build_trait_items(
    original_item: &TraitItem,
    trait_ident: &Ident,
    gen_idents: &[&Ident],
    cfg_names: &mut CfgNames,
) -> syn::Result<(TokenStream, TokenStream, TokenStream)> {
    let gen_pat: TokenStream = gen_idents.iter().flat_map(|id| quote! {$#id,}).collect();
    let macro_name = macro_name(trait_ident);
    let mut res = match original_item {
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
        TraitItem::Fn(original_method) => {
            let method_sig = original_method.sig.to_token_stream();
            let method_sig = replace_gen_idents(method_sig, gen_idents);
            (
                {
                    let field_ident = match receiver_type(&original_method.sig)? {
                        ReceiverType::Owned => {
                            quote!(#macro_name!(check_non_empty(
                                "target_owned was not specified but was needed", 
                                self.$($ident_owned)*)))
                        }
                        ReceiverType::Ref => {
                            quote!(#macro_name!(check_non_empty(
                                "target_ref was not specified but was needed", 
                                self.$($ident_ref)*)))
                        }
                        ReceiverType::MutRef => {
                            quote!(#macro_name!(check_non_empty(
                                "target_mut was not specified but was needed",
                                self.$($ident_ref_mut)*)))
                        }
                    };
                    let method_invocation = build_method_invocation(original_method, &field_ident);
                    build_method(&method_sig, method_invocation, quote!())
                },
                {
                    let method_invocation =
                        build_method_invocation(original_method, &quote!(inner));
                    let method_invocation = quote! {
                        match self {
                            $($variants(inner) => #method_invocation),*
                        }
                    };
                    build_method(&method_sig, method_invocation, quote!())
                },
                {
                    let method_invocation = build_method_invocation(original_method, &quote!(self));
                    build_method(
                        &method_sig,
                        method_invocation,
                        quote!(#[deny(unconditional_recursion)]),
                    )
                },
            )
        }
        _ => return error!(original_item.span(), "unsupported trait item"),
    };
    let attrs: &[Attribute] = match original_item {
        TraitItem::Const(c) => &c.attrs,
        TraitItem::Type(t) => &t.attrs,
        TraitItem::Fn(m) => &m.attrs,
        _ => &[],
    };
    if let Some(pred) = extract_cfg(attrs) {
        let wrapper_macro = cfg_names.get_macro(pred);
        let wrap = |x: TokenStream| quote!(#macro_name::#wrapper_macro!{#x});
        res = (wrap(res.0), wrap(res.1), wrap(res.2));
    }
    Ok(res)
}

fn build_method_invocation(
    original_method: &syn::TraitItemFn,
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

    let generics = method_sig.generics.params.iter().filter_map(|x| match x {
        GenericParam::Type(t) => Some(&t.ident),
        GenericParam::Lifetime(_) => None,
        GenericParam::Const(c) => Some(&c.ident),
    });
    let post = if original_method.sig.asyncness.is_some() {
        quote!(.await)
    } else {
        quote!()
    };

    let method_invocation =
        quote! { #field_ident.#method_ident::<#(#generics,)*>(#argument_list) #post };
    method_invocation
}
