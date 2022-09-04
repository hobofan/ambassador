use super::delegate_shared::{self, add_auto_where_clause, error, try_option};
use super::register::{macro_name, match_name};
use itertools::process_results;
use proc_macro::TokenStream;
use proc_macro2::{Ident, Span, TokenStream as TokenStream2};
use quote::quote;
use std::default::Default;
use syn::spanned::Spanned;
use syn::{parse_macro_input, parse_quote, DeriveInput, Generics, LitStr, Result, WherePredicate};

#[derive(Debug)]
struct DelegateImplementer {
    generics: Generics,
    ty: Ident,
    info: DelegateImplementerInfo,
}

#[derive(Debug)]
enum DelegateImplementerInfo {
    Enum {
        variant_idents: Vec<Ident>,
        first_type: syn::Type,
        other_types: Vec<syn::Type>,
    },
    SingleFieldStruct {
        field_ident: syn::Member,
        field_type: syn::Type,
    },
    MultiFieldStruct {
        fields: Vec<(syn::Member, syn::Type)>,
    },
}

fn try_info_from_data(span: Span, data: syn::Data) -> Result<DelegateImplementerInfo> {
    let res = match data {
        syn::Data::Enum(enum_data) => {
            let iter = enum_data.variants.into_iter().map(|n| {
                let span = n.span();
                let mut it = n.fields.into_iter();
                match it.next() {
                    None => error!(span, "enum variant has no fields"),
                    Some(f) => {
                        if it.count() != 0 {
                            error!(span, "enum variant has multiple fields")?
                        };
                        Ok((n.ident, f.ty))
                    }
                }
            });
            let (variant_idents, mut variant_types): (Vec<_>, Vec<_>) =
                process_results(iter, |iter| iter.unzip())?;
            let first_type = variant_types.pop().expect("enum has no variants");
            DelegateImplementerInfo::Enum {
                variant_idents,
                first_type,
                other_types: variant_types,
            }
        }
        syn::Data::Struct(struct_data) => match struct_data.fields.len() {
            1 => {
                let field = struct_data.fields.into_iter().next().unwrap();
                let field_ident = match field.ident {
                    Some(id) => syn::Member::Named(id),
                    None => syn::Member::Unnamed(0.into()),
                };
                DelegateImplementerInfo::SingleFieldStruct {
                    field_ident,
                    field_type: field.ty,
                }
            }
            _ => {
                let fields = struct_data
                    .fields
                    .into_iter()
                    .enumerate()
                    .map(|(i, field)| match field.ident {
                        Some(id) => (syn::Member::Named(id), field.ty),
                        None => (syn::Member::Unnamed(i.into()), field.ty),
                    })
                    .collect();
                DelegateImplementerInfo::MultiFieldStruct { fields }
            }
        },
        _ => error!(
            span,
            "ambassador currently only supports #[derive(Delegate)] for: \n\
             - single-field enums\n\
             - (tuple) structs"
        )?,
    };
    Ok(res)
}

enum DelegateTarget {
    Field(syn::Member),
    TrgNone,
    TrgSelf,
}

impl Default for DelegateTarget {
    fn default() -> Self {
        DelegateTarget::TrgNone
    }
}

impl delegate_shared::DelegateTarget for DelegateTarget {
    fn try_update(&mut self, key: &str, lit: LitStr) -> Option<Result<()>> {
        match key {
            "target" => {
                if !matches!(self, DelegateTarget::TrgNone) {
                    try_option!(error!(
                        lit.span(),
                        "\"target\" value for delegate attribute can only be specified once"
                    ));
                }
                *self = if lit.value() == "self" {
                    DelegateTarget::TrgSelf
                } else {
                    let target_val = try_option!(lit.parse());
                    DelegateTarget::Field(target_val)
                };
                Some(Ok(()))
            }
            _ => None,
        }
    }
}

type DelegateArgs = delegate_shared::DelegateArgs<DelegateTarget>;

/// Select the correct field_ident based on the `target`.
pub fn get_field<'a>(
    target: &syn::Member,
    field_idents: &'a [(syn::Member, syn::Type)],
) -> Result<&'a (syn::Member, syn::Type)> {
    let field = field_idents.iter().find(|n| n.0 == *target);
    match field {
        Some(field) => Ok(field),
        None => error!(
            target.span(),
            "Unknown field specified as \"target\" value in #[delegate] attribute"
        ),
    }
}

pub fn delegate_macro(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let input = parse_macro_input!(input as DeriveInput);
    let info = match try_info_from_data(input.span(), input.data) {
        Ok(info) => info,
        Err(err) => return err.into_compile_error().into(),
    };
    let implementer = DelegateImplementer {
        info,
        generics: input.generics,
        ty: input.ident,
    };
    delegate_shared::delegate_macro(&implementer, input.attrs, delegate_single_attr).into()
}

fn delegate_single_attr(
    implementer: &DelegateImplementer,
    delegate_attr: TokenStream2,
) -> Result<TokenStream2> {
    let span = delegate_attr.span();
    let (trait_path_full, args) = DelegateArgs::from_tokens(delegate_attr)?;
    let (trait_ident, trait_generics_p) = delegate_shared::trait_info(&trait_path_full)?;
    let macro_name: Ident = macro_name(trait_ident);

    let generics = &implementer.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let mut where_clause = delegate_shared::build_where_clause(args.where_clauses, where_clause);
    let impl_generics = delegate_shared::merge_impl_generics(impl_generics, args.generics);
    let implementer_ident = &implementer.ty;
    use {DelegateImplementerInfo::*, DelegateTarget::*};
    let res = match (&args.target, &implementer.info) {
        (TrgSelf, _) => quote! {
            impl <#(#impl_generics,)*> #trait_path_full for #implementer_ident #ty_generics #where_clause {
                #macro_name!{body_self(<#trait_generics_p>)}
            }
        },
        (Field(field), Enum {..}) => error!(
            field.span(),
            "\"target\" value on #[delegate] attribute can not be specified for enums"
        )?,
        (TrgNone, Enum {variant_idents, first_type, other_types}) => {
            if !args.inhibit_automatic_where_clause {
                add_auto_where_clause(&mut where_clause, &trait_path_full, first_type);
            }
            let match_name = match_name(trait_ident);
            where_clause
                .predicates
                .extend(other_types.iter().map::<WherePredicate, _>(
                    |arg| parse_quote!(#arg : #match_name<#trait_generics_p #first_type>),
                ));
            let mod_name = quote::format_ident!(
                "ambassador_module_{}_for_{}",
                trait_ident,
                implementer_ident
            );
            quote! {
                #[allow(non_snake_case)]
                mod #mod_name {
                    use super::*;
                    #macro_name!{use_assoc_ty_bounds}
                    impl <#(#impl_generics,)*> #trait_path_full for #implementer_ident #ty_generics #where_clause {
                        #macro_name!{body_enum(<#trait_generics_p>, #first_type, (#(#other_types),*), (#(#implementer_ident::#variant_idents),*))}
                    }
                }
            }
        }
        (Field(field), SingleFieldStruct {..}) => error!(
            field.span(),
            "\"target\" value on #[delegate] attribute can not be specified for structs with a single field"
        )?,
        (TrgNone, SingleFieldStruct {field_ident, field_type}) => {
            if !args.inhibit_automatic_where_clause {
                add_auto_where_clause(&mut where_clause, &trait_path_full, field_type);
            }

            quote! {
                impl <#(#impl_generics,)*> #trait_path_full for #implementer_ident #ty_generics #where_clause {
                    #macro_name!{body_struct(<#trait_generics_p>, #field_type, #field_ident)}
                }
            }
        }
        (TrgNone, MultiFieldStruct {..}) => error!(
            span,
            "\"target\" value on #[delegate] attribute has to be specified for structs with multiple fields"
        )?,
        (Field(field), MultiFieldStruct {fields}) => {
            let field = get_field(field, fields)?;
            let field_ident = &field.0;
            let field_type = &field.1;
            if !args.inhibit_automatic_where_clause {
                add_auto_where_clause(&mut where_clause, &trait_path_full, field_type);
            }

            quote! {
                impl <#(#impl_generics,)*> #trait_path_full for #implementer_ident #ty_generics #where_clause {
                    #macro_name!{body_struct(<#trait_generics_p>, #field_type, #field_ident)}
                }
            }
        }
    };
    Ok(res)
}
