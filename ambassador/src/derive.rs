use super::delegate_shared::{self, add_auto_where_clause};
use super::register::{macro_name, match_name};
use proc_macro::TokenStream;
use proc_macro2::{Ident, TokenStream as TokenStream2};
use quote::quote;
use std::default::Default;
use syn::WherePredicate;
use syn::{parse_macro_input, parse_quote, DeriveInput, Generics, LitStr};

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

impl From<syn::Data> for DelegateImplementerInfo {
    fn from(data: syn::Data) -> Self {
        match data {
            syn::Data::Enum(enum_data) => {
                let (variant_idents, mut variant_types) = enum_data
                    .variants
                    .into_iter()
                    .map(|n| {
                        let mut it = n.fields.into_iter();
                        match it.next() {
                            None => panic!("enum variant {} has no fields", n.ident),
                            Some(f) => {
                                if it.count() != 0 {
                                    panic!("enum variant {} has multiple fields", n.ident)
                                };
                                (n.ident, f.ty)
                            }
                        }
                    })
                    .unzip::<_, _, Vec<_>, Vec<_>>();
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
            _ => panic!(
                "ambassador currently only supports #[derive(Delegate)] for: \n\
                 - single-field enums\n\
                 - (tuple) structs"
            ),
        }
    }
}

enum DelegateTarget {
    Field(syn::Member),
    None,
    TrgSelf,
}

impl Default for DelegateTarget {
    fn default() -> Self {
        DelegateTarget::None
    }
}

impl delegate_shared::DelegateTarget for DelegateTarget {
    fn try_update(&mut self, key: &str, lit: LitStr) -> Option<()> {
        match key {
            "target" => {
                if !matches!(self, DelegateTarget::None) {
                    panic!("\"target\" value for delegate attribute can only be specified once");
                }
                *self = if lit.value() == "self" {
                    DelegateTarget::TrgSelf
                } else {
                    let target_val = lit.parse().expect("Invalid syntax for delegate attribute; Expected ident as value for \"target\"");
                    DelegateTarget::Field(target_val)
                };
                Some(())
            }
            _ => None,
        }
    }
}

type DelegateArgs = delegate_shared::DelegateArgs<DelegateTarget>;

impl DelegateTarget {
    /// Select the correct field_ident based on the `target`.
    pub fn get_field<'a>(
        &self,
        field_idents: &'a [(syn::Member, syn::Type)],
    ) -> &'a (syn::Member, syn::Type) {
        let target = match self {
            DelegateTarget::Field(target) => target,
            _ => panic!("\"target\" value on #[delegate] attribute has to be specified for structs with multiple fields"),
        };

        let field = field_idents.iter().find(|n| n.0 == *target);
        if field.is_none() {
            panic!(
                "Unknown field \"{}\" specified as \"target\" value in #[delegate] attribute",
                PrettyTarget(target.clone())
            );
        }
        field.unwrap()
    }
}

struct PrettyTarget(syn::Member);

impl std::fmt::Display for PrettyTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self.0 {
            syn::Member::Named(ref ident) => write!(f, "{}", ident),
            syn::Member::Unnamed(ref index) => write!(f, "{}", index.index),
        }
    }
}

pub fn delegate_macro(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let input = parse_macro_input!(input as DeriveInput);
    let implementer = DelegateImplementer {
        info: input.data.into(),
        generics: input.generics,
        ty: input.ident,
    };
    delegate_shared::delegate_macro(implementer, input.attrs, delegate_single_attr)
}

fn delegate_single_attr(
    implementer: &DelegateImplementer,
    delegate_attr: TokenStream2,
) -> TokenStream2 {
    let (trait_path_full, args) = DelegateArgs::from_tokens(delegate_attr);
    let (trait_ident, trait_generics_p) = delegate_shared::trait_info(&trait_path_full);
    let macro_name: Ident = macro_name(trait_ident);

    let (impl_generics, ty_generics, mut where_clause) =
        delegate_shared::generics_for_impl(args.where_clauses, &implementer.generics);
    let impl_generics = delegate_shared::merge_generics(impl_generics, args.generics);
    let implementer_ident = &implementer.ty;
    if matches!(&args.target, DelegateTarget::TrgSelf) {
        quote! {
            impl <#(#impl_generics,)*> #trait_path_full for #implementer_ident #ty_generics #where_clause {
                #macro_name!{body_self(<#trait_generics_p>)}
            }
        }
    } else {
        match &implementer.info {
            DelegateImplementerInfo::Enum {
                variant_idents,
                first_type,
                other_types,
            } => {
                if !matches!(&args.target, DelegateTarget::None) {
                    panic!(
                        "\"target\" value on #[delegate] attribute can not be specified for enums"
                    );
                }
                add_auto_where_clause(&mut where_clause, &trait_path_full, first_type);
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
            DelegateImplementerInfo::SingleFieldStruct {
                field_ident,
                field_type,
            } => {
                if !matches!(&args.target, DelegateTarget::None) {
                    panic!("\"target\" value on #[delegate] attribute can not be specified for structs with a single field");
                }
                add_auto_where_clause(&mut where_clause, &trait_path_full, field_type);

                quote! {
                    impl <#(#impl_generics,)*> #trait_path_full for #implementer_ident #ty_generics #where_clause {
                        #macro_name!{body_struct(<#trait_generics_p>, #field_type, #field_ident)}
                    }
                }
            }
            DelegateImplementerInfo::MultiFieldStruct { fields } => {
                let field = args.target.get_field(fields);
                let field_ident = &field.0;
                let field_type = &field.1;
                add_auto_where_clause(&mut where_clause, &trait_path_full, field_type);

                quote! {
                    impl <#(#impl_generics,)*> #trait_path_full for #implementer_ident #ty_generics #where_clause {
                        #macro_name!{body_struct(<#trait_generics_p>, #field_type, #field_ident)}
                    }
                }
            }
        }
    }
}
