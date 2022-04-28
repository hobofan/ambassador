use super::register::{macro_name, match_name};
use itertools::Itertools;
use proc_macro::TokenStream;
use proc_macro2::{Ident, TokenStream as TokenStream2, TokenTree};
use quote::{quote, ToTokens};
use std::cmp::Ordering;
use std::default::Default;
use syn::{
    parse_macro_input, parse_quote, DeriveInput, GenericParam, Generics, ImplGenerics, LitStr,
    PathArguments, Token, WhereClause,
};
use syn::{punctuated::Punctuated, token::Comma, WherePredicate};

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

struct DelegateArgs {
    target: DelegateTarget,
    where_clauses: Punctuated<WherePredicate, Comma>,
    generics: Vec<GenericParam>,
}

fn is_comma(tt: &TokenTree) -> bool {
    matches!(tt, TokenTree::Punct(p) if p.as_char() == ',')
}

impl DelegateArgs {
    pub fn from_tokens(tokens: TokenStream2) -> (syn::Path, Self) {
        const INVALID_MSG: &str = "Invalid syntax for delegate attribute";
        let mut outer_iter = tokens.into_iter();
        let mut iter = match outer_iter.next() {
            Some(TokenTree::Group(g)) => g.stream().into_iter(),
            _ => panic!("{}", INVALID_MSG),
        };
        assert!(outer_iter.next().is_none(), "{}", INVALID_MSG);
        let path: TokenStream2 = iter.by_ref().take_while(|tt| !is_comma(tt)).collect();
        let path = parse_quote! {#path};
        let mut res = DelegateArgs {
            target: DelegateTarget::None,
            where_clauses: Punctuated::new(),
            generics: Vec::new(),
        };
        loop {
            match iter.next_tuple() {
                Some((TokenTree::Ident(key), TokenTree::Punct(eq), TokenTree::Literal(val)))
                    if eq.as_char() == '=' =>
                {
                    let lit: LitStr = parse_quote!(#val);
                    match &*key.to_string() {
                        "target" => {
                            if !matches!(res.target, DelegateTarget::None) {
                                panic!("\"target\" value for delegate attribute can only be specified once");
                            }
                            res.target = if lit.value() == "self" {
                                DelegateTarget::TrgSelf
                            } else {
                                let target_val = lit.parse().expect("Invalid syntax for delegate attribute; Expected ident as value for \"target\"");
                                DelegateTarget::Field(target_val)
                            };
                        }
                        "where" => {
                            let where_clause_val = lit.parse_with(Punctuated::<WherePredicate, Comma>::parse_terminated).expect("Invalid syntax for delegate attribute; Expected where clause syntax as value for \"where\"");
                            res.where_clauses.extend(where_clause_val);
                        }
                        "generics" => {
                            let generics_val = lit.parse_with(Punctuated::<GenericParam, Comma>::parse_terminated).expect("Invalid syntax for delegate attribute; Expected list of generic parameters as value for \"generics\"");
                            res.generics.extend(generics_val);
                        }
                        _ => {
                            panic!("{} is not a valid key for a delegate attribute", key)
                        }
                    }
                }
                Some(_) => panic!("{}", INVALID_MSG),
                None => break, // We might have looped around with a trailing comma
            }
            match iter.next() {
                Some(p) if is_comma(&p) => continue, // comma go around again
                None => break,                       // no comma so we're done
                _ => panic!("{}", INVALID_MSG),
            }
        }
        (path, res)
    }
}

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

fn generics_for_impl(
    mut explicit_where_clauses: Punctuated<WherePredicate, Token![,]>,
    implementer: &DelegateImplementer,
) -> (ImplGenerics<'_>, syn::TypeGenerics<'_>, WhereClause) {
    let generics = &implementer.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    // Merges the where clause based on the type generics with all the where clauses specified
    // via "where" macro attributes.
    explicit_where_clauses.extend(where_clause.into_iter().flat_map(|n| n.predicates.clone()));
    let merged_where_clause = WhereClause {
        where_token: Default::default(),
        predicates: explicit_where_clauses,
    };

    (impl_generics, ty_generics, merged_where_clause)
}

fn add_auto_where_clause(clause: &mut WhereClause, trait_path_full: &syn::Path, ty: &syn::Type) {
    clause.predicates.push(parse_quote!(#ty : #trait_path_full))
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

fn merge_generics(
    impl_generics: ImplGenerics,
    added_generics: Vec<GenericParam>,
) -> impl Iterator<Item = GenericParam> {
    let tokens = impl_generics.into_token_stream();
    let impl_generics = if tokens.is_empty() {
        Punctuated::new()
    } else {
        let generics: Generics = parse_quote!(#tokens);
        generics.params
    };
    // Make sure all lifetimes come first
    impl_generics.into_iter().merge_by(added_generics, |x, _| {
        matches!(x, GenericParam::Lifetime(_))
    })
}

pub fn delegate_macro(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let input = parse_macro_input!(input as DeriveInput);
    let implementer = DelegateImplementer {
        info: input.data.into(),
        generics: input.generics,
        ty: input.ident,
    };

    let mut delegate_attributes = input
        .attrs
        .into_iter()
        .filter(|attr| attr.path.is_ident("delegate"))
        .map(|attr| attr.tokens)
        .peekable();
    if delegate_attributes.peek().is_none() {
        panic!("No #[delegate] attribute specified. If you want to delegate an implementation of trait `SomeTrait` add the attribute:\n#[delegate(SomeTrait)]")
    }

    let res: TokenStream2 = delegate_attributes
        .into_iter()
        .flat_map(|attr| delegate_single_atrr(&implementer, attr))
        .collect();

    res.into()
}

fn delegate_single_atrr(
    implementer: &DelegateImplementer,
    delegate_attr: TokenStream2,
) -> TokenStream2 {
    let implementer_ident = &implementer.ty;
    let (trait_path_full, args) = DelegateArgs::from_tokens(delegate_attr);
    let trait_segment = trait_path_full.segments.last().unwrap();
    let trait_ident: &Ident = &trait_segment.ident;
    let empty = Punctuated::new();
    let trait_generics = match &trait_segment.arguments {
        PathArguments::None => &empty,
        PathArguments::AngleBracketed(seg) => &seg.args,
        _ => panic!("cannot delegate to Fn* traits"),
    };
    let trait_generics_p = super::util::TailingPunctuated::wrap_ref(trait_generics);
    let mut added_generics = args.generics;
    added_generics.sort_unstable_by(|x, y| match (x, y) {
        (GenericParam::Lifetime(_), GenericParam::Lifetime(_)) => Ordering::Equal,
        (GenericParam::Lifetime(_), _) => Ordering::Less,
        (_, GenericParam::Lifetime(_)) => Ordering::Greater,
        _ => Ordering::Equal,
    });
    let macro_name: Ident = macro_name(trait_ident);

    let (impl_generics, ty_generics, mut where_clause) =
        generics_for_impl(args.where_clauses, implementer);
    let impl_generics = merge_generics(impl_generics, added_generics);
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
                if !matches!(args.target, DelegateTarget::None) {
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
                if !matches!(args.target, DelegateTarget::None) {
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
