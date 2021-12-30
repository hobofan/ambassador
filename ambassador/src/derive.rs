use proc_macro::{TokenStream};
use std::cmp::Ordering;
use proc_macro2::{TokenStream as TokenStream2, TokenTree};
use quote::{quote, ToTokens};
use std::default::Default;
use syn::{parse_macro_input, parse_quote, DeriveInput, Generics, PathArguments, GenericParam, ImplGenerics, LitStr};
use syn::{punctuated::Punctuated, token::Comma, WherePredicate};
use lazy_regex::regex_is_match;
use itertools::Itertools;
use super::register::{macro_name, match_name};

#[derive(Clone, Debug)]
enum DelegateImplementer {
    Enum {
        variant_idents: Vec<syn::Ident>,
        first_type: syn::Type,
        other_types: Vec<syn::Type>,
        generics: Generics,
    },
    SingleFieldStruct {
        field_ident: syn::Member,
        field_type: syn::Type,
        generics: Generics,
    },
    MultiFieldStruct {
        fields: Vec<(syn::Member, syn::Type)>,
        generics: Generics,
    },
}

impl DelegateImplementer {
    fn new(data: syn::Data, generics: syn::Generics) -> Self {
        let implementer: DelegateImplementer = match data {
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
                DelegateImplementer::Enum {
                    variant_idents,
                    first_type,
                    other_types: variant_types,
                    generics,
                }
            }
            syn::Data::Struct(struct_data) => match struct_data.fields.len() {
                0 => panic!("struct has no fields"),
                1 => {
                    let field = struct_data.fields.into_iter().next().unwrap();
                    let field_ident = match field.ident {
                        Some(id) => syn::Member::Named(id),
                        None => syn::Member::Unnamed(0.into()),
                    };
                    DelegateImplementer::SingleFieldStruct {
                        field_ident,
                        field_type: field.ty,
                        generics,
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
                    DelegateImplementer::MultiFieldStruct { fields, generics }
                }
            },
            _ => panic!(
                "ambassador currently only supports #[derive(Delegate)] for: \n\
                 - single-field enums\n\
                 - (tuple) structs"
            ),
        };
        implementer
    }
}

struct DelegateArgs {
    target: Option<syn::Member>,
    where_clauses: Punctuated<WherePredicate, Comma>,
}

fn is_comma(tt: &TokenTree) -> bool{
    matches!(tt, TokenTree::Punct(p) if p.as_char() == ',')
}

impl DelegateArgs {
    pub fn from_tokens(tokens: TokenStream2) -> (syn::Path, Self) {
        const INVALID_MSG: &str = "Invalid syntax for delegate attribute";
        let mut outer_iter = tokens.into_iter();
        let mut iter= match outer_iter.next() {
            Some(TokenTree::Group(g)) => g.stream().into_iter(),
            _ => panic!("1{}", INVALID_MSG)
        };
        assert!(outer_iter.next().is_none(), "2{}", INVALID_MSG);
        let path: TokenStream2 = iter.by_ref().take_while(|tt| !is_comma(tt)).collect();
        let path = parse_quote!{#path};
        let mut target = None;
        let mut where_clauses = Punctuated::new();
        loop {
            match iter.next_tuple() {
                Some((TokenTree::Ident(key), TokenTree::Punct(eq), TokenTree::Literal(val)))
                if eq.as_char() == '=' => {
                    let lit: LitStr = parse_quote!(#val);
                    match &*key.to_string() {
                        "target" => {
                            let target_val: syn::Member = lit.parse().expect("Invalid syntax for delegate attribute; Expected ident as value for \"target\"");
                            if target.is_some() {
                                panic!("\"target\" value for delegate attribute can only be specified once");
                            }
                            target = Some(target_val)
                        }
                        "where" => {
                            let where_clause_val = lit.parse_with(Punctuated::<WherePredicate, Comma>::parse_terminated).expect("Invalid syntax for delegate attribute; Expected where clause syntax as value for \"where\"");
                            where_clauses.extend(where_clause_val);
                        }
                        _ => {
                            panic!("3{} is not a valid key for a delegate attribute", key)
                        }
                    }
                }
                Some(_) => panic!("4{}", INVALID_MSG),
                None => break // We might have looped around with a trailing comma
            }
            match iter.next() {
                Some(p) if is_comma(&p) => continue, // comma go around again
                None => break, // no comma so we're done
                _ => panic!("6{}", INVALID_MSG)
            }
        }
        (path, Self{target, where_clauses})
    }

    /// Select the correct field_ident based on the `target`.
    pub fn get_field<'a>(
        &self,
        field_idents: &'a [(syn::Member, syn::Type)],
    ) ->  &'a (syn::Member, syn::Type) {
        if self.target.is_none() {
            panic!("\"target\" value on #[delegate] attribute has to be specified for structs with multiple fields");
        }
        let target = self.target.as_ref().unwrap();

        let field = field_idents.iter().find(|n| n.0 == *target);
        if field.is_none() {
            panic!(
                "Unknown field \"{}\" specified as \"target\" value in #[delegate] attribute",
                PrettyTarget(target.clone())
            );
        }
        field.unwrap()
    }

    fn generics_for_impl<'a>(
        self,
        trait_path_full: &syn::Path,
        implementer: &'a DelegateImplementer,
        ty: &syn::Type,
    ) -> (syn::ImplGenerics<'a>, syn::TypeGenerics<'a>, syn::WhereClause) {
        let generics = match implementer {
            DelegateImplementer::Enum { ref generics, .. } => generics,
            DelegateImplementer::SingleFieldStruct { ref generics, .. } => generics,
            DelegateImplementer::MultiFieldStruct { ref generics, .. } => generics,
        };
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        // Merges the where clause based on the type generics with all the where clauses specified
        // via "where" macro attributes.
        let Self{where_clauses: mut explicit_where_clauses, .. } = self;
        explicit_where_clauses.extend(where_clause.into_iter().flat_map(|n| n.predicates.clone()));
        explicit_where_clauses.push(parse_quote!(#ty : #trait_path_full));
        let merged_where_clause = syn::WhereClause {
            where_token: Default::default(),
            predicates: explicit_where_clauses,
        };

        (impl_generics, ty_generics, merged_where_clause)
    }
}

struct PrettyTarget(syn::Member);

impl std::fmt::Display for PrettyTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self.0 {
            syn::Member::Named(ref ident) => write!(f, "{}", ident.to_string()),
            syn::Member::Unnamed(ref index) => write!(f, "{}", index.index),
        }
    }
}

fn merge_generics(impl_generics: ImplGenerics, added_generics: Vec<GenericParam>) -> impl Iterator<Item=GenericParam> {
    let tokens = impl_generics.into_token_stream();
    let impl_generics = if tokens.is_empty() {
        Punctuated::new()
    } else {
        let generics: Generics = parse_quote!(#tokens);
        generics.params
    };
    // Make sure all lifetimes come first
    impl_generics.into_iter().merge_by(added_generics, |x, _| matches!(x, GenericParam::Lifetime(_)))
}



fn find_added_generics(ts: TokenStream2, dst: &mut Vec<GenericParam>) {
    let mut iter = ts.into_iter().peekable();
     match iter.peek() {
        Some(TokenTree::Ident(id)) if regex_is_match!(r"X\d*", &id.to_string()) => dst.push(parse_quote!(#id)),
         Some(TokenTree::Group(g)) => find_added_generics(g.stream(), dst),
        _ => {}
    };
    iter.tuple_windows().for_each(|x| match x {
        (_, TokenTree::Ident(id)) if regex_is_match!(r"X\d*", &id.to_string()) => dst.push(parse_quote!(#id)),
        (TokenTree::Punct(p), TokenTree::Ident(id)) if regex_is_match!(r"x\d*", &id.to_string()) && p.as_char() == '\''
        => dst.push(parse_quote!{#p #id}),
        (_, TokenTree::Group(g)) => find_added_generics(g.stream(), dst),
        _ => {}
    })
}

pub fn delegate_macro(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let input = parse_macro_input!(input as DeriveInput);
    let implementer = DelegateImplementer::new(input.data, input.generics);
    let implementer_ident = input.ident;

    let mut delegate_attributes = input
        .attrs
        .into_iter()
        .filter(|attr| attr.path.is_ident("delegate"))
        .map(|attr| attr.tokens)
        .peekable();
    if delegate_attributes.peek().is_none() {
        panic!("No #[delegate] attribute specified. If you want to delegate an implementation of trait `SomeTrait` add the attribute:\n#[delegate(SomeTrait)]")
    }

    let mut res = TokenStream2::new();

    for delegate_attr in delegate_attributes {
        let (trait_path_full, args) = DelegateArgs::from_tokens(delegate_attr);
        let trait_segment = trait_path_full.segments.last().unwrap();
        let trait_ident: &syn::Ident = &trait_segment.ident;
        let empty = Punctuated::new();
        let trait_generics = match &trait_segment.arguments {
            PathArguments::None => &empty,
            PathArguments::AngleBracketed(seg) => &seg.args,
            _ => panic!("cannot delegate to Fn* traits")
        };
        let trait_generics_p = super::util::TailingPunctuated::wrap_ref(trait_generics);
        let mut added_generics = Vec::new();
        trait_generics.iter().for_each(|x| find_added_generics(x.to_token_stream(), &mut added_generics));
        added_generics.sort_unstable_by(|x, y| match (x, y) {
            (GenericParam::Lifetime(_), GenericParam::Lifetime(_)) => Ordering::Equal,
            (GenericParam::Lifetime(_), _) => Ordering::Less,
            (_, GenericParam::Lifetime(_)) => Ordering::Greater,
            _ => Ordering::Equal
        });
        let macro_name: syn::Ident = macro_name(trait_ident);

        let impl_macro = match &implementer {
            DelegateImplementer::Enum {
                variant_idents,
                first_type,
                other_types,
                ..
            } => {
                if args.target.is_some() {
                    panic!(
                        "\"target\" value on #[delegate] attribute can not be specified for enums"
                    );
                }
                let (impl_generics, ty_generics, mut where_clause) =
                    args.generics_for_impl(&trait_path_full, &implementer, first_type);
                let impl_generics = merge_generics(impl_generics, added_generics);
                let match_name = match_name(trait_ident);
                where_clause.predicates.extend(
                    other_types
                        .into_iter()
                        .map::<WherePredicate, _>(|arg| parse_quote!(#arg : #match_name<#trait_generics_p #first_type>)),
                );
                let mod_name = quote::format_ident!("ambassador_module_{}_for_{}", trait_ident, implementer_ident);
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
            DelegateImplementer::SingleFieldStruct {
                field_ident,
                field_type,
                ..
            } => {
                if args.target.is_some() {
                    panic!("\"target\" value on #[delegate] attribute can not be specified for structs with a single field");
                }
                let (impl_generics, ty_generics, where_clause) =
                    args.generics_for_impl(&trait_path_full, &implementer, field_type);
                let impl_generics = merge_generics(impl_generics, added_generics);

                quote! {
                    impl <#(#impl_generics,)*> #trait_path_full for #implementer_ident #ty_generics #where_clause {
                        #macro_name!{body_struct(<#trait_generics_p>, #field_type, #field_ident)}
                    }
                }
            }
            DelegateImplementer::MultiFieldStruct { fields, .. } => {
                let field = args.get_field(fields);
                let field_ident = &field.0;
                let field_type = &field.1;
                let (impl_generics, ty_generics, where_clause) =
                    args.generics_for_impl(&trait_path_full, &implementer, field_type);
                let impl_generics = merge_generics(impl_generics, added_generics);
                quote! {
                    impl <#(#impl_generics,)*> #trait_path_full for #implementer_ident #ty_generics #where_clause {
                        #macro_name!{body_struct(<#trait_generics>, #field_type, #field_ident)}
                    }
                }
            }
        };
        res.extend(impl_macro)
    }

    // Hand the output tokens back to the compiler
    res.into()
}
