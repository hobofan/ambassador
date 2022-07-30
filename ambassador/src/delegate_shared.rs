use itertools::Itertools;
use proc_macro2::{Ident, TokenStream as TokenStream2, TokenTree};
use quote::ToTokens;
use std::cmp::Ordering;
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{
    parse_quote, GenericParam, Generics, ImplGenerics, LitStr, PathArguments, Token, WhereClause,
    WherePredicate,
};

pub(super) trait DelegateTarget: Default {
    fn try_update(&mut self, key: &str, lit: LitStr) -> Option<()>;
    fn update(&mut self, key: &str, lit: LitStr) {
        self.try_update(key, lit)
            .unwrap_or_else(|| panic!("{} is not a valid key for a delegate attribute", key))
    }
}

#[derive(Default)]
pub(super) struct DelegateArgs<T: DelegateTarget> {
    pub(crate) target: T,
    pub(crate) where_clauses: Punctuated<WherePredicate, Comma>,
    pub(crate) generics: Vec<GenericParam>,
    pub(crate) inhibit_automatic_where_clause: bool,
}

fn is_comma(tt: &TokenTree) -> bool {
    matches!(tt, TokenTree::Punct(p) if p.as_char() == ',')
}

impl<T: DelegateTarget> DelegateArgs<T> {
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
        let mut res = DelegateArgs::<T>::default();
        loop {
            match iter.next_tuple() {
                Some((TokenTree::Ident(key), TokenTree::Punct(eq), TokenTree::Literal(val)))
                    if eq.as_char() == '=' =>
                {
                    let lit: LitStr = parse_quote!(#val);
                    match &*key.to_string() {
                        "where" => {
                            let where_clause_val = lit.parse_with(Punctuated::<WherePredicate, Comma>::parse_terminated).expect("Invalid syntax for delegate attribute; Expected where clause syntax as value for \"where\"");
                            res.where_clauses.extend(where_clause_val);
                        }
                        "generics" => {
                            let generics_val = lit.parse_with(Punctuated::<GenericParam, Comma>::parse_terminated).expect("Invalid syntax for delegate attribute; Expected list of generic parameters as value for \"generics\"");
                            res.generics.extend(generics_val);
                        }
                        "inhibit_automatic_where_clause" => {
                            let val = val.to_string();
                            if &val == "\"true\"" {
                                res.inhibit_automatic_where_clause = true;
                            } else if &val == "\"false\"" {
                                res.inhibit_automatic_where_clause = false;
                            } else {
                                panic!("inhibit_automatic_where_clause delegate attribute should have value \"true\" or \"false\".")
                            }
                        }
                        key => res.target.try_update(key, lit).unwrap_or_else(|| {
                            panic!("{} is not a valid key for a delegate attribute", key)
                        }),
                    }
                }
                Some(_) => panic!("{}", INVALID_MSG),
                None => {
                    // We might have looped around with a trailing comma, no attributes
                    //  at all or some unfinished attribute
                    // Unfortunately, it is not easy to discriminate those cases
                    // because of `Itertools::next_tuple` throws away partial data
                    // (which would have contained indication of possible error) and returns None.
                    break;
                }
            }
            match iter.next() {
                Some(p) if is_comma(&p) => continue, // comma go around again
                None => break,                       // no comma so we're done
                _ => panic!("{}", INVALID_MSG),
            }
        }
        res.generics.sort_unstable_by(|x, y| match (x, y) {
            (GenericParam::Lifetime(_), GenericParam::Lifetime(_)) => Ordering::Equal,
            (GenericParam::Lifetime(_), _) => Ordering::Less,
            (_, GenericParam::Lifetime(_)) => Ordering::Greater,
            _ => Ordering::Equal,
        });
        (path, res)
    }
}

pub(super) fn delegate_macro<I>(
    input: &I,
    attrs: Vec<syn::Attribute>,
    delegate_single: impl Fn(&I, TokenStream2) -> TokenStream2,
) -> TokenStream2 {
    // Parse the input tokens into a syntax tree
    let mut delegate_attributes = attrs
        .into_iter()
        .filter(|attr| attr.path.is_ident("delegate"))
        .map(|attr| attr.tokens)
        .peekable();
    if delegate_attributes.peek().is_none() {
        panic!("No #[delegate] attribute specified. If you want to delegate an implementation of trait `SomeTrait` add the attribute:\n#[delegate(SomeTrait)]")
    }

    let res: TokenStream2 = delegate_attributes
        .into_iter()
        .flat_map(|attr| delegate_single(input, attr))
        .collect();
    res
}

pub(super) fn trait_info(trait_path_full: &syn::Path) -> (&Ident, impl ToTokens + '_) {
    let trait_segment = trait_path_full.segments.last().unwrap();
    let trait_ident: &Ident = &trait_segment.ident;
    let trait_generics = match &trait_segment.arguments {
        PathArguments::None => None,
        PathArguments::AngleBracketed(seg) => Some(super::util::TailingPunctuated(&seg.args)),
        _ => panic!("cannot delegate to Fn* traits"),
    };
    (trait_ident, trait_generics)
}

pub(super) fn merge_impl_generics(
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

pub(super) fn merge_generics<'a>(
    impl_generics: &'a Punctuated<GenericParam, Token![,]>,
    added_generics: &'a [GenericParam],
) -> impl Iterator<Item = &'a GenericParam> {
    // Make sure all lifetimes come first
    impl_generics.iter().merge_by(added_generics, |&x, _| {
        matches!(x, GenericParam::Lifetime(_))
    })
}

pub(super) fn build_where_clause(
    mut explicit_where_clauses: Punctuated<WherePredicate, Token![,]>,
    where_clause: Option<&WhereClause>,
) -> WhereClause {
    // Merges the where clause based on the type generics with all the where clauses specified
    // via "where" macro attributes.
    explicit_where_clauses.extend(where_clause.into_iter().flat_map(|n| n.predicates.clone()));
    WhereClause {
        where_token: Default::default(),
        predicates: explicit_where_clauses,
    }
}

pub(super) fn add_auto_where_clause(
    clause: &mut WhereClause,
    trait_path_full: &syn::Path,
    ty: &syn::Type,
) {
    clause.predicates.push(parse_quote!(#ty : #trait_path_full))
}
