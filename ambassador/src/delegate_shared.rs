use crate::util::{error, process_results};
use itertools::Itertools;
use proc_macro2::{Ident, TokenStream as TokenStream2};
use quote::ToTokens;
use std::cmp::Ordering;
use syn::ext::IdentExt;
use syn::parse::{ParseStream, Parser};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::Comma;
use syn::{
    parse_quote, Error, GenericParam, Generics, ImplGenerics, LitBool, LitStr, PathArguments,
    Result, Token, WhereClause, WherePredicate,
};

pub(super) trait DelegateTarget: Default {
    fn try_update(&mut self, key: &str, lit: LitStr) -> Option<Result<()>>;
}

#[derive(Default)]
pub(super) struct DelegateArgs<T: DelegateTarget> {
    pub(crate) target: T,
    pub(crate) where_clauses: Punctuated<WherePredicate, Comma>,
    pub(crate) generics: Vec<GenericParam>,
    pub(crate) inhibit_automatic_where_clause: bool,
}

impl<T: DelegateTarget> DelegateArgs<T> {
    fn add_key_value(&mut self, key: Ident, lit: LitStr) -> Result<()> {
        let span = key.span();
        match &*key.to_string() {
            "where" => {
                let where_clause_val =
                    lit.parse_with(Punctuated::<WherePredicate, Comma>::parse_terminated)?;
                self.where_clauses.extend(where_clause_val);
            }
            "generics" => {
                let generics_val =
                    lit.parse_with(Punctuated::<GenericParam, Comma>::parse_terminated)?;
                self.generics.extend(generics_val);
            }
            "automatic_where_clause" => {
                let auto_where_val: LitBool = lit.parse()?;
                self.inhibit_automatic_where_clause = !auto_where_val.value;
            }
            key => self
                .target
                .try_update(key, lit)
                .unwrap_or_else(|| error!(span, "invalid key for a delegate attribute"))?,
        }
        Ok(())
    }
}

pub(super) fn delegate_attr_as_trait_and_iter<T: DelegateTarget>(
    outer_steam: ParseStream<'_>,
) -> Result<(syn::Path, DelegateArgs<T>)> {
    let items;
    syn::parenthesized!(items in outer_steam);
    let path = items.parse()?;
    let mut delegate_args = DelegateArgs::default();
    while !items.is_empty() {
        let _: Token![,] = items.parse()?;
        let key = items.call(Ident::parse_any)?;
        let _: Token![=] = items.parse()?;
        let val = items.parse()?;
        delegate_args.add_key_value(key, val)?;
    }
    Ok((path, delegate_args))
}

impl<T: DelegateTarget> DelegateArgs<T> {
    pub fn from_tokens(tokens: TokenStream2) -> Result<(syn::Path, Self)> {
        let (path, mut res) = delegate_attr_as_trait_and_iter.parse2(tokens)?;
        res.generics.sort_unstable_by(|x, y| match (x, y) {
            (GenericParam::Lifetime(_), GenericParam::Lifetime(_)) => Ordering::Equal,
            (GenericParam::Lifetime(_), _) => Ordering::Less,
            (_, GenericParam::Lifetime(_)) => Ordering::Greater,
            _ => Ordering::Equal,
        });
        Ok((path, res))
    }
}

pub(super) fn delegate_macro<I>(
    input: &I,
    attrs: Vec<syn::Attribute>,
    delegate_single: impl Fn(&I, TokenStream2) -> Result<TokenStream2>,
) -> TokenStream2 {
    // Parse the input tokens into a syntax tree
    let mut delegate_attributes = attrs
        .into_iter()
        .filter(|attr| attr.path.is_ident("delegate"))
        .map(|attr| attr.tokens)
        .peekable();
    if delegate_attributes.peek().is_none() {
        return error!(
            proc_macro2::Span::call_site(),
            "No #[delegate] attribute specified. If you want to delegate an implementation of trait `SomeTrait` add the attribute:\n#[delegate(SomeTrait)]"
        ).unwrap_or_else(Error::into_compile_error);
    }

    let iter = delegate_attributes
        .into_iter()
        .map(|attr| delegate_single(input, attr));
    let res = process_results(iter, |iter| iter.flatten().collect());
    res.unwrap_or_else(Error::into_compile_error)
}

pub(super) fn trait_info(trait_path_full: &syn::Path) -> Result<(&Ident, impl ToTokens + '_)> {
    let trait_segment = trait_path_full.segments.last().unwrap();
    let trait_ident: &Ident = &trait_segment.ident;
    let trait_generics = match &trait_segment.arguments {
        PathArguments::None => None,
        PathArguments::AngleBracketed(seg) => Some(super::util::TailingPunctuated(&seg.args)),
        _ => error!(trait_path_full.span(), "cannot delegate to Fn* traits")?,
    };
    Ok((trait_ident, trait_generics))
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
