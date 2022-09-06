use super::delegate_shared::{self, add_auto_where_clause};
use super::register::macro_name;
use super::util;
use crate::util::{error, try_option, ReceiverType};
use itertools::Itertools;
use proc_macro::TokenStream;
use proc_macro2::{Ident, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use std::cell::Cell;
use std::convert::{TryFrom, TryInto};
use std::default::Default;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, GenericParam, ItemImpl, LitStr, Result, ReturnType, Token, Type, WhereClause,
};

struct DelegateImplementer {
    ty: Type,
    impl_generics: Punctuated<GenericParam, Token![,]>,
    where_clause: Option<WhereClause>,
    methods: Vec<MethodInfo>,
    invalid_methods: Vec<(Ident, syn::Error)>,
}

struct MethodInfo {
    name: Ident,
    receiver: ReceiverType,
    ret: Type,
    used: Cell<bool>, // modified when a usage is found
}

impl TryFrom<syn::ImplItemMethod> for MethodInfo {
    type Error = (Ident, syn::Error);

    fn try_from(method: syn::ImplItemMethod) -> std::result::Result<Self, (Ident, syn::Error)> {
        let receiver_or_err = util::receiver_type(&method.sig);
        let return_span = method.sig.paren_token.span;
        let mut ident = Some(method.sig.ident);
        let mut add_ident = |err| (ident.take().unwrap(), err);
        let receiver = receiver_or_err.map_err(&mut add_ident)?;
        let ret = match method.sig.output {
            ReturnType::Default => {
                error!(return_span, "delegated to methods must return").map_err(&mut add_ident)?
            }
            ReturnType::Type(_, t) => *t,
        };
        let ret = match (ret, receiver) {
            (ret, ReceiverType::Owned) => ret,
            (
                Type::Reference(syn::TypeReference {
                    mutability, elem, ..
                }),
                ReceiverType::Ref,
            ) if mutability.is_none() => *elem,
            (
                Type::Reference(syn::TypeReference {
                    mutability, elem, ..
                }),
                ReceiverType::MutRef,
            ) if mutability.is_some() => *elem,
            (ret, _) => error!(
                ret.span(),
                "delegated to methods must have mutability of return type must match \"self\""
            )
            .map_err(&mut add_ident)?,
        };
        if method.sig.inputs.len() != 1 {
            // Just the receiver
            error!(
                ret.span(),
                "delegated to methods must only take \"self\" parameter"
            )
            .map_err(&mut add_ident)?
        }
        Ok(MethodInfo {
            name: ident.unwrap(),
            receiver,
            ret,
            used: Cell::new(false),
        })
    }
}

#[derive(Default)]
struct DelegateTarget {
    owned_id: Option<Ident>,
    ref_id: Option<Ident>,
    ref_mut_id: Option<Ident>,
}

impl delegate_shared::DelegateTarget for DelegateTarget {
    fn try_update(&mut self, key: &str, lit: LitStr) -> Option<Result<()>> {
        match key {
            "target_owned" => {
                self.owned_id = try_option!(lit.parse());
                Some(Ok(()))
            }
            "target_ref" => {
                self.ref_id = try_option!(lit.parse());
                Some(Ok(()))
            }
            "target_mut" => {
                self.ref_mut_id = try_option!(lit.parse());
                Some(Ok(()))
            }
            _ => None,
        }
    }
}

impl DelegateTarget {
    fn as_arr(&self) -> [(ReceiverType, Option<&Ident>); 3] {
        use ReceiverType::*;
        [
            (Owned, self.owned_id.as_ref()),
            (Ref, self.ref_id.as_ref()),
            (MutRef, self.ref_mut_id.as_ref()),
        ]
    }
}

type DelegateArgs = delegate_shared::DelegateArgs<DelegateTarget>;

fn search_methods<'a>(
    id: &Ident,
    implementer: &'a DelegateImplementer,
    receiver: ReceiverType,
) -> Result<&'a Type> {
    let DelegateImplementer {
        methods,
        invalid_methods,
        ..
    } = implementer;
    match methods.iter().find(|m| &m.name == id) {
        None => match invalid_methods.iter().find(|(name, _)| name == id) {
            Some((_, err)) => {
                let mut err: syn::Error = err.clone();
                let note = syn::Error::new(id.span(), "Note: method used in #[delegate] attribute");
                err.combine(note);
                Err(err)
            }
            None => error!(
                id.span(),
                "impl block doesn't have any methods with this name"
            ),
        },
        Some(res) => {
            res.used.set(true);
            if res.receiver != receiver {
                error!(
                    id.span(),
                    "method needs to have a receiver of type {}", receiver
                )
            } else {
                Ok(&res.ret)
            }
        }
    }
}

impl DelegateTarget {
    /// Select the correct return.
    pub fn get_ret_type<'a>(
        &self,
        span: proc_macro2::Span,
        implementer: &'a DelegateImplementer,
    ) -> Result<&'a Type> {
        let res = self
            .as_arr()
            .iter()
            .flat_map(|(recv_ty, id)| id.map(|id| search_methods(id, implementer, *recv_ty)))
            .fold(None, |rsf, x| match (rsf, x) {
                (None, x) => Some(x),
                (_, Err(x)) | (Some(Err(x)), _) => Some(Err(x)),
                (Some(Ok(y)), Ok(x)) if y == x => Some(Ok(x)),
                (Some(Ok(y)), Ok(x)) => {
                    let mut err =
                        syn::Error::new(span, "target methods have different return types");
                    let err1 = syn::Error::new(x.span(), "Note: first return type defined here");
                    let err2 = syn::Error::new(y.span(), "Note: other return type defined here");
                    err.combine(err1);
                    err.combine(err2);
                    Some(Err(err))
                }
            });
        res.unwrap_or_else(|| error!(span, "no targets were specified"))
    }
}

// Checks that:
//   - all the items in an impl are methods
//   - all the methods are _empty_ (no body, just the signature)
fn check_for_method_impls_and_extras(impl_items: &[syn::ImplItem]) -> Result<()> {
    let iter = impl_items.iter().filter_map(|i| {
        // We're looking for *only* empty methods (no block).
        if let syn::ImplItem::Method(m) = i {
            let block = &m.block;
            let empty_block = syn::parse2::<syn::ImplItemMethod>(quote! { fn foo(); })
                .unwrap()
                .block;

            // We'll accept `{}` blocks and omitted blocks (i.e. `fn foo();`):
            if block.stmts.is_empty() || block == &empty_block {
                None
            } else {
                Some(syn::Error::new(
                    block.span(),
                    "Only method signatures are allowed here (no blocks!)",
                ))
            }
        } else {
            // Everything else is an error:
            Some(syn::Error::new(
                i.span(),
                "Only method signatures are allowed here, everything else is discarded",
            ))
        }
    });
    fold_errors(Ok(()), iter)
}

// Checks that:
//   - all the methods provided are actually referenced in the `delegate` attributes on the impl
fn check_for_unused_methods(other_errs: Result<()>, methods: &[MethodInfo]) -> Result<()> {
    let iter = methods.iter().filter_map(|m| {
        if m.used.get() {
            None
        } else {
            Some(syn::Error::new(
                m.name.span(),
                "This method is not used by any `delegate` attributes; please remove it",
            ))
        }
    });
    fold_errors(other_errs, iter)
}

fn fold_errors(init: Result<()>, i: impl Iterator<Item = syn::Error>) -> Result<()> {
    i.fold(init, |errors, error| match (errors, error) {
        (Ok(_), err) => Err(err),
        (Err(mut errs), err) => {
            errs.extend(err);
            Err(errs)
        }
    })
}

enum KeepInfo {
    Keep(TokenStream2),
    Discard(Result<()>),
}

pub fn delegate_macro(input: TokenStream, keep_impl_block: bool) -> TokenStream {
    use KeepInfo::*;
    // Parse the input tokens into a syntax tree
    let mut input = parse_macro_input!(input as ItemImpl);
    let attrs = std::mem::take(&mut input.attrs);
    assert!(
        attrs.iter().all(|attr| attr.path.is_ident("delegate")),
        "All attributes must be \"delegate\""
    );
    let keep_info = if keep_impl_block {
        Keep(input.to_token_stream())
    } else {
        Discard(check_for_method_impls_and_extras(&input.items))
    };
    let (methods, invalid_methods) = input
        .items
        .into_iter()
        .filter_map(|item| match item {
            syn::ImplItem::Method(method) => Some(method.try_into()),
            _ => None,
        })
        .partition_result();
    let implementer = DelegateImplementer {
        ty: *input.self_ty,
        impl_generics: input.generics.params,
        where_clause: input.generics.where_clause,
        methods,
        invalid_methods,
    };
    let mut res = delegate_shared::delegate_macro(&implementer, attrs, delegate_single_attr);
    match keep_info {
        Keep(input_copy) => res.extend(input_copy),
        Discard(other_errs) => {
            let unused_err = check_for_unused_methods(other_errs, &implementer.methods);
            let invalid_err_iter = implementer.invalid_methods.into_iter().map(|(_, err)| err);
            let invalid_err = fold_errors(unused_err, invalid_err_iter);
            if let Err(err) = invalid_err {
                res.extend(err.into_compile_error());
            }
        }
    }
    res.into()
}

fn delegate_single_attr(
    implementer: &DelegateImplementer,
    delegate_attr: TokenStream2,
) -> Result<TokenStream2> {
    let span = delegate_attr.span();
    let (trait_path_full, args) = DelegateArgs::from_tokens(delegate_attr)?;
    let (trait_ident, trait_generics_p) = delegate_shared::trait_info(&trait_path_full)?;
    let macro_name: Ident = macro_name(trait_ident);

    let impl_generics = delegate_shared::merge_generics(&implementer.impl_generics, &args.generics);
    let implementer_ty = &implementer.ty;
    let mut where_clause =
        delegate_shared::build_where_clause(args.where_clauses, implementer.where_clause.as_ref());

    let delegate_ty = args.target.get_ret_type(span, implementer)?;
    let owned_ident = args.target.owned_id.into_iter();
    let ref_ident = args.target.ref_id.into_iter();
    let ref_mut_ident = args.target.ref_mut_id.into_iter();
    add_auto_where_clause(&mut where_clause, &trait_path_full, delegate_ty);
    let res = quote! {
        impl <#(#impl_generics,)*> #trait_path_full for #implementer_ty #where_clause {
            #macro_name!{body_struct(<#trait_generics_p>, #delegate_ty, (#(#owned_ident())*), (#(#ref_ident())*), (#(#ref_mut_ident())*))}
        }
    };
    Ok(res)
}
