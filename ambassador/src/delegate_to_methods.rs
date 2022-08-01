use super::delegate_shared::{self, add_auto_where_clause};
use super::register::macro_name;
use super::util;
use crate::util::ReceiverType;
use proc_macro::TokenStream;
use proc_macro2::{Ident, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use std::collections::HashSet;
use std::convert::{TryFrom, TryInto};
use std::default::Default;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, GenericParam, ItemImpl, LitStr, ReturnType, Token, Type, WhereClause,
};

struct DelegateImplementer {
    ty: Type,
    impl_generics: Punctuated<GenericParam, Token![,]>,
    where_clause: Option<WhereClause>,
    methods: Vec<MethodInfo>,
}

struct MethodInfo {
    name: Ident,
    receiver: ReceiverType,
    ret: Type,
}

impl TryFrom<syn::ImplItemMethod> for MethodInfo {
    type Error = ();

    fn try_from(method: syn::ImplItemMethod) -> Result<Self, ()> {
        let receiver = util::try_receiver_type(&method).ok_or(())?;
        let ret = match method.sig.output {
            ReturnType::Default => return Err(()),
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
            _ => return Err(()),
        };
        if method.sig.inputs.len() != 1 {
            // Just the receiver
            return Err(());
        }
        Ok(MethodInfo {
            name: method.sig.ident,
            receiver,
            ret,
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
    fn try_update(&mut self, key: &str, lit: LitStr) -> Option<()> {
        match key {
            "target_owned" => {
                self.owned_id = Some(lit.parse().expect(
                    "Invalid syntax for delegate attribute; Expected ident as value for \"target_owned\"",
                ));
                Some(())
            }
            "target_ref" => {
                self.ref_id = Some(lit.parse().expect(
                    "Invalid syntax for delegate attribute; Expected ident as value for \"target_ref\"",
                ));
                Some(())
            }
            "target_mut" => {
                self.ref_mut_id = Some(lit.parse().expect(
                    "Invalid syntax for delegate attribute; Expected ident as value for \"target_mut\"",
                ));
                Some(())
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
    id: Option<&Ident>,
    methods: &'a [MethodInfo],
    receiver: ReceiverType,
) -> Option<&'a Type> {
    id.map(|id| {
        let res = methods
            .iter()
            .find(|&m| &m.name == id)
            .unwrap_or_else(|| panic!("impl block doesn't have any valid methods named {}", id));
        assert!(
            res.receiver == receiver,
            "method {} needs to have a receiver of type {}",
            id,
            receiver
        );
        &res.ret
    })
}

impl DelegateTarget {
    /// Select the correct return.
    pub fn get_ret_type<'a>(&self, methods: &'a [MethodInfo]) -> &'a Type {
        let res = self
            .as_arr()
            .iter()
            .flat_map(|(recv_ty, id)| search_methods(*id, methods, *recv_ty))
            .fold(None, |rsf, x| match rsf {
                None => Some(x),
                Some(y) if y == x => Some(x),
                _ => panic!("target methods have different return types"),
            });

        res.unwrap_or_else(|| panic!("no targets were specified"))
    }
}

// Checks that:
//   - all the items in an impl are methods
//   - all the methods are _empty_ (no body, just the signature)
//   - all the methods provided are actually referenced in the `delegate` attributes on the impl
fn check_for_method_impls_and_extras(
    attrs: &[syn::Attribute],
    impl_items: &[syn::ImplItem],
) -> Result<(), syn::Error> {
    use delegate_shared::DelegateTarget as _;

    let referenced_target_methods = {
        let mut hs: HashSet<syn::Ident> = HashSet::new();

        for delegate_attr in attrs {
            let mut delegate_target = DelegateTarget::default();
            for (k, v) in
                delegate_shared::delegate_attr_as_trait_and_iter(delegate_attr.tokens.clone()).1
            {
                let _ = delegate_target.try_update(&*k, v);

                hs.extend(
                    delegate_target
                        .as_arr()
                        .iter()
                        .filter_map(|(_, func_name)| func_name.cloned()),
                );
            }
        }

        hs
    };

    let error_message = impl_items.iter()
        .filter_map(|i| {
            // We're looking for *only* empty methods (no block).
            if let syn::ImplItem::Method(m) = i {
                let block = &m.block;
                let empty_block = syn::parse2::<syn::ImplItemMethod>(quote!{ fn foo(); })
                    .unwrap().block;

                // We'll accept `{}` blocks and omitted blocks (i.e. `fn foo();`):
                if block.stmts.is_empty() || block == &empty_block {
                    // Provided that they're actually referenced by a
                    // `delegate(...)` attr on this impl:
                    if referenced_target_methods.contains(&m.sig.ident) {
                        None
                    } else {
                        Some(syn::Error::new(m.span(), "This method is not used by any `delegate` attributes; please remove it"))
                    }
                } else {
                    Some(syn::Error::new(block.span(), "Only method signatures are allowed here (no blocks!)"))
                }
            } else {
                // Everything else is an error:
                Some(syn::Error::new(i.span(), "Only method signatures are allowed here, everything else is discarded"))
            }
        })
        .fold(None, |errors, error| {
            match (errors, error) {
                (None, err) => Some(err),
                (Some(mut errs), err) => {
                    errs.extend(err);
                    Some(errs)
                },
            }
        });

    if let Some(err) = error_message {
        Err(err)
    } else {
        Ok(())
    }
}

pub fn delegate_macro(input: TokenStream, keep_impl_block: bool) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let mut input = parse_macro_input!(input as ItemImpl);
    let attrs = std::mem::take(&mut input.attrs);
    assert!(
        attrs.iter().all(|attr| attr.path.is_ident("delegate")),
        "All attributes must be \"delegate\""
    );
    let input_copy = if keep_impl_block {
        Some(input.to_token_stream())
    } else {
        if let Err(e) = check_for_method_impls_and_extras(&attrs, &input.items) {
            return e.into_compile_error().into();
        }

        None
    };
    let implementer = DelegateImplementer {
        ty: *input.self_ty,
        impl_generics: input.generics.params,
        where_clause: input.generics.where_clause,
        methods: input
            .items
            .into_iter()
            .filter_map(|item| match item {
                syn::ImplItem::Method(method) => Some(method.try_into().ok()?),
                _ => None,
            })
            .collect(),
    };
    let mut res = delegate_shared::delegate_macro(&implementer, attrs, delegate_single_attr);
    if let Some(input_copy) = input_copy {
        res.extend(input_copy)
    }
    res.into()
}

fn delegate_single_attr(
    implementer: &DelegateImplementer,
    delegate_attr: TokenStream2,
) -> TokenStream2 {
    let (trait_path_full, args) = DelegateArgs::from_tokens(delegate_attr);
    let (trait_ident, trait_generics_p) = delegate_shared::trait_info(&trait_path_full);
    let macro_name: Ident = macro_name(trait_ident);

    let impl_generics = delegate_shared::merge_generics(&implementer.impl_generics, &args.generics);
    let implementer_ty = &implementer.ty;
    let mut where_clause =
        delegate_shared::build_where_clause(args.where_clauses, implementer.where_clause.as_ref());

    let delegate_ty = args.target.get_ret_type(&implementer.methods);
    let owned_ident = args.target.owned_id.into_iter();
    let ref_ident = args.target.ref_id.into_iter();
    let ref_mut_ident = args.target.ref_mut_id.into_iter();
    add_auto_where_clause(&mut where_clause, &trait_path_full, delegate_ty);
    quote! {
        impl <#(#impl_generics,)*> #trait_path_full for #implementer_ty #where_clause {
            #macro_name!{body_struct(<#trait_generics_p>, #delegate_ty, (#(#owned_ident())*), (#(#ref_ident())*), (#(#ref_mut_ident())*))}
        }
    }
}
