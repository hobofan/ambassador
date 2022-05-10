use proc_macro2::TokenStream;
use quote::ToTokens;
use std::fmt::{Display, Formatter};
use syn::punctuated::Punctuated;
use syn::Receiver;

pub(crate) struct TailingPunctuated<'a, T, P>(pub(crate) &'a Punctuated<T, P>);

impl<'a, T, P: Default + ToTokens> ToTokens for TailingPunctuated<'a, T, P>
where
    Punctuated<T, P>: ToTokens,
{
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.to_tokens(tokens);
        if !self.0.empty_or_trailing() {
            P::default().to_tokens(tokens)
        }
    }
}
#[derive(Copy, Clone, Eq, PartialEq)]
pub(crate) enum ReceiverType {
    Owned,
    Ref,
    MutRef,
}

impl Display for ReceiverType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ReceiverType::Owned => write!(f, "\"self\""),
            ReceiverType::Ref => write!(f, "\"&self\""),
            ReceiverType::MutRef => write!(f, "\"&mut self\""),
        }
    }
}

pub(crate) fn try_receiver_type(method: &syn::ImplItemMethod) -> Option<ReceiverType> {
    match method.sig.receiver() {
        Some(syn::FnArg::Receiver(r)) => Some(receiver_type_inner(r)),
        Some(syn::FnArg::Typed(_)) => None,
        None => None,
    }
}

fn receiver_type_inner(r: &Receiver) -> ReceiverType {
    if r.reference.is_none() {
        ReceiverType::Owned
    } else if r.mutability.is_none() {
        ReceiverType::Ref
    } else {
        ReceiverType::MutRef
    }
}

pub(crate) fn receiver_type(method: &syn::TraitItemMethod) -> ReceiverType {
    match method.sig.receiver() {
        Some(syn::FnArg::Receiver(r)) => receiver_type_inner(r),
        Some(syn::FnArg::Typed(_)) => panic!(
            "Method {}'s receiver type is not supported (must one of self, &self, or &mut self)",
            method.sig.ident
        ),
        None => panic!(
            "Method {} in delegatable trait does not have a receiver",
            method.sig.ident
        ),
    }
}
