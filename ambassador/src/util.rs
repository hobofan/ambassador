use proc_macro2::TokenStream;
use quote::ToTokens;
use std::fmt::{Display, Formatter};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{Receiver, Result};

macro_rules! error {
    ($span:expr, $($rest:expr),*) => {Err(syn::parse::Error::new($span, format_args!($($rest),*)))};
}

/// Like the '?' operator but returns Some(err) instead of err
macro_rules! try_option {
    ($result:expr) => {
        match $result {
            Ok(ok) => ok,
            Err(err) => return Some(Err(err)),
        }
    };
}

pub(crate) use {error, try_option};

pub(crate) struct ProcessResult<'a, T, I: Iterator<Item = Result<T>>> {
    iter: I,
    err: &'a mut Option<syn::Error>,
}

fn add_err(old_err: &mut Option<syn::Error>, err: syn::Error) {
    match old_err {
        Some(old_err) => old_err.combine(err),
        None => *old_err = Some(err),
    }
}

impl<'a, T, I: Iterator<Item = Result<T>>> Iterator for ProcessResult<'a, T, I> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.iter.next() {
                None => return None,
                Some(Ok(x)) => return Some(x),
                Some(Err(err)) => add_err(self.err, err),
            }
        }
    }

    fn fold<B, F>(self, init: B, mut f: F) -> B
    where
        Self: Sized,
        F: FnMut(B, Self::Item) -> B,
    {
        let self_err = self.err;
        self.iter.fold(init, |acc, elt| match elt {
            Ok(elt) => f(acc, elt),
            Err(err) => {
                add_err(self_err, err);
                acc
            }
        })
    }
}

/// Similar to [`Itertools::process_results`] but runs iter to completion and combines errors
pub(crate) fn process_results<T, I, F, R>(iter: I, f: F) -> Result<R>
where
    I: Iterator<Item = Result<T>>,
    F: FnOnce(ProcessResult<T, I>) -> R,
{
    let mut err = None;
    let pr = ProcessResult {
        iter,
        err: &mut err,
    };
    let res = f(pr);
    match err {
        None => Ok(res),
        Some(err) => Err(err),
    }
}

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

fn receiver_type_inner(r: &Receiver) -> ReceiverType {
    if r.reference.is_none() {
        ReceiverType::Owned
    } else if r.mutability.is_none() {
        ReceiverType::Ref
    } else {
        ReceiverType::MutRef
    }
}

pub(crate) fn receiver_type(sig: &syn::Signature) -> Result<ReceiverType> {
    match sig.receiver() {
        Some(syn::FnArg::Receiver(r)) => Ok(receiver_type_inner(r)),
        Some(syn::FnArg::Typed(t)) => error!(
            t.span(),
            "method's receiver type is not supported (must one of self, &self, or &mut self)"
        ),
        None => error!(sig.paren_token.span, "method must have a receiver"),
    }
}
