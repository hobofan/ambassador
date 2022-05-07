use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::punctuated::Punctuated;

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
