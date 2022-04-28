use bytemuck::TransparentWrapper;
use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::punctuated::Punctuated;

#[derive(TransparentWrapper)]
#[repr(transparent)]
pub(crate) struct TailingPunctuated<T, P>(pub(crate) Punctuated<T, P>);

impl<T, P> TailingPunctuated<T, P> {
    pub(crate) fn wrap_ref(inner: &Punctuated<T, P>) -> &Self {
        TransparentWrapper::wrap_ref(inner)
    }
}

impl<T, P: Default + ToTokens> ToTokens for TailingPunctuated<T, P>
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
