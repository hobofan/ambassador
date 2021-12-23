extern crate proc_macro;

mod derive;
mod register;
mod util;

use proc_macro::TokenStream;
use quote::quote;

use crate::register::build_register_trait;

#[proc_macro_derive(Delegate, attributes(delegate))]
pub fn delegate_macro(input: TokenStream) -> TokenStream {
    crate::derive::delegate_macro(input)
}

#[proc_macro_attribute]
pub fn delegatable_trait(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let original_item: syn::ItemTrait = syn::parse(item).unwrap();
    let register_trait = build_register_trait(&original_item);

    let expanded = quote! {
        #original_item

        #register_trait
    };
    TokenStream::from(expanded)
}

#[proc_macro_attribute]
pub fn delegatable_trait_remote(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let original_item: syn::ItemTrait = syn::parse(item).unwrap();
    let register_trait = build_register_trait(&original_item);

    let expanded = quote! {
        #register_trait
    };
    TokenStream::from(expanded)
}
