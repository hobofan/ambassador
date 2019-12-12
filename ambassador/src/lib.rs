extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Generics};

#[derive(Clone, Debug)]
enum DelegateImplementer {
    Enum {
        variant_idents: Vec<syn::Ident>,
        generics: Generics,
    },
    SingleFieldStruct {
        field_ident: syn::Member,
        generics: Generics,
    },
    MultiFieldStruct {
        field_idents: Vec<syn::Member>,
        generics: Generics,
    },
}

struct DelegateArgs<'a> {
    trait_path_full: &'a syn::Path,
    target: Option<syn::Member>,
}

impl<'a> DelegateArgs<'a> {
    pub fn from_meta(meta: &'a syn::Meta) -> Self {
        let meta_list = match meta {
            syn::Meta::List(meta_list) => meta_list,
            _ => panic!("Invalid syntax for delegate attribute"),
        };

        let nested_meta_items: Vec<&syn::Meta> = meta_list
            .nested
            .iter()
            .map(|n| match n {
                syn::NestedMeta::Meta(meta) => meta,
                _ => panic!("Invalid syntax for delegate attribute"),
            })
            .collect();
        let trait_path_full = match nested_meta_items[0] {
            syn::Meta::Path(ref path) => path,
            _ => panic!(
                "Invalid syntax for delegate attribute; First value has to be the Trait name"
            ),
        };

        let mut target = None;
        for meta_item in nested_meta_items.iter().skip(1) {
            match meta_item {
                syn::Meta::NameValue(name_value) => {
                    if name_value.path.is_ident("target") {
                        match name_value.lit {
                        syn::Lit::Str(ref lit) => {
                            let target_val: syn::Member = lit.parse().expect("Invalid syntax for delegate attribute; Expected ident as value for \"target\"");
                            if target.is_some() {
                                panic!("\"target\" value for delegate attribute can only be specified once");
                            }

                            target = Some(target_val);
                        }
                        _ => panic!("Invalid syntax for delegate attribute; delegate attribute values have to be strings"),
                    }
                    }
                }
                _ => panic!("Invalid syntax for delegate attribute"),
            }
        }

        Self {
            trait_path_full,
            target,
        }
    }
}

#[proc_macro_derive(Delegate, attributes(delegate))]
pub fn delegate_macro(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let input = parse_macro_input!(input as DeriveInput);

    let delegate_attributes: Vec<&syn::Attribute> = input
        .attrs
        .iter()
        .filter(|n| n.path.is_ident("delegate"))
        .collect();
    if delegate_attributes.is_empty() {
        panic!("No #[delegate] attribute specified. If you want to delegate an implementation of trait `SomeTrait` add the attribute:\n#[delegate(SomeTrait)]")
    }

    let implementer_ident = input.ident;
    let generics = input.generics;
    let implementer: Option<DelegateImplementer> = match input.data {
        syn::Data::Enum(enum_data) => {
            let variant_idents = enum_data.variants.into_iter().map(|n| n.ident).collect();
            Some(DelegateImplementer::Enum {
                variant_idents,
                generics,
            })
        }
        syn::Data::Struct(struct_data) => match struct_data.fields {
            syn::Fields::Unnamed(fields_unnamed) => match fields_unnamed.unnamed.len() {
                1 => Some(DelegateImplementer::SingleFieldStruct {
                    field_ident: syn::parse_quote! { 0 },
                    generics,
                }),
                _ => {
                    let field_idents: Vec<_> = fields_unnamed
                        .unnamed
                        .into_iter()
                        .enumerate()
                        .map(|(i, _)| i)
                        .map(|i| syn::parse_str(&i.to_string()).unwrap())
                        .collect();
                    Some(DelegateImplementer::MultiFieldStruct {
                        field_idents,
                        generics,
                    })
                }
            },
            syn::Fields::Named(fields_named) => match fields_named.named.len() {
                1 => {
                    let field_ident = fields_named.named[0].ident.as_ref().unwrap();
                    Some(DelegateImplementer::SingleFieldStruct {
                        field_ident: syn::parse_quote! { #field_ident },
                        generics,
                    })
                }
                _ => {
                    let field_idents: Vec<_> = fields_named
                        .named
                        .into_iter()
                        .map(|field| field.ident.unwrap())
                        .map(|field_ident| syn::parse_quote! { #field_ident })
                        .collect();
                    Some(DelegateImplementer::MultiFieldStruct {
                        field_idents,
                        generics,
                    })
                }
            },
            _ => None,
        },
        _ => None,
    };
    if implementer.is_none() {
        panic!(
            "ambassador currently only supports #[derive(Delegate)] for: \n\
             - single-field enums\n\
             - (tuple) structs"
        )
    }
    let implementer = implementer.unwrap();

    let mut impl_macros = vec![];

    for delegate_attr in delegate_attributes {
        let meta = delegate_attr.parse_meta().unwrap();
        let args = DelegateArgs::from_meta(&meta);
        let trait_path_full: syn::Path = args.trait_path_full.clone();
        let trait_ident: &syn::Ident = &trait_path_full.segments.last().unwrap().ident;

        let (trait_path, trait_path_colon) = build_invocation_path(&trait_path_full);
        let macro_name_body_single_struct: syn::Ident =
            quote::format_ident!("ambassador_impl_{}_body_single_struct", trait_ident);
        let macro_name_body_enum: syn::Ident =
            quote::format_ident!("ambassador_impl_{}_body_enum", trait_ident);

        let impl_macro = match implementer {
            DelegateImplementer::Enum {
                ref variant_idents,
                ref generics,
            } => {
                let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

                quote! {
                    impl #impl_generics #trait_ident for #implementer_ident #ty_generics #where_clause {
                        #trait_path#trait_path_colon#macro_name_body_enum!{#(#implementer_ident::#variant_idents),*}
                    }
                }
            }
            DelegateImplementer::SingleFieldStruct {
                ref field_ident,
                ref generics,
            } => {
                let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

                quote! {
                    impl #impl_generics #trait_ident for #implementer_ident #ty_generics #where_clause {
                        #trait_path#trait_path_colon#macro_name_body_single_struct!{#field_ident}
                    }
                }
            }
            DelegateImplementer::MultiFieldStruct {
                ref field_idents,
                ref generics,
            } => {
                if args.target.is_none() {
                    panic!("\"target\" value on #[delegate] attribute has to be specified for structs with multiple fields");
                }

                let field_ident = field_idents
                    .iter()
                    .find(|n| **n == args.target.clone().unwrap());
                if field_ident.is_none() {
                    panic!("Unknown field \"{:?}\" specified as \"target\" value in #[delegate] attribute");
                }
                let field_ident = field_ident.unwrap();
                let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

                quote! {
                    impl #impl_generics #trait_ident for #implementer_ident #ty_generics #where_clause {
                        #trait_path#trait_path_colon#macro_name_body_single_struct!{#field_ident}
                    }
                }
            }
        };
        impl_macros.push(impl_macro);
    }

    // Build the output, possibly using quasi-quotation
    let expanded = quote! {
        #(#impl_macros)*
    };

    // Hand the output tokens back to the compiler
    TokenStream::from(expanded)
}

/// Build the invocation path prefix for the macro invocation.
///
/// Macros are always imported from the crate root
///
/// (None, None) -> ""
/// (Some(...), Some(...)) -> "`foo_crate`::"
fn build_invocation_path(
    trait_path_full: &syn::Path,
) -> (Option<syn::Path>, Option<syn::token::Colon2>) {
    let to_take = trait_path_full.segments.len() - 1;
    let trait_path: Vec<&syn::PathSegment> = trait_path_full
        .segments
        .iter()
        .take(usize::max(to_take, 0))
        .collect();
    let trait_path_str = trait_path
        .into_iter()
        .map(|n| n.ident.to_string())
        .collect::<Vec<_>>()
        .join("::");

    let (trait_path, trait_path_colon): (Option<syn::Path>, Option<syn::token::Colon2>) =
        match trait_path_str.as_ref() {
            "" => (None, None),
            _ => (
                Some(syn::parse_str(&trait_path_str).unwrap()),
                Some(syn::parse_quote!(::)),
            ),
        };
    (trait_path, trait_path_colon)
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

fn build_register_trait(original_item: &syn::ItemTrait) -> proc_macro2::TokenStream {
    let trait_ident = &original_item.ident;
    let macro_name_body_single_struct: syn::Ident =
        quote::format_ident!("ambassador_impl_{}_body_single_struct", trait_ident);
    let macro_name_body_enum: syn::Ident =
        quote::format_ident!("ambassador_impl_{}_body_enum", trait_ident);

    let original_trait_methods: Vec<&syn::TraitItemMethod> = original_item
        .items
        .iter()
        .map(|n| match n {
            syn::TraitItem::Method(method) => method,
            _ => unimplemented!(),
        })
        .collect();

    let enum_trait_methods = build_enum_trait_methods(&*original_trait_methods);
    let single_field_struct_methods = build_single_field_struct_methods(&*original_trait_methods);

    let register_trait = quote! {
        #[macro_export]
        macro_rules! #macro_name_body_single_struct {
            ($field_ident:tt) => {
                #(#single_field_struct_methods)*
            }
        }

        #[macro_export]
        macro_rules! #macro_name_body_enum {
            ($( $variants:path ),+) => {
                #(#enum_trait_methods)*
            }
        }
    };

    register_trait
}

fn build_enum_trait_methods(
    original_trait_methods: &[&syn::TraitItemMethod],
) -> Vec<proc_macro2::TokenStream> {
    let mut enum_trait_methods = vec![];
    for original_method in original_trait_methods {
        let method_sig = &original_method.sig;
        let method_invocation = build_method_invocation(&original_method, &quote!(inner));

        let method_impl = quote! {
            #method_sig {
                match self {
                    $($variants(inner) => #method_invocation),*
                }
            }
        };
        enum_trait_methods.push(method_impl);
    }
    enum_trait_methods
}

fn build_single_field_struct_methods(
    original_trait_methods: &[&syn::TraitItemMethod],
) -> Vec<proc_macro2::TokenStream> {
    let mut enum_trait_methods = vec![];
    for original_method in original_trait_methods {
        let method_sig = &original_method.sig;
        let method_invocation =
            build_method_invocation(original_method, &quote!(self.$field_ident));

        let method_impl = quote! {
            #method_sig {
                #method_invocation
            }
        };
        enum_trait_methods.push(method_impl);
    }
    enum_trait_methods
}

fn build_method_invocation(
    original_method: &syn::TraitItemMethod,
    field_ident: &proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let method_sig = &original_method.sig;
    let method_ident = &method_sig.ident;
    let argument_list: syn::punctuated::Punctuated<&Box<syn::Pat>, syn::token::Comma> = method_sig
        .inputs
        .iter()
        .filter_map(|fn_arg| match fn_arg {
            syn::FnArg::Receiver(_) => None,
            syn::FnArg::Typed(pat_type) => Some(&pat_type.pat),
        })
        .collect();

    let method_invocation = quote! { #field_ident.#method_ident(#argument_list) };
    method_invocation
}
