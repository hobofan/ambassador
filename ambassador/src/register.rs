use quote::quote;

pub fn build_register_trait(original_item: &syn::ItemTrait) -> proc_macro2::TokenStream {
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
