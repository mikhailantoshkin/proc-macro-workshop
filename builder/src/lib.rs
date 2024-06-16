use core::panic;

use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, parse_quote, Data, DeriveInput, Field, Fields, GenericParam, Generics,
    Ident, Index, Type,
};

fn is_option(field: &Field) -> bool {
    match &field.ty {
        Type::Path(p) => p
            .path
            .segments
            .first()
            .is_some_and(|seg| seg.ident == "Option"),
        _ => false,
    }
}
fn option_inner_ty(ty: &Type) -> &Type {
    match &ty {
        Type::Path(p) => {
            let seg = p.path.segments.first().unwrap();
            let gen_arg = match &seg.arguments {
                syn::PathArguments::AngleBracketed(args) => {
                    args.args.first().expect("Option without generic?")
                }
                _ => panic!("Incorrect syntax"),
            };
            match gen_arg {
                syn::GenericArgument::Type(ty) => ty,
                _ => panic!("Incorrect generic argument"),
            }
        }
        _ => panic!("Invalid type"),
    }
}
fn to_ident_ty(field: &Field) -> (&Option<Ident>, &Type) {
    (&field.ident, &field.ty)
}

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let builder_ident = format_ident!("{}Builder", input.ident);
    let fields = match input.data {
        Data::Struct(s) => s.fields.clone(),
        _ => panic!("Only structs are supported"),
    };
    let optional_fields = fields.iter().filter(|f| is_option(f)).collect::<Vec<_>>();
    let req_fields = fields.iter().filter(|f| !is_option(f)).collect::<Vec<_>>();

    let optional_fields_builder = optional_fields
        .iter()
        .map(|f| to_ident_ty(f))
        .map(|(ident, ty)| quote! {#ident: #ty});
    let req_fields_builder = req_fields
        .iter()
        .map(|f| to_ident_ty(f))
        .map(|(ident, ty)| quote! {#ident: Option<#ty>});

    let default_impl = fields.iter().map(|f| {
        let name = &f.ident;
        quote! {#name: None}
    });
    let opt_builder_methods = optional_fields.iter().map(|f| {
        let name = &f.ident;
        let ty = option_inner_ty(&f.ty);
        quote! {
            fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });
    let req_builder_methods = req_fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        quote! {
            fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });
    let req_constructors = req_fields.iter().map(|f| {
        let ident = &f.ident;
        let err_msg = format!("Field '{}' must be set", ident.as_ref().unwrap());
        quote! {#ident: self.#ident.clone().ok_or(#err_msg)?}
    });
    let opt_constructors = optional_fields.iter().map(|f| {
        let ident = &f.ident;
        quote! {#ident: self.#ident.clone()}
    });

    let struct_ident = input.ident;
    let struct_impl = quote! {
        use std::error::Error;
        impl #struct_ident {
            pub fn builder() -> #builder_ident {
                #builder_ident{
                    #(#default_impl),*
                }
            }
        }
        pub struct #builder_ident {
            #(#optional_fields_builder),*
            #(#req_fields_builder),*
        }

        impl #builder_ident {
            pub fn build(&mut self) -> Result<#struct_ident, Box<dyn Error>> {
                Ok( #struct_ident{
                    #(#req_constructors),*
                    #(#opt_constructors),*
                } )
            }
            #(#req_builder_methods)*
            #(#opt_builder_methods)*
        }
    };
    struct_impl.into()
}
