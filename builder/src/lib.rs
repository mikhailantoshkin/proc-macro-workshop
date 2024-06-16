use core::panic;

use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse_macro_input, spanned::Spanned, token::Token, Attribute, Data, DeriveInput, Error, Expr,
    Field, Ident, Lit, Result, Type,
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
fn inner_ty(ty: &Type) -> &Type {
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
fn renamed(field: &Field) -> Option<Ident> {
    field.attrs.first().and_then(|attr| {
        if !attr.path().is_ident("builder") {
            return None;
        }
        match attr.parse_args().expect("Unable to parse") {
            Expr::Assign(a) => {
                let Expr::Path(left) = *a.left else {
                    return None;
                };
                let Expr::Lit(right) = *a.right else {
                    return None;
                };
                if left.path.is_ident("each") {
                    let Lit::Str(s) = right.lit else { return None };
                    return Some(Ident::new(&s.value(), Span::call_site()));
                } else {
                    return None;
                }
            }
            _ => panic!("Incorect syntax for attribute"),
        }
    })
}
fn get_attribute(field: &Field) -> Option<&Attribute> {
    field.attrs.first()
}
fn parse_attribute(attr: &Attribute, expected: &str) -> Result<Ident> {
    if !attr.path().is_ident("builder") {
        return Err(Error::new(
            attr.span(),
            "Only `builder` attribute is supported",
        ));
    }
    match attr.parse_args()? {
        Expr::Assign(a) => {
            let Expr::Path(ref left) = *a.left else {
                return Err(Error::new(
                    attr.meta.span(),
                    "invalid syntax for left hand side of assignment",
                ));
            };
            let Expr::Lit(ref right) = *a.right else {
                return Err(Error::new(
                    attr.meta.span(),
                    "invalid syntax for right hand side of assignment",
                ));
            };
            if left.path.is_ident(expected) {
                let Lit::Str(ref s) = right.lit else {
                    return Err(Error::new(
                        attr.meta.span(),
                        "field name should be a string literal",
                    ));
                };
                return Ok(Ident::new(&s.value(), Span::call_site()));
            } else {
                return Err(Error::new(attr.span(), "unknown attribute"));
            }
        }
        _ => {
            return Err(Error::new(
                attr.meta.span(),
                "invalid syntax in attribute definition. Only assignment is available",
            ))
        }
    }
}
enum FieldType {
    Optional,
    MultiVal(Ident),
    Required,
}

struct MacroField {
    field: Field,
    ty: FieldType,
}
impl TryFrom<Field> for MacroField {
    type Error = syn::Error;

    fn try_from(field: Field) -> std::result::Result<Self, Self::Error> {
        match get_attribute(&field) {
            None => {
                if is_option(&field) {
                    Ok(MacroField {
                        field,
                        ty: FieldType::Optional,
                    })
                } else {
                    Ok(MacroField {
                        field,
                        ty: FieldType::Required,
                    })
                }
            }
            Some(attr) => {
                let new_ident = parse_attribute(attr, "each")?;
                Ok(MacroField {
                    field,
                    ty: FieldType::MultiVal(new_ident),
                })
            }
        }
    }
}
impl MacroField {
    fn as_builder_field_def(&self) -> TokenStream {
        let ident = &self.field.ident;
        let ty = &self.field.ty;
        match self.ty {
            FieldType::Optional | FieldType::MultiVal(_) => quote! {#ident: #ty},
            FieldType::Required => quote! {#ident: Option<#ty>},
        }
    }
    fn as_builder_field_default_impl(&self) -> TokenStream {
        let ident = &self.field.ident;
        let ty = &self.field.ty;
        match self.ty {
            FieldType::MultiVal(_) => quote! {#ident: <#ty>::new()},
            FieldType::Optional | FieldType::Required => quote! {#ident: None},
        }
    }
    fn as_builder_method(&self) -> TokenStream {
        let ident = &self.field.ident;
        let ty = &self.field.ty;
        match &self.ty {
            FieldType::MultiVal(new) => {
                let inner_ty = inner_ty(&self.field.ty);
                if new == self.field.ident.as_ref().unwrap() {
                    quote! {
                        fn #new(&mut self, #new: #inner_ty) -> &mut Self {
                            self.#ident.push(#new);
                            self
                        }
                    }
                } else {
                    quote! {
                        fn #new(&mut self, #new: #inner_ty) -> &mut Self {
                            self.#ident.push(#new);
                            self
                        }
                        fn #ident(&mut self, #ident: #ty) -> &mut Self {
                            self.#ident = #ident;
                            self
                        }
                    }
                }
            }
            FieldType::Optional => {
                let ty = inner_ty(&self.field.ty);
                quote! {
                    fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = Some(#ident);
                        self
                    }
                }
            }
            FieldType::Required => quote! {
                fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
                }
            },
        }
    }
    fn as_constructor(&self) -> TokenStream {
        let ident = self.field.ident.as_ref();
        match &self.ty {
            FieldType::Optional | FieldType::MultiVal(_) => quote! {#ident: self.#ident.clone(),},
            FieldType::Required => {
                let err_msg = format!("Field '{}' must be set", ident.as_ref().unwrap());
                quote! {#ident: self.#ident.clone().ok_or(#err_msg)?,}
            }
        }
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    expand(input)
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

fn expand(input: DeriveInput) -> Result<TokenStream> {
    let builder_ident = format_ident!("{}Builder", input.ident);
    let fields = match &input.data {
        Data::Struct(s) => s.fields.clone(),
        _ => return Err(Error::new(input.span(), "Only structs are supported")),
    };
    let mut macro_fields: Vec<MacroField> = Vec::new();
    for field in fields {
        macro_fields.push(field.try_into()?);
    }
    let default_impl = macro_fields
        .iter()
        .map(|f| f.as_builder_field_default_impl());
    let builder_fields = macro_fields.iter().map(|f| f.as_builder_field_def());
    let constructors = macro_fields.iter().map(|f| f.as_constructor());
    let builder_methods = macro_fields.iter().map(|f| f.as_builder_method());

    let struct_ident = input.ident;
    let token_stream = quote! {
        impl #struct_ident {
            pub fn builder() -> #builder_ident {
                #builder_ident{
                    #(#default_impl),*
                }
            }
        }

        use std::error::Error;

        pub struct #builder_ident {
            #(#builder_fields),*
        }

        impl #builder_ident {
            pub fn build(&mut self) -> Result<#struct_ident, Box<dyn Error>> {
                Ok( #struct_ident{
                    #(#constructors)*
                } )
            }
            #(#builder_methods)*
        }
    };
    let struct_impl = token_stream;
    Ok(struct_impl)
}
