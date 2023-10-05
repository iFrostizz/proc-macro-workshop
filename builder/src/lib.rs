use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, Attribute, Data, DataStruct, DeriveInput, Expr, Field, Fields,
    GenericArgument, Lit, MetaNameValue, PathArguments, Type,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let DeriveInput { ident, data, .. } = parse_macro_input!(input as DeriveInput);
    let builder_struct_name = Ident::new(&(ident.to_string() + "Builder"), Span::call_site());
    let fields = match &data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => &fields.named,
        _ => panic!("expected a struct with named fields"),
    };
    let field_name_1 = fields.iter().map(|t| &t.ident);
    let field_name_2 = field_name_1.clone();

    let builder_type = fields.iter().map(|t| {
        let ty = &t.ty;
        if is_optional(ty) {
            quote! {
                #ty
            }
        } else {
            quote! {
                std::option::Option<#ty>
            }
        }
    });

    let check_field_none = fields.iter().map(|t| {
        let name = &t.ident;
        let ty = &t.ty;
        if is_optional(ty) {
            TokenStream::new().into()
        } else {
            quote! {
                if self.#name.is_none() {
                    return Err("noooo".into());
                }
            }
        }
    });

    let unwrap_if_opt = fields.iter().map(|t| {
        let name = &t.ident;
        let ty = &t.ty;
        if is_optional(ty) {
            quote! {
                #name: self.#name.take(),
            }
        } else {
            quote! {
                #name: self.#name.take().unwrap(),
            }
        }
    });

    let each_builder_fn = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        match parse_builder_attr(f) {
            Some(Ok((inner_name, inner_ty))) => {
                let inner_name = Ident::new(&inner_name, Span::call_site());
                let inner_ty = Ident::new(&inner_ty, Span::call_site());
                quote! {
                    pub fn #inner_name(&mut self, #inner_name: #inner_ty) -> &mut Self {
                        let vec = self.#name.get_or_insert_with(|| Vec::new());
                        vec.push(#inner_name);
                        self
                    }
                }
            }
            Some(Err(err)) => err.to_compile_error(),
            _ => {
                if let Some(inner) = get_optional_inner(ty) {
                    quote! {
                        pub fn #name(&mut self, #name: #inner) -> &mut Self {
                            self.#name = Some(#name);
                            self
                        }
                    }
                } else {
                    quote! {
                        pub fn #name(&mut self, #name: #ty) -> &mut Self {
                            self.#name = Some(#name);
                            self
                        }
                    }
                }
            }
        }
    });

    quote! {
        pub struct #builder_struct_name {
            #(
                #field_name_1: #builder_type,
             )*
        }

        impl #ident {
            pub fn builder() -> #builder_struct_name {
                #builder_struct_name {
                    #(
                        #field_name_2: None,
                     )*
                }
            }
        }

        impl #builder_struct_name {
            #(
                #each_builder_fn
             )*

            pub fn build(&mut self) -> std::result::Result<#ident, std::boxed::Box<dyn std::error::Error>> {
                #(
                    #check_field_none
                 )*
                Ok(#ident {
                    #(
                        #unwrap_if_opt
                     )*
                })
            }
        }
    }
    .into()
}

fn is_optional(ty: &Type) -> bool {
    get_optional_inner(ty).is_some()
}

fn get_optional_inner(ty: &Type) -> std::option::Option<Type> {
    if let Type::Path(p) = ty {
        if let Some(seg) = p.path.segments.first() {
            if seg.ident == "Option" {
                if let PathArguments::AngleBracketed(ang) = &seg.arguments {
                    if let Some(GenericArgument::Type(Type::Path(tp))) = ang.args.first() {
                        if let Some(ty_str) = tp.path.segments.first() {
                            return syn::parse_str(ty_str.ident.to_string().as_str()).ok();
                        }
                    }
                }
            }
        }
    }

    None
}

fn parse_each_attr(attr: &Attribute) -> syn::Result<String> {
    if attr.path().is_ident("builder") {
        let meta = &attr.meta;
        let args: MetaNameValue = attr.parse_args()?;
        let path = &args.path;
        let seg = &path.segments;
        if let Some(fseg) = seg.first() {
            if fseg.ident == "each" {
                if let Expr::Lit(lit) = args.value {
                    if let Lit::Str(lit_str) = lit.lit {
                        Ok(lit_str.value())
                    } else {
                        Err(syn::Error::new(attr.span(), "unsupported literal"))
                    }
                } else {
                    Err(syn::Error::new(attr.span(), "unsupported rhs expression"))
                }
            } else {
                Err(syn::Error::new_spanned(
                    meta,
                    "expected `builder(each = \"...\")`",
                ))
            }
        } else {
            Err(syn::Error::new(
                attr.span(),
                "Should be of the form `builder(each = \"arg\")",
            ))
        }
    } else {
        Err(syn::Error::new(
            attr.span(),
            "should use the builder attribute",
        ))
    }
}

fn is_builder(attr: &Attribute) -> bool {
    attr.path().is_ident("builder")
}

fn parse_vec(ty: &Type) -> syn::Result<String> {
    if is_vec(ty) {
        match ty {
            Type::Path(path) => {
                let path_seg = path.path.segments.first().unwrap();
                match &path_seg.arguments {
                    PathArguments::AngleBracketed(arg) => {
                        if let Some(ang_arg) = arg.args.first() {
                            match ang_arg {
                                GenericArgument::Type(Type::Path(path)) => {
                                    let seg = path.path.segments.first().unwrap();
                                    Ok(seg.ident.to_string())
                                }
                                _ => unimplemented!(),
                            }
                        } else {
                            Err(syn::Error::new(Span::call_site(), "ill-formed Vec"))
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            _ => unreachable!(),
        }
    } else {
        Err(syn::Error::new(
            Span::call_site(),
            "The type is not a Vec<_>",
        ))
    }
}

fn is_vec(ty: &Type) -> bool {
    match ty {
        Type::Path(path) => {
            if let Some(first) = path.path.segments.first() {
                first.ident == "Vec"
            } else {
                false
            }
        }
        _ => unimplemented!(),
    }
}

fn parse_both(attr: &Attribute, ty: &Type) -> syn::Result<(String, String)> {
    let pattr = parse_each_attr(attr)?;
    let pvec = parse_vec(ty)?;
    Ok((pattr, pvec))
}

fn parse_builder_attr(field: &Field) -> std::option::Option<syn::Result<(String, String)>> {
    let ty = &field.ty;
    field
        .attrs
        .iter()
        .find(|attr| is_builder(attr) && is_vec(ty))
        .map(|attr| parse_both(attr, ty))
}
