use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Expr, GenericParam, Lit, Meta};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let DeriveInput {
        ident: struct_name_ident,
        data,
        generics,
        ..
    }: DeriveInput = parse_macro_input!(input as DeriveInput);

    let (impl_generics, ty_generics, _where_clause) = generics.split_for_impl();

    let fields = match data {
        Data::Struct(my_struct) => my_struct.fields,
        _ => unimplemented!(),
    };

    let params = &generics.params;
    // if params.len() > 1 {
    //     compile_error!("Can only use one generic max");
    // }
    let param = &params[0];

    let gen_ident = match param {
        GenericParam::Type(t) => &t.ident,
        _ => unimplemented!(),
    };

    let debug_fields = fields.iter().map(|field| {
        let field_name = field.ident.as_ref().unwrap();
        let field_ty = &field.ty;
        eprintln!("{:?}", field_ty);
        if let Some(attr) = field
            .attrs
            .iter()
            .find(|attr| attr.path().is_ident("debug"))
        {
            let token = if let Meta::NameValue(name_value) = &attr.meta {
                if let Expr::Lit(lit) = &name_value.value {
                    if let Lit::Str(lit_str) = &lit.lit {
                        Some(lit_str.token())
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            };
            if let Some(token) = token {
                quote! {
                    .field(stringify!(#field_name), &format_args!(#token, &self.#field_name))
                }
            } else {
                quote! {
                    .field(stringify!(#field_name), &self.#field_name)
                }
            }
        } else {
            quote! {
                .field(stringify!(#field_name), &self.#field_name)
            }
        }
    });

    let where_clause = quote! {
        where #gen_ident: ToString
    };

    quote! {
        impl #impl_generics std::fmt::Debug for #struct_name_ident #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!(#struct_name_ident))
                    #(#debug_fields)*
                    .finish()
            }
        }
    }
    .into()
}
