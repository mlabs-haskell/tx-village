use proc_macro::TokenStream;
use quote::quote;
use syn::{self, parse_macro_input, DeriveInput};

#[derive(deluxe::ExtractAttributes)]
#[deluxe(attributes(diesel_derive))]
struct Opts {
    sql_type: syn::Type,
}

#[derive(deluxe::ExtractAttributes)]
#[deluxe(attributes(diesel_derive))]
struct FieldAttrs {
    sql_type: syn::Type,
}

#[proc_macro_derive(PgCustomType, attributes(diesel_derive))]
pub fn derive_sql_fn(input: TokenStream) -> TokenStream {
    let mut ast = parse_macro_input!(input as DeriveInput);
    let opts: Opts = deluxe::extract_attributes(&mut ast).expect("Wrong options");

    let ident = &ast.ident;
    let sql_type = opts.sql_type;

    let (to_sql_impl, from_sql_impl) = match &ast.data {
        syn::Data::Struct(data_struct) => match &data_struct.fields {
            syn::Fields::Named(fields_named) => impl_struct(fields_named),
            syn::Fields::Unnamed(fields_unnamed) => {
                if fields_unnamed.unnamed.len() == 1 {
                    impl_newtype(&fields_unnamed.unnamed[0])
                } else {
                    unimplemented!("Tuples are not implemented yet")
                }
            }
            syn::Fields::Unit => unimplemented!("Units are unsupported"),
        },
        syn::Data::Enum(_data_enum) => unimplemented!("Enums are not implemented yet"),
        syn::Data::Union(_data_union) => unimplemented!("Unions are unsupported"),
    };

    let (impl_generics, ty_generics, where_clause) = &ast.generics.split_for_impl();

    let expanded = quote! {
        impl #impl_generics diesel::deserialize::FromSql<#sql_type, diesel::pg::Pg> for #ident #ty_generics #where_clause {
            fn from_sql(bytes: diesel::pg::PgValue) -> diesel::deserialize::Result<Self> {
                #from_sql_impl
            }
        }

        impl #impl_generics diesel::serialize::ToSql<#sql_type, diesel::pg::Pg> for #ident #ty_generics #where_clause {
            fn to_sql<'b>(
                &'b self,
                out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
            ) -> diesel::serialize::Result {
                #to_sql_impl
            }
        }
    };

    TokenStream::from(expanded)
}

fn impl_newtype(field: &syn::Field) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
    let inner_type = &field.ty;

    let attrs: FieldAttrs =
        deluxe::extract_attributes(&mut field.clone()).expect("Wrong field attributes");
    let sql_inner_type = attrs.sql_type;

    let to_sql_impl = quote! {
        <#inner_type as diesel::serialize::ToSql<#sql_inner_type, diesel::pg::Pg>>::to_sql(
            &self.0,
            &mut out.reborrow(),
        )

    };

    let from_sql_impl = quote! {
        let inner =
            diesel::deserialize::FromSql::<#sql_inner_type, diesel::pg::Pg>::from_sql(
                bytes,
            )?;
        Ok(Self(inner))
    };

    (to_sql_impl, from_sql_impl)
}

fn impl_struct(
    fields_named: &syn::FieldsNamed,
    // sql_inner_types: Vec<syn::Type>,
) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
    let named = &fields_named.named;

    let sql_inner_types: Vec<_> = named
        .clone()
        .iter_mut()
        .map(|field| {
            let attrs: FieldAttrs =
                deluxe::extract_attributes(field).expect("Wrong field attributes");
            attrs.sql_type
        })
        .collect();

    let cloned_fields = named
        .iter()
        .map(|field| &field.ident)
        .map(|field_ident| quote! { self.#field_ident.clone() });

    let to_sql_impl = quote! {
        diesel::serialize::WriteTuple::<(
            #(#sql_inner_types),*
        )>::write_tuple(
            &(#(#cloned_fields),*),
            &mut out.reborrow(),
        )
    };

    let field_idents = named.iter().map(|field| &field.ident);
    let field_idents2 = field_idents.clone();

    let from_sql_impl = quote! {
        let (#(#field_idents),*) =
            diesel::deserialize::FromSql::<diesel::sql_types::Record<(#(#sql_inner_types),*)>, diesel::pg::Pg>::from_sql(bytes)?;
        Ok(Self {
            #(#field_idents2),*
        })
    };

    (to_sql_impl, from_sql_impl)
}
