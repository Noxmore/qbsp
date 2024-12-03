use proc_macro2::*;
use quote::quote;
use syn::*;

/// Automatically implements BspValue on structs in the order of the fields, or unit enums with `#[repr(...)]` and explicit discriminants (e.g. Foo = 1).
#[proc_macro_derive(BspValue)]
pub fn bsp_value_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = input.ident;

    let (bsp_parse_contents, bsp_struct_size_contents) = match input.data {
        Data::Struct(data) => {
            match data.fields {
                Fields::Named(fields) => {
                    let types = fields.named.iter().map(|field| &field.ty);
                    let field_names = fields.named.iter().map(|field| field.ident.as_ref().expect("Ident required"));

                    (
                        quote! {
                            Ok(Self {
                                #(#field_names: BspValue::bsp_parse(reader).job(concat!("Reading field \"", stringify!(#field_names), "\" on type ", stringify!(#ident)))?,)*
                            })
                        },
                        quote! { #(<#types as BspValue>::bsp_struct_size(ctx) + )* 0 },
                    )
                }
                Fields::Unnamed(_) => panic!("Tuple structs not supported"),
                Fields::Unit => panic!("Unit structs not supported"),
            }
        }
        Data::Enum(data) => {
            for variant in &data.variants {
                if !variant.fields.is_empty() {
                    panic!("Only unit enums are supported!");
                }
            }
            
            let repr = input.attrs.iter()
                .flat_map(|attr| attr.meta.require_list().ok())
                .filter(|attr| attr.path.segments == [PathSegment { ident: Ident::new("repr", Span::mixed_site()), arguments: PathArguments::None }].into_iter().collect())
                .map(|attr| &attr.tokens)
                .next()
                .expect("#[repr(...)] required");

            let variants = data.variants.iter().map(|variant| &variant.ident);
            let numbers = data.variants.iter().map(|variant| &variant.discriminant.as_ref().expect("Explicit discriminants required for all variants! (e.g. Foo = 1)").1);
            let (variants2, numbers2) = (variants.clone(), numbers.clone());

            (
                quote! { match <#repr as BspValue>::bsp_parse(reader)? {
                    #(#numbers => Ok(Self::#variants)),*,
                    n => Err(BspParseError::InvalidVariant { value: n as i32, acceptable: concat!(#(stringify!(#numbers2), " - ", stringify!(#variants2), "\n"),*) }),
                } },
                quote! { size_of::<#repr>() },
            )
        }
        _ => panic!("Only structs and unit enums supported"),
    };
    
    quote! {
        impl BspValue for #ident {
            fn bsp_parse(reader: &mut BspByteReader) -> BspResult<Self> {
                #bsp_parse_contents
            }
            fn bsp_struct_size(ctx: &BspParseContext) -> usize {
                #bsp_struct_size_contents
            }
        }
    }.into()
}