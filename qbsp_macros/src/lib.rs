use proc_macro2::*;
use quote::quote;
use syn::*;

/// Automatically implement `BspVariableValue` on single-field tuple structs. Note: This will additionally implement `From<T> where (inner field type): From<T>` for
/// the type, in order to convert from the individual format types, and will implement `{Deref,DerefMut}<Target = (inner field type)>`. Specify the variant types per
/// BSP format with `#[bsp2(..)]`, `#[bsp29(..)]`, `#[bsp30(..)]` and `#[bsp38(..)]` annotations respectively. All format variants _must_ be specified, if a
/// field does not exist in a format use `NoField` and implement `From<NoField>` for the inner field type.
#[proc_macro_derive(BspVariableValue, attributes(bsp2, bsp29, bsp30, bsp38))]
pub fn bsp_variable_value_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let input = parse_macro_input!(input as DeriveInput);
	let ident = input.ident;

	let (bsp_variable_value_contents, inner_field_ty) = match &input.data {
		Data::Struct(data) => match &data.fields {
			Fields::Unnamed(fields) => {
				assert_eq!(fields.unnamed.len(), 1, "Only tuple structs with a single field are supported");

				let field_inner_type = &fields.unnamed.first().unwrap().ty;

				let mut bsp29_type: Option<Type> = None;
				let mut bsp2_type: Option<Type> = None;
				let mut bsp30_type: Option<Type> = None;
				let mut bsp38_type: Option<Type> = None;

				for attr in input.attrs.iter() {
					let Ok(bsp_repr) = attr.meta.require_list() else {
						continue;
					};

					if compare_path(&bsp_repr.path, "bsp29") {
						bsp29_type = Some(parse2(bsp_repr.tokens.clone()).expect("Argument to #[bsp29(..)] must be a type"));
					} else if compare_path(&bsp_repr.path, "bsp2") {
						bsp2_type = Some(parse2(bsp_repr.tokens.clone()).expect("Argument to #[bsp2(..)] must be a type"));
					} else if compare_path(&bsp_repr.path, "bsp30") {
						bsp30_type = Some(parse2(bsp_repr.tokens.clone()).expect("Argument to #[bsp30(..)] must be a type"));
					} else if compare_path(&bsp_repr.path, "bsp38") {
						bsp38_type = Some(parse2(bsp_repr.tokens.clone()).expect("Argument to #[bsp38(..)] must be a type"));
					}
				}

				let Some(bsp29_type) = bsp29_type else {
					panic!("#[bsp29({{type}})] must be specified");
				};
				let Some(bsp2_type) = bsp2_type else {
					panic!("#[bsp2({{type}})] must be specified");
				};
				let Some(bsp30_type) = bsp30_type else {
					panic!("#[bsp30({{type}})] must be specified");
				};
				let Some(bsp38_type) = bsp38_type else {
					panic!("#[bsp38({{type}})] must be specified");
				};
				(
					quote! {
						impl ::qbsp::reader::BspVariableValue for #ident {
							type Bsp29 = #bsp29_type;
							type Bsp2 = #bsp2_type;
							type Bsp30 = #bsp30_type;
							type Bsp38 = #bsp38_type;
						}
					},
					field_inner_type,
				)
			}
			_ => panic!("Only tuple structs with a single field are supported"),
		},
		_ => panic!("Only tuple structs with a single field are supported"),
	};

	quote! {
		#bsp_variable_value_contents

		impl<__T> From<__T> for #ident where __T: Into<#inner_field_ty> {
			fn from(other: __T) -> Self {
				Self(other.into())
			}
		}

		impl ::core::ops::Deref for #ident {
			type Target = #inner_field_ty;

			fn deref(&self) -> &Self::Target {
				&self.0
			}
		}

		impl ::core::ops::DerefMut for #ident {
			fn deref_mut(&mut self) -> &mut Self::Target {
				&mut self.0
			}
		}
	}
	.into()
}

/// Automatically implements BspValue on structs in the order of the fields, or unit enums with `#[repr(...)]` and explicit discriminants (e.g. Foo = 1).
#[proc_macro_derive(BspValue)]
pub fn bsp_value_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let input = parse_macro_input!(input as DeriveInput);
	let ident = input.ident;

	let (bsp_parse_contents, bsp_struct_size_contents) = match input.data {
		Data::Struct(data) => match data.fields {
			Fields::Named(fields) => {
				let types = fields.named.iter().map(|field| &field.ty);
				let field_names = fields.named.iter().map(|field| field.ident.as_ref().expect("Ident required"));

				(
					quote! {
						Ok(Self {
							#(
								#field_names: ::qbsp::BspParseResultDoingJobExt::job(::qbsp::reader::BspValue::bsp_parse(reader), concat!(
									"Reading field \"",
										stringify!(#field_names),
										"\" on type ",
										stringify!(#ident)
								))?,
							)*
						})
					},
					quote! { #(<#types as ::qbsp::reader::BspValue>::bsp_struct_size(ctx) + )* 0 },
				)
			}
			Fields::Unnamed(_) => panic!("Tuple structs not supported"),
			Fields::Unit => panic!("Unit structs not supported"),
		},
		Data::Enum(data) => {
			for variant in &data.variants {
				if !variant.fields.is_empty() {
					panic!("Only unit enums are supported!");
				}
			}

			let repr = input
				.attrs
				.iter()
				.flat_map(|attr| attr.meta.require_list().ok())
				.filter(|attr| compare_path(&attr.path, "repr"))
				.map(|attr| &attr.tokens)
				.next()
				.expect("#[repr(...)] required");

			let variants = data.variants.iter().map(|variant| &variant.ident);
			let numbers = data.variants.iter().map(|variant| {
				&variant
					.discriminant
					.as_ref()
					.expect("Explicit discriminants required for all variants! (e.g. Foo = 1)")
					.1
			});
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
		impl ::qbsp::reader::BspValue for #ident {
			fn bsp_parse(reader: &mut ::qbsp::reader::BspByteReader) -> ::qbsp::BspResult<Self> {
				#bsp_parse_contents
			}
			fn bsp_struct_size(ctx: &::qbsp::reader::BspParseContext) -> usize {
				#bsp_struct_size_contents
			}
		}
	}
	.into()
}

fn compare_path(path: &Path, s: &str) -> bool {
	path.segments
		== [PathSegment {
			ident: Ident::new(s, Span::mixed_site()),
			arguments: PathArguments::None,
		}]
		.into_iter()
		.collect()
}
