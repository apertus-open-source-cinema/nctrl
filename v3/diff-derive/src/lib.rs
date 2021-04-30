use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields, Ident};

#[proc_macro_derive(Diff)]
pub fn derive_diff(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match &input.data {
        Data::Struct(data) => {
            let name = input.ident;
            let patch_name = Ident::new(&format!("{}Patch", name), name.span());


            let field_patch_types = &data
                .fields
                .iter()
                .map(|f| {
                    let ty = &f.ty;
                    quote!(<#ty as Diff>::Patch)
                })
                .collect::<Vec<_>>();

            let (patch_impl, diff_impl) = match &data.fields {
                Fields::Named(fields) => {
                    let field_names =
                        &fields.named.iter().map(|f| f.ident.as_ref().unwrap()).collect::<Vec<_>>();
                    let patch_impl = quote! {
                        #[derive(Clone, Debug)]
                        struct #patch_name {
                            #(#field_names: #field_patch_types),*
                        }

                        impl v3::diff::Patch for #patch_name {
                            fn score(&self) -> usize {
                                let mut score = 0;
                                #(score += self.#field_names.score();)*
                                score
                            }

                            fn is_equal(&self) -> bool {
                                let mut is_equal = true;
                                #(is_equal = is_equal && self.#field_names.is_equal();)*
                                is_equal
                            }

                            fn is_maximal(&self) -> bool {
                                let mut is_maximal = true;
                                #(is_maximal = is_maximal && self.#field_names.is_maximal();)*
                                is_maximal
                            }

                            fn unchanged() -> Self {
                                Self {
                                    #(#field_names: #field_patch_types::unchanged(),)*
                                }
                            }
                        }
                    };

                    let diff_impl = quote! {
                        impl Diff for #name {
                            type Patch = #patch_name;

                            fn diff(&self, other: &Self) -> Self::Patch {
                                #patch_name {
                                    #(#field_names: self.#field_names.diff(&other.#field_names),)*
                                }
                            }
                        }
                    };

                    (patch_impl, diff_impl)
                }
                _ => todo!(),
            };


            let tokens = quote! {
            #patch_impl
            #diff_impl
                };

            println!("{}", tokens);
            tokens.into()
        }
        _ => todo!(),
    }
}
