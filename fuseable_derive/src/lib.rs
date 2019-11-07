#![recursion_limit = "512"]

extern crate proc_macro;

use proc_macro::TokenStream as TS;
use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::{
    parse_macro_input,
    Data::{Enum, Struct},
    DeriveInput,
    Field,
};

#[proc_macro_derive(Fuseable, attributes(fuseable))]
pub fn fuse_derive(input: TS) -> TS {
    let ast = parse_macro_input!(input as DeriveInput);
    let gen = impl_fuseable(&ast);
    // println!("{}", gen);
    gen.into()
}

fn impl_fuseable(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let (is_dir, read, write) = impl_body(ast);

    let dummy_const = Ident::new(&format!("_IMPL_FUSEABLE_FOR_{}", name), Span::call_site());

    let ret = quote! {
        #[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
        const #dummy_const: () = {
            use fuseable::{Result, Fuseable, FuseableError, type_name};

            #[allow(unused_variables)]
            impl Fuseable for #name {

                fn is_dir(&self, path: &mut Iterator<Item = &str>) -> Result<bool> {
                    #is_dir
                }

                fn read(&self, path: &mut Iterator<Item = &str>) -> Result<Either<Vec<String>, String>> {
                    #read
                }

                fn write(&mut self, path: &mut Iterator<Item = &str>, value: Vec<u8>) -> Result<()> {
                    #write
                }
            }
        };
    };


    // println!("{}", ret);

    ret
}

#[derive(Debug, Clone)]
enum IsDirImpl {
    FunctionCall(TokenStream),
    Static(syn::LitBool),
}

#[derive(Debug)]
struct VirtualField {
    name: Ident,
    is_dir: IsDirImpl,
    read: TokenStream,
    write: TokenStream,
}

fn impl_body(ast: &syn::DeriveInput) -> (TokenStream, TokenStream, TokenStream) {
    let attrs: Vec<_> = ast
        .attrs
        .iter()
        .filter(|a| a.path.segments.iter().map(|s| &s.ident).next().unwrap() == "fuseable")
        .collect();

    use syn::{
        Meta::{List, NameValue},
        MetaList,
        MetaNameValue,
        NestedMeta::Meta,
    };

    let mut virtual_fields = Vec::new();

    fn lit_to_direct_string(lit: &syn::Lit) -> String {
        match lit {
            syn::Lit::Str(str) => str.value(),
            _ => panic!("could not convert literal to string: {:?}", lit),
        }
    }

    fn lit_to_ident(lit: &syn::Lit) -> syn::Ident {
        let string = lit_to_direct_string(lit);
        let tokens = syn::parse_str(&string).unwrap();

        syn::parse2(tokens).unwrap()
    }

    /*
    fn lit_to_expr_path(lit: &syn::Lit) -> syn::ExprPath {
        let string = lit_to_direct_string(lit);
        let tokens = syn::parse_str(&string).unwrap();

        syn::parse2(tokens).unwrap()
    }
    */

    fn lit_to_token_stream(lit: &syn::Lit) -> TokenStream {
        let string = lit_to_direct_string(lit);
        syn::parse_str(&string).unwrap()
    }

    for attr in &attrs {
        if let Ok(syn::Meta::List(syn::MetaList { nested, .. })) = &attr.parse_meta() {
            for nested_meta in nested {
                match nested_meta {
                    Meta(List(MetaList { nested, path, .. })) => {
                        if path.get_ident().unwrap() == "virtual_field" {
                            let mut name = None;
                            let mut is_dir = None;
                            let mut read = None;
                            let mut write = None;

                            for nested_meta in nested {
                                if let Meta(NameValue(MetaNameValue { path, lit, .. })) =
                                    nested_meta
                                {
                                    let ident = path.get_ident().unwrap();

                                    if ident == "name" {
                                        name = Some(lit_to_ident(lit))
                                    } else if ident == "is_dir" {
                                        is_dir = Some(lit_to_token_stream(lit))
                                    } else if ident == "read" {
                                        read = Some(lit_to_token_stream(lit))
                                    } else if ident == "write" {
                                        write = Some(lit_to_token_stream(lit))
                                    }
                                }
                            }

                            let name = name.unwrap();
                            let is_dir = is_dir.unwrap();
                            let read = read.unwrap();
                            let write = write.unwrap();

                            let is_dir = match syn::parse2::<syn::LitBool>(is_dir.clone()) {
                                Ok(lit) => IsDirImpl::Static(lit),
                                Err(_) => IsDirImpl::FunctionCall(is_dir),
                            };

                            virtual_fields.push(VirtualField { name, is_dir, read, write });
                        } else {
                            panic!("unhandled meta {:?}", attrs)
                        }
                    }
                    _ => panic!("unhandled meta {:?}", nested_meta),
                }
            }
        }
    }

    /*
    for virtual_field in &virtual_fields {
        println!();
        println!("name: {}", virtual_field.name);
        println!("is_dir: {:?}", virtual_field.is_dir);
        println!("read: {:?}", virtual_field.read);
        println!("write: {:?}", virtual_field.write);
    }
    */

    // &attr.interpret_meta()

    match ast.data {
        Struct(ref data) => impl_struct(data, &virtual_fields),
        Enum(ref data) => {
            if !virtual_fields.is_empty() {
                panic!("cannot handle virtual fields in enums yet");
            }

            impl_enum(&ast.ident, data)
        }
        _ => unimplemented!(),
    }
}

fn impl_struct(
    data: &syn::DataStruct,
    virtual_fields: &[VirtualField],
) -> (TokenStream, TokenStream, TokenStream) {
    let (is_dir, read, write) = match data.fields {
        syn::Fields::Named(ref fields) => {
            /*
            let fields_normal: Vec<_> = fields
                .named
                .iter()
                .map(|f| &f.ident)
                .map(|f| quote! { #f })
                .collect();
            let wrapped_fields: Vec<_> = fields
                .named
                .iter()
                .map(|f| &f.ident)
                .map(|f| quote! { &self.#f })
                .collect();
            */

            let read_prefix = quote! { &self. };
            let write_prefix = quote! { &mut self. };

            // println!("implementing virtual fields {:?}", virtual_fields);
            impl_fields(
                &fields.named.iter().collect::<Vec<_>>(),
                &read_prefix,
                &write_prefix,
                virtual_fields,
            )
        }
        _ => unimplemented!(),
    };

    (is_dir, read, write)
}

fn impl_enum(name: &Ident, data: &syn::DataEnum) -> (TokenStream, TokenStream, TokenStream) {
    let variants: Vec<_> = data.variants.iter().map(impl_enum_variant).collect();
    let variant_names_read: Vec<_> = data.variants.iter().map(|v| &v.ident).collect();
    let variant_names_is_dir: Vec<_> = variant_names_read.clone();
    let variant_names_write: Vec<_> = variant_names_read.clone();

    let is_dir: Vec<_> = variants.iter().map(|v| &v.0).collect();
    let read: Vec<_> = variants.iter().map(|v| &v.1).collect();
    let write: Vec<_> = variants.iter().map(|v| &v.2).collect();

    let is_dir = quote! {
        use self::#name::{#(#variant_names_is_dir),*};

        match self {
            #(#is_dir, )*
        }
    };

    let read = quote! {
        use self::#name::{#(#variant_names_read),*};

        match self {
            #(#read, )*
        }
    };

    let write = quote! {
        use self::#name::{#(#variant_names_write),*};

        match self {
            #(#write, )*
        }
    };

    (is_dir, read, write)
}

fn impl_enum_variant(variant: &syn::Variant) -> (TokenStream, TokenStream, TokenStream) {
    let name = &variant.ident;

    let (is_dir, read, write) = match variant.fields {
        syn::Fields::Named(ref fields) => {
            let fields: Vec<_> = fields.named.iter().collect();

            if fields.len() == 1 {
                let name = fields[0].clone();
                impl_enum_variant_flatten(&name, false)
            } else {
                impl_enum_variant_namend(&fields)
            }
        }
        syn::Fields::Unnamed(ref fields) => {
            let fields: Vec<_> = fields.unnamed.iter().collect();
            if fields.len() != 1 {
                unimplemented!()
            } else {
                let mut field = fields[0].clone();
                field.ident = Some(Ident::new("value_", Span::call_site()));
                impl_enum_variant_flatten(&field, true)
            }
        }
        syn::Fields::Unit => impl_enum_variant_unit(&name),
    };

    let is_dir = quote! {
        #name #is_dir
    };

    let read = quote! {
        #name #read
    };

    let write = quote! {
        #name #write
    };

    (is_dir, read, write)
}

fn impl_enum_variant_unit(name: &syn::Ident) -> (TokenStream, TokenStream, TokenStream) {
    let is_dir = quote! {
        => {
            match path.next() {
                Some(s) => Err(FuseableError::not_a_directory(stringify!($t), s)),
                None => Ok(false)
            }
        }
    };

    let read = quote! {
        => {
            match path.next() {
                Some(s) => Err(FuseableError::not_a_directory(stringify!($t), s)),
                None => Ok(stringify!(#name).to_owned())
            }
        }
    };

    let write = quote! {
        => {
            None => Err(FuseableError::unsupported("write", type_name(&self))),
        }
    };

    (is_dir, read, write)
}

fn impl_enum_variant_flatten(
    name: &syn::Field,
    unnamed: bool,
) -> (TokenStream, TokenStream, TokenStream) {
    let name = name.ident.clone().unwrap();
    let wrapped_name = if unnamed {
        quote! {
            ( #name )
        }
    } else {
        quote! {
            { #name }
        }
    };

    let wrapped_name_write = if unnamed {
        quote! {
            ( ref mut #name )
        }
    } else {
        quote! {
            { ref mut #name }
        }
    };

    let is_dir = quote! {
        #wrapped_name => Fuseable::is_dir(#name, path)
    };

    let read = quote! {
        #wrapped_name => Fuseable::read(#name, path)
    };

    let write = quote! {
        #wrapped_name_write => Fuseable::write(#name, path, value)
    };

    (is_dir, read, write)
}

fn impl_enum_variant_namend(fields: &[&syn::Field]) -> (TokenStream, TokenStream, TokenStream) {
    let fields_is_dir: Vec<_> = fields
        .iter()
        .map(|f| {
            let f = f.ident.clone().unwrap();
            quote! { #f }
        })
        .collect();

    /*
    let fields_write: Vec<_> = fields.iter().map(|f| {
        let f = f.ident.clone().unwrap();
        quote! { ref mut #f }
    }).collect();
    */

    let fields_read: Vec<_> = fields_is_dir.clone();
    let fields_write: Vec<_> = fields_is_dir.clone();

    let (fields_impl_is_dir, fields_impl_read, fields_impl_write) =
        impl_fields(&fields, &quote! {}, &quote! {}, &Vec::new());

    let is_dir = quote! {
        { #(#fields_is_dir),* } => {
            #fields_impl_is_dir
        }
    };

    let read = quote! {
        { #(#fields_read),* } => {
            #fields_impl_read
        }
    };

    let write = quote! {
        { #(#fields_write),* } => {
            #fields_impl_write
        }
    };

    (is_dir, read, write)
}

#[derive(Debug)]
struct ParsedField {
    ident: Ident,
    skip: bool,
    writable: bool,
    readable: bool,
}

fn parse_field(field: &&syn::Field) -> ParsedField {
    let ident = field.ident.clone().unwrap();

    let attrs: Vec<_> = field
        .attrs
        .iter()
        .filter(|a| a.path.segments.iter().map(|s| &s.ident).next().unwrap() == "fuseable")
        .collect();

    let mut skip = false;
    let mut writable = true;
    let mut readable = true;

    for attr in attrs {
        if let Ok(syn::Meta::List(syn::MetaList { nested, .. })) = &attr.parse_meta() {
            if let Some(syn::NestedMeta::Meta(syn::Meta::Path(path))) = nested.iter().next() {
                let ident = path.get_ident().unwrap();

                if ident == "skip" {
                    skip = true;
                } else if ident == "ro" {
                    writable = false;
                } else if ident == "wo" {
                    readable = false;
                } else if ident == "rw" {
                    readable = true;
                    writable = true;
                }
            }
        }
    }

    ParsedField { ident, skip, readable, writable }
    //    println!("parsed field {:#?}", field);
}

fn impl_fields(
    fields: &[&Field],
    prefix_read: &TokenStream,
    prefix_write: &TokenStream,
    virtual_fields: &[VirtualField],
) -> (TokenStream, TokenStream, TokenStream) {
    let fields_read: Vec<_> = fields
        .iter()
        .map(parse_field)
        .filter(|f| (!f.skip) && f.readable)
        .map(|f| f.ident.clone())
        .collect();
    let fields_write: Vec<_> = fields
        .iter()
        .map(parse_field)
        .filter(|f| (!f.skip) && f.writable)
        .map(|f| f.ident.clone())
        .collect();
    let fields_is_dir = fields_read.clone();

    let wrapped_fields_read: Vec<_> =
        fields_read.iter().map(|f| quote! { #prefix_read #f }).collect();
    let wrapped_fields_is_dir: Vec<_> =
        fields_is_dir.iter().map(|f| quote! { #prefix_read #f }).collect();
    let wrapped_fields_write: Vec<_> =
        fields_write.iter().map(|f| quote! { #prefix_write #f }).collect();

    let mut all_fields = fields_read.clone();

    let virtual_names_read: Vec<_> = virtual_fields.iter().map(|f| f.name.clone()).collect();
    let virtual_names_is_dir = virtual_names_read.clone();
    let virtual_names_write = virtual_names_read.clone();

    let virtual_reads: Vec<_> = virtual_fields.iter().map(|f| f.read.clone()).collect();
    let virtual_writes: Vec<_> = virtual_fields.iter().map(|f| f.write.clone()).collect();
    let virtual_is_dirs: Vec<_> = virtual_fields.iter().map(|f| f.is_dir.clone()).collect();

    all_fields.append(&mut virtual_names_read.clone());

    let read = if fields_read.len() + virtual_names_read.len() > 0 {
        quote! {
            match path.next() {
                Some(ref name) => {
                    match name.as_ref() {
                        #(stringify!(#fields_read) => Fuseable::read(#wrapped_fields_read, path), )*
                        #(stringify!(#virtual_names_read) => #virtual_reads(path), )*
                        _ => Err(FuseableError::not_found(name)),
                    }
                }
                None => Ok(Either::Left(vec![#(stringify!(#all_fields).to_string()),*]))
            }
        }
    } else {
        quote! {
            match path.next() {
                Some(ref name) => {
                    Err(FuseableError::not_found(name))
                },
                None => Ok(Either::Left(vec![])),
            }
        }
    };

    let write = if fields_write.len() + virtual_names_write.len() > 0 {
        quote! {
            match path.next() {
                Some(ref name) => {
                    match name.as_ref() {
                        #(stringify!(#fields_write) => Fuseable::write(#wrapped_fields_write, path, value), )*
                        #(stringify!(#virtual_names_write) => #virtual_writes(path, value), )*
                        _ => Err(FuseableError::not_found(name)),
                    }
                }
                None => Err(FuseableError::unsupported("write", type_name(&self))),
            }
        }
    } else {
        quote! {
            match path.next() {
                Some(ref name) => {
                    Err(FuseableError::not_found(name))
                },
                None => Err(FuseableError::unsupported("write", type_name(&self))),
            }
        }
    };

    let is_dirs_impls: Vec<_> = virtual_names_is_dir
        .iter()
        .zip(virtual_is_dirs)
        .map(|(name, is_dir_impl)| match is_dir_impl {
            IsDirImpl::Static(lit) => {
                quote! {
                    stringify!(#name) => Ok(#lit),
                }
            }
            IsDirImpl::FunctionCall(stream) => {
                quote! {
                    stringify!(#name) => #stream(path),
                }
            }
        })
        .collect();

    let is_dir = if fields_is_dir.len() + is_dirs_impls.len() > 0 {
        quote! {
            match path.next() {
                Some(ref name) => {
                    match name.as_ref() {
                        #(stringify!(#fields_is_dir) => Fuseable::is_dir(#wrapped_fields_is_dir, path), )*
                        #(#is_dirs_impls)*
                        _ => Err(FuseableError::not_found(name)),
                    }
                }
                None => Ok(true)
            }
        }
    } else {
        quote! {
            match path.next() {
                Some(ref name) => Err(FuseableError::not_found(name)),
                None => Ok(true)
            }
        }
    };

    (is_dir, read, write)
}
