use std::rc::{Rc, Weak};

use indexmap::IndexMap;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use sleigh_rs::{GlobalAnonReference, GlobalElement, TokenField};

use crate::builder::WorkType;

use super::formater::from_sleigh;
use super::helper::{bitrange_from_value, from_endian_bytes};
use super::{Disassembler, Meanings, ToLiteral};

pub struct Tokens {
    pub token_structs: IndexMap<*const sleigh_rs::Token, Rc<TokenStruct>>,
    pub field_structs:
        IndexMap<*const sleigh_rs::TokenField, Rc<TokenFieldStruct>>,
}

pub struct TokenStruct {
    pub name: Ident,
    pub parse_fun: Ident,
    sleigh: GlobalAnonReference<sleigh_rs::Token>,
}

pub struct TokenFieldStruct {
    pub name: Ident,
    pub create_fun: Ident,
    pub display_fun: Ident,
    disassembler: Weak<Disassembler>,
    sleigh: GlobalAnonReference<sleigh_rs::TokenField>,
}

impl Tokens {
    pub fn new(
        disassembler: Weak<Disassembler>,
        sleigh: &sleigh_rs::Sleigh,
    ) -> Self {
        let token_structs = sleigh
            .tokens()
            .map(|token| {
                let ptr = token.element_ptr();
                let token = TokenStruct::new(token);
                (ptr, Rc::new(token))
            })
            .collect();
        let field_structs = sleigh
            .token_fields()
            .map(|field| {
                let ptr = field.element_ptr();
                let field =
                    TokenFieldStruct::new(Weak::clone(&disassembler), field);
                (ptr, Rc::new(field))
            })
            .collect();
        Self {
            token_structs,
            field_structs,
        }
    }
}

impl TokenStruct {
    pub fn new(token: &GlobalElement<sleigh_rs::Token>) -> Self {
        let name = format_ident!("Token_{}", from_sleigh(token.name()));
        Self {
            sleigh: token.reference(),
            name,
            parse_fun: format_ident!("new"),
        }
    }

    pub fn create_new(&self, token: impl ToTokens) -> TokenStream {
        let name = &self.name;
        let parse_fun = &self.parse_fun;
        quote! { #name::#parse_fun(#token) }
    }
}

impl TokenFieldStruct {
    pub fn new(
        disassembler: Weak<Disassembler>,
        token_field: &GlobalElement<sleigh_rs::TokenField>,
    ) -> Self {
        let name =
            format_ident!("TokenField_{}", from_sleigh(token_field.name()));
        Self {
            disassembler,
            sleigh: token_field.reference(),
            name,
            create_fun: format_ident!("new"),
            display_fun: format_ident!("display"),
        }
    }

    pub fn create_new(&self, token: impl ToTokens) -> TokenStream {
        let name = &self.name;
        let create_fun = &self.create_fun;
        quote! { #name::#create_fun(#token) }
    }

    pub fn display_fun(&self, instance: impl ToTokens) -> TokenStream {
        let display_fun = &self.display_fun;
        quote! { #instance.#display_fun() }
    }

    pub fn value(&self, instance: impl ToTokens) -> TokenStream {
        quote! { #instance.0 }
    }

    pub fn inline_value(&self, token: impl ToTokens) -> TokenStream {
        let name = &self.name;
        let create_fun = &self.create_fun;
        quote! { #name::#create_fun(#token).0 }
    }
}

impl ToTokens for Tokens {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for token in self.token_structs.values() {
            token.to_tokens(tokens);
        }
        for field in self.field_structs.values() {
            field.to_tokens(tokens);
        }
    }
}

impl ToTokens for TokenStruct {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            name,
            parse_fun,
            sleigh,
        } = self;
        let sleigh = sleigh.element();
        let token_bytes: u32 = sleigh.len_bytes().get().try_into().unwrap();
        let token_endian = from_endian_bytes(sleigh.endian());
        let token_type = WorkType::unsigned_from_bytes(token_bytes);
        let token_bytes = token_bytes.unsuffixed();
        tokens.extend(quote! {
            pub struct #name(pub #token_type);
            impl #name {
                pub fn #parse_fun(tokens: &[u8]) -> Self {
                    Self(#token_type::#token_endian(
                        tokens[0..#token_bytes].try_into().unwrap()
                    ))
                }
            }
        })
    }
}

impl ToTokens for TokenFieldStruct {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            name,
            create_fun,
            display_fun,
            sleigh,
            disassembler,
        } = self;
        let disassembler = disassembler.upgrade().unwrap();
        let sleigh = sleigh.element();
        let display_type = &disassembler.display_element().name;
        let signed = sleigh.meaning.is_signed();
        let range = &sleigh.range;

        let token = disassembler
            .tokens()
            .token_structs
            .get(&sleigh.token.element_ptr())
            .unwrap();
        let token_struct = &token.name;
        let token_new = &token.parse_fun;

        let field_type = WorkType::new_int_bits(
            range.len().get().try_into().unwrap(),
            signed,
        );
        let field_value = format_ident!("field_value");
        let tokens_param = format_ident!("tokens");

        let convert = bitrange_from_value(
            &field_value,
            field_type,
            quote! {#token_struct::#token_new(#tokens_param).0},
            range,
            signed,
        );

        let display = token_field_display(
            quote! {self.0},
            &sleigh,
            disassembler.meanings(),
        );

        tokens.extend(quote! {
            pub struct #name(pub #field_type);
            impl #name {
                pub fn #create_fun(#tokens_param: &[u8]) -> Self {
                    #convert
                    Self(#field_value)
                }
                pub fn #display_fun(&self) -> #display_type {
                    #display
                }
            }
        });
    }
}

pub fn token_field_final_type(token_field: &TokenField) -> WorkType {
    WorkType::new_int_bits(
        token_field.range.len().get().try_into().unwrap(),
        token_field.meaning.is_signed(),
    )
}

pub fn token_field_display(
    data: impl ToTokens,
    token_field: &TokenField,
    meanings: &Meanings,
) -> TokenStream {
    let sleigh_meaning = token_field.meaning();
    meanings.display_function_call(data, sleigh_meaning)
}
