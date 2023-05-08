use std::rc::Weak;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use sleigh_rs::{GlobalAnonReference, GlobalElement, TokenField};

use crate::builder::WorkType;

use super::formater::from_sleigh;
use super::helper::{bitrange_from_value, from_endian_bytes};
use super::{DisassemblerGlobal, Meanings, ToLiteral};

pub struct TokenFieldStruct {
    pub name: Ident,
    pub create_fun: Ident,
    pub display_fun: Ident,
    disassembler: Weak<dyn DisassemblerGlobal>,
    pub sleigh: GlobalAnonReference<sleigh_rs::TokenField>,
}

impl TokenFieldStruct {
    pub fn new(
        disassembler: Weak<dyn DisassemblerGlobal>,
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

        let token = &sleigh.token;
        let token_value = format_ident!("token_value");
        let token_type = WorkType::unsigned_from_bytes(
            token.len_bytes().get().try_into().unwrap(),
        );
        let token_bytes = token.len_bytes.get().unsuffixed();
        let token_endian = from_endian_bytes(sleigh.token.endian());

        let field_type = WorkType::new_int_bits(
            range.len().get().try_into().unwrap(),
            signed,
        );
        let field_value = format_ident!("field_value");

        let convert = bitrange_from_value(
            &field_value,
            field_type,
            &token_value,
            range,
            signed,
        );
        tokens.extend(quote! {
            pub struct #name(pub #field_type);
            impl #name {
                pub fn #create_fun(tokens: &[u8]) -> Self {
                    let Some(token) = tokens.get(0..#token_bytes) else {
                        unreachable!();
                    };
                    let token = <[u8; #token_bytes]>::try_from(token).unwrap();
                    let #token_value = #token_type::#token_endian(token);
                    #convert
                    Self(#field_value)
                }
                pub fn #display_fun(&self) -> #display_type {
                    todo!();
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

//pub fn read_token_field(data: &Ident, token_field: &TokenField) -> TokenStream {
//    let big_endian = token_field.token.endian.is_big();
//    let (data_addr, data_lsb) = sleigh4rust::bytes_from_varnode(
//        big_endian,
//        0,
//        token_field.token().len_bytes.get(),
//        token_field.range.start(),
//        token_field.range.len().get(),
//    );
//    let read_type = WorkType::new_int_bits(
//        NonZeroTypeU::new(data_lsb + token_field.range.len().get()).unwrap(),
//        token_field.meaning.is_signed(),
//    );
//    let read_function = read_type.sleigh4rust_read_memory();
//    let len_bits = token_field.range.len().get().unsuffixed();
//    let data_addr = data_addr.unsuffixed();
//    let data_lsb = data_lsb.unsuffixed();
//    quote! {
//        #data.#read_function::<#big_endian>(
//            #data_addr,
//            #data_lsb,
//            #len_bits,
//        )
//    }
//}

//pub fn token_field_read_execution(
//    data: &Ident,
//    token_field: &TokenField,
//) -> TokenStream {
//    let final_type = token_field_final_type(token_field);
//    let read_execution = read_token_field(data, token_field);
//    quote! {
//        #read_execution.map(|x| x as #final_type)
//    }
//}
//
//pub fn token_field_read_disassembly(
//    data: &Ident,
//    token_field: &TokenField,
//) -> TokenStream {
//    let read_execution = read_token_field(data, token_field);
//    quote! {
//        #read_execution.map(|x| x as #DISASSEMBLY_WORK_TYPE)
//    }
//}

pub fn token_field_display(
    data: impl ToTokens,
    token_field: &TokenField,
    meanings: &Meanings,
) -> TokenStream {
    let sleigh_meaning = token_field.meaning();
    meanings.display_function_call(data, sleigh_meaning)
}
