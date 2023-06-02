use std::collections::HashMap;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use sleigh_rs::{Endian, FieldBits, NumberNonZeroUnsigned};

use crate::builder::helper::bytes_from_value;
use crate::builder::WorkType;

use super::helper::{bitrange_from_value, from_endian_bytes};
use super::{Disassembler, ToLiteral};

#[derive(Hash, PartialEq, Eq)]
struct TokenFieldKey {
    token_bytes: NumberNonZeroUnsigned,
    token_endian: Endian,
    bits: FieldBits,
}

pub struct TokenFieldFunction {
    pub read: Ident,
    pub read_type: WorkType,
    pub ids: Vec<sleigh_rs::TokenFieldId>,
}

pub struct TokenFieldFunctions {
    // is very common to have multiple token_fields that are identical,
    // so use the HashMap to join then
    functions: HashMap<TokenFieldKey, TokenFieldFunction>,
}

impl TokenFieldFunctions {
    pub fn new<'a>(sleigh: &sleigh_rs::Sleigh) -> Self {
        let token_fields = sleigh
            .token_fields()
            .iter()
            .enumerate()
            .map(|(i, field)| (sleigh_rs::TokenFieldId(i), field));
        let mut count = 0usize;
        let mut functions = HashMap::new();
        for (id, token_field) in token_fields {
            let token = sleigh.token(token_field.token);
            let key = TokenFieldKey {
                token_bytes: token.len_bytes,
                token_endian: token.endian,
                bits: token_field.bits.clone(),
            };
            functions
                .entry(key)
                .and_modify(|entry: &mut TokenFieldFunction| entry.ids.push(id))
                .or_insert_with(|| {
                    count += 1;
                    TokenFieldFunction {
                        read: format_ident!("token_{}", count),
                        read_type: WorkType::unsigned_from_bits(
                            token_field.bits.len().get().try_into().unwrap(),
                        ),
                        ids: vec![id],
                    }
                });
        }
        Self { functions }
    }

    pub fn read_function(
        &self,
        sleigh: &sleigh_rs::Sleigh,
        id: sleigh_rs::TokenFieldId,
    ) -> &TokenFieldFunction {
        let token_field = sleigh.token_field(id);
        let token = sleigh.token(token_field.token);
        let key = TokenFieldKey {
            bits: token_field.bits.clone(),
            token_bytes: token.len_bytes,
            token_endian: token.endian,
        };
        &self.functions.get(&key).unwrap()
    }

    pub fn to_tokens(
        &self,
        tokens: &mut TokenStream,
        disassembler: &Disassembler,
    ) {
        for (key, value) in self.functions.iter() {
            let TokenFieldKey {
                bits,
                token_bytes,
                token_endian,
            } = key;
            let TokenFieldFunction {
                read,
                ids,
                read_type,
            } = value;
            let from_endian_call = from_endian_bytes(*token_endian);
            let token_type = WorkType::unsigned_from_bytes(
                token_bytes.get().try_into().unwrap(),
            );
            let token_bytes_un = token_bytes.get().unsuffixed();
            let tokens_param = format_ident!("tokens");

            let mut doc = "Create token_fields:".to_string();
            for id in ids.iter() {
                doc.push(' ');
                doc.push_str(disassembler.sleigh.token_field(*id).name());
            }
            //TODO attach value should be converted here or in execution?
            let body = if u32::try_from(token_bytes.get()).unwrap()
                != token_type.len_bytes()
            {
                let bytes = format_ident!("bytes");
                let value = format_ident!("value");
                let token_type_bytes = token_type.len_bytes().unsuffixed();
                let (bytes_start, bytes_end) = bytes_from_value(
                    *token_endian,
                    token_type.len_bytes(),
                    token_bytes.get().try_into().unwrap(),
                );
                let convert = bitrange_from_value(
                    bits.start().try_into().unwrap()
                        ..bits.end().get().try_into().unwrap(),
                    *read_type,
                    &value,
                );
                quote! {
                    let #bytes = [0u8; #token_type_bytes];
                    #bytes[#bytes_start..#bytes_end]
                        .copy_from_slice(#tokens_param[0..#token_bytes_un]);
                    let #value = #token_type::#from_endian_call(#bytes);
                    #convert
                }
            } else {
                bitrange_from_value(
                    bits.start().try_into().unwrap()
                        ..bits.end().get().try_into().unwrap(),
                    *read_type,
                    quote! {#token_type::#from_endian_call(
                        #tokens_param[0..#token_bytes_un].try_into().unwrap()
                    )},
                )
            };
            tokens.extend(quote! {
                #[doc = #doc]
                fn #read(#tokens_param: &[u8]) -> #read_type {
                    #body
                }
            });
        }
    }
}
