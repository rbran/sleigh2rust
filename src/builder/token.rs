use std::collections::HashMap;
use std::rc::Rc;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use sleigh_rs::Assembly;
use sleigh_rs::NonZeroTypeU;

use crate::builder::formater::*;
use crate::builder::WorkType;

use super::SLEIGH_IDENT;

#[derive(Debug, Clone)]
pub struct TokenField {
    name: Ident,
    bit_start: u32,
    bit_len: u32,
    work_type: WorkType,
    sleigh: Rc<sleigh_rs::Assembly>,
}

impl TokenField {
    pub fn new(ass: Rc<sleigh_rs::Assembly>) -> Rc<Self> {
        use sleigh_rs::semantic::assembly::AssemblyType::*;
        let (bit_start, bit_len, work_type) = match &ass.assembly_type {
            Field(field) => (
                field.bit_range.start,
                field.bit_range.end - field.bit_range.start,
                WorkType::from_bits(
                    (field.bit_range.end - field.bit_range.start)
                        .try_into()
                        .unwrap(),
                    field.signed,
                ),
            ),
            _ => unreachable!(),
        };
        let name = format_ident!("{}", snake_case(from_sleigh(&ass.name)));
        let sleigh = ass;
        Rc::new(Self {
            name,
            bit_start: bit_start.try_into().unwrap(),
            bit_len: bit_len.try_into().unwrap(),
            work_type,
            sleigh,
        })
    }
    pub fn read(&self) -> &Ident {
        &self.name
    }
    pub fn return_type(&self) -> &WorkType {
        &self.work_type
    }
    pub fn gen_function(&self, work_type: &WorkType) -> TokenStream {
        let name = &self.name;
        let return_type = &self.work_type;
        let bit_start = self.bit_start;
        let bit_mask = (1u128 << self.bit_len) - 1;
        quote! {
            pub fn #name(&self) -> #return_type {
                let mut tmp = self.0;
                tmp >>= #bit_start as #work_type;
                tmp &= #bit_mask as #work_type;
                tmp as #return_type
            }
        }
    }
}

const TOKEN_PARSER_NAME: [&str; 3] = [SLEIGH_IDENT, "token", "parser"];
const TOKEN_PARSER_NEW: &str = "new";
#[derive(Debug, Clone)]
pub struct TokenParser {
    name: Ident,
    work_type: WorkType,
    tokens: HashMap<NonZeroTypeU, Ident>,
    fields: HashMap<*const sleigh_rs::Assembly, Rc<TokenField>>,
}
impl TokenParser {
    pub fn new(sleigh: &sleigh_rs::Sleigh) -> Self {
        let token_size =
            sleigh.tokens().map(|x| x.size.get()).max().unwrap_or(0);
        assert!(token_size % 8 == 0);

        let work_type =
            WorkType::unsigned_from_bits(token_size.try_into().unwrap());
        let tokens = sleigh
            .tokens()
            .map(|token| {
                let token_len = token.size;
                let token_name =
                    format_ident!("{}_{}", TOKEN_PARSER_NEW, token_len.get());
                (token_len, token_name)
            })
            .collect();
        let fields = sleigh
            .token_fields()
            .map(|field| {
                let ptr = Rc::as_ptr(&field);
                let token = TokenField::new(field);
                (ptr, token)
            })
            .collect();
        let name = format_ident!(
            "{}",
            upper_cammel_case(TOKEN_PARSER_NAME.into_iter())
        );
        Self {
            name,
            work_type,
            tokens,
            fields,
        }
    }
    pub fn name(&self) -> &Ident {
        &self.name
    }
    pub fn creator(&self, len: NonZeroTypeU) -> &Ident {
        self.tokens.get(&len).unwrap()
    }
    pub fn field(&self, assembly: &Rc<Assembly>) -> &Rc<TokenField> {
        self.fields.get(&Rc::as_ptr(assembly)).as_ref().unwrap()
    }
    pub fn gen_read_call(
        &self,
        instance: &Ident,
        ass: &Rc<Assembly>,
    ) -> (TokenStream, WorkType) {
        let token_field = self.field(ass);
        let token_type = *token_field.return_type();
        let token_read_name = token_field.read();
        let call = quote! {
            #instance.#token_read_name()
        };
        (call, token_type)
    }
    pub fn gen_struct(&self) -> TokenStream {
        let name = &self.name;
        let token_type = &self.work_type;
        let token_size = token_type.len_bytes();
        let tokens = self.tokens.iter().map(|(token_len, fun_name)| {
            let read_size: usize = token_len.get().try_into().unwrap();
            assert!(read_size % 8 == 0);
            let read_size = read_size / 8;
            let read_start = token_size - read_size;
            quote! {
                pub fn #fun_name(data: &[u8]) -> Option<(&[u8], Self)> {
                    if data.len() < #read_size {
                        return None
                    }
                    let (data, rest) = data.split_at(#read_size);
                    let mut inner_raw = [0u8; #token_size];
                    inner_raw[#read_start..].copy_from_slice(data);
                    let inner = #token_type::from_be_bytes(inner_raw);
                    Some((rest, Self(inner)))
                }
            }
        });
        let fields = self.fields.values().map(|x| x.gen_function(token_type));
        quote! {
            pub struct #name(#token_type);
            impl #name {
                #(#tokens)*
                #(#fields)*
            }
        }
    }
}
