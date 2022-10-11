use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use sleigh_rs::Assembly;
use sleigh_rs::NonZeroTypeU;

use crate::builder::formater::*;
use crate::builder::WorkType;

#[derive(Debug, Clone)]
pub struct TokenField<'a> {
    read_func: Ident,
    sleigh: &'a sleigh_rs::Assembly,
}

impl<'a> TokenField<'a> {
    pub fn new(ass: &'a sleigh_rs::Assembly) -> Rc<Self> {
        assert!(ass.field().is_some());
        let name = format_ident!("{}", from_sleigh(&ass.name));
        let sleigh = ass;
        Rc::new(Self {
            read_func: name,
            sleigh,
        })
    }
    pub fn read(&self) -> &Ident {
        &self.read_func
    }
    pub fn return_type(&self) -> WorkType {
        let field = self.sleigh.field().unwrap();
        let len = field.bit_range.end - field.bit_range.start;
        WorkType::unsigned_from_bits(len.try_into().unwrap())
    }
}

//Token parser will be based on len, any assembly field that is smaller then the
//len of the token can be read.
//eg: assembly `field_a` is part of the token `token_a`, that is 2bytes.
//    assembly `field_b` is part of the token `token_b`, that is 4bytes.
//It will be created a TokenParser for `token_a` and `token_b`,
//`token_a` work_type will be a u16, and will only produce `field_a`.
//`token_b` work_type will be a u32, and will produce both fields.
pub fn gen_token_parsers<'a>(
    sleigh: &'a sleigh_rs::Sleigh,
) -> impl Iterator<Item = TokenParser> + 'a {
    let token_lens: HashSet<NonZeroTypeU> =
        sleigh.tokens().map(|token| token.size).collect();
    token_lens
        .into_iter()
        .map(move |len| TokenParser::new(sleigh, len))
}
#[derive(Debug, Clone)]
pub struct TokenParser<'a> {
    //struct name
    name: Ident,
    //new function
    new_func: Ident,
    //internal value len in bytes
    token_bytes: NonZeroTypeU,
    //assembly fields that this parser can produce
    fields: HashMap<*const sleigh_rs::Assembly, Rc<TokenField<'a>>>,
}
impl<'a> TokenParser<'a> {
    pub fn new(
        sleigh: &'a sleigh_rs::Sleigh,
        token_bits: NonZeroTypeU,
    ) -> Self {
        //Tokens need to be multiple of 8, if not, this is an sleigh_rs
        //logic/parsing error
        assert!(token_bits.get() % 8 == 0);
        assert!(token_bits.get() / 8 != 0);
        //list of fields that this token is able to produce
        let fields = sleigh
            .token_fields()
            .filter(|ass| {
                //only if this field is smaller then the token that we read
                if let Some(field) = ass.field() {
                    field.token.size <= token_bits
                } else {
                    false
                }
            })
            .map(|ass| {
                let ptr = Rc::as_ptr(&ass);
                let token = TokenField::new(&ass);
                (ptr, token)
            })
            .collect();
        let name = format_ident!("TokenParser{}", token_bits.get());
        let token_bytes = NonZeroTypeU::new(token_bits.get() / 8).unwrap();
        Self {
            name,
            fields,
            token_bytes,
            new_func: format_ident!("new"),
        }
    }
    pub fn token_len(&self) -> NonZeroTypeU {
        self.token_bytes
    }
    pub fn name(&self) -> &Ident {
        &self.name
    }
    pub fn creator(&self) -> &Ident {
        &self.new_func
    }
    pub fn field(&self, assembly: *const Assembly) -> &Rc<TokenField> {
        self.fields.get(&assembly).as_ref().unwrap()
    }
    pub fn gen_read_call(
        &self,
        token_parser_instance: &Ident,
        ass: &'a Assembly,
    ) -> (TokenStream, WorkType) {
        let token_field = self.field(ass);
        let token_type = token_field.return_type();
        let token_read_name = token_field.read();
        let call = quote! {
            #token_parser_instance.#token_read_name()
        };
        (call, token_type)
    }
    pub fn gen_function(&self, token: &TokenField) -> TokenStream {
        let name = &token.read_func;
        let field = token.sleigh.field().unwrap();
        let len = field.bit_range.end - field.bit_range.start;
        let bit_start = field.bit_range.start;
        let bit_mask = (1u128 << len) - 1;
        let work_type = WorkType::unsigned_from_bytes(
            self.token_bytes.get().try_into().unwrap(),
        );
        let work_len = work_type.len_bytes();
        let read_bytes_start =
            work_len - usize::try_from(self.token_bytes.get()).unwrap();
        let return_type =
            WorkType::from_bits(len.try_into().unwrap(), field.signed);
        let from_xx_bytes = match &field.token.endian {
            sleigh_rs::semantic::Endian::Little => quote! {from_le_bytes},
            sleigh_rs::semantic::Endian::Big => quote! {from_be_bytes},
        };
        let value_endian = if read_bytes_start == 0 {
            quote! {
                #work_type::#from_xx_bytes(self.0)
            }
        } else {
            quote! { {
                let mut inner_raw = [0u8; #work_len];
                inner_raw[#read_bytes_start..].copy_from_slice(self.0);
                #work_type::#from_xx_bytes(inner_raw)
            } }
        };
        let value = format_ident!("raw_value");
        let solve_signed = if field.signed {
            let unsigned_mask = bit_mask >> 1;
            let sign_bit = unsigned_mask + 1;
            quote! {
                let unsigned = #value & #unsigned_mask as #work_type;
                //unsigned we extend the value with zeros
                //signed we extend the value with ones
                if #value & #sign_bit as #work_type != 0 {
                    (!#unsigned_mask as #work_type | unsigned) as #return_type
                } else {
                    unsigned as #return_type
                }
            }
        } else {
            quote! { #value as #return_type }
        };
        quote! {
            pub fn #name(&self) -> #return_type {
                let mut #value = #value_endian;
                #value >>= #bit_start as #work_type;
                #value &= #bit_mask as #work_type;
                #solve_signed
            }
        }
    }
    pub fn gen_struct_and_impl(&self) -> TokenStream {
        let name = &self.name;
        //NOTE internal value and read len are diferent, eg: token is 3 bytes
        //internal value could be a u32 (4bytes), but always value >= token
        let token_bytes = usize::try_from(self.token_bytes.get()).unwrap();
        let creator = self.creator();
        let fields = self.fields.values().map(|x| self.gen_function(x));
        //let from_bytes_function = if self.
        quote! {
            pub struct #name([u8; #token_bytes]);
            impl #name {
                pub fn #creator(data: &[u8]) -> Option<Self> {
                    <[u8; #token_bytes]>::try_from(data).ok().map(Self)
                }
                #(#fields)*
            }
        }
    }
}
