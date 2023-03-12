use indexmap::IndexMap;
use std::rc::Rc;

use proc_macro2::{Ident, TokenStream};
use quote::ToTokens;
use quote::{format_ident, quote};
use sleigh4rust::NonZeroTypeU;
use sleigh_rs::{GlobalAnonReference, GlobalElement};

use crate::builder::formater::*;
use crate::builder::WorkType;

use super::{
    DisplayElement, Meaning, MeaningExecutionType, Meanings, ToLiteral,
    DISASSEMBLY_WORK_TYPE,
};

#[derive(Debug, Clone)]
pub struct TokenFieldStruct {
    pub struct_name: Ident,
    inner_type: WorkType,
    pub execution_type: MeaningExecutionType,
    pub execution_fun: Ident,
    pub disassembly_fun: Ident,
    pub display_fun: Ident,
    display: Rc<DisplayElement>,
    meanings: Rc<Meanings>,
    pub sleigh: GlobalAnonReference<sleigh_rs::TokenField>,
}
impl TokenFieldStruct {
    pub fn new(
        display: Rc<DisplayElement>,
        meanings: Rc<Meanings>,
        token_field: &GlobalElement<sleigh_rs::TokenField>,
    ) -> Self {
        let name = from_sleigh(&token_field.name());
        let meaning = meanings.from_sleigh(token_field.meaning());
        //TODO this calculation is done again in `BitrangeFromMemory` for
        //`return_type`, deduplicate it
        //how the token field will be stored on the struct, u8, u16, i16, etc...
        let inner_type = WorkType::from_token_field(&token_field);
        //this can be a value or register during the execution of it
        let execution_type = meaning
            .execution_type()
            .unwrap_or(MeaningExecutionType::Value(inner_type));
        Self {
            execution_fun: format_ident!("execution"),
            disassembly_fun: format_ident!("disassembly"),
            display_fun: format_ident!("display"),
            display,
            sleigh: token_field.reference(),
            struct_name: format_ident!("TokenField_{}", name),
            inner_type,
            meanings,
            execution_type,
        }
    }
}

impl ToTokens for TokenFieldStruct {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            struct_name: type_name,
            inner_type,
            execution_type,
            execution_fun,
            disassembly_fun,
            display_fun,
            display,
            meanings,
            sleigh,
        } = self;
        let display_type = display.name();
        let sleigh_token_field = sleigh.element();
        let sleigh_meaning = sleigh_token_field.meaning();
        let meaning = meanings.from_sleigh(sleigh_meaning);
        let execution_body = match &meaning {
            Meaning::Literal(_) | Meaning::Names(_) => quote! { self.0 },
            Meaning::Values(_, values) => {
                let meaning_function = &values.value_func;
                quote! {
                    #meaning_function(self.0)
                }
            }
            Meaning::Variables(vars) => {
                let meaning_function = &vars.value_func;
                quote! {
                    #meaning_function(self.0)
                }
            }
        };
        let disassembly_body = match &meaning {
            Meaning::Variables(_) | Meaning::Literal(_) | Meaning::Names(_) => {
                quote! {
                    #DISASSEMBLY_WORK_TYPE::try_from(self.0).unwrap()
                }
            }
            Meaning::Values(_, _) => {
                quote! {
                    #DISASSEMBLY_WORK_TYPE::try_from(self.#execution_fun()).unwrap()
                }
            }
        };
        let display_body =
            meanings.display_function_call(quote! {self.0}, sleigh_meaning);
        tokens.extend(quote! {
            #[derive(Clone, Copy, Debug)]
            struct #type_name(#inner_type);
            impl #type_name {
                fn #execution_fun(&self) -> #execution_type {
                    #execution_body
                }
                fn #disassembly_fun(&self) -> #DISASSEMBLY_WORK_TYPE {
                    #disassembly_body
                }
                fn #display_fun(&self) -> #display_type {
                    #display_body
                }
            }
        });
    }
}

//TODO doc is outdated
//Token parser will be based on len, any assembly field that is smaller then the
//len of the token can be read.
//eg: assembly `field_a` is part of the token `token_a`, that is 2bytes.
//    assembly `field_b` is part of the token `token_b`, that is 4bytes.
//It will be created a TokenParser for `token_a` and `token_b`,
//`token_a` work_type will be a [u8; 2], and will only produce `field_a`.
//`token_b` work_type will be a [u8; 4], and will produce both fields.
#[derive(Debug, Clone)]
pub struct TokenParser {
    //struct name
    pub name: Ident,
    //new function
    new_func: Ident,
    ////len in bytes
    //pub token_bytes: NonZeroTypeU,
    //token_field that this parser can produce
    fields:
        IndexMap<*const sleigh_rs::TokenField, (Ident, Rc<TokenFieldStruct>)>,
}
impl TokenParser {
    pub fn new(
        sleigh: &sleigh_rs::Sleigh,
        display: &Rc<DisplayElement>,
        meanings: &Rc<Meanings>,
    ) -> Self {
        //list of fields that this token is able to produce
        let fields = sleigh
            .token_fields()
            .map(|ass| {
                let ptr = ass.element_ptr();
                let name =
                    format_ident!("TokenField{}", from_sleigh(ass.name()));
                let token = Rc::new(TokenFieldStruct::new(
                    Rc::clone(display),
                    Rc::clone(meanings),
                    &ass,
                ));
                (ptr, (name, token))
            })
            .collect();
        Self {
            name: format_ident!("TokenParser"),
            fields,
            new_func: format_ident!("new"),
        }
    }
    pub fn token_field_struct(
        &self,
        token_field: *const sleigh_rs::TokenField,
    ) -> &Rc<TokenFieldStruct> {
        self.fields.get(&token_field).map(|(_, x)| x).unwrap()
    }
    pub fn name(&self) -> &Ident {
        &self.name
    }
    pub fn gen_new_call(
        &self,
        token_bytes: NonZeroTypeU,
        data: impl ToTokens,
    ) -> TokenStream {
        let struct_name = &self.name;
        let new_function = &self.new_func;
        let token_bytes = usize::try_from(token_bytes.get()).unwrap();
        quote! {
            <#struct_name<#token_bytes>>::#new_function(
                #data
            )
        }
    }
    pub fn gen_read(
        &self,
        token_parser_instance: impl ToTokens,
        assembly: *const sleigh_rs::TokenField,
    ) -> (TokenStream, &TokenFieldStruct) {
        let (token_name, token_field) = self.fields.get(&assembly).unwrap();
        let call = quote! {
            #token_parser_instance.#token_name()
        };
        (call, token_field)
    }
    pub fn gen_disassembly_read_call(
        &self,
        token_parser_instance: impl ToTokens,
        assembly: *const sleigh_rs::TokenField,
    ) -> TokenStream {
        let (call, token_field) =
            self.gen_read(token_parser_instance, assembly);
        let disassembly_read = &token_field.disassembly_fun;
        quote! {
            #call.#disassembly_read()
        }
    }
    //pub fn gen_disassembly_read_call(
    //    &self,
    //    token_parser_instance: impl ToTokens,
    //    assembly: *const sleigh_rs::TokenField,
    //) -> TokenStream {
    //    let token_field = self.fields.get(&assembly);
    //    let token_field = token_field.as_ref().unwrap();
    //    let token_read_name = token_field.read_disassembly();
    //    let call = quote! {
    //        #token_parser_instance.#token_read_name()
    //    };
    //    call
    //}
}

impl<'a> ToTokens for TokenParser {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = &self.name;
        //NOTE internal value and read len are diferent, eg: token is 3 bytes
        //internal value could be a u32 (4bytes), but always value >= token
        let creator = &self.new_func;
        let fields_structs = self.fields.values().map(|(_, x)| Rc::as_ref(x));
        let fields = self.fields.values().map(|(name, token_field_struct)| {
            let token_field_struct_name = &token_field_struct.struct_name;
            let token_field = token_field_struct.sleigh.element();
            let range = token_field.range();
            let signed = token_field.meaning().is_signed();
            let big_endian = token_field.token().endian().is_big();
            let param_type = WorkType::new_int_bits(range.len(), signed);
            let len_bits = range.len().get().unsuffixed();
            let (data_addr, data_lsb) = sleigh4rust::bytes_from_varnode(
                big_endian,
                0,
                token_field.token().len_bytes.get(),
                range.start(),
                range.len().get(),
            );
            let work_type = WorkType::new_int_bits(
                NonZeroTypeU::new(data_lsb + range.len().get()).unwrap(),
                signed,
            );
            let read_function = work_type.sleigh4rust_read_memory();
            let data_addr = data_addr.unsuffixed();
            let data_lsb = data_lsb.unsuffixed();
            quote! {
                fn #name(
                    &self,
                ) -> #token_field_struct_name {
                    let inner_value = self.#read_function::<#big_endian>(
                        #data_addr,
                        #data_lsb,
                        #len_bits,
                    ).unwrap();
                    #token_field_struct_name(
                        #param_type::try_from(inner_value).unwrap()
                    )
                }
            }
        });
        tokens.extend(quote! {
            #(#fields_structs)*
            struct #name<const LEN: usize>([u8; LEN]);
            impl<const LEN: usize> MemoryRead for TokenParser<LEN>{
                type AddressType = usize;
                fn read(
                    &self,
                    addr: Self::AddressType,
                    buf: &mut [u8],
                ) -> Result<(), MemoryReadError<Self::AddressType>> {
                    let end = addr + buf.len();
                    self.0
                        .get(addr..end)
                        .map(|src| buf.copy_from_slice(src))
                        .ok_or(MemoryReadError::UnableToReadMemory(addr, end))
                }
            }
            impl<const LEN: usize> #name<LEN> {
                fn #creator(data: &[u8]) -> Option<Self> {
                    let token_slice: &[u8] = data.get(..LEN)?;
                    let token_data = <[u8; LEN]>::try_from(token_slice).unwrap();
                    Some(Self(token_data))
                }
                #(#fields)*
            }
        })
    }
}
