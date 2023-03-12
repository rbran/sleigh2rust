use std::rc::Rc;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use sleigh_rs::{GlobalAnonReference, GlobalElement};

use crate::builder::formater::*;
use crate::builder::{ToLiteral, WorkType};

use super::{chunks_from_varnodes, ChunkBytes, MemoryChunk, SpaceTrait};

#[derive(Debug, Clone)]
pub struct SpaceStructRelease {
    //struct name
    pub name: Ident,
    //function to read from arbitrary address
    //read: Ident,
    //function to write to arbitrary address
    //write: Option<Ident>,
    pub space: GlobalAnonReference<sleigh_rs::Space>,
    pub space_trait: Rc<SpaceTrait>,
    pub chunks: Vec<MemoryChunk>,
}

impl SpaceStructRelease {
    pub fn new<I>(
        space: &GlobalElement<sleigh_rs::Space>,
        space_trait: Rc<SpaceTrait>,
        varnodes: I,
    ) -> Self
    where
        I: Iterator<Item = ChunkBytes>,
    {
        let name = format_ident!("Context{}Struct", from_sleigh(space.name()));
        let chunks = chunks_from_varnodes(varnodes);
        Self {
            name,
            space: space.reference(),
            space_trait,
            chunks,
        }
    }
    fn gen_read_fun_impl(&self) -> TokenStream {
        let space = self.space.element();
        let struct_name = &self.name;
        let buf_len = format_ident!("buf_len");

        let addr_type = WorkType::new_int_bytes(space.addr_bytes(), false);
        let addr_param = format_ident!("addr");
        let buf_param = format_ident!("buf");
        let addr_end = format_ident!("addr_end");
        let chunks = self.chunks.iter().map(|chunk| {
            let chunk_start = chunk.addr_start().unsuffixed();
            let chunk_end_excl =
                chunk.addr_end().checked_sub(1).unwrap().unsuffixed();
            let chunk_end = chunk.addr_end().unsuffixed();
            let chunk_name = chunk.ident();
            quote! {
                (#chunk_start..=#chunk_end_excl, #chunk_start..=#chunk_end) => {
                    let start = #addr_param - #chunk_start;
                    let end = usize::try_from(start + #buf_len).unwrap();
                    let start = usize::try_from(start).unwrap();
                    #buf_param.copy_from_slice(&self.#chunk_name[start..end]);
                }
            }
        });
        quote! {
            impl MemoryRead for #struct_name {
                type AddressType = #addr_type;
                fn read(
                    &self,
                    #addr_param: Self::AddressType,
                    #buf_param: &mut [u8],
                )-> Result<(), MemoryReadError<Self::AddressType>> {
                    let #buf_len =
                        <Self::AddressType>::try_from(#buf_param.len()).unwrap();
                    let #addr_end = #addr_param + #buf_len;
                    match (#addr_param, #addr_end) {
                        #(#chunks),*
                        (addr_start, addr_end) => {
                            return Err(MemoryReadError::UnableToReadMemory(
                                addr_start, addr_end,
                            ))
                        }
                    }
                    Ok(())
                }
            }
        }
    }
    fn gen_write_fun_impl(&self) -> Option<TokenStream> {
        let space = self.space.element();
        if !space.can_write() {
            return None;
        }
        let struct_name = &self.name;
        let buf_len = format_ident!("buf_len");

        let addr_param = format_ident!("addr");
        let buf_param = format_ident!("buf");
        let addr_end = format_ident!("addr_end");
        let chunks = self.chunks.iter().map(|chunk| {
            let chunk_start = chunk.addr_start().unsuffixed();
            let chunk_end = chunk.addr_end().unsuffixed();
            let chunk_end_excl =
                chunk.addr_end().checked_sub(1).unwrap().unsuffixed();
            let chunk_name = chunk.ident();
            quote! {
                (#chunk_start..=#chunk_end_excl, #chunk_start..=#chunk_end) => {
                    let start = #addr_param - #chunk_start;
                    let end = usize::try_from(start + #buf_len).unwrap();
                    let start = usize::try_from(start).unwrap();
                    self.#chunk_name[start..end].copy_from_slice(#buf_param);
                }
            }
        });
        Some(quote! {
            impl MemoryWrite for #struct_name {
                fn write(
                    &mut self,
                    #addr_param: Self::AddressType,
                    #buf_param: &[u8],
                ) -> Result<(), MemoryWriteError<Self::AddressType>> {
                    let #buf_len =
                        <Self::AddressType>::try_from(#buf_param.len()).unwrap();
                    let #addr_end = #addr_param + #buf_len;
                    match (#addr_param, #addr_end) {
                        #(#chunks),*
                        (addr_start, addr_end) => {
                            return Err(MemoryWriteError::UnableToWriteMemory(
                                addr_start, addr_end,
                            ))
                        }
                    }
                    Ok(())
                }
            }
        })
    }
}
impl ToTokens for SpaceStructRelease {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = &self.name;
        let chunks = self.chunks.iter().map(MemoryChunk::struct_chunk);
        let impl_read = self.gen_read_fun_impl();
        let impl_write = self.gen_write_fun_impl();
        let space_trait_name = &self.space_trait.name;
        tokens.extend(quote! {
            #[derive(Debug, Clone, Copy, Default)]
            pub struct #ident {
                #(#chunks),*
            }
            impl #space_trait_name for #ident {}
            #impl_read
            #impl_write
        })
    }
}
