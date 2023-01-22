use std::rc::Rc;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use sleigh_rs::semantic::{GlobalAnonReference, GlobalElement};

use crate::builder::{formater::*, ToLiteral};

use super::{
    chunks_from_varnodes, ChunkBytes, MemoryChunk, SpaceTrait, WorkType,
};

#[derive(Debug, Clone)]
pub struct SpaceStructDebug {
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
impl SpaceStructDebug {
    pub fn new(
        space: &GlobalElement<sleigh_rs::Space>,
        space_trait: Rc<SpaceTrait>,
        varnodes: impl Iterator<Item = ChunkBytes>,
    ) -> Rc<Self> {
        let name =
            format_ident!("Context{}StructDebug", from_sleigh(space.name()));
        let chunks = chunks_from_varnodes(varnodes);
        Rc::new(Self {
            name,
            space: space.reference(),
            space_trait,
            chunks,
        })
    }
    fn gen_read_bits_fun_impl(&self) -> TokenStream {
        let buf_len = format_ident!("buf_len");

        let addr_param = format_ident!("addr");
        let buf_param = format_ident!("buf");
        let mask_param = format_ident!("mask");
        let addr_end = format_ident!("addr_end");
        let chunks = self.chunks.iter().map(|chunk| {
            let chunk_start = chunk.addr_start().unsuffixed();
            let chunk_end_excl =
                chunk.addr_end().checked_sub(1).unwrap().unsuffixed();
            let chunk_end = chunk.addr_end().unsuffixed();
            let chunk_name = chunk.ident();
            quote! {
                (#chunk_start..=#chunk_end_excl, #chunk_start..=#chunk_end) => {
                    let bit_offset = usize::try_from(#addr_param - #chunk_start).unwrap() * 8;
                    for ((buf_byte, mask_byte), chunk_index) in
                        #buf_param.iter_mut().zip(#mask_param.iter()).zip(bit_offset..)
                    {
                        for bit in (0..8)
                            .into_iter()
                            .filter(|bit| ((*mask_byte >> bit) & 1) != 0)
                        {
                            *buf_byte |= (self.#chunk_name[chunk_index + bit].unwrap()
                                as u8)
                                << bit;
                        }
                    }
                }
            }
        });
        quote! {
            fn read_bits(
                &self,
                #addr_param: <Self as MemoryRead>::AddressType,
                #buf_param: &mut [u8],
                #mask_param: &[u8],
            ) -> Result<(), MemoryReadError<<Self as MemoryRead>::AddressType>> {
                assert_eq!(buf.len(), mask.len());
                let #buf_len =
                    <<Self as MemoryRead>::AddressType>::try_from(buf.len()).unwrap();
                let #addr_end = #addr_param + ((#buf_len + 7) / 8);
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
    fn gen_write_bits_fun_impl(&self) -> Option<TokenStream> {
        let space = self.space.element();
        if !space.can_write() {
            return None;
        }
        let buf_len = format_ident!("buf_len");

        let addr_param = format_ident!("addr");
        let buf_param = format_ident!("buf");
        let mask_param = format_ident!("mask");
        let addr_end = format_ident!("addr_end");
        let chunks = self.chunks.iter().map(|chunk| {
            let chunk_start = chunk.addr_start().unsuffixed();
            let chunk_end = chunk.addr_end().unsuffixed();
            let chunk_end_excl =
                chunk.addr_end().checked_sub(1).unwrap().unsuffixed();
            let chunk_name = chunk.ident();
            quote! {
                (#chunk_start..=#chunk_end_excl, #chunk_start..=#chunk_end) => {
                    let bit_offset = usize::try_from(#addr_param - #chunk_start).unwrap() * 8;
                    for ((buf_byte, mask_byte), chunk_index) in
                        #buf_param.iter().zip(#mask_param.iter()).zip(bit_offset..)
                    {
                        for bit in (0..8)
                            .into_iter()
                            .filter(|bit| ((*mask_byte >> bit) & 1) != 0)
                        {
                            self.#chunk_name[chunk_index + bit] =
                                Some(*buf_byte & (1 << bit) != 0);
                        }
                    }
                }
            }
        });
        Some(quote! {
            fn write_bits(
                &mut self,
                #addr_param: <Self as MemoryRead>::AddressType,
                #buf_param: &[u8],
                #mask_param: &[u8],
            ) -> Result<(), MemoryWriteError<<Self as MemoryRead>::AddressType>> {
                assert_eq!(buf.len(), mask.len());
                let #buf_len =
                    <<Self as MemoryRead>::AddressType>::try_from(#buf_param.len()).unwrap();
                let #addr_end = #addr_param + ((#buf_len + 7) / 8);
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
        })
    }
    fn gen_read_fun_impl(&self) -> TokenStream {
        let space = self.space.element();
        let struct_name = &self.name;
        let addr_type = WorkType::new_int_bytes(space.addr_bytes(), false);
        let read_type_impls = [
            ("u8", "read_u8"),
            ("u16", "read_u16"),
            ("u32", "read_u32"),
            ("u64", "read_u64"),
            ("u128", "read_u128"),
        ]
        .iter()
        .map(|(int_type, fun_name)| {
            (format_ident!("{}", int_type), format_ident!("{}", fun_name))
        })
        .map(|(int_type, fun_name)| gen_read_types_impl(&int_type, &fun_name));
        quote! {
            impl MemoryRead for #struct_name {
                type AddressType = #addr_type;
                fn read(
                    &self,
                    addr: <Self as MemoryRead>::AddressType,
                    buf: &mut [u8],
                )-> Result<(), MemoryReadError<<Self as MemoryRead>::AddressType>> {
                    //read all the bits
                    let mut inner_buf = vec![0xFF; buf.len()];
                    self.read_bits(addr, buf, &mut inner_buf)
                }
                #(#read_type_impls)*
            }
        }
    }
    fn gen_write_fun_impl(&self) -> Option<TokenStream> {
        let space = self.space.element();
        if !space.can_write() {
            return None;
        }
        let struct_name = &self.name;
        let write_type_impls = [
            ("u8", "write_u8"),
            ("u16", "write_u16"),
            ("u32", "write_u32"),
            ("u64", "write_u64"),
            ("u128", "write_u128"),
        ]
        .iter()
        .map(|(int_type, fun_name)| {
            (format_ident!("{}", int_type), format_ident!("{}", fun_name))
        })
        .map(|(int_type, fun_name)| gen_write_types_impl(&int_type, &fun_name));
        Some(quote! {
            impl MemoryWrite for #struct_name {
                fn write(
                    &mut self,
                    addr: <Self as MemoryRead>::AddressType,
                    buf: &[u8],
                ) -> Result<(), MemoryWriteError<<Self as MemoryRead>::AddressType>> {
                    let mut inner_buf = vec![0xFF; buf.len()];
                    self.write_bits(addr, buf, &inner_buf)
                }
                #(#write_type_impls)*
            }
        })
    }
}
impl ToTokens for SpaceStructDebug {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = &self.name;
        let chunks = self.chunks.iter().map(MemoryChunk::struct_chunk_debug);
        let chunks_default = self
            .chunks
            .iter()
            .map(MemoryChunk::struct_chunk_debug_default);
        let impl_read = self.gen_read_fun_impl();
        let impl_write = self.gen_write_fun_impl();
        let impl_read_bits = self.gen_read_bits_fun_impl();
        let impl_write_bits = self.gen_write_bits_fun_impl();
        let space_trait_name = &self.space_trait.name;
        tokens.extend(quote! {
            #[derive(Debug, Clone, Copy)]
            pub struct #ident {
                #(#chunks),*
            }
            impl Default for #ident {
                fn default() -> Self {
                    Self {
                        #(#chunks_default),*
                    }
                }
            }
            impl #ident {
                #impl_read_bits
                #impl_write_bits
            }
            impl #space_trait_name for #ident {}
            #impl_read
            #impl_write
        })
    }
}

fn gen_read_types_impl(int_type: &Ident, fun_name: &Ident) -> TokenStream {
    quote! {
        fn #fun_name<const BIG_ENDIAN: bool>(
            &self,
            data_addr: <Self as MemoryRead>::AddressType,
            varnode_lsb: usize,
            data_bits: usize,
        ) -> Result<#int_type, MemoryReadError<<Self as MemoryRead>::AddressType>> {
            const TYPE_BITS: usize = <#int_type>::BITS as usize;
            const TYPE_BYTES: usize = TYPE_BITS / 8;
            assert!(data_bits > 0);
            let data_lsb = varnode_lsb % 8;
            let read_bytes = (data_bits + data_lsb + 7) / 8;
            assert!(read_bytes <= TYPE_BYTES);

            let data_start = if BIG_ENDIAN {
                TYPE_BYTES - read_bytes
            } else {
                0
            };
            let data_end = data_start + read_bytes;
            let mut data = [0u8; TYPE_BYTES];
            let mask = (<#int_type>::MAX >> (TYPE_BITS - data_bits)) << data_lsb;
            let mask = if BIG_ENDIAN {
                mask.to_be_bytes()
            } else {
                mask.to_le_bytes()
            };
            self.read_bits(
                data_addr,
                &mut data[data_start..data_end],
                &mask[data_start..data_end],
            )?;
            let data = if BIG_ENDIAN {
                <#int_type>::from_be_bytes(data)
            } else {
                <#int_type>::from_le_bytes(data)
            };
            let value_mask = <#int_type>::MAX >> (TYPE_BITS - data_bits);
            Ok((data >> data_lsb) & value_mask)
        }
    }
}
fn gen_write_types_impl(int_type: &Ident, fun_name: &Ident) -> TokenStream {
    quote! {
        fn #fun_name<const BIG_ENDIAN: bool>(
            &mut self,
            value: #int_type,
            data_addr: <Self as MemoryRead>::AddressType,
            varnode_lsb: usize,
            data_bits: usize,
        ) -> Result<(), MemoryWriteError<<Self as MemoryRead>::AddressType>> {
            const TYPE_BITS: usize = <#int_type>::BITS as usize;
            const TYPE_BYTES: usize = TYPE_BITS / 8;
            assert!(data_bits > 0);
            let data_lsb = varnode_lsb % 8;
            let read_bytes = (data_bits + data_lsb + 7) / 8;
            assert!(read_bytes <= TYPE_BYTES);
            let mask = (<#int_type>::MAX >> (TYPE_BITS - data_bits)) << data_lsb;
            let mask_raw = if BIG_ENDIAN {
                mask.to_be_bytes()
            } else {
                mask.to_le_bytes()
            };

            let data_start = if BIG_ENDIAN {
                TYPE_BYTES - read_bytes
            } else {
                0
            };
            let data_end = data_start + read_bytes;
            let value = (value << data_lsb) & mask;
            let final_mem = if BIG_ENDIAN {
                value.to_be_bytes()
            } else {
                value.to_le_bytes()
            };
            self.write_bits(
                data_addr,
                &final_mem[data_start..data_end],
                &mask_raw[data_start..data_end],
            )
        }
    }
}
