pub mod formater;

mod memory;
pub use memory::*;

mod token;
pub use token::*;

mod disassembly;
pub use disassembly::*;

mod disassembler;
pub use disassembler::*;

mod display;
pub use display::*;

mod register;
pub use register::*;

mod globalset;
pub use globalset::*;

mod meaning;
pub use meaning::*;

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use sleigh_rs::semantic::varnode::{Context, VarnodeType};

#[derive(Clone, Copy, Debug)]
pub enum WorkType {
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    U128,
    I128,
    Array(usize, bool),
}

impl WorkType {
    pub fn from_bits(bits: usize, signed: bool) -> Self {
        match bits {
            0 => unreachable!(),
            1..=8 if signed => Self::I8,
            1..=8 /*if !signed*/ => Self::U8,
            9..=16 if signed => Self::I16,
            9..=16 /*if !signed*/ => Self::U16,
            17..=32 if signed => Self::I32,
            17..=32 /*if !signed*/ => Self::U32,
            33..=64 if signed => Self::I64,
            33..=64 /*if !signed*/ => Self::U64,
            65..=128 if signed => Self::I128,
            65..=128 /*if !signed*/ => Self::U128,
            //x => Self::Array(x),
            _x => todo!(),
        }
    }
    pub fn unsigned_from_bits(bits: usize) -> Self {
        Self::from_bits(bits, false)
    }
    pub fn from_ass(ass: &sleigh_rs::Assembly) -> Self {
        let field = ass.field().unwrap();
        Self::unsigned_from_bits(
            (field.bit_range.end - field.bit_range.start)
                .try_into()
                .unwrap(),
        )
    }
    pub fn from_varnode(varnode: &sleigh_rs::Varnode) -> Self {
        match &varnode.varnode_type {
            VarnodeType::Memory(memory) => {
                let byte_len: usize = memory.size.get().try_into().unwrap();
                Self::unsigned_from_bits(byte_len * 8)
            }
            VarnodeType::BitRange(bitrange) => {
                let bit_len: usize =
                    bitrange.range.n_bits.get().try_into().unwrap();
                Self::from_bits(bit_len, false)
            }
            VarnodeType::Context(Context {
                bitrange, signed, ..
            }) => {
                let bit_len: usize =
                    bitrange.range.n_bits.get().try_into().unwrap();
                Self::from_bits(bit_len, *signed)
            }
        }
    }
    pub fn len_bytes(&self) -> usize {
        match self {
            WorkType::I8 | WorkType::U8 => (u8::BITS / 8) as usize,
            WorkType::I16 | WorkType::U16 => (u16::BITS / 8) as usize,
            WorkType::I32 | WorkType::U32 => (u32::BITS / 8) as usize,
            WorkType::I64 | WorkType::U64 => (u64::BITS / 8) as usize,
            WorkType::I128 | WorkType::U128 => (u128::BITS / 8) as usize,
            WorkType::Array(x, _signed) => *x,
        }
    }
}
impl ToTokens for WorkType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            WorkType::U8 => tokens.extend(quote! {u8}),
            WorkType::I8 => tokens.extend(quote! {i8}),
            WorkType::U16 => tokens.extend(quote! {u16}),
            WorkType::I16 => tokens.extend(quote! {i16}),
            WorkType::U32 => tokens.extend(quote! {u32}),
            WorkType::I32 => tokens.extend(quote! {i32}),
            WorkType::U64 => tokens.extend(quote! {u64}),
            WorkType::I64 => tokens.extend(quote! {i64}),
            WorkType::U128 => tokens.extend(quote! {u128}),
            WorkType::I128 => tokens.extend(quote! {i128}),
            WorkType::Array(x, _signed) => tokens.extend(quote! {[u8; #x]}),
        }
    }
}

const SLEIGH_IDENT: &str = "sleigh";
//pub struct Emulator<'a> {
//    sleigh: &'a Sleigh,
//    memory: Memory,
//    function: Function,
//}
//
//impl<'a> Emulator<'a> {
//    pub fn new(sleigh: &'a Sleigh) -> Self {
//        let memory = Memory::new(sleigh);
//        let function = Function::new(sleigh);
//        Self {
//            sleigh,
//            memory,
//            function,
//        }
//    }
//    pub fn memory_struct(&self) -> TokenStream {
//        self.memory.gen_memory_struct()
//    }
//    pub fn memory_trait(&self) -> TokenStream {
//        self.memory.gen_memory_trait()
//    }
//    pub fn context_struct(&self) -> TokenStream {
//        todo!()
//    }
//    pub fn context_trait(&self) -> TokenStream {
//        todo!()
//    }
//    pub fn memory_impl_trait(&self) -> TokenStream {
//        self.memory.struct_impl_trait()
//    }
//
//    //pub fn impl_memory_addr_function(&mut self) -> TokenStream {
//    //    let mut memory_addr_function = HashMap::new();
//
//    //    let data = format_ident!("data");
//    //    let addr = format_ident!("addr");
//    //    let read = format_ident!("read");
//    //    let funs = self
//    //        .sleigh
//    //        .global_scope
//    //        .values()
//    //        .filter_map(|x| x.unwrap_space())
//    //        .map(|space| {
//    //            match space.space_type {
//    //                SpaceType::Const => quote!{},
//    //                SpaceType::Unique => quote!{},
//    //                SpaceType::Rom(_) => {
//    //            let chunks = self
//    //                .space_struct()
//    //                .chunks.iter()
//    //                .filter(|(chunk_space, _, _)| chunk_space.name == space.name)
//    //                .map(|(_space, ident, range)| {
//    //                    let chunk_start = range.start;
//    //                    let chunk_end = range.end - 1;
//    //                    quote!{
//    //                        #chunk_start..=#chunk_end => {
//    //                            if !#read {
//    //                                unreachable!("Rom can't be written")
//    //                            }
//    //                            let start = #addr - #chunk_start;
//    //                            let end = start + #data.len();
//    //                            #data.copy_from_slice(&self.#ident[start..end]);
//    //                        }
//    //                    }
//    //            });
//    //            let fn_name = format_ident!("rw_{}_addr", *space.name);
//    //            memory_addr_function
//    //                .insert(Rc::clone(&space.name), fn_name.clone());
//    //            quote! {
//    //                pub fn #fn_name(&mut self, #addr: usize, #data: &mut [u8]) {
//    //                    match #addr {
//    //                        #(#chunks),*
//    //                        _ => unreachable!(),
//    //                    }
//    //                }
//    //            }
//    //                },
//    //                SpaceType::Ram(_) |
//    //                SpaceType::Register(_) => {
//    //            let chunks = self
//    //                .space_struct()
//    //                .chunks.iter()
//    //                .filter(|(chunk_space, _, _)| chunk_space.name == space.name)
//    //                .map(|(_space, ident, range)| {
//
//    //                    let chunk_start = range.start;
//    //                    let chunk_end = range.end - 1;
//    //                    quote!{
//    //                        #chunk_start..=#chunk_end => {
//    //                            let start = #addr - #chunk_start;
//    //                            let end = start + #data.len();
//    //                            if read {
//    //                                #data.copy_from_slice(&self.#ident[start..end]);
//    //                            } else {
//    //                                self.#ident[start..end].copy_from_slice(#data);
//    //                            }
//    //                        }
//    //                    }
//    //            });
//    //            let fn_name = format_ident!("rw_{}_addr", *space.name);
//    //            memory_addr_function
//    //                .insert(Rc::clone(&space.name), fn_name.clone());
//    //            quote! {
//    //                pub fn #fn_name(&mut self, #read:bool, #addr: usize, #data: &mut [u8]) {
//    //                    match #addr {
//    //                        #(#chunks),*
//    //                        _ => unreachable!(),
//    //                    }
//    //                }
//    //            }
//    //                }
//    //            }
//    //        });
//    //    let memory_struct = &self.space_struct().ident;
//    //    quote! {
//    //        impl #memory_struct {
//    //            #(#funs)*
//    //        }
//    //    }
//    //}
//    pub fn impl_varnode_function(&mut self) -> TokenStream {
//        todo!();
//        //let mut varnode_functions = HashMap::new();
//        //let funs = self.sleigh
//        //.global_scope
//        //.values()
//        //.filter_map(|x| x.unwrap_varnode())
//        //.map(|varnode| {
//        //    let fun_read_name = format_ident!("varnode_read_{}", *varnode.name);
//        //    let fun_write_name = format_ident!("varnode_write_{}", *varnode.name);
//        //    varnode_functions.insert(
//        //        Rc::clone(&varnode.name),
//        //        VarnodeFunction {
//        //            read: fun_read_name.clone(),
//        //            write: fun_write_name.clone(),
//        //        },
//        //    );
//        //    //TODO impl only read function if spacetype is Rom
//        //    let (chunk_name, chunk_offset) = self
//        //        .memory_struct
//        //        .as_ref()
//        //        .unwrap()
//        //        .chunk_from_memory(varnode.memory())
//        //        .unwrap();
//        //    match &varnode.varnode_type {
//        //        VarnodeType::Memory(memory) => {
//        //            let type_bytes: usize = memory.size.get().try_into().unwrap();
//        //            let addr: usize = memory.offset.try_into().unwrap();
//        //            let chunk_start: usize = addr - chunk_offset;
//        //            let chunk_end: usize = chunk_start + type_bytes;
//        //            quote! {
//        //                fn #fun_read_name(&self) -> [u8; #type_bytes] {
//        //                  let mut data = [0u8; #type_bytes];
//        //                  data.copy_from_slice(&self.#chunk_name[#chunk_start..#chunk_end]);
//        //                  data
//        //                }
//        //                fn #fun_write_name(&mut self, data: [u8; #type_bytes]) {
//        //                  self.#chunk_name[#chunk_start..#chunk_end].copy_from_slice(&data);
//        //                }
//        //            }
//        //        }
//        //        VarnodeType::BitRange(bitrange)
//        //            | VarnodeType::Context(Context{bitrange, ..}) => {
//        //                let bit_start = bitrange.range.lsb_bit % 8;
//        //                let bit_len: usize = bitrange.range.n_bits.get().try_into().unwrap();
//        //                let bit_mask: usize = (1 << bit_len) - 1;
//
//        //                let byte_offset: usize = usize::try_from(bitrange.range.lsb_bit).unwrap() / 8;
//        //                let byte_start: usize = chunk_offset + byte_offset;
//        //                let byte_len: usize = ((bit_len - 1) / 8) + 1;
//        //                let byte_end: usize = byte_start + byte_len;
//
//        //                let (work_len, work_type) = match bit_len {
//        //                    0 => unreachable!(),
//        //                    1..=8 => (u8::BITS/8, format_ident!("u8")),
//        //                    0..=16 => (u16::BITS/8, format_ident!("u16")),
//        //                    17..=32 => (u32::BITS/8, format_ident!("u32")),
//        //                    33..=64 => (u64::BITS/8, format_ident!("u64")),
//        //                    69..=128 => (u128::BITS/8, format_ident!("u128")),
//        //                    x => todo!("bitrange size {}", x),
//        //                };
//        //                let work_len = work_len as usize;
//
//        //                quote! {
//        //                    fn #fun_read_name(&self) -> [u8; #byte_len] {
//        //                        let mut tmp = [0u8; #work_len];
//        //                        tmp[0..#byte_len].copy_from_slice(
//        //                            &self.#chunk_name[#byte_start..#byte_end]
//        //                        );
//        //                        let mut tmp = #work_type::from_be_bytes(tmp);
//        //                        tmp >>= #bit_start;
//        //                        tmp &= #bit_mask as #work_type;
//        //                        tmp.to_be_bytes()
//        //                    }
//        //                    fn #fun_write_name(&mut self, value: [u8; #byte_len]) {
//        //                        let mut tmp = [0u8; #work_len];
//        //                        tmp[0..#byte_len].copy_from_slice(&value);
//        //                        let mut tmp = #work_type::from_be_bytes(tmp);
//        //                        tmp &= #bit_mask as #work_type;
//        //                        tmp <<= #bit_start;
//
//        //                        let mut raw_data = [0u8; #work_len];
//        //                        raw_data[0..#byte_len].copy_from_slice(
//        //                            &self.#chunk_name[#byte_start..#byte_end]
//        //                        );
//        //                        let mut raw_data = #work_type::from_be_bytes(raw_data);
//        //                        raw_data &= !((#bit_mask as #work_type) << #bit_start);
//
//        //                        let result = (raw_data | tmp).to_be_bytes();
//        //                        self.#chunk_name[#byte_start..#byte_end].copy_from_slice(
//        //                            &result[0..#byte_len]
//        //                        );
//        //                    }
//        //                }
//        //        },
//        //    }
//        //});
//        //let memory_struct = &self.space_struct().ident;
//        //let out = quote! {
//        //    impl #memory_struct {
//        //        #(#funs)*
//        //    }
//        //};
//        //self.varnode_functions = Some(varnode_functions);
//        //out
//    }
//    pub fn impl_user_function(&mut self) -> TokenStream {
//        todo!()
//        //let mut user_functions = HashMap::new();
//        //let user_functions = self
//        //    .sleigh
//        //    .global_scope
//        //    .values()
//        //    .filter_map(|x| x.unwrap_pcode_macro())
//        //    .map(|user_function| {
//        //        let function_name = user_functions
//        //            .entry(Rc::clone(&user_function.name))
//        //            .or_insert(format_ident!(
//        //                "user_function_{}",
//        //                *user_function.name
//        //            ));
//        //        quote! {
//        //            fn #function_name(&self) {
//        //                todo!()
//        //            }
//        //        }
//        //    });
//        //let memory_struct = &self.space_struct().ident;
//        //quote! {
//        //    impl #memory_struct {
//        //        #(#user_functions)*
//        //    }
//        //}
//    }
//
//    pub fn impl_pcode_macro_function(&mut self) -> TokenStream {
//        todo!();
//        //let pcode_macros = self
//        //    .sleigh
//        //    .global_scope
//        //    .values()
//        //    .filter_map(|x| x.unwrap_pcode_macro())
//        //    .map(|pcode_macro| function::do_the_thing(self, &pcode_macro));
//        //quote! {
//        //    #(#pcode_macros)*
//        //}
//    }
//
//    //pub fn impl_space_functions(&mut self) -> TokenStream {
//    //    let addr_fun = self.space_function_addr();
//    //    let varnode_read_write = self.varnodes_function_access();
//    //    let user_functions = self.functions_user_function();
//    //    let pcode_macros = self.functions_pcode_macro();
//
//    //    quote! {
//    //        impl AddressSpaces {
//    //            #addr_fun
//    //            #varnode_read_write
//    //            #user_functions
//    //            #pcode_macros
//    //        }
//    //    }
//    //}
//}
