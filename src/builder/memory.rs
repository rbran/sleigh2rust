use std::collections::HashMap;
use std::ops::Range;
use std::rc::Rc;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use sleigh_rs::semantic::varnode::VarnodeType;
use sleigh_rs::semantic::varnode::{Context, VarnodeField};
use sleigh_rs::semantic::Sleigh;

use crate::builder::formater::*;

use super::{WorkType, SLEIGH_IDENT};

fn bitrange_size(
    bitrange: &VarnodeField,
) -> (usize, usize, WorkType, usize, usize) {
    let bit_start = usize::try_from(bitrange.range.lsb_bit).unwrap();
    let bit_len = usize::try_from(bitrange.range.n_bits.get()).unwrap();
    let bit_end = bit_start + bit_len;
    let bit_mask = (1 << bit_len) - 1;
    let byte_start = bit_start / 8;
    let byte_end = (bit_end + 7) / 8;
    let byte_len = byte_end - byte_start;

    let memory = bitrange.varnode.memory();
    let addr = usize::try_from(memory.offset).unwrap() + byte_start;

    //NOTE don't use bit_len here, is work type, not return_type
    let work_type = WorkType::unsigned_from_bits(byte_len * 8);
    (addr, byte_len, work_type, bit_len, bit_mask)
}

#[derive(Debug, Clone)]
pub struct MemoryTraitSpace {
    read: Ident,
    //write function only Some if is not a Read-Only address space
    write: Option<Ident>,
    //sleigh: Rc<sleigh_rs::Space>,
}
impl MemoryTraitSpace {
    pub fn new(sleigh: &sleigh_rs::Space) -> Self {
        let space_name = from_sleigh(&sleigh.name);
        let read = format_ident!("read_{}", space_name);
        use sleigh_rs::semantic::space::SpaceType::*;
        let write = (!matches!(&sleigh.space_type, Rom(_)))
            .then(|| format_ident!("write_{}", space_name));
        Self {
            read,
            write,
            //sleigh,
        }
    }
    pub fn read_function(&self) -> TokenStream {
        let name = &self.read;
        quote! {fn #name(&self, addr: usize, buf: &mut [u8])}
    }
    pub fn write_function(&self) -> Option<TokenStream> {
        let name = self.write.as_ref()?;
        Some(quote! {fn #name(&mut self, addr: usize, buf: &[u8])})
    }
}

const MEMORY_TRAIT_VARNODE_READ: &str = "read";
const MEMORY_TRAIT_VARNODE_WRITE: &str = "write";
#[derive(Debug, Clone)]
pub struct MemoryTraitVarnode<'a> {
    //the name that is conventional to use as variable
    //TODO remove this
    name: Ident,
    //TODO remote this, no need to store this, it can be calculated on demand
    //from the varnode itself
    return_type: WorkType,
    read: Ident,
    //write function only Some if is not a Read-Only address space
    write: Option<Ident>,
    sleigh: &'a sleigh_rs::Varnode,
}

impl<'a> MemoryTraitVarnode<'a> {
    pub fn new(varnode: &'a sleigh_rs::Varnode) -> Self {
        let name = from_sleigh(&varnode.name);
        let read = format_ident!("read_{}", name);
        let write = varnode
            .space()
            .space_type
            .can_write()
            .then(|| format_ident!("write_{}", name));
        let return_type = WorkType::from_varnode(&varnode);
        let name = format_ident!("{}", name);
        Self {
            name,
            read,
            write,
            sleigh: varnode,
            return_type,
        }
    }
    pub fn name(&self) -> &Ident {
        &self.name
    }
    pub fn read(&self) -> &Ident {
        &self.read
    }
    pub fn write(&self) -> Option<&Ident> {
        self.write.as_ref()
    }
    pub fn return_type(&self) -> &WorkType {
        &self.return_type
    }
    pub fn gen_read_function_sig(&self) -> TokenStream {
        let name = &self.read;
        let out_type = &self.return_type;
        quote! {
            fn #name(&self) -> #out_type;
        }
    }
    pub fn gen_write_function_sig(&self) -> Option<TokenStream> {
        let name = self.write.as_ref()?;
        let value_type = &self.return_type;
        Some(quote! {
            fn #name(&mut self, value: #value_type);
        })
    }
    pub fn gen_read_function_impl(
        &self,
        read_function: &TokenStream,
    ) -> TokenStream {
        let name = &self.read;
        let value_type = &self.return_type;
        match &self.sleigh.varnode_type {
            VarnodeType::Memory(memory) => {
                let addr = usize::try_from(memory.offset).unwrap();
                let value_len = self.return_type.len_bytes();
                quote! {
                    fn #name(&self) -> #value_type {
                        let mut data = [0u8; #value_len];
                        #read_function(#addr, &mut data);
                        #value_type::from_be_bytes(data)
                    }
                }
            }
            VarnodeType::BitRange(bitrange)
            | VarnodeType::Context(Context { bitrange, .. }) => {
                let (addr, mem_bytes, work_type, bit_start, bit_mask) =
                    bitrange_size(bitrange);
                let work_len = work_type.len_bytes();
                let mem_start = work_len - mem_bytes;
                quote! {
                    fn #name(&self) -> #value_type {
                        let mut mem = [0u8; #work_len];
                        #read_function(#addr, &mut mem[#mem_start..]);
                        let mut work = #work_type::from_be_bytes(mem);
                        work >>= #bit_start;
                        work &= #bit_mask as #work_type;
                        work as #value_type
                    }
                }
            }
        }
    }
    pub fn gen_write_function_impl(
        &self,
        read_space: &TokenStream,
        write_space: &TokenStream,
    ) -> Option<TokenStream> {
        let name = self.write.as_ref()?;
        let value_type = &self.return_type;

        match &self.sleigh.varnode_type {
            VarnodeType::Memory(memory) => {
                let addr = usize::try_from(memory.offset).unwrap();
                Some(quote! {
                    fn #name(&mut self, value: #value_type) {
                        let mut data = value.to_be_bytes();
                        #write_space(#addr, &data);
                    }
                })
            }
            VarnodeType::BitRange(bitrange)
            | VarnodeType::Context(Context { bitrange, .. }) => {
                let (addr, mem_bytes, work_type, bit_start, bit_mask) =
                    bitrange_size(bitrange);
                let work_len = work_type.len_bytes();
                let mem_start = work_len - mem_bytes;
                Some(quote! {
                    fn #name(&mut self, value: #value_type) {
                        let mut mem = [0u8; #work_len];
                        #read_space(#addr, &mut mem[#mem_start..]);
                        let mut mem = #work_type::from_be_bytes(mem);
                        mem &= !((#bit_mask as #work_type) << #bit_start);

                        let mut param = value as #work_type;
                        param &= #bit_mask as #work_type;
                        param <<= #bit_start;

                        let result = (mem | param).to_be_bytes();
                        #write_space(#addr, &result[#mem_start..]);
                    }
                })
            }
        }
    }
}

const MEMORY_TRAIT_NAME: [&str; 3] = [SLEIGH_IDENT, "memory", "trait"];
#[derive(Debug, Clone)]
pub struct MemoryTrait<'a> {
    //trait name
    name: Ident,
    //addressable spaces read/write function names
    spaces: HashMap<*const sleigh_rs::Space, MemoryTraitSpace>,
    varnodes: HashMap<*const sleigh_rs::Varnode, MemoryTraitVarnode<'a>>,
}

impl<'a> MemoryTrait<'a> {
    pub fn new(sleigh: &'a sleigh_rs::Sleigh) -> Self {
        let name = format_ident!("memory_trait");
        let spaces = sleigh
            .spaces()
            .map(|space| (Rc::as_ptr(&space), MemoryTraitSpace::new(space)))
            .collect();
        let varnodes = sleigh
            .varnodes()
            .map(|varnode| {
                (Rc::as_ptr(&varnode), MemoryTraitVarnode::new(&varnode))
            })
            .collect();
        Self {
            name,
            spaces,
            varnodes,
        }
    }
    pub fn gen_trait(&self) -> TokenStream {
        let name = &self.name;
        let functions = self.spaces.values().map(|space| {
            let read = space.read_function();
            let write = space.write_function();
            quote! {
                #read;
                #write;
            }
        });
        let varnodes_impl = self.varnodes.values().map(|varnode| {
            let space = self
                .spaces
                .get(&Rc::as_ptr(&varnode.sleigh.memory().space))
                .expect(
                    "Space address is not presend in the MemoryTrait struct",
                );
            let space_read = &space.read;
            let space_read_func = quote! {self.#space_read};
            let read_func = varnode.gen_read_function_impl(&space_read_func);
            let write_func = space.write.as_ref().map(|space_write| {
                let space_write_func = quote! {self.#space_write};
                varnode.gen_write_function_impl(
                    &space_read_func,
                    &space_write_func,
                )
            });
            quote! {
                #read_func
                #write_func
            }
        });
        quote! {
            pub trait #name {
                #(#functions)*
                #(#varnodes_impl)*
            }
        }
    }
    pub fn impl_context_trait(&self, context: &ContextTrait) -> TokenStream {
        let trait_name = &context.name;
        let memory_name = &self.name;
        let varnodes_impl = context.varnodes.values().map(|varnode| {
            let varnode_ptr: *const sleigh_rs::Varnode = varnode.sleigh;
            let memory_varnode = self.varnodes.get(&varnode_ptr).unwrap();
            let memory_read_func = &memory_varnode.read;
            let return_type = &memory_varnode.return_type;
            let read_func = &varnode.read;
            let read_func = quote! {
                fn #read_func(&self) -> #return_type {
                    #memory_name::#memory_read_func(self)
                }
            };
            let write_func = &varnode.write.as_ref().map(|write_func| {
                let memory_write_func = memory_varnode.write.as_ref().unwrap();
                quote! {
                    fn #write_func(&mut self, value: #return_type) {
                        #memory_name::#memory_write_func(self, value)
                    }
                }
            });
            quote! {
                #read_func
                #write_func
            }
        });
        quote! {
            impl #trait_name for #memory_name {
                #(#varnodes_impl)*
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ContextTrait<'a> {
    name: Ident,
    varnodes: HashMap<*const sleigh_rs::Varnode, MemoryTraitVarnode<'a>>,
}
impl<'a> ContextTrait<'a> {
    pub fn new(sleigh: &'a sleigh_rs::Sleigh) -> Self {
        let name = format_ident!("ContextTrait");
        let varnodes = sleigh
            .varnodes()
            .filter(|varnode| varnode.varnode_type.is_context())
            .map(|varnode| {
                (Rc::as_ptr(&varnode), MemoryTraitVarnode::new(&varnode))
            })
            .collect();
        Self { name, varnodes }
    }
    pub fn name(&self) -> &Ident {
        &self.name
    }
    pub fn varnode(
        &self,
        ptr: *const sleigh_rs::Varnode,
    ) -> &MemoryTraitVarnode {
        self.varnodes.get(&ptr).unwrap()
    }
    pub fn gen_trait(&self) -> TokenStream {
        let name = &self.name;
        let varnodes_impl = self.varnodes.values().map(|varnode| {
            let return_type = &varnode.return_type;
            let read_func = &varnode.read;
            let read_func = quote! {
                fn #read_func(&self) -> #return_type;
            };
            let write_func = varnode.write.as_ref().map(|write_func| {
                quote! {
                    fn #write_func(&mut self, value: #return_type);
                }
            });
            quote! {
                #read_func
                #write_func
            }
        });
        quote! {
            pub trait #name {
                #(#varnodes_impl)*
            }
        }
    }
}

const MEMORY_SPACE_PREFIX: [&str; 3] = [SLEIGH_IDENT, "memory", "space"];
#[derive(Debug, Clone)]
pub struct MemoryStruct<'a> {
    //name of the global memory struct
    name: Ident,
    //spaces that this struct can read/write
    spaces: HashMap<*const sleigh_rs::Space, (Ident, SpaceStruct<'a>)>,
}

impl<'a> MemoryStruct<'a> {
    pub fn new(sleigh: &'a Sleigh) -> Self {
        let name = format_ident!("memory");
        //iterator for all varnodes to this space
        let spaces = sleigh
            .spaces()
            .map(|space| {
                let space_ptr = Rc::as_ptr(&space);
                let varnodes = sleigh
                    .varnodes()
                    .filter(|varnode| Rc::as_ptr(varnode.space()) == space_ptr)
                    .map(|varnode| varnode.as_ref());
                let ident = format!("MemorySpace{}", from_sleigh(&space.name));
                SpaceStruct::new(&ident, space, varnodes)
            })
            .map(|space| {
                let sleigh: *const _ = space.sleigh;
                let name = format_ident!("{}", from_sleigh(&space.sleigh.name));
                (sleigh, (name, space))
            })
            .collect();
        Self { name, spaces }
    }
    pub fn gen_struct(&self) -> TokenStream {
        let ident = &self.name;
        let space_structs =
            self.spaces.values().map(|(_space_field, space)| {
                let space_struct = space.gen_struct();
                let space_struct_name = &space.name;
                let space_impl_read = space.gen_read_fun_impl();
                let space_impl_write = space.gen_write_fun_impl();
                quote! {
                    #space_struct
                    impl #space_struct_name {
                        #space_impl_read
                        #space_impl_write
                    }
                }
            });
        let space_names = self.spaces.values().map(|(space_field, space)| {
            let struct_name = &space.name;
            //TODO don't make it public, make a default or creator function
            quote! {pub #space_field: #struct_name}
        });
        quote! {
            #(#space_structs)*
            #[derive(Debug, Clone, Copy)]
            pub struct #ident {
                #(#space_names),*
            }
        }
    }
    pub fn impl_memory_trait(&self, memory: &MemoryTrait) -> TokenStream {
        let struct_name = &self.name;
        let trait_name = &memory.name;
        let functions = self.spaces.values().map(|(space_field, space)| {
            let space_ptr: *const _ = space.sleigh;
            let space_trait = memory.spaces.get(&space_ptr).unwrap();
            let read_trait = &space_trait.read;
            let read_func = &space.read;
            let read = quote! {
                fn #read_trait(&self, addr: usize, buf: &mut [u8]) {
                    self.#space_field.#read_func(addr, buf)
                }
            };
            let write = space.write.as_ref().map(|write_fun| {
                let write_trait = space_trait.write.as_ref().unwrap();
                quote! {
                    fn #write_trait(&mut self, addr: usize, buf: &[u8]) {
                        self.#space_field.#write_fun(addr, buf)
                    }
                }
            });
            quote! {
                #read
                #write
            }
        });
        quote! {
            impl #trait_name for #struct_name {
                #(#functions)*
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct SpaceStruct<'a> {
    //struct name
    name: Ident,
    //function to read from arbitrary address
    read: Ident,
    //function to write to arbitrary address
    write: Option<Ident>,
    sleigh: &'a sleigh_rs::Space,
    chunks: Vec<MemoryChunk>,
}

impl<'a> SpaceStruct<'a> {
    fn new<I>(name: &str, sleigh: &'a sleigh_rs::Space, varnodes: I) -> Self
    where
        I: Iterator<Item = &'a sleigh_rs::Varnode>,
    {
        let name = format_ident!("{}", name);

        let read_func = format_ident!("read");
        let write_func = sleigh
            .space_type
            .can_write()
            .then(|| format_ident!("write"));
        let chunks = chunks_from_varnodes(varnodes);
        Self {
            name,
            read: read_func,
            write: write_func,
            sleigh,
            chunks,
        }
    }
    fn gen_struct(&self) -> TokenStream {
        let ident = &self.name;
        let chunks = self.chunks.iter().map(MemoryChunk::struct_chunk);
        quote! {
            #[derive(Debug, Clone, Copy)]
            pub struct #ident {
                #(#chunks),*
            }
        }
    }
    fn gen_read_fun_impl(&self) -> Option<TokenStream> {
        let fun_ident = &self.read;
        let addr_param = format_ident!("addr");
        let buf_param = format_ident!("buf");
        let addr_end = format_ident!("addr_end");
        let chunks = self.chunks.iter().map(|chunk| {
            let chunk_start = chunk.addr_start();
            let chunk_end_excl = chunk.addr_end().checked_sub(1).unwrap();
            let chunk_end = chunk.addr_end();
            let chunk_name = chunk.ident();
            quote! {
                (#chunk_start..=#chunk_end_excl, #chunk_start..=#chunk_end) => {
                    let start = #addr_param - #chunk_start;
                    let end = start + #buf_param.len();
                    #buf_param.copy_from_slice(&self.#chunk_name[start..end]);
                }
            }
        });
        Some(quote! {
            pub fn #fun_ident(&self, #addr_param: usize, #buf_param: &mut [u8]) {
                let #addr_end = #addr_param + #buf_param.len();
                match (#addr_param, #addr_end) {
                    #(#chunks),*
                    _ => panic!(
                        "undefined mem {}:{}",
                        #addr_param,
                        #buf_param.len()
                    ),
                }
            }
        })
    }
    fn gen_write_fun_impl(&self) -> Option<TokenStream> {
        let fun_ident = self.write.as_ref()?;
        let addr_param = format_ident!("addr");
        let buf_param = format_ident!("buf");
        let addr_end = format_ident!("addr_end");
        let chunks = self.chunks.iter().map(|chunk| {
            let chunk_start = chunk.addr_start();
            let chunk_end = chunk.addr_end();
            let chunk_end_excl = chunk.addr_end().checked_sub(1).unwrap();
            let chunk_name = chunk.ident();
            quote! {
                (#chunk_start..=#chunk_end_excl, #chunk_start..=#chunk_end) => {
                    let start = #addr_param - #chunk_start;
                    let end = start + #buf_param.len();
                    self.#chunk_name[start..end].copy_from_slice(#buf_param);
                }
            }
        });
        Some(quote! {
            pub fn #fun_ident(&mut self, #addr_param: usize, #buf_param: &[u8]) {
                let #addr_end = #addr_param + #buf_param.len();
                match (#addr_param, #addr_end) {
                    #(#chunks),*
                    _ => panic!(
                        "undefined mem {}:{}",
                        #addr_param,
                        #buf_param.len()
                    ),
                }
            }
        })
    }
}

const CONTEXT_SPACE_PREFIX: [&str; 3] = [SLEIGH_IDENT, "context", "space"];
#[derive(Debug, Clone)]
pub struct ContextStruct<'a> {
    //name of the global memory struct
    name: Ident,
    //spaces that this struct can read/write
    spaces: HashMap<*const sleigh_rs::Space, (Ident, SpaceStruct<'a>)>,
    //varnodes: HashMap<*const sleigh_rs::Varnode, MemoryTraitVarnode>,
}

impl<'a> ContextStruct<'a> {
    pub fn new(sleigh: &'a Sleigh) -> Self {
        let name = format_ident!("Context");
        //let varnodes = sleigh
        //    .varnodes()
        //    .filter(|varnode| varnode.varnode_type.is_context())
        //    .map(|varnode| {
        //        (Rc::as_ptr(&varnode), MemoryTraitVarnode::new(varnode))
        //    })
        //    .collect();
        let spaces = sleigh
            .spaces()
            .filter(|space| {
                //TODO have sleigh_rs include the list of all varnodes in space
                //only spaces with context
                let space_ptr = Rc::as_ptr(&space);
                sleigh
                    .varnodes()
                    .filter(|varnode| Rc::as_ptr(varnode.space()) == space_ptr)
                    .find(|varnode| varnode.varnode_type.is_context())
                    .is_some()
            })
            .map(|space| {
                let space_ptr = Rc::as_ptr(&space);
                let varnodes = sleigh
                    .varnodes()
                    .filter(|varnode| {
                        Rc::as_ptr(varnode.space()) == space_ptr
                            && varnode.varnode_type.is_context()
                    })
                    .map(|varnode| varnode.as_ref());
                let ident = format!("ContextSpace{}", from_sleigh(&space.name));
                SpaceStruct::new(&ident, space, varnodes)
            })
            .map(|space| {
                let sleigh: *const _ = space.sleigh;
                let name = format_ident!("{}", from_sleigh(&space.sleigh.name));
                (sleigh, (name, space))
            })
            .collect();
        Self {
            name,
            spaces,
            //varnodes,
        }
    }
    pub fn name(&self) -> &Ident {
        &self.name
    }
    pub fn gen_struct(&self) -> TokenStream {
        let ident = &self.name;
        let space_structs =
            self.spaces.values().map(|(_space_field, space)| {
                let space_struct = space.gen_struct();
                let space_struct_name = &space.name;
                let space_impl_read = space.gen_read_fun_impl();
                let space_impl_write = space.gen_write_fun_impl();
                quote! {
                    #space_struct
                    impl #space_struct_name {
                        #space_impl_read
                        #space_impl_write
                    }
                }
            });
        let space_names = self.spaces.values().map(|(space_field, space)| {
            let struct_name = &space.name;
            //TODO don't make it public, make a default or creator function
            quote! {pub #space_field: #struct_name}
        });
        quote! {
            #(#space_structs)*
            #[derive(Debug, Clone, Copy)]
            pub struct #ident {
                #(#space_names),*
            }
        }
    }
    pub fn impl_context_trait(&self, context: &ContextTrait) -> TokenStream {
        let trait_name = &context.name;
        let memory_name = &self.name;
        let varnodes_impl = context.varnodes.values().map(|varnode| {
            let (space_field, space) = self
                .spaces
                .get(&Rc::as_ptr(varnode.sleigh.space()))
                .unwrap();

            let space_read = &space.read;
            let space_read_func = quote! {self.#space_field.#space_read};
            let read_func = varnode.gen_read_function_impl(&space_read_func);

            let write_func = space.write.as_ref().map(|space_write| {
                let space_write_func = quote! {self.#space_field.#space_write};
                varnode.gen_write_function_impl(
                    &space_read_func,
                    &space_write_func,
                )
            });
            quote! {
                #read_func
                #write_func
            }
        });
        quote! {
            impl #trait_name for #memory_name {
                #(#varnodes_impl)*
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MemoryChunk {
    offset: usize,
    len: usize,
    //struct name
    ident: Ident,
}

impl MemoryChunk {
    fn new(offset: usize, len: usize) -> Self {
        let ident = format_ident!("chunk_0x{:x}", offset);
        Self { offset, len, ident }
    }
    pub fn bytes_len(&self) -> usize {
        self.len
    }
    pub fn addr_start(&self) -> usize {
        self.offset
    }
    pub fn addr_end(&self) -> usize {
        self.offset + self.len
    }
    pub fn ident(&self) -> &Ident {
        &self.ident
    }
    pub fn struct_chunk(&self) -> TokenStream {
        let ident = self.ident();
        let len = self.bytes_len();
        //TODO don't make it public, make a default or creator function
        quote! { pub #ident: [u8; #len] }
    }
}

fn chunks_from_varnodes<'a, I>(varnodes: I) -> Vec<MemoryChunk>
where
    I: Iterator<Item = &'a sleigh_rs::Varnode>,
{
    #[derive(Debug, PartialEq, Eq, Clone)]
    struct Chunk {
        offset: usize,
        len: usize,
    }
    let mut chunks: Vec<Chunk> = varnodes
        .map(|varnode| {
            let memory = varnode.memory();
            Chunk {
                offset: memory.offset.try_into().unwrap(),
                len: memory.size.get().try_into().unwrap(),
            }
        })
        .collect();
    chunks.sort_by(|a, b| {
        //sort first by:
        // * addr (lower address first)
        // * size (bigger first)
        match a.offset.cmp(&b.offset) {
            std::cmp::Ordering::Equal => b.len.cmp(&a.len),
            x => x,
        }
    });
    let mut iter = chunks.drain(..);
    let mut acc = match iter.next() {
        Some(x) => x,
        None => return vec![],
    };
    let mut chunks: Vec<MemoryChunk> = iter
        .filter_map(|chunk| {
            let acc_end = acc.offset + acc.len;
            if chunk.offset > acc_end {
                //can't merge, return acc and start a new one
                let ret = acc.clone();
                acc = chunk;
                return Some(MemoryChunk::new(ret.offset, ret.len));
            } else {
                //merge with the acc, AKA extend acc len, if bigger
                let chunk_end = chunk.offset + chunk.len;
                acc.len = chunk_end.max(acc_end) - acc.offset;
                None
            }
        })
        .collect();

    //push the acc that was not consumed by the iterator
    chunks.push(MemoryChunk::new(acc.offset, acc.len));
    chunks
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct MemoryChunkInner(Range<usize>);

impl PartialOrd for MemoryChunkInner {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for MemoryChunkInner {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.0.start.cmp(&other.0.start) {
            std::cmp::Ordering::Equal => self.0.end.cmp(&other.0.end),
            x => x,
        }
    }
}
