use indexmap::IndexMap;
use std::ops::Range;
use std::rc::Rc;

use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use sleigh_rs::semantic::{GlobalAnonReference, GlobalElement};
use sleigh_rs::{IntTypeU, NonZeroTypeU};

use crate::builder::formater::*;

use super::{
    BitrangeFromMemory, BitrangeRW, DisplayElement, Meanings, WorkType,
};

#[derive(Debug, Clone)]
pub struct DisassemblerMemory {
    pub memory_read_trait: Rc<MemoryAccessTrait>,
    pub memory_write_trait: Rc<MemoryAccessTrait>,
    pub space_traits: IndexMap<*const sleigh_rs::Space, Rc<SpaceTrait>>,
    pub space_structs: IndexMap<*const sleigh_rs::Space, Rc<SpaceStruct>>,
    pub spaces_trait: Rc<SpacesTrait>,
    pub spaces_struct: Rc<SpacesStruct>,
}

impl DisassemblerMemory {
    pub fn new(
        bitrange_rw: &Rc<BitrangeRW>,
        display_element: &Rc<DisplayElement>,
        meanings: &Rc<Meanings>,
        sleigh: &sleigh_rs::Sleigh,
    ) -> Self {
        let memory_read_trait = MemoryAccessTrait::new_read();
        let memory_write_trait = MemoryAccessTrait::new_write();

        //in disasembly we only care about space that contains contexts
        let spaces: Box<[GlobalElement<sleigh_rs::Space>]> = sleigh
            .contexts()
            .map(|context| context.varnode().space())
            .map(|space| (space.element_ptr(), space))
            .collect::<IndexMap<*const _, &GlobalElement<_>>>()
            .into_iter()
            .map(|(_k, v)| v.clone())
            .collect();

        let space_traits: IndexMap<*const sleigh_rs::Space, Rc<SpaceTrait>> =
            spaces
                .iter()
                .map(|space| {
                    let ptr = space.element_ptr();
                    let space_trait = SpaceTrait::new(
                        bitrange_rw,
                        sleigh.endian().is_big(),
                        display_element,
                        meanings,
                        space,
                        format_ident!(
                            "Context{}Trait",
                            from_sleigh(space.name())
                        ),
                        [].into_iter(), //no varnodes
                        [].into_iter(), //no bitranges
                        sleigh.contexts().filter(|context| {
                            context.varnode().space().element_ptr()
                                == space.element_ptr()
                        }),
                        Rc::clone(&memory_read_trait),
                        space
                            .can_write()
                            .then(|| Rc::clone(&memory_write_trait)),
                    );
                    (ptr, space_trait)
                })
                .collect();

        let spaces_trait = SpacesTrait::new(
            format_ident!("ContextTrait"),
            space_traits.values(),
        );

        let space_structs: IndexMap<*const sleigh_rs::Space, Rc<SpaceStruct>> =
            spaces
                .iter()
                .map(|space| {
                    let ptr = space.element_ptr();
                    let space_struct = SpaceStruct::new(
                        space,
                        Rc::clone(
                            space_traits.get(&space.element_ptr()).unwrap(),
                        ),
                        sleigh
                            .contexts()
                            .filter(|context| {
                                context.varnode().space().element_ptr()
                                    == space.element_ptr()
                            })
                            .map(|context| ChunkBytes {
                                offset: context.varnode().offset,
                                len: context.varnode().len_bytes,
                            }),
                        &memory_read_trait,
                        &memory_write_trait,
                    );
                    (ptr, space_struct)
                })
                .collect();

        let spaces_struct =
            SpacesStruct::new(Rc::clone(&spaces_trait), space_structs.values());
        Self {
            memory_read_trait,
            memory_write_trait,
            space_traits,
            space_structs,
            spaces_trait,
            spaces_struct,
        }
    }
}

impl ToTokens for DisassemblerMemory {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            memory_read_trait,
            memory_write_trait,
            space_traits,
            space_structs,
            spaces_trait,
            spaces_struct,
        } = self;
        let space_traits = space_traits.values();
        let space_structs = space_structs.values();
        tokens.extend(quote! {
            #memory_read_trait
            #memory_write_trait
            #(#space_traits)*
            #spaces_trait
            #(#space_structs)*
            #spaces_struct
        })
    }
}

#[derive(Debug, Clone)]
pub struct MemoryAccessTrait {
    pub name: Ident,
    pub address_type: Ident,
    pub function: Ident,
    pub write: bool,
}
impl MemoryAccessTrait {
    pub fn new_read() -> Rc<Self> {
        Rc::new(Self {
            name: format_ident!("MemoryRead"),
            address_type: format_ident!("AddressType"),
            function: format_ident!("read"),
            write: false,
        })
    }
    pub fn new_write() -> Rc<Self> {
        Rc::new(Self {
            name: format_ident!("MemoryWrite"),
            address_type: format_ident!("AddressType"),
            function: format_ident!("write"),
            write: true,
        })
    }
}
impl ToTokens for MemoryAccessTrait {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = &self.name;
        let address_type = &self.address_type;
        let function = &self.function;
        let self_mut = self.write.then(|| quote! {mut});
        let buf_mut = (!self.write).then(|| quote! {mut});
        tokens.extend(quote! {
            pub trait #name {
                type #address_type;
                fn #function(
                    &#self_mut self,
                    addr: Self::#address_type,
                    buf: &#buf_mut [u8],
                );
            }
        })
    }
}
#[derive(Debug, Clone)]
pub struct SpaceTraitElement<T: Clone + std::fmt::Debug> {
    pub function_read: Ident,
    pub function_write: Option<Ident>,
    pub element: GlobalAnonReference<T>,
}
impl<'a, T: Clone + std::fmt::Debug> SpaceTraitElement<T> {
    pub fn new(element: &'a GlobalElement<T>, can_write: bool) -> Self {
        let name = from_sleigh(element.name());
        Self {
            function_read: format_ident!("read_{}", name),
            function_write: can_write.then(|| format_ident!("write_{}", name)),
            element: element.reference(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct ContextAccess {
    bitrange_rw: Rc<BitrangeRW>,
    big_endian: bool,
    display_element: Rc<DisplayElement>,
    meanings: Rc<Meanings>,
    pub raw_read: Ident,
    pub execution_read: Ident,
    pub disassembly_read: Ident,
    pub display_func: Ident,
    pub mem_type: WorkType,
    pub write: Option<(Ident, Ident, Ident)>,

    pub display: Ident,
    pub context: GlobalAnonReference<sleigh_rs::Context>,
}
impl ContextAccess {
    pub fn new(
        context: &GlobalElement<sleigh_rs::Context>,
        display_element: Rc<DisplayElement>,
        meanings: Rc<Meanings>,
        bitrange_rw: Rc<BitrangeRW>,
        big_endian: bool,
    ) -> Self {
        let name = from_sleigh(context.name());
        let write = context.varnode().space().can_write();
        Self {
            bitrange_rw,
            big_endian,

            display_element,
            meanings,

            raw_read: format_ident!("read_{}_raw", name),
            execution_read: format_ident!("read_{}_execution", name),
            disassembly_read: format_ident!("read_{}_disassembly", name),
            display_func: format_ident!("{}_display", name),
            write: write.then(|| {
                (
                    format_ident!("write_{}_raw", name),
                    format_ident!("write_{}_disassembly", name),
                    format_ident!("write_{}_execution", name),
                )
            }),

            mem_type: WorkType::from_context(&context),
            display: format_ident!("display_{}", name,),
            context: context.reference(),
        }
    }
}
impl ToTokens for ContextAccess {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            bitrange_rw,
            big_endian,
            display_element,
            meanings,

            raw_read,
            execution_read,
            disassembly_read,
            display_func,
            write,

            mem_type: _,
            display: _,
            context,
        } = self;
        let context = context.element();
        let varnode = context.varnode();
        let range = context.range();
        let bit_len = range.n_bits;
        let signed = context.meaning().is_signed();

        let varnode_addr = varnode.offset();
        let varnode_len = context.varnode().len_bytes();

        let bitrange_from_varnode = BitrangeFromMemory::new(
            *big_endian,
            Rc::clone(bitrange_rw),
            varnode_addr,
            varnode_len,
            range.lsb_bit,
            bit_len,
            signed,
        );
        let addr_type =
            WorkType::new_int_bytes(varnode.space().addr_size, false);
        let disassembly_type = WorkType::int_type(true);

        //raw function
        let read =
            |work_value: &Ident, read_bytes: NonZeroTypeU, addr, work_start| {
                let work_start = usize::try_from(work_start).unwrap();
                let work_end =
                    work_start + usize::try_from(read_bytes.get()).unwrap();
                let work_end = Literal::usize_unsuffixed(work_end);
                let work_start = Literal::usize_unsuffixed(work_start);
                quote! {
                    self.read(
                        #addr as #addr_type,
                        &mut #work_value[#work_start..#work_end],
                    );
                }
            };
        let mem_type = bitrange_from_varnode.return_type();
        let read_bitrange = bitrange_from_varnode.read_value(read);
        let write_functions = write.as_ref().map(|(raw, _dis, _exe)| {
            let param_type = &bitrange_from_varnode.work_type;
            let write_bitrange_fun = bitrange_rw.write_function(param_type);
            let bit_start = bitrange_from_varnode.bit_start;
            let bit_len = bitrange_from_varnode.bit_len.get();
            let addr = bitrange_from_varnode.mem_start;
            let work_len = usize::try_from(
                bitrange_from_varnode.work_type.len_bytes().get(),
            )
            .unwrap();
            let work_start =
                usize::try_from(bitrange_from_varnode.work_start).unwrap();
            let work_end = work_start
                + usize::try_from(bitrange_from_varnode.read_bytes.get())
                    .unwrap();
            let work_end = Literal::usize_unsuffixed(work_end);
            let work_len = Literal::usize_unsuffixed(work_len);
            let work_start = Literal::usize_unsuffixed(work_start);
            let endian = if self.big_endian {
                quote! {from_be_bytes}
            } else {
                quote! {from_le_bytes}
            };
            quote! {
                fn #raw(&mut self, param: #mem_type) {
                    let mut mem = [0u8; #work_len];
                    self.read(
                        #addr as #addr_type,
                        &mut mem[#work_start..#work_end],
                    );
                    let mem = #param_type::#endian(mem);
                    let mem = #write_bitrange_fun::<#big_endian>(
                        param as #param_type,
                        mem,
                        #bit_start as usize,
                        #bit_len as usize,
                    );
                    self.write(
                        #addr as #addr_type,
                        &mem[#work_start..#work_end],
                    );
                }
            }
        });
        tokens.extend(quote! {
            fn #raw_read(&self) -> #mem_type {
                #read_bitrange
            }
            #write_functions
        });

        //disassembly function
        match context.meaning() {
            //TODO the context is transformed into a value?
            sleigh_rs::Meaning::Value(_, _) => todo!(),
            //does disassembly returns the raw value? Or this is forbidden?
            sleigh_rs::Meaning::Variable(_) => {
                let write = write.as_ref().map(|(_raw, dis, _exe)| {
                    quote! {
                        fn #dis(&mut self, _param: #disassembly_type) {
                            todo!()
                        }
                    }
                });
                tokens.extend(quote! {
                    fn #disassembly_read(&self) -> #disassembly_type {
                        todo!()
                    }
                    #write
                });
            }
            sleigh_rs::Meaning::Literal(_) | sleigh_rs::Meaning::Name(_) => {
                let write = write.as_ref().map(|(raw, dis, _exe)| {
                    quote! {
                        fn #dis(&mut self, param: #disassembly_type) {
                            self.#raw(#mem_type::try_from(param).unwrap())
                        }
                    }
                });
                tokens.extend(quote! {
                    fn #disassembly_read(&self) -> #disassembly_type {
                        #disassembly_type::try_from(self.#raw_read()).unwrap()
                    }
                    #write
                });
            }
        }

        //execution function
        match context.meaning() {
            sleigh_rs::Meaning::Variable(vars) => {
                let vars = self.meanings.vars.get(&Rc::as_ptr(vars)).unwrap();
                let exec_type =
                    WorkType::new_int_bytes(vars.varnode_bytes, false);
                let function = &vars.value_func;
                let index_type = &vars.index_type;
                let write = write.as_ref().map(|(_raw, _dis, exe)| {
                    quote! {
                        fn #exe(&mut self, param: #exec_type) {
                            let varnode = #function(
                                #index_type::try_from(self.#raw_read()).unwrap()
                            );
                            todo!("Write {} into the varnode {}", param, varnode)
                        }
                    }
                });
                tokens.extend(quote! {
                    fn #execution_read(&self) -> #exec_type {
                        let varnode = #function(
                            #index_type::try_from(self.#raw_read()).unwrap()
                        );
                        todo!("Read from the varnode {}", varnode)
                    }
                    #write
                });
            }
            sleigh_rs::Meaning::Value(_print_base, values) => {
                let vars =
                    self.meanings.values.get(&Rc::as_ptr(values)).unwrap();
                let super::ValueMeaning {
                    value_func,
                    index_type,
                    value_type,
                    ..
                } = Rc::as_ref(vars);
                let write = write.as_ref().map(|(_raw, _dis, exe)| {
                    let todo_msg = format!(
                        "The param is the mem_type ({}) or value_type ({})?",
                        mem_type.into_token_stream().to_string(),
                        value_type.into_token_stream().to_string()
                    );
                    quote! {
                        fn #exe(&mut self, param: ()) {
                            todo!(#todo_msg)
                        }
                    }
                });
                tokens.extend(quote! {
                    fn #execution_read(&self) -> #value_type {
                        #value_func(
                            #index_type::try_from(self.#raw_read()).unwrap()
                        )
                    }
                    #write
                });
            }
            sleigh_rs::Meaning::Literal(_) | sleigh_rs::Meaning::Name(_) => {
                let write = write.as_ref().map(|(raw, _dis, exe)| {
                    quote! {
                        fn #exe(&mut self, param: #mem_type) {
                            self.#raw(param)
                        }
                    }
                });
                tokens.extend(quote! {
                    fn #execution_read(&self) -> #mem_type{
                        self.#raw_read()
                    }
                    #write
                });
            }
        }

        //display function
        let display_type = display_element.name();
        let display_body = meanings.display_function_call(
            quote! {self.#raw_read()},
            context.meaning(),
        );
        tokens.extend(quote! {
            fn #display_func(&self) -> #display_type {
                #display_body
            }
        });
    }
}

#[derive(Debug, Clone)]
pub struct SpaceTrait {
    pub space: GlobalAnonReference<sleigh_rs::Space>,
    pub addr_type: WorkType,
    pub name: Ident,
    pub trait_read: Rc<MemoryAccessTrait>,
    pub trait_write: Option<Rc<MemoryAccessTrait>>,
    pub varnodes: IndexMap<
        *const sleigh_rs::Varnode,
        SpaceTraitElement<sleigh_rs::Varnode>,
    >,
    pub bitranges: IndexMap<
        *const sleigh_rs::Bitrange,
        SpaceTraitElement<sleigh_rs::Bitrange>,
    >,
    pub contexts: IndexMap<*const sleigh_rs::Context, ContextAccess>,
}

impl SpaceTrait {
    pub fn new<'a>(
        bitrange_rw: &Rc<BitrangeRW>,
        big_endian: bool,
        display_element: &Rc<DisplayElement>,
        meanings: &Rc<Meanings>,
        space: &'a GlobalElement<sleigh_rs::Space>,
        name: Ident,
        varnodes: impl Iterator<Item = &'a GlobalElement<sleigh_rs::Varnode>> + 'a,
        bitranges: impl Iterator<Item = &'a GlobalElement<sleigh_rs::Bitrange>> + 'a,
        contexts: impl Iterator<Item = &'a GlobalElement<sleigh_rs::Context>> + 'a,
        trait_read: Rc<MemoryAccessTrait>,
        trait_write: Option<Rc<MemoryAccessTrait>>,
    ) -> Rc<Self> {
        let varnodes = varnodes
            .map(|varnode| {
                let ptr = varnode.element_ptr();
                let can_write = varnode.space().can_write();
                (ptr, SpaceTraitElement::new(varnode, can_write))
            })
            .collect();
        let bitranges = bitranges
            .map(|bitrange| {
                let ptr = bitrange.element_ptr();
                let can_write = bitrange.varnode().space().can_write();
                (ptr, SpaceTraitElement::new(bitrange, can_write))
            })
            .collect();
        let contexts = contexts
            .map(|context| {
                let functions = ContextAccess::new(
                    context,
                    Rc::clone(display_element),
                    Rc::clone(meanings),
                    Rc::clone(bitrange_rw),
                    big_endian,
                );
                (context.element_ptr(), functions)
            })
            .collect();
        Rc::new(Self {
            space: space.reference(),
            addr_type: WorkType::new_int_bytes(
                space.addr_bytes().get().try_into().unwrap(),
                false,
            ),
            name,
            varnodes,
            bitranges,
            contexts,
            trait_read,
            trait_write,
        })
    }
}
impl ToTokens for SpaceTrait {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let varnodes = self.varnodes.values();
        let bitranges = self.bitranges.values();
        let contexts = self.contexts.values();
        let name = &self.name;
        let addr_type = &self.addr_type;
        let trait_read_name = &self.trait_read.name;
        let trait_read_addr = &self.trait_read.address_type;
        let trait_write = self.trait_write.as_ref().map(|write| {
            let trait_write_name = &write.name;
            let trait_write_addr = &write.address_type;
            quote! {
                + #trait_write_name<#trait_write_addr = #addr_type>
            }
        });
        tokens.extend(quote! {
            pub trait #name:
                #trait_read_name<#trait_read_addr = #addr_type> #trait_write
            {
                #(#varnodes)*
                #(#bitranges)*
                #(#contexts)*
            }
        })
    }
}

impl ToTokens for SpaceTraitElement<sleigh_rs::Varnode> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let read_name = &self.function_read;
        let varnode = self.element.element();
        let value_bytes = varnode.len_bytes().get();
        let value_addr = varnode.offset();
        let addr_type =
            WorkType::new_int_bytes(varnode.space().addr_bytes(), false);
        let write_function = self.function_write.as_ref().map(|write_func| {
            quote! {
                fn #write_func(&mut self, value: [u8; #value_bytes as usize]) {
                    self.write(#value_addr as #addr_type, &value)
                }
            }
        });
        tokens.extend(quote! {
            fn #read_name(&self) -> [u8; #value_bytes as usize] {
                let mut data = [0u8; #value_bytes as usize];
                self.read(#value_addr as #addr_type, &mut data);
                data
            }
            #write_function
        })
    }
}
impl ToTokens for SpaceTraitElement<sleigh_rs::Bitrange> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let bitrange = self.element.element();
        tokens.extend(read_write_bitrange(
            &self.function_read,
            self.function_write.as_ref(),
            bitrange.varnode(),
            bitrange.range(),
        ))
    }
}
impl ToTokens for SpaceTraitElement<sleigh_rs::Context> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let context = self.element.element();
        tokens.extend(read_write_bitrange(
            &self.function_read,
            self.function_write.as_ref(),
            context.varnode(),
            context.range(),
        ))
    }
}

fn read_write_bitrange(
    read_name: &Ident,
    write_name: Option<&Ident>,
    varnode: &GlobalElement<sleigh_rs::Varnode>,
    range: &sleigh_rs::RangeBits,
) -> TokenStream {
    let bit_start = range.lsb_bit;
    let bit_len = range.n_bits.get();
    let bit_mask = (1 << bit_len) - 1;

    let varnode_addr = varnode.offset();
    let varnode_byte_start = bit_start / 8;

    let addr_type =
        WorkType::new_int_bytes(varnode.space().addr_bytes(), false);
    let addr = varnode_addr + varnode_byte_start;
    let mem_bytes = (bit_len + 7) / 8;
    let output_bytes = (bit_len + 7) / 8;

    let work_type =
        WorkType::new_int_bytes(mem_bytes.try_into().unwrap(), false);
    let work_bytes = work_type.len_bytes().get();

    let write_function = write_name.map(|write_func| {
        quote! {
            fn #write_func(&mut self, param: [u8; #output_bytes as usize]) {
                let mut mem = [0u8; #work_bytes as usize];
                self.read(#addr as #addr_type, &mut mem[..#mem_bytes as usize]);
                let mut mem = <#work_type>::from_be_bytes(mem);
                mem &= !((#bit_mask as #work_type) << #bit_start);

                let mut param_tmp = [0u8; #work_bytes as usize];
                param_tmp[..#output_bytes as usize].copy_from_slice(&param);
                let mut param = <#work_type>::from_be_bytes(param_tmp);
                param &= #bit_mask as #work_type;
                param <<= #bit_start;

                let output = (mem | param).to_be_bytes();
                self.write(#addr as #addr_type, &output[..#output_bytes as usize]);
            }
        }
    });
    quote! {
        fn #read_name(&self) -> [u8; #output_bytes as usize] {
            let mut mem = [0u8; #work_bytes as usize];
            self.read(#addr as #addr_type, &mut mem[..#mem_bytes as usize]);
            let mut work = <#work_type>::from_be_bytes(mem);
            work >>= #bit_start;
            work &= #bit_mask as #work_type;
            let work = work.to_be_bytes();
            <[u8; #output_bytes as usize]>::try_from(
                &work[..#mem_bytes as usize]
            ).unwrap()
        }
        #write_function
    }
}

#[derive(Debug, Clone)]
pub struct SpacesTraitAssociatedType {
    pub type_name: Ident,
    pub type_trait: Rc<SpaceTrait>,
    pub function: Ident,
    pub function_mut: Option<Ident>,
}

#[derive(Debug, Clone)]
pub struct SpacesTrait {
    //trait name
    pub name: Ident,
    //addressable spaces read/write function names
    pub spaces: IndexMap<*const sleigh_rs::Space, SpacesTraitAssociatedType>,
}

impl SpacesTrait {
    pub fn new<'a>(
        name: Ident,
        spaces: impl Iterator<Item = &'a Rc<SpaceTrait>> + 'a,
    ) -> Rc<Self> {
        let spaces = spaces
            .map(|space_trait| {
                let ptr = space_trait.space.element_ptr();
                let space = space_trait.space.element();
                let type_name =
                    format_ident!("Type{}", space_trait.space.name());
                let space_trait = SpacesTraitAssociatedType {
                    type_name,
                    type_trait: Rc::clone(space_trait),
                    function: format_ident!("{}", space.name()),
                    function_mut: space.can_write().then(|| {
                        format_ident!("{}_mut", space_trait.space.name())
                    }),
                };
                (ptr, space_trait)
            })
            .collect();
        Rc::new(Self { name, spaces })
    }
    pub fn build_context_display_call(
        &self,
        instance: impl ToTokens,
        context: &sleigh_rs::Context,
    ) -> TokenStream {
        let space = context.varnode().space();
        let space_trait = self.spaces.get(&space.element_ptr()).unwrap();
        let space_fun = &space_trait.function;
        let ptr: *const _ = context;
        let context_fun = &space_trait.type_trait.contexts.get(&ptr).unwrap();
        let read_context_fun = &context_fun.display_func;
        quote! {#instance.#space_fun().#read_context_fun()}
    }
    pub fn build_context_disassembly_read_call(
        &self,
        instance: impl ToTokens,
        context: &sleigh_rs::Context,
    ) -> TokenStream {
        let space = context.varnode().space();
        let space_trait = self.spaces.get(&space.element_ptr()).unwrap();
        let space_fun = &space_trait.function;
        let ptr: *const _ = context;
        let context_fun = &space_trait.type_trait.contexts.get(&ptr).unwrap();
        let read_context_fun = &context_fun.disassembly_read;
        quote! {#instance.#space_fun().#read_context_fun()}
    }
    pub fn build_context_disassembly_write_call(
        &self,
        instance: impl ToTokens,
        context: &sleigh_rs::Context,
        value: impl ToTokens,
    ) -> TokenStream {
        let space = context.varnode().space();
        let space_trait = self.spaces.get(&space.element_ptr()).unwrap();
        let space_fun = &space_trait.function_mut.as_ref().unwrap();
        let ptr: *const _ = context;
        let context_fun = &space_trait.type_trait.contexts.get(&ptr).unwrap();
        let (_raw, write_context_fun, __exec) =
            &context_fun.write.as_ref().unwrap();
        quote! {#instance.#space_fun().#write_context_fun(#value)}
    }
}

impl ToTokens for SpacesTrait {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = &self.name;
        let spaces = self.spaces.values().map(|memory_space| {
            let name = &memory_space.type_name;
            let space_trait = &memory_space.type_trait.name;
            let space_function = &memory_space.function;
            let space_function_mut =
                memory_space.function_mut.as_ref().map(|fun| {
                    quote! {
                        fn #fun(&mut self) -> &mut Self::#name;
                    }
                });
            quote! {
                type #name: #space_trait;
                fn #space_function(&self) -> &Self::#name;
                #space_function_mut
            }
        });
        tokens.extend(quote! {
            pub trait #name {
                #(#spaces)*
            }
        })
    }
}

#[derive(Debug, Clone)]
pub struct SpaceStruct {
    //struct name
    pub name: Ident,
    //function to read from arbitrary address
    //read: Ident,
    //function to write to arbitrary address
    //write: Option<Ident>,
    pub space: GlobalAnonReference<sleigh_rs::Space>,
    pub space_trait: Rc<SpaceTrait>,
    pub chunks: Vec<MemoryChunk>,
    pub read: Rc<MemoryAccessTrait>,
    pub write: Option<Rc<MemoryAccessTrait>>,
}

impl SpaceStruct {
    fn new(
        space: &GlobalElement<sleigh_rs::Space>,
        space_trait: Rc<SpaceTrait>,
        varnodes: impl Iterator<Item = ChunkBytes>,
        read: &Rc<MemoryAccessTrait>,
        write: &Rc<MemoryAccessTrait>,
    ) -> Rc<Self> {
        let name = format_ident!("Context{}Struct", from_sleigh(space.name()));
        let chunks = chunks_from_varnodes(varnodes);
        Rc::new(Self {
            name,
            space: space.reference(),
            space_trait,
            chunks,
            read: Rc::clone(read),
            write: space.can_write().then(|| Rc::clone(write)),
        })
    }
    fn gen_read_fun_impl(&self) -> TokenStream {
        let space = self.space.element();
        let memory_read = &self.read.name;
        let struct_name = &self.name;
        let addr_associated_type = &self.read.address_type;
        //TODO use the native tipe for the addr intead of IntTypeU
        let addr_type_tmp = WorkType::new_int_bits(
            NonZeroTypeU::new(IntTypeU::BITS.into()).unwrap(),
            false,
        );
        let addr_param_inner = format_ident!("addr");
        let buf_len = format_ident!("buf_len");

        let addr_type = WorkType::new_int_bytes(space.addr_bytes(), false);
        let fun_ident = &self.read.function;
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
                    let start = #addr_param_inner - #chunk_start;
                    let end = usize::try_from(start + #buf_len).unwrap();
                    let start = usize::try_from(start).unwrap();
                    #buf_param.copy_from_slice(&self.#chunk_name[start..end]);
                }
            }
        });
        quote! {
            impl #memory_read for #struct_name {
                type #addr_associated_type = #addr_type;
                fn #fun_ident(
                    &self,
                    #addr_param: Self::#addr_associated_type,
                    #buf_param: &mut [u8],
                ) {
                    let #addr_param_inner =
                        <#addr_type_tmp>::try_from(#addr_param).unwrap();
                    let #buf_len =
                        <#addr_type_tmp>::try_from(#buf_param.len()).unwrap();
                    let #addr_end = #addr_param_inner + #buf_len;
                    match (#addr_param_inner, #addr_end) {
                        #(#chunks),*
                        _ => panic!(
                            "undefined mem {}:{}",
                            #addr_param,
                            #buf_param.len()
                        ),
                    }
                }
            }
        }
    }
    fn gen_write_fun_impl(&self) -> Option<TokenStream> {
        let write = self.write.as_ref()?;
        let space = self.space.element();
        let fun_ident = &write.function;
        let memory_write = &write.name;
        let struct_name = &self.name;
        let addr_associated_type = &write.address_type;
        //TODO use the native tipe for the addr intead of IntTypeU
        let addr_type_tmp = WorkType::new_int_bits(
            NonZeroTypeU::new(IntTypeU::BITS.into()).unwrap(),
            false,
        );
        let addr_param_inner = format_ident!("addr");
        let buf_len = format_ident!("buf_len");

        let addr_type = WorkType::new_int_bytes(space.addr_bytes(), false);
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
                    let start = #addr_param_inner - #chunk_start;
                    let end = usize::try_from(start + #buf_len).unwrap();
                    let start = usize::try_from(start).unwrap();
                    self.#chunk_name[start..end].copy_from_slice(#buf_param);
                }
            }
        });
        Some(quote! {
            impl #memory_write for #struct_name {
                type #addr_associated_type = #addr_type;
                fn #fun_ident(
                    &mut self,
                    #addr_param: Self::#addr_associated_type,
                    #buf_param: &[u8],
                ) {
                    let #addr_param_inner =
                        <#addr_type_tmp>::try_from(#addr_param).unwrap();
                    let #buf_len =
                        <#addr_type_tmp>::try_from(#buf_param.len()).unwrap();
                    let #addr_end = #addr_param_inner + #buf_len;
                    match (#addr_param_inner, #addr_end) {
                        #(#chunks),*
                        _ => panic!(
                            "undefined mem {}:{}",
                            #addr_param,
                            #buf_param.len()
                        ),
                    }
                }
            }
        })
    }
}
impl ToTokens for SpaceStruct {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = &self.name;
        let chunks = self.chunks.iter().map(MemoryChunk::struct_chunk);
        let impl_read = self.gen_read_fun_impl();
        let impl_write = self.gen_write_fun_impl();
        let space_trait_name = &self.space_trait.name;
        tokens.extend(quote! {
            #[derive(Debug, Clone, Copy)]
            pub struct #ident {
                #(#chunks),*
            }
            impl #space_trait_name for #ident {}
            #impl_read
            #impl_write
        })
    }
}

#[derive(Debug, Clone)]
pub struct SpacesStruct {
    //name of the global memory struct
    pub name: Ident,
    //spaces that this struct can read/write
    pub spaces: IndexMap<*const sleigh_rs::Space, (Ident, Rc<SpaceStruct>)>,
    pub spaces_trait: Rc<SpacesTrait>,
}

impl SpacesStruct {
    pub fn new<'a>(
        spaces_trait: Rc<SpacesTrait>,
        spaces: impl Iterator<Item = &'a Rc<SpaceStruct>>,
    ) -> Rc<Self> {
        let name = format_ident!("SpacesStruct");
        //iterator for all varnodes to this space
        let spaces = spaces
            .map(|space_struct| {
                let ptr = space_struct.space.element_ptr();
                let name =
                    format_ident!("{}", from_sleigh(space_struct.space.name()));
                (ptr, (name, Rc::clone(space_struct)))
            })
            .collect();
        Rc::new(Self {
            name,
            spaces,
            spaces_trait,
        })
    }
}

impl ToTokens for SpacesStruct {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let struct_name = &self.name;
        let trait_name = &self.spaces_trait.name;
        let struct_fields = self.spaces.values().map(|(space_field, space)| {
            let struct_name = &space.name;
            //TODO don't make it public, make a default or creator function
            quote! {pub #space_field: #struct_name}
        });
        let trait_functions = self.spaces_trait.spaces.iter().map(
            |(
                ptr,
                SpacesTraitAssociatedType {
                    type_name,
                    type_trait: _,
                    function,
                    function_mut,
                },
            )| {
                let (field_name, space_struct) = &self.spaces.get(ptr).unwrap();
                let space_struct_name = &space_struct.name;
                let function_mut = function_mut.as_ref().map(|function_mut| {
                    quote! {
                        fn #function_mut(&mut self) -> &mut Self::#type_name {
                            &mut self.#field_name
                        }
                    }
                });
                quote! {
                    type #type_name = #space_struct_name;
                    fn #function(&self) -> &Self::#type_name {
                        &self.#field_name
                    }
                    #function_mut
                }
            },
        );
        tokens.extend(quote! {
            #[derive(Debug, Clone, Copy)]
            pub struct #struct_name {
                #(#struct_fields),*
            }
            impl #trait_name for #struct_name {
                #(#trait_functions)*
            }
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MemoryChunk {
    offset: IntTypeU,
    len: NonZeroTypeU,
    //struct name
    ident: Ident,
}

impl MemoryChunk {
    fn new(offset: IntTypeU, len: NonZeroTypeU) -> Self {
        let ident = format_ident!("chunk_0x{:x}", offset);
        Self { offset, len, ident }
    }
    pub fn bytes_len(&self) -> NonZeroTypeU {
        self.len
    }
    pub fn addr_start(&self) -> IntTypeU {
        self.offset
    }
    pub fn addr_end(&self) -> IntTypeU {
        self.offset + self.len.get()
    }
    pub fn ident(&self) -> &Ident {
        &self.ident
    }
    pub fn struct_chunk(&self) -> TokenStream {
        let ident = self.ident();
        let len = self.bytes_len().get();
        //TODO don't make it public, make a default or creator function
        quote! { pub #ident: [u8; #len as usize] }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct ChunkBytes {
    offset: IntTypeU,
    len: NonZeroTypeU,
}
fn chunks_from_varnodes(
    chunks: impl Iterator<Item = ChunkBytes>,
) -> Vec<MemoryChunk> {
    let mut chunks: Vec<ChunkBytes> = chunks.collect();
    chunks.sort_by(|a, b| {
        //sort first by:
        // * addr (lower address first)
        // * size (bigger first)
        match a.offset.cmp(&b.offset) {
            std::cmp::Ordering::Equal => b.len.cmp(&a.len),
            x => x,
        }
    });
    let mut iter = chunks.into_iter();
    let mut acc = match iter.next() {
        Some(x) => x,
        None => return vec![],
    };
    let mut chunks: Vec<MemoryChunk> = iter
        .filter_map(|chunk| {
            let acc_end = acc.offset + acc.len.get();
            if chunk.offset > acc_end {
                //can't merge, return acc and start a new one
                let ret = acc.clone();
                acc = chunk;
                return Some(MemoryChunk::new(ret.offset, ret.len));
            } else {
                //merge with the acc, AKA extend acc len, if bigger
                let chunk_end = chunk.offset + chunk.len.get();
                let chunk_len = chunk_end.max(acc_end) - acc.offset;
                assert!(chunk_len != 0);
                acc.len = NonZeroTypeU::new(chunk_len).unwrap();
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
