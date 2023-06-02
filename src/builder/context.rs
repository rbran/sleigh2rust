use proc_macro2::{Ident, TokenStream};

use quote::{format_ident, quote, ToTokens};
use sleigh_rs::NumberUnsigned;

use super::formater::from_sleigh;
use super::helper::{bitrange_from_value, rotation_and_mask_from_range};
use super::{Disassembler, ToLiteral, WorkType};

#[derive(Debug, Clone)]
pub struct ContextMemory {
    pub name: Ident,
    pub context_bytes: NumberUnsigned,
    //TODO clone taking the noflow in consideration
    contexts: Vec<ContextFunctions>,
    pub globalset: GlobalSet,
}

#[derive(Debug, Clone)]
pub struct ContextFunctions {
    pub read: Ident,
    pub write: Ident,
    pub context: sleigh_rs::ContextId,
}

#[derive(Debug, Clone)]
pub struct GlobalSet {
    pub name: Ident,
    pub set_fun: Ident,
    pub new_fun: Ident,
}

//TODO for now only allow the context to be a single varnode
impl ContextMemory {
    fn contexts(sleigh: &sleigh_rs::Sleigh) -> Vec<ContextFunctions> {
        sleigh
            .contexts()
            .iter()
            .enumerate()
            .map(|(i, context)| {
                ContextFunctions::new(sleigh_rs::ContextId(i), context)
            })
            .collect()
    }

    pub fn new(sleigh: &sleigh_rs::Sleigh, name: Ident) -> Self {
        let context_bytes = (sleigh.context_memory.memory_bits + 7) / 8;
        let contexts = Self::contexts(sleigh);
        let globalset = GlobalSet {
            name: format_ident!("GlobalSet"),
            set_fun: format_ident!("set"),
            new_fun: format_ident!("new"),
        };
        Self {
            name,
            context_bytes,
            contexts,
            globalset,
        }
    }

    pub fn context_functions(
        &self,
        context: sleigh_rs::ContextId,
    ) -> &ContextFunctions {
        &self.contexts[context.0]
    }

    pub fn read_call(
        &self,
        context: sleigh_rs::ContextId,
        instance: &Ident,
    ) -> TokenStream {
        let read_fun = &self.context_functions(context).read;
        quote! { #instance.#read_fun() }
    }

    pub fn write_call(
        &self,
        disassembler: &Disassembler,
        instance: &Ident,
        context_id: sleigh_rs::ContextId,
        value: impl ToTokens,
    ) -> TokenStream {
        let context = disassembler.sleigh.context(context_id);
        let signed = context.is_signed();
        let context = self.context_functions(context_id);
        let write_fun = &context.write;
        let value_type = context.value_type(disassembler);
        if signed {
            quote! {
                #instance.#write_fun(#value_type::try_from(#value).unwrap())
            }
        } else {
            let mask = context.value_mask(disassembler).unsuffixed();
            quote! {
                #instance.#write_fun(#value_type::try_from(#value & #mask).unwrap())
            }
        }
    }

    pub fn display_call(
        &self,
        disassembler: &Disassembler,
        instance: &Ident,
        id: sleigh_rs::ContextId,
    ) -> TokenStream {
        let read = &self.context_functions(id).read;
        let context = disassembler.sleigh.context(id);
        disassembler.meanings.display_function_call(
            context.bitrange.bits.len().get().try_into().unwrap(),
            quote! {#instance.#read()},
            context.meaning(),
        )
    }

    pub fn to_tokens(
        &self,
        tokens: &mut TokenStream,
        disassembler: &Disassembler,
    ) {
        let Self {
            name: context_name,
            context_bytes: context_len,
            contexts,
            globalset,
        } = self;
        let (context_type, impl_block) = if *context_len == 0 {
            (quote! {()}, None)
        } else {
            let context_type = WorkType::unsigned_from_bytes(
                (*context_len).try_into().unwrap(),
            );
            let functions = contexts.iter().map(|context| {
                context.functions_from_type(disassembler, context_type)
            });
            let impl_block = quote! {
                impl #context_name {
                    #(#functions)*
                }
            };
            (context_type.into_token_stream(), Some(impl_block))
        };
        let globalset = globalset.tokens(disassembler, self);
        tokens.extend(quote! {
            #[derive(Clone, Copy, Default)]
            pub struct #context_name(pub #context_type);
            #impl_block

            #globalset
        });
    }
}

impl ContextFunctions {
    fn new(
        id: sleigh_rs::ContextId,
        context: &sleigh_rs::varnode::Context,
    ) -> Self {
        Self {
            read: format_ident!("read_{}", from_sleigh(context.name())),
            write: format_ident!("write_{}", from_sleigh(context.name())),
            context: id,
        }
    }
    fn value_type(&self, disassembler: &Disassembler) -> WorkType {
        let context = disassembler.sleigh.context(self.context);
        let signed = context.is_signed();
        let len: u32 = context.bitrange.bits.len().get().try_into().unwrap();
        WorkType::new_int_bits(len, signed)
    }
    fn value_mask(&self, disassembler: &Disassembler) -> u128 {
        let context = disassembler.sleigh.context(self.context);
        let len: u32 = context.bitrange.bits.len().get().try_into().unwrap();
        u128::MAX >> (u128::BITS - len)
    }
    fn functions_from_type(
        &self,
        disassembler: &Disassembler,
        context_type: WorkType,
    ) -> TokenStream {
        let Self {
            read,
            write,
            context: _,
        } = self;
        // NOTE context bit endian is always msb (index 0) -> lsb (index len - 1)
        // the byte endian can be the native CPU, because we are using the
        // context memory mapping, and not the raw context bits from sleigh.
        // To make this work, all we need to do is invert the bit order,
        // so bit_0 => bit_(len-1) and bit_(len_1) => bit_0
        let value_type = self.value_type(disassembler);
        let context_memory =
            disassembler.sleigh.context_memory.context(self.context);
        let bit_end =
            (context_type.len_bytes() as u64 * 8) - context_memory.start();
        let bit_start =
            (context_type.len_bytes() as u64 * 8) - context_memory.end().get();
        let bits = bit_start.try_into().unwrap()..bit_end.try_into().unwrap();
        let convert = bitrange_from_value(
            bits.clone(),
            value_type,
            quote! {self.0.reverse_bits()},
        );
        let (rotation, mask) = rotation_and_mask_from_range(bits);
        let rotation = rotation.unsuffixed();
        let mask = mask.unsuffixed();
        quote! {
            pub fn #read(&self) -> #value_type {
                #convert
            }
            pub fn #write(&mut self, value: #value_type) {
                self.0 = ((self.0.reverse_bits() & !(#mask << #rotation)) | ((value as #context_type & #mask) << #rotation)).reverse_bits();
            }
        }
    }
}

impl GlobalSet {
    fn tokens(
        &self,
        disassembler: &Disassembler,
        context: &ContextMemory,
    ) -> TokenStream {
        let addr_type = &disassembler.addr_type;
        let Self {
            name,
            set_fun,
            new_fun,
        } = self;
        let context_name = &context.name;
        if context.context_bytes == 0 {
            quote! {
                #[derive(Clone, Copy, Default)]
                pub struct #name(());
                impl #name {
                    pub fn #new_fun(_: #context_name) -> Self {
                        Self(())
                    }
                    pub fn #set_fun(&mut self, _: Option<#addr_type>, _: impl FnOnce(&mut #context_name)) {
                        unreachable!()
                    }
                }
            }
        } else {
            quote! {
                #[derive(Clone)]
                pub struct #name {
                    default: #context_name,
                    branches: std::collections::HashMap<#addr_type, #context_name>,
                }
                impl #name {
                    pub fn #new_fun(default: #context_name) -> Self {
                        Self {
                            default,
                            branches: std::collections::HashMap::new(),
                        }
                    }
                    pub fn #set_fun(&mut self, address: Option<#addr_type>, set: impl FnOnce(&mut #context_name)) {
                        let Some(address) = address else { return };
                        //TODO use the noflow clone instead of simple clone.
                        let entry = self.branches.entry(address).or_insert_with(|| self.default.clone());
                        set(entry);
                    }
                }
            }
        }
    }
}
