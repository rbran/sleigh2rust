use std::rc::Weak;

use indexmap::IndexMap;

use proc_macro2::{Ident, TokenStream};

use quote::{format_ident, quote, ToTokens};

use sleigh_rs::{
    GlobalAnonReference, GlobalElement, NonZeroTypeU, NumberUnsigned,
};

use super::formater::from_sleigh;
use super::helper::{bitrange_from_value, rotation_and_mask_from_range};
use super::{Disassembler, ToLiteral, WorkType};

#[derive(Debug, Clone)]
pub struct ContextMemory {
    pub name: Ident,
    pub context_len: NumberUnsigned,
    //TODO clone taking the noflow in consideration
    pub contexts: IndexMap<*const sleigh_rs::Context, ContextFunctions>,
    pub globalset: GlobalSet,
    disassembler: Weak<Disassembler>,
}

#[derive(Debug, Clone)]
pub struct ContextFunctions {
    pub read: Ident,
    pub write: Ident,
    pub display: Ident,
    pub context: GlobalAnonReference<sleigh_rs::Context>,
}

#[derive(Debug, Clone)]
pub struct GlobalSet {
    pub name: Ident,
    pub set_fun: Ident,
    pub new_fun: Ident,
}

//TODO for now only allow the context to be a single varnode
impl ContextMemory {
    fn contexts(
        sleigh: &sleigh_rs::Sleigh,
    ) -> IndexMap<*const sleigh_rs::Context, ContextFunctions> {
        sleigh
            .global_scope
            .values()
            .filter_map(sleigh_rs::GlobalScope::context)
            .map(|context| {
                (context.element_ptr(), ContextFunctions::new(context))
            })
            .collect()
    }

    pub fn new(
        disassembler: Weak<Disassembler>,
        sleigh: &sleigh_rs::Sleigh,
        name: Ident,
    ) -> Self {
        let context_len = sleigh_rs::Sleigh::context_len(sleigh.contexts())
            .map(NonZeroTypeU::get)
            .unwrap_or(0);
        let contexts = Self::contexts(sleigh);
        let globalset = GlobalSet {
            name: format_ident!("GlobalSet"),
            set_fun: format_ident!("set"),
            new_fun: format_ident!("new"),
        };
        Self {
            name,
            disassembler,
            context_len,
            contexts,
            globalset,
        }
    }

    pub fn read_call(
        &self,
        context: &sleigh_rs::Context,
        instance: &Ident,
    ) -> TokenStream {
        let ptr: *const _ = context;
        let read_fun = &self.contexts.get(&ptr).unwrap().read;
        quote! { #instance.#read_fun() }
    }

    pub fn write_call(
        &self,
        context: &sleigh_rs::Context,
        instance: &Ident,
        value: impl ToTokens,
    ) -> TokenStream {
        let signed = context.meaning().is_signed();
        let ptr: *const _ = context;
        let context = self.contexts.get(&ptr).unwrap();
        let write_fun = &context.write;
        let value_type = context.value_type();
        if signed {
            quote! {
                #instance.#write_fun(#value_type::try_from(#value).unwrap())
            }
        } else {
            let mask = context.value_mask().unsuffixed();
            quote! {
                #instance.#write_fun(#value_type::try_from(#value & #mask).unwrap())
            }
        }
    }

    pub fn display_call(
        &self,
        context: &sleigh_rs::Context,
        instance: &Ident,
    ) -> TokenStream {
        let ptr: *const _ = context;
        let display_fun = &self.contexts.get(&ptr).unwrap().display;
        quote! { #instance.#display_fun() }
    }
}

impl ContextFunctions {
    fn new(context: &GlobalElement<sleigh_rs::Context>) -> Self {
        Self {
            read: format_ident!("read_{}", from_sleigh(context.name())),
            write: format_ident!("write_{}", from_sleigh(context.name())),
            display: format_ident!("display_{}", from_sleigh(context.name())),
            context: context.reference(),
        }
    }
    fn value_type(&self) -> WorkType {
        let sleigh = self.context.element();
        let signed = sleigh.meaning().is_signed();
        let len: u32 = sleigh.range.len().get().try_into().unwrap();
        WorkType::new_int_bits(len, signed)
    }
    fn value_mask(&self) -> u128 {
        let sleigh = self.context.element();
        let len: u32 = sleigh.range.len().get().try_into().unwrap();
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
            context,
            display,
        } = self;
        let display_element = &disassembler.display_element().name;
        let context = context.element();
        let value_type = self.value_type();
        let from_xx = match disassembler.sleigh().endian {
            sleigh_rs::Endian::Little => quote! {from_le},
            sleigh_rs::Endian::Big => quote! {from_be},
        };
        let to_xx = match disassembler.sleigh().endian {
            sleigh_rs::Endian::Little => quote! {to_le},
            sleigh_rs::Endian::Big => quote! {to_be},
        };
        let read_function = match context.meaning() {
            sleigh_rs::Meaning::Literal(_)
            | sleigh_rs::Meaning::Variable(_)
            | sleigh_rs::Meaning::Name(_) => {
                let value = format_ident!("value");
                let convert = bitrange_from_value(
                    &value,
                    value_type,
                    quote! {#context_type::#from_xx(self.0)},
                    &context.range,
                    context.meaning.is_signed(),
                );
                quote! {
                    pub fn #read(&self) -> #value_type {
                        #convert
                        #value
                    }
                }
            }
            sleigh_rs::Meaning::Value(_, _) => todo!(),
        };
        let write_function = match context.meaning() {
            sleigh_rs::Meaning::Literal(_)
            | sleigh_rs::Meaning::Variable(_)
            | sleigh_rs::Meaning::Name(_) => {
                let (rotation, mask) =
                    rotation_and_mask_from_range(&context.range);
                let rotation = rotation.unsuffixed();
                let mask = mask.unsuffixed();
                quote! {
                    pub fn #write(&mut self, value: #value_type) {
                        let mut tmp = #context_type::#from_xx(self.0);
                        tmp = (tmp & !(#mask << #rotation)) | ((value as #context_type & #mask) << #rotation);
                        self.0 = tmp.#to_xx();
                    }
                }
            }
            sleigh_rs::Meaning::Value(_, _) => todo!(),
        };
        let display_call = disassembler.meanings().display_function_call(
            quote! {#context_type::#from_xx(self.0)},
            context.meaning(),
        );
        let display_function = quote! {
            pub fn #display(&self) -> #display_element {
                #display_call
            }
        };
        quote! {
            #read_function
            #write_function
            #display_function
        }
    }
}

impl GlobalSet {
    fn tokens(
        &self,
        disassembler: &Disassembler,
        context: &ContextMemory,
    ) -> TokenStream {
        let addr_type = disassembler.addr_type();
        let Self {
            name,
            set_fun,
            new_fun,
        } = self;
        let context_name = &context.name;
        if context.context_len == 0 {
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

impl ToTokens for ContextMemory {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            name: context_name,
            context_len,
            contexts,
            disassembler,
            globalset,
        } = self;
        let disassembler = disassembler.upgrade().unwrap();
        let (context_type, impl_block) = if *context_len == 0 {
            (quote! {()}, None)
        } else {
            let context_type = WorkType::unsigned_from_bytes(
                (*context_len).try_into().unwrap(),
            );
            let functions = contexts.values().map(|context| {
                context.functions_from_type(&*disassembler, context_type)
            });
            let impl_block = quote! {
                impl #context_name {
                    #(#functions)*
                }
            };
            (context_type.into_token_stream(), Some(impl_block))
        };
        let globalset = globalset.tokens(&disassembler, self);
        tokens.extend(quote! {
            #[derive(Clone, Copy, Default)]
            pub struct #context_name(pub #context_type);
            #impl_block

            #globalset
        });
    }
}
