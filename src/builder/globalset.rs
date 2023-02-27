use std::rc::{Rc, Weak};

use indexmap::IndexMap;

use proc_macro2::{Ident, TokenStream};
use quote::ToTokens;
use quote::{format_ident, quote};
use sleigh_rs::GlobalAnonReference;
use sleigh_rs::GlobalElement;

use super::{formater::*, SpacesTrait, DISASSEMBLY_WORK_TYPE};
use super::{Disassembler, WorkType};

#[derive(Debug, Clone)]
pub struct GlobalSetContext {
    function: Ident,
    sleigh: GlobalAnonReference<sleigh_rs::Context>,
}
impl GlobalSetContext {
    pub fn new(context: &GlobalElement<sleigh_rs::Context>) -> Self {
        let function = format_ident!("set_{}", from_sleigh(context.name()));
        Self {
            function,
            sleigh: context.reference(),
        }
    }
    pub fn function(&self) -> &Ident {
        &self.function
    }
}

#[derive(Debug)]
pub struct GlobalSetStruct {
    disassembler: Weak<Disassembler>,
    global_set_trait: Rc<GlobalSetTrait>,
    context_trait: Rc<SpacesTrait>,
    name: Ident,
}
impl GlobalSetStruct {
    pub fn new(
        disassembler: Weak<Disassembler>,
        global_set_trait: Rc<GlobalSetTrait>,
        context_trait: Rc<SpacesTrait>,
    ) -> Self {
        let name = format_ident!("GlobalSetDefault");
        Self {
            disassembler,
            global_set_trait,
            context_trait,
            name,
        }
    }
}
impl ToTokens for GlobalSetStruct {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            disassembler,
            name,
            context_trait,
            global_set_trait,
        } = self;
        let disassembler = disassembler.upgrade().unwrap();
        let addr_type = &disassembler.addr_type;
        let global_set_trait_name = &global_set_trait.name;
        let (gen_declaration, gen_use, struct_data) = if global_set_trait
            .varnodes
            .is_empty()
        {
            (None, None, None)
        } else {
            let gen_type = format_ident!("C");
            let context_trait_name = &context_trait.name;
            let gen_declaration = quote! {<#gen_type: #context_trait_name>};
            let gen_use = quote! {<#gen_type>};
            let struct_data = quote! {
                #gen_declaration(pub std::collections::HashMap<#addr_type, #gen_type>)
            };
            (Some(gen_declaration), Some(gen_use), Some(struct_data))
        };
        let functions = global_set_trait.varnodes.values().map(|gs_context| {
            let name = &gs_context.function;
            let context = gs_context.sleigh.element();
            let context_space = context.varnode().space();
            let context_space = self.context_trait.spaces.get(&context_space.element_ptr()).unwrap();
            let context_space_fun = context_space.function_mut.as_ref().unwrap();
            let write_context_fun = &context_space.type_trait.contexts.get(&context.element_ptr()).unwrap().write.as_ref().unwrap().1;
            quote!{
                fn #name(&mut self, inst_start: Option<#addr_type>, value: #DISASSEMBLY_WORK_TYPE) {
                    let Some(inst_start) = inst_start else {
                        return
                    };
                    self.0.entry(inst_start).or_insert_with(|| {
                        let mut context = C::default();
                        context.#context_space_fun().#write_context_fun(value).unwrap();
                        context
                    });
                }
            }
        });
        tokens.extend(quote! {
            #[derive(Default)]
            pub struct #name #struct_data;
            impl #gen_declaration #global_set_trait_name for #name #gen_use {
                #(#functions)*
            }
        })
    }
}

#[derive(Clone, Debug)]
pub struct GlobalSetTrait {
    name: Ident,
    varnodes: IndexMap<*const sleigh_rs::Context, GlobalSetContext>,
    addr_type: WorkType,
}

impl GlobalSetTrait {
    pub fn new(sleigh: &sleigh_rs::Sleigh) -> Self {
        //TODO is a context in read-only memory valid?
        let name = format_ident!("GlobalSetTrait");
        let varnodes = sleigh
            .contexts()
            .map(|context| {
                (context.element_ptr(), GlobalSetContext::new(context))
            })
            .collect();
        let addr_type = WorkType::new_int_bytes(
            sleigh.addr_len_bytes().get().try_into().unwrap(),
            false,
        );
        Self {
            name,
            varnodes,
            addr_type,
        }
    }
    pub fn trait_name(&self) -> &Ident {
        &self.name
    }
    pub fn context(
        &self,
        context: *const sleigh_rs::Context,
    ) -> &GlobalSetContext {
        self.varnodes.get(&context).unwrap()
    }
}

impl ToTokens for GlobalSetTrait {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = self.trait_name();
        let addr_type = &self.addr_type;
        let functions = self.varnodes.values().map(|var| {
            let function = &var.function;
            quote! {
                fn #function(
                    &mut self,
                    address: Option<#addr_type>,
                    value: #DISASSEMBLY_WORK_TYPE,
                );
            }
        });
        tokens.extend(quote! {
            pub trait #name {
                #(#functions)*
            }
        })
    }
}
