use indexmap::IndexMap;

use proc_macro2::{Ident, TokenStream};
use quote::ToTokens;
use quote::{format_ident, quote};
use sleigh_rs::semantic::GlobalAnonReference;
use sleigh_rs::semantic::GlobalElement;

use super::formater::*;
use super::WorkType;

#[derive(Debug, Clone)]
pub struct GlobalSetContext {
    function: Ident,
    _sleigh: GlobalAnonReference<sleigh_rs::Context>,
}
impl GlobalSetContext {
    pub fn new(context: &GlobalElement<sleigh_rs::Context>) -> Self {
        let function = format_ident!("set_{}", from_sleigh(context.name()));
        Self {
            function,
            _sleigh: context.reference(),
        }
    }
    pub fn function(&self) -> &Ident {
        &self.function
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
            let value_type = WorkType::int_type(true);
            quote! {
                fn #function(
                    &mut self,
                    address: Option<#addr_type>,
                    value: #value_type,
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
