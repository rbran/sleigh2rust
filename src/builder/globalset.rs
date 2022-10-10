use std::{collections::HashMap, rc::Rc};

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use sleigh_rs::Varnode;

use super::formater::*;
use super::WorkType;

#[derive(Debug, Clone)]
pub struct GlobalSetContext<'a> {
    function: Ident,
    sleigh: &'a sleigh_rs::Varnode,
}
const MEMORY_TRAIT_VARNODE_WRITE: &str = "set";
impl<'a> GlobalSetContext<'a> {
    pub fn new(varnode: &'a sleigh_rs::Varnode) -> Self {
        let function = format_ident!("set_{}", from_sleigh(&varnode.name));
        Self {
            function,
            sleigh: varnode,
        }
    }
    pub fn function(&self) -> &Ident {
        &self.function
    }
    pub fn value_type(&self) -> WorkType {
        WorkType::from_varnode(&self.sleigh)
    }
    pub fn generate(&self, addr_type: &WorkType) -> TokenStream {
        let function = &self.function;
        let value_type = self.value_type();
        quote! {
            fn #function(&mut self, address: #addr_type, value: #value_type);
        }
    }
}

#[derive(Clone, Debug)]
pub struct GlobalSetTrait<'a> {
    name: Ident,
    varnodes: HashMap<*const sleigh_rs::Varnode, GlobalSetContext<'a>>,
}

impl<'a> GlobalSetTrait<'a> {
    pub fn new(sleigh: &'a sleigh_rs::Sleigh) -> Self {
        //TODO is a context in read-only memory valid?
        let name = format_ident!("GlobalSetTrait");
        let varnodes = sleigh
            .varnodes()
            .filter(|varnode| varnode.context().is_some())
            .map(|varnode| {
                (Rc::as_ptr(&varnode), GlobalSetContext::new(varnode))
            })
            .collect();
        Self { name, varnodes }
    }
    pub fn trait_name(&self) -> &Ident {
        &self.name
    }
    pub fn context(&self, varnode: &Rc<Varnode>) -> &GlobalSetContext {
        self.varnodes.get(&Rc::as_ptr(varnode)).unwrap()
    }
    pub fn generate(&self, addr_type: &WorkType) -> TokenStream {
        let name = self.trait_name();
        let functions =
            self.varnodes.values().map(|var| var.generate(addr_type));
        quote! {
            pub trait #name {
                #(#functions)*
            }
        }
    }
}
