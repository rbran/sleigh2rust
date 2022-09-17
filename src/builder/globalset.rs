use std::{collections::HashMap, rc::Rc};

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use sleigh_rs::Varnode;

use super::formater::*;
use super::WorkType;

#[derive(Debug, Clone)]
pub struct GlobalSetContext {
    value_type: WorkType,
    function: Ident,
    sleigh: Rc<sleigh_rs::Varnode>,
}
const MEMORY_TRAIT_VARNODE_WRITE: &str = "set";
impl GlobalSetContext {
    pub fn new(varnode: Rc<sleigh_rs::Varnode>) -> Self {
        let function = format_ident!(
            "{}",
            &snake_case(
                [MEMORY_TRAIT_VARNODE_WRITE, &varnode.name].into_iter()
            )
        );
        let return_type = WorkType::from_varnode(&varnode);
        Self {
            function,
            value_type: return_type,
            sleigh: varnode,
        }
    }
    pub fn function(&self) -> &Ident {
        &self.function
    }
    pub fn value_type(&self) -> &WorkType {
        &self.value_type
    }
    pub fn generate(&self, addr_type: &WorkType) -> TokenStream {
        let function = &self.function;
        let value_type = &self.value_type;
        quote! {
            fn #function(&mut self, address: #addr_type, value: #value_type);
        }
    }
}

#[derive(Clone, Debug)]
pub struct GlobalSet {
    name: Ident,
    varnodes: HashMap<*const sleigh_rs::Varnode, GlobalSetContext>,
}

impl GlobalSet {
    pub fn new(name: Ident, sleigh: &sleigh_rs::Sleigh) -> Self {
        //TODO is a context in read-only memory valid?
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
