use std::collections::HashSet;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};

use sleigh_rs::semantic::GlobalElement;
use sleigh_rs::Varnode;

use super::formater::*;

#[derive(Clone, Debug)]
pub struct RegistersEnum {
    name: Ident,
    registers: Box<[(Ident, GlobalElement<sleigh_rs::Varnode>)]>,
}

impl RegistersEnum {
    pub fn from_all(name: Ident, sleigh: &sleigh_rs::Sleigh) -> Self {
        let iter = sleigh.varnodes().cloned();
        Self::from_iterator(name, iter)
    }

    //enum from only the Registers that can be printed
    pub fn from_printable(name: Ident, sleigh: &sleigh_rs::Sleigh) -> Self {
        let mut iter_vec = Vec::new();
        let mut iter_set = HashSet::new();
        let iter_vec_ptr: *mut Vec<_> = &mut iter_vec;
        let iter_set_ptr: *mut HashSet<_> = &mut iter_set;

        let add_reg = |reg: GlobalElement<sleigh_rs::Varnode>| {
            let ptr: *const _ = reg.element_ptr();
            //iter_vec/iter_set is only used here, so this is fine
            unsafe {
                if !(*iter_set_ptr).contains(&ptr) {
                    (*iter_set_ptr).insert(ptr);
                    (*iter_vec_ptr).push(reg);
                }
            }
        };
        let add_attach = |meaning: &sleigh_rs::Meaning| match meaning {
            sleigh_rs::Meaning::Variable(variables) => {
                variables.iter().for_each(|(_i, v)| add_reg(v.element()))
            }
            sleigh_rs::Meaning::Literal(_)
            | sleigh_rs::Meaning::Name(_)
            | sleigh_rs::Meaning::Value(_, _) => (),
        };
        for table in sleigh.tables() {
            for constructor in table.constructors.iter() {
                for element in constructor.display.elements().iter() {
                    use sleigh_rs::semantic::display::DisplayScope::*;
                    match element {
                        Varnode(var) => add_reg(var.element()),
                        Context(context) => {
                            add_attach(context.element().meaning())
                        }
                        TokenField(token_field) => {
                            add_attach(token_field.element().meaning())
                        }
                        InstStart(_) | InstNext(_) | Disassembly(_)
                        | Table(_) | Literal(_) => (),
                    }
                }
            }
        }
        //TODO: from meaning, with could use the meaning to display
        Self::from_iterator(name, iter_vec.into_iter())
    }

    pub fn from_iterator(
        name: Ident,
        registers: impl Iterator<Item = GlobalElement<sleigh_rs::Varnode>>,
    ) -> Self {
        let registers = registers
            .map(|register| {
                let name = format_ident!("{}", from_sleigh(register.name()));
                (name, register)
            })
            .collect();
        Self { name, registers }
    }
    pub fn name(&self) -> &Ident {
        &self.name
    }
    pub fn registers<'a>(
        &'a self,
    ) -> impl Iterator<Item = &(Ident, GlobalElement<sleigh_rs::Varnode>)> + 'a
    {
        self.registers.iter()
    }
    pub fn register(&self, register_ptr: *const Varnode) -> Option<&Ident> {
        self.registers()
            .find(|(_name, register)| register.element_ptr() == register_ptr)
            .map(|(name, _register)| name)
    }
}

impl ToTokens for RegistersEnum {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = self.name();
        let elements_names = self.registers().map(|(name, _reg)| name);
        let elements_names2 = self.registers().map(|(name, _reg)| name);
        let elements_display = self.registers().map(|(_name, reg)| reg.name());
        tokens.extend(quote! {
            #[derive(Clone, Copy, Debug)]
            pub enum #name {
                #(#elements_names),*
            }
            impl core::fmt::Display for #name {
                fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> std::fmt::Result {
                    match self {
                        #(Self::#elements_names2 => write!(f, #elements_display),)*
                    }
                }
            }
        })
    }
}
