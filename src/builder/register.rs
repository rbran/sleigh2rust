use std::collections::HashSet;
use std::rc::Rc;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use sleigh_rs::Varnode;

use super::formater::*;

#[derive(Clone, Debug)]
pub struct RegistersEnum {
    name: Ident,
    registers: Vec<(Ident, Rc<Varnode>)>,
}

impl RegistersEnum {
    pub fn new_empty(name: Ident) -> Self {
        Self {
            name,
            registers: Vec::new(),
        }
    }
    pub fn from_all(name: Ident, sleigh: &sleigh_rs::Sleigh) -> Self {
        let iter = sleigh.varnodes();
        Self::from_iterator(name, iter)
    }
    pub fn from_context(name: Ident, sleigh: &sleigh_rs::Sleigh) -> Self {
        let iter = sleigh
            .varnodes()
            .filter(|varnode| varnode.context().is_some());
        Self::from_iterator(name, iter)
    }

    //enum from only the Registers that can be printed
    pub fn from_printable(name: Ident, sleigh: &sleigh_rs::Sleigh) -> Self {
        let mut iter_vec = Vec::new();
        let mut iter_set = HashSet::new();
        let iter_vec_ptr: *mut Vec<_> = &mut iter_vec;
        let iter_set_ptr: *mut HashSet<_> = &mut iter_set;

        let add_reg = |reg: &Rc<sleigh_rs::Varnode>| {
            let ptr = Rc::as_ptr(reg);
            //iter_vec/iter_set is only used here, so this is fine
            unsafe {
                if !(*iter_set_ptr).contains(&ptr) {
                    (*iter_set_ptr).insert(ptr);
                    (*iter_vec_ptr).push(Rc::clone(reg));
                }
            }
        };
        let add_attach = |attach: &std::cell::RefCell<
            Option<Rc<sleigh_rs::semantic::Meaning>>,
        >|
         -> bool {
            let attach = attach.borrow();
            let attach = if let Some(attach) = attach.as_ref() {
                attach
            } else {
                return false;
            };
            let mut added = false;
            if let sleigh_rs::semantic::Meaning::Variable { size: _, vars } =
                attach.as_ref()
            {
                vars.iter().filter_map(|x| x.as_ref()).for_each(|var| {
                    added |= true;
                    add_reg(var)
                })
            }
            added
        };
        for table in sleigh.tables() {
            for constructor in table.constructors.iter() {
                for element in constructor.display.elements().iter() {
                    use sleigh_rs::semantic::assembly::AssemblyType::*;
                    use sleigh_rs::semantic::display::Element::*;
                    use sleigh_rs::semantic::varnode::VarnodeType::*;
                    match element {
                        Varnode(varnode) => match &varnode.varnode_type {
                            Context(
                                sleigh_rs::semantic::varnode::Context {
                                    attach,
                                    ..
                                },
                            ) => {
                                if !add_attach(attach) {
                                    add_reg(varnode)
                                }
                            }
                            Memory(_) | BitRange(_) => add_reg(varnode),
                        },
                        Assembly(ass) => match &ass.assembly_type {
                            Field(field) => {
                                add_attach(&field.attach);
                            }
                            Epsilon | Start(_) | Next(_) => (),
                        },
                        Disassembly(_) | Table(_) | Literal(_) => (),
                    }
                }
            }
        }
        Self::from_iterator(name, iter_vec.into_iter())
    }

    pub fn from_iterator(
        name: Ident,
        registers: impl Iterator<Item = Rc<Varnode>>,
    ) -> Self {
        let registers = registers
            .map(|register| {
                let name = format_ident!(
                    "{}",
                    upper_cammel_case(from_sleigh(register.name.as_ref()))
                );
                (name, register)
            })
            .collect();
        Self { name, registers }
    }
    pub fn name(&self) -> &Ident {
        &self.name
    }
    pub fn registers(&self) -> impl Iterator<Item = &(Ident, Rc<Varnode>)> {
        self.registers.iter()
    }
    pub fn register(
        &self,
        register: &Rc<Varnode>,
    ) -> Option<&(Ident, Rc<Varnode>)> {
        let ptr = Rc::as_ptr(register);
        self.registers()
            .find(|(_name, register)| Rc::as_ptr(&register) == ptr)
    }
    pub fn generate(&self) -> TokenStream {
        let name = self.name();
        let elements = self.registers().map(|(name, _reg)| {
            quote! { #name, }
        });
        quote! {
            #[derive(Clone, Copy, Debug)]
            pub enum #name {
                #(#elements)*
            }
        }
    }
}
