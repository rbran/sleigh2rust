use std::collections::HashSet;
use std::rc::Rc;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use sleigh_rs::Varnode;

use super::formater::*;

#[derive(Clone, Debug)]
pub struct RegistersEnum<'a> {
    name: Ident,
    registers: Vec<(Ident, &'a Varnode)>,
}

impl<'a> RegistersEnum<'a> {
    pub fn new_empty(name: Ident) -> Self {
        Self {
            name,
            registers: Vec::new(),
        }
    }
    pub fn from_all(name: Ident, sleigh: &'a sleigh_rs::Sleigh) -> Self {
        let iter = sleigh.varnodes().map(|x| x.as_ref());
        Self::from_iterator(name, iter)
    }
    pub fn from_context(name: Ident, sleigh: &'a sleigh_rs::Sleigh) -> Self {
        let iter = sleigh
            .varnodes()
            .filter(|varnode| varnode.context().is_some())
            .map(|x| x.as_ref());
        Self::from_iterator(name, iter)
    }

    //enum from only the Registers that can be printed
    pub fn from_printable(name: Ident, sleigh: &'a sleigh_rs::Sleigh) -> Self {
        let mut iter_vec = Vec::new();
        let mut iter_set = HashSet::new();
        let iter_vec_ptr: *mut Vec<_> = &mut iter_vec;
        let iter_set_ptr: *mut HashSet<_> = &mut iter_set;

        let add_reg = |reg: &'a sleigh_rs::Varnode| {
            let ptr: *const _ = reg;
            //iter_vec/iter_set is only used here, so this is fine
            unsafe {
                if !(*iter_set_ptr).contains(&ptr) {
                    (*iter_set_ptr).insert(ptr);
                    (*iter_vec_ptr).push(reg);
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
                    let var = sleigh
                        .global_scope
                        .get(&var.name)
                        .unwrap()
                        .unwrap_varnode()
                        .unwrap();
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
        //TODO: from meaning, with could use the meaning to display
        Self::from_iterator(name, iter_vec.into_iter())
    }

    pub fn from_iterator(
        name: Ident,
        registers: impl Iterator<Item = &'a Varnode> + 'a,
    ) -> Self {
        let registers = registers
            .map(|register| {
                let name =
                    format_ident!("{}", from_sleigh(register.name.as_ref()));
                (name, register)
            })
            .collect();
        Self { name, registers }
    }
    pub fn name(&self) -> &Ident {
        &self.name
    }
    pub fn registers(&self) -> impl Iterator<Item = &(Ident, &'a Varnode)> {
        self.registers.iter()
    }
    pub fn register(
        &self,
        register: &'a Varnode,
    ) -> Option<&(Ident, &'a Varnode)> {
        let ptr: *const Varnode = register;
        self.registers()
            .find(|(_name, register)| *register as *const Varnode == ptr)
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
    pub fn generate_impl_display(&self) -> TokenStream {
        let name = self.name();
        let elements = self.registers().map(|(name, _reg)| name);
        let elements_display = self.registers().map(|(_name, reg)| &reg.name);
        quote! {
            impl core::fmt::Display for #name {
                fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> std::fmt::Result {
                    match self {
                        #(Self::#elements => write!(f, #elements_display),)*
                    }
                }
            }
        }
    }
}
