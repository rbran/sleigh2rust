use std::rc::Rc;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use sleigh_rs::IntTypeS;

use super::{DisplayElement, RegistersEnum};

#[derive(Debug, Clone)]
pub struct Meaning<'a> {
    name: Ident,
    //TODO remove this Rc by removing RefCell from the final values in sleigh_rs
    sleigh: Rc<sleigh_rs::semantic::Meaning>,
    registers: Rc<RegistersEnum<'a>>,
    display: Rc<DisplayElement>,
}

impl<'a> Meaning<'a> {
    pub fn new(
        sleigh: &Rc<sleigh_rs::semantic::Meaning>,
        registers: Rc<RegistersEnum<'a>>,
        display: Rc<DisplayElement>,
    ) -> Self {
        let ptr: *const _ = Rc::as_ptr(sleigh);
        let name = format_ident!("meaning_{}", ptr as usize);
        Self {
            name,
            sleigh: Rc::clone(sleigh),
            registers,
            display,
        }
    }
    pub fn name(&self) -> &Ident {
        &self.name
    }
    pub fn sleigh(&self) -> &sleigh_rs::semantic::Meaning {
        &self.sleigh
    }
    pub fn generate(&self) -> TokenStream {
        match self.sleigh() {
            sleigh_rs::semantic::Meaning::Variable { size: _, vars } => {
                self.gen_variable(&vars)
            }
            sleigh_rs::semantic::Meaning::Name(literals) => {
                self.gen_name(&literals)
            }
            sleigh_rs::semantic::Meaning::Value(values) => {
                self.gen_value(&values)
            }
        }
    }
    fn gen_name(&self, literal: &[Option<String>]) -> TokenStream {
        let name = self.name();
        let num = format_ident!("value");
        let display_element = self.display.name();
        let ele_num = literal
            .iter()
            .enumerate()
            .filter_map(|(i, x)| x.as_ref().map(|_| i));
        let ele_value =
            literal.iter().enumerate().filter_map(|(_, x)| x.as_ref());
        let variant = self.display.literal_var();
        quote! {
            pub fn #name(#num: usize) -> #display_element {
                match #num {
                    #(#ele_num => #display_element::#variant(#ele_value),)*
                    _ => unreachable!("Invalid Attach Value"),
                }
            }
        }
    }
    fn gen_value(&self, values: &[Option<IntTypeS>]) -> TokenStream {
        let name = self.name();
        let num = format_ident!("value");
        let display_element = self.display.name();
        let ele_num = values
            .iter()
            .enumerate()
            .filter_map(|(i, x)| x.as_ref().map(|_| i));
        let ele_value =
            values.iter().enumerate().filter_map(|(_, x)| x.as_ref());
        //if all positive, the unsiged, otherwise signed
        let all_positive =
            values.iter().filter_map(|x| x.as_ref()).all(|x| *x > 0);
        let variant = if all_positive {
            self.display.unsigned_var()
        } else {
            self.display.signed_var()
        };
        quote! {
            pub fn #name(#num: usize) -> #display_element {
                match #num {
                    #(#ele_num => #display_element::#variant(true, #ele_value),)*
                    _ => unreachable!("Invalid Attach Value"),
                }
            }
        }
    }
    fn gen_variable(
        &self,
        values: &[Option<Rc<sleigh_rs::Varnode>>],
    ) -> TokenStream {
        let name = self.name();
        let num = format_ident!("value");
        let display_element = self.display.name();
        let ele_num = values
            .iter()
            .enumerate()
            .filter_map(|(i, x)| x.as_ref().map(|_| i));
        let ele_value = values
            .iter()
            .enumerate()
            .filter_map(|(_, x)| x.as_ref())
            .map(|x| {
                let regs = self.registers.name();
                let variant = &self.registers.register(x).as_ref().unwrap().0;
                quote! { #regs::#variant }
            });
        let variant = self.display.register_var();
        quote! {
            pub fn #name(#num: usize) -> #display_element {
                match #num {
                    #(#ele_num => #display_element::#variant(#ele_value),)*
                    _ => unreachable!("Invalid Attach Value"),
                }
            }
        }
    }
}
