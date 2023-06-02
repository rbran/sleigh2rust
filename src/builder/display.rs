use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};

use super::{Disassembler, WorkType};

pub const DISPLAY_WORK_TYPE: WorkType =
    WorkType::new_int_bits(sleigh_rs::NumberSigned::BITS, false);
#[derive(Debug, Clone)]
pub struct DisplayElement {
    pub name: Ident,
    pub literal_var: Ident,
    pub register_var: Ident,
    pub number_var: Ident,
}
impl DisplayElement {
    pub fn new(name: Ident) -> Self {
        Self {
            name,
            literal_var: format_ident!("Literal"),
            register_var: format_ident!("Register"),
            number_var: format_ident!("Number"),
        }
    }
    pub fn to_tokens(
        &self,
        tokens: &mut TokenStream,
        disassembler: &Disassembler,
    ) {
        let Self {
            name,
            literal_var,
            register_var,
            number_var,
        } = self;
        let registers = disassembler.registers.name();
        let number_type = DISPLAY_WORK_TYPE;
        tokens.extend(quote! {
            #[derive(Clone, Copy, Debug)]
            pub enum #name {
                #literal_var(&'static str),
                #register_var(#registers),
                #number_var(bool, bool, #number_type),
            }
            impl core::fmt::Display for #name {
                fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                    match self {
                        Self::Literal(lit) => lit.fmt(f),
                        Self::Register(reg) => reg.fmt(f),
                        Self::Number(true, false, value) => {
                            write!(f, "0x{:x}", value)
                        }
                        Self::Number(true, true, value) => {
                            write!(f, "-0x{:x}", value)
                        }
                        Self::Number(false, false, value) => value.fmt(f),
                        Self::Number(false, true, value) => {
                            write!(f, "-{:x}", value)
                        }
                    }
                }
            }
        })
    }
}
