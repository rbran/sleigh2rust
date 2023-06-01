use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};

use super::{Disassembler, WorkType, DISASSEMBLY_WORK_TYPE};

pub const DISPLAY_WORK_TYPE: WorkType = DISASSEMBLY_WORK_TYPE;
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
                #number_var(bool, #number_type),
            }
            impl core::fmt::Display for #name {
                fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                    match self {
                        Self::Literal(lit) => lit.fmt(f),
                        Self::Register(reg) => reg.fmt(f),
                        Self::Number(hex, value) => match (*hex, value.is_negative()) {
                            (true, true) => write!(f, "-0x{:x}", value.abs()),
                            (true, false) => write!(f, "0x{:x}", value),
                            (false, _) => value.fmt(f),
                        },
                    }
                }
            }
        })
    }
}
