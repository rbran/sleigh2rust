use std::rc::Rc;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};

use super::{RegistersEnum, WorkType};

pub const DISPLAY_WORK_TYPE: WorkType = super::DISASSEMBLY_WORK_TYPE;
#[derive(Debug, Clone)]
pub struct DisplayElement {
    pub name: Ident,
    pub literal_var: Ident,
    pub register_var: Ident,
    pub number_var: Ident,
    pub registers: Rc<RegistersEnum>,
}
impl DisplayElement {
    pub fn new(name: Ident, registers: Rc<RegistersEnum>) -> Rc<Self> {
        Rc::new(Self {
            name,
            registers,
            literal_var: format_ident!("Literal"),
            register_var: format_ident!("Register"),
            number_var: format_ident!("Number"),
        })
    }
}

impl ToTokens for DisplayElement {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            name,
            registers,
            literal_var,
            register_var,
            number_var,
        } = self;
        let registers = registers.name();
        let number_type = WorkType::NUMBER_SUPER_SIGNED;
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
