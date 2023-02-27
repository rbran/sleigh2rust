use std::rc::Rc;

use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, ToTokens};

use super::{RegistersEnum, WorkType};

pub const DISPLAY_WORK_TYPE: WorkType = super::DISASSEMBLY_WORK_TYPE;
#[derive(Debug, Clone)]
pub struct DisplayElement {
    name: Ident,
    named: Ident,
    register: Ident,
    number: Ident,
    registers: Rc<RegistersEnum>,
}
impl DisplayElement {
    pub fn new(name: Ident, registers: Rc<RegistersEnum>) -> Rc<Self> {
        let named = Ident::new("Literal", Span::call_site());
        let register = Ident::new("Register", Span::call_site());
        let number = Ident::new("Number", Span::call_site());
        Rc::new(Self {
            name,
            named,
            register,
            number,
            registers,
        })
    }
    pub fn name(&self) -> &Ident {
        &self.name
    }
    pub fn var_named(&self) -> &Ident {
        &self.named
    }
    pub fn var_register(&self) -> &Ident {
        &self.register
    }
    pub fn var_number(&self) -> &Ident {
        &self.number
    }
}
impl ToTokens for DisplayElement {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            name,
            named,
            register,
            number,
            registers,
        } = self;
        let register_enum = registers.name();
        tokens.extend(quote! {
            #[derive(Clone, Copy, Debug)]
            pub enum #name {
                #named(&'static str),
                #register(#register_enum),
                #number(bool, #DISPLAY_WORK_TYPE),
            }
            impl core::fmt::Display for #name {
                fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> std::fmt::Result {
                    match self {
                        Self::#named(lit) => lit.fmt(f),
                        Self::#register(reg) => reg.fmt(f),
                        Self::#number(hex, value) => {
                            match (*hex, value.is_negative()) {
                                (true, true) => write!(f, "-0x{:x}", value.abs()),
                                (true, false) => write!(f, "0x{:x}", value),
                                (false, _) => value.fmt(f),
                            }
                        }
                    }
                }
            }
        })
    }
}
