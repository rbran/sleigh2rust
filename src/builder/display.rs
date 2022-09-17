use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;

#[derive(Debug, Clone)]
pub struct DisplayElement {
    name: Ident,
    literal: Ident,
    register: Ident,
    signed: Ident,
    unsigned: Ident,
}
impl DisplayElement {
    pub fn new(name: Ident) -> Self {
        let literal = Ident::new("Literal", Span::call_site());
        let register = Ident::new("Register", Span::call_site());
        let signed = Ident::new("Signed", Span::call_site());
        let unsigned = Ident::new("Unsigned", Span::call_site());
        Self {
            name,
            literal,
            register,
            signed,
            unsigned,
        }
    }
    pub fn name(&self) -> &Ident {
        &self.name
    }
    pub fn literal_var(&self) -> &Ident {
        &self.literal
    }
    pub fn register_var(&self) -> &Ident {
        &self.register
    }
    pub fn signed_var(&self) -> &Ident {
        &self.signed
    }
    pub fn unsigned_var(&self) -> &Ident {
        &self.unsigned
    }
    pub fn gen_display_element_enum(
        &self,
        register_enum: &Ident,
    ) -> TokenStream {
        let name = &self.name;
        let literal = &self.literal;
        let register = &self.register;
        let signed = &self.signed;
        let unsigned = &self.unsigned;
        //TODO replace i64 for IntTypeS type
        quote! {
            #[derive(Clone, Copy, Debug)]
            pub enum #name {
                #literal(&'static str),
                #register(#register_enum),
                #signed(bool, i64),
                #unsigned(bool, u64),
            }
        }
    }
}
