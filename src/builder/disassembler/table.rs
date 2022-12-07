use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::rc::Weak;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use sleigh_rs::semantic::GlobalAnonReference;

use super::{ConstructorStruct, Disassembler};
use crate::builder::formater::*;

#[derive(Debug)]
pub struct TableEnum {
    //enum declaration ident
    pub enum_name: Ident,
    //parse function ident
    pub parse_fun: Ident,
    //disassembly (pos) function ident
    //pub disassembly_fun: Ident,
    //display_extend function ident
    pub display_fun: Ident,
    //constructors are mapped from the sleigh_rs constructors by index
    pub constructors: RefCell<Vec<ConstructorStruct>>,
    disassembler: Weak<Disassembler>,
    me: Weak<Self>,
    pub sleigh: GlobalAnonReference<sleigh_rs::Table>,
}
impl TableEnum {
    pub fn new_empty(
        sleigh: &GlobalAnonReference<sleigh_rs::Table>,
        disassembler: Weak<Disassembler>,
    ) -> Rc<Self> {
        Rc::new_cyclic(|me| {
            //only non root tables have a disassembly function
            Self {
                me: me.clone(),
                enum_name: format_ident!(
                    "{}",
                    from_sleigh(sleigh.name().as_ref())
                ),
                parse_fun: format_ident!("parse"),
                //disassembly_fun: format_ident!("disassembly"),
                display_fun: format_ident!("display_extend"),
                constructors: RefCell::new(vec![]),
                disassembler,
                sleigh: sleigh.clone(),
            }
        })
    }
    pub fn populate(
        &self,
        tables: &HashMap<*const sleigh_rs::Table, Rc<TableEnum>>,
    ) {
        let sleigh = self.sleigh.element();
        let constructors = sleigh
            .constructors
            .iter()
            .enumerate()
            .map(|(index, constructor)| {
                ConstructorStruct::new(
                    Rc::clone(constructor),
                    tables,
                    Weak::clone(&self.disassembler),
                    sleigh.name(),
                    index,
                )
            })
            .collect();
        *self.constructors.borrow_mut() = constructors;
    }
    pub fn me(&self) -> Rc<Self> {
        //I'm, therefore, I'm valid
        self.me.upgrade().unwrap()
    }
    pub fn gen_parse_call<A, B, C, D>(
        &self,
        tokens: A,
        context: B,
        inst_start: C,
        global_set: D,
    ) -> TokenStream
    where
        A: ToTokens,
        B: ToTokens,
        C: ToTokens,
        D: ToTokens,
    {
        let table_enum = &self.enum_name;
        let table_creator = &self.parse_fun;
        quote! {
            #table_enum::#table_creator(
                #tokens,
                #context,
                #inst_start,
                #global_set,
            )
        }
    }
}

impl ToTokens for TableEnum {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            enum_name,
            parse_fun,
            display_fun,
            constructors,
            me: _,
            sleigh: _,
            disassembler,
        } = self;
        let disassembler = disassembler.upgrade().unwrap();
        let constructors = constructors.borrow();
        let constructors_structs = constructors.iter();
        let variants_names_1 = constructors.iter().map(|con| &con.variant_name);
        let variants_names_2 = variants_names_1.clone();
        let variants_names_3 = variants_names_1.clone();
        let variants_struct_1 = constructors.iter().map(|con| &con.struct_name);
        let variants_struct_2 = variants_struct_1.clone();
        let display_struct_name = disassembler.display.name();
        let variants_display_fun =
            constructors.iter().map(|con| &con.display_fun);
        let variants_parser_fun = constructors.iter().map(|con| &con.parser_fun);
        let inst_work_type = &disassembler.inst_work_type;
        let global_set_enum = disassembler.global_set.trait_name();
        let context_trait_name = &disassembler.memory.spaces_trait.name;
        tokens.extend(quote! {
            #(#constructors_structs)*
            #[derive(Clone, Debug)]
            enum #enum_name {
                #(#variants_names_1(#variants_struct_1)),*
            }
            impl #enum_name {
                fn #display_fun<T>(
                    &self,
                    display: &mut Vec<#display_struct_name>,
                    context: &T,
                    inst_start: #inst_work_type,
                    inst_next: #inst_work_type,
                    global_set_param: &mut impl #global_set_enum,
                ) where T: #context_trait_name + Clone {
                    match self {
                        #(Self::#variants_names_2(x) => x.#variants_display_fun(
                              display,
                              context,
                              inst_start,
                              inst_next,
                              global_set_param,
                          )),*
                    }
                }
                fn #parse_fun<T>(
                    tokens_param: &[u8],
                    context_param: &mut T,
                    inst_start: #inst_work_type,
                ) -> Option<(#inst_work_type, Self)>
                    where T: #context_trait_name + Clone
                {
                    //clone context, so we updated it only if we found the correct
                    //match variant
                    let mut context_current = context_param.clone();
                    //try to parse each of the constructors, return if success
                    #(if let Some((inst_len, parsed)) =
                      #variants_struct_2::#variants_parser_fun(
                        tokens_param,
                        &mut context_current,
                        inst_start,
                    ) {
                        *context_param = context_current;
                        return Some((inst_len, Self::#variants_names_3(parsed)));
                    })*
                    None
                }
            }
        })
    }
}
