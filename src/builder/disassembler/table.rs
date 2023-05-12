use indexmap::IndexMap;
use std::cell::RefCell;
use std::rc::Rc;
use std::rc::Weak;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use sleigh_rs::GlobalAnonReference;

use super::{ConstructorStruct, Disassembler};
use crate::builder::formater::*;
use crate::builder::ToLiteral;

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
                    "Table{}",
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
        tables: &IndexMap<*const sleigh_rs::Table, Rc<TableEnum>>,
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
                    &from_sleigh(sleigh.name()),
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
        let display_struct_name = &disassembler.display_element().name;
        let variants_display_fun =
            constructors.iter().map(|con| &con.display_fun);
        let variants_parser_fun =
            constructors.iter().map(|con| &con.parser_fun);
        let variants_min_len = constructors
            .iter()
            .map(|con| con.sleigh.pattern.len().min().unsuffixed());
        let context_len =
            sleigh_rs::Sleigh::context_len(disassembler.sleigh().contexts())
                .map(|x| x.get().try_into().unwrap())
                .unwrap_or(0);

        //only verify variants if not in debug mode
        let variants_constraint = constructors.iter().map(|con| {
                (!disassembler.debug).then(|| {
                    let (context, pattern) = disassembler.sleigh().pattern_bytes(&con.sleigh).unwrap();
                    let (context_value, context_mask) =
                        context.into_iter().enumerate().fold(
                            (0u128, 0u128),
                            |(acc_value, acc_mask), (byte_num, byte)| {
                                (
                                    acc_value | ((byte.defined_value() as u128) << (byte_num * 8)),
                                    acc_mask | ((byte.defined_bits() as u128) << (byte_num * 8)),
                                )
                            },
                        );
                    //only constraint if mask != 0
                    let pattern_constraint = pattern
                        .into_iter()
                        .enumerate()
                        .filter(|(_, byte)| byte.defined_bits() != 0)
                        .map(|(i, byte)| {
                            let i = i.unsuffixed();
                            let value = byte.defined_value().unsuffixed();
                            let mask = byte.defined_bits().unsuffixed();
                            quote!{ (tokens_param[#i] & #mask) == #value}
                        });
                    let context_constraint = (context_mask != 0).then(|| {
                        let context_value = context_value.unsuffixed();
                        let context_mask = context_mask.unsuffixed();
                        quote!{ context_param.0 & #context_mask == #context_value }
                    }).into_iter();

                    context_constraint
                        .chain(pattern_constraint)
                        .fold(quote!{}, |mut acc, x| {
                            acc.extend(quote!{&& #x});
                            acc
                        })
                },
            )
        });
        let addr_type = &disassembler.addr_type();
        let context_struct = &disassembler.context().name;
        let globalset_struct = &disassembler.context().globalset.name;
        tokens.extend(quote! {
            #(#constructors_structs)*
            #[derive(Clone, Debug)]
            enum #enum_name {
                #(#variants_names_1(#variants_struct_1)),*
            }
            impl #enum_name {
                fn #display_fun(
                    &self,
                    display: &mut Vec<#display_struct_name>,
                    context: &#context_struct,
                    inst_start: #addr_type,
                    inst_next: #addr_type,
                    global_set_param: &mut #globalset_struct,
                ) {
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
                fn #parse_fun(
                    tokens_param: &[u8],
                    context_param: &mut #context_struct,
                    inst_start: #addr_type,
                ) -> Option<(#addr_type, Self)> {
                    //clone context, so we updated it only if we found the
                    //correct match variant
                    let mut context_current = context_param.clone();
                    //try to parse each of the constructors, return if success
                    #(if tokens_param.len() >= #variants_min_len #variants_constraint {
                        if let Some((inst_len, parsed)) =
                          #variants_struct_2::#variants_parser_fun(
                            tokens_param,
                            &mut context_current,
                            inst_start,
                        ) {
                            *context_param = context_current;
                            return Some((inst_len, Self::#variants_names_3(parsed)));
                        }
                    })*
                    None
                }
            }
        })
    }
}
