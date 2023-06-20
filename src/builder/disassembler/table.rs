use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};

use super::{ConstructorStruct, Disassembler};
use crate::builder::formater::*;
use crate::builder::helper::PatternByte;
use crate::builder::ToLiteral;

pub struct TableEnum {
    //enum declaration ident
    pub name: Ident,
    //parse function ident
    pub parse_fun: Ident,
    //disassembly (pos) function ident
    //pub disassembly_fun: Ident,
    //display_extend function ident
    pub display_fun: Ident,
    //constructors are mapped from the sleigh_rs constructors by index
    pub constructors: Vec<ConstructorStruct>,
    pub table_id: sleigh_rs::TableId,
}

impl TableEnum {
    pub fn new(
        sleigh: &sleigh_rs::Sleigh,
        table: &sleigh_rs::table::Table,
        table_id: sleigh_rs::TableId,
    ) -> Self {
        let constructors = table
            .constructors()
            .iter()
            .enumerate()
            .map(|(index, constructor)| {
                ConstructorStruct::new(
                    sleigh,
                    table_id,
                    constructor,
                    sleigh_rs::table::ConstructorId(index),
                    &from_sleigh(table.name()),
                    index,
                )
            })
            .collect();
        //only non root tables have a disassembly function
        Self {
            name: format_ident!("Table{}", from_sleigh(table.name())),
            parse_fun: format_ident!("parse"),
            //disassembly_fun: format_ident!("disassembly"),
            display_fun: format_ident!("display_extend"),
            constructors,
            table_id,
        }
    }

    pub fn to_tokens(
        &self,
        tokens: &mut TokenStream,
        disassembler: &Disassembler,
    ) {
        let Self {
            name: enum_name,
            parse_fun,
            display_fun,
            constructors,
            table_id,
        } = self;
        let table = disassembler.sleigh.table(*table_id);
        let constructors_structs = constructors.iter();
        let constructor_enum_name_1 = constructors.iter().map(|con| &con.enum_name);
        let constructor_enum_name_2 = constructor_enum_name_1.clone();
        let constructor_struct = constructors.iter().map(|con| &con.struct_name);
        let display_struct_name = &disassembler.display.name;
        let variant_names = table.matcher_order.iter().map(|matcher| {
            &self.constructors[matcher.constructor.0].enum_name
        });
        let variant_structs = table.matcher_order.iter().map(|matcher| {
            &self.constructors[matcher.constructor.0].struct_name
        });
        let variant_display_fun = table.matcher_order.iter().map(|matcher| {
            &self.constructors[matcher.constructor.0].display_fun
        });
        let variants_parser_fun = table.matcher_order.iter().map(|matcher| {
            &self.constructors[matcher.constructor.0].parser_fun
        });
        let variants_min_len = table.matcher_order.iter().map(|matcher| {
            table
                .constructor(matcher.constructor)
                .pattern
                .len
                .min()
                .unsuffixed()
        });

        //only verify constructors byte_pattern if not in debug mode
        let variants_constraint = table.matcher_order.iter().map(|matcher| {
                (!disassembler.debug).then(|| {
                    let constructor = table.constructor(matcher.constructor);
                    let (context, token) = constructor.variant(matcher.variant);
                    let context = PatternByte::from_bit_constraints(context);
                    let token = PatternByte::from_bit_constraints(token);
                    let (context_value, context_mask) = context.into_iter().enumerate().fold(
                        (0u128, 0u128),
                        |(acc_value, acc_mask), (byte_num, byte)| {
                            (
                                acc_value | ((byte.defined_value() as u128) << (byte_num * 8)),
                                acc_mask | ((byte.defined_bits() as u128) << (byte_num * 8)),
                            )
                        },
                    );
                    //only constraint if mask != 0
                    let pattern_constraint = token
                        .into_iter()
                        .enumerate()
                        .filter(|(_, byte)| byte.defined_bits() != 0)
                        .map(|(i, byte)| {
                            let i = i.unsuffixed();
                            let value = byte.defined_value().unsuffixed();
                            let mask = byte.defined_bits().unsuffixed();
                            quote! { (tokens_param[#i] & #mask) == #value}
                        });
                    let context_constraint = (context_mask != 0)
                        .then(|| {
                            let context_value = context_value.unsuffixed();
                            let context_mask = context_mask.unsuffixed();
                            quote! { context_param.0 & #context_mask == #context_value }
                        })
                        .into_iter();

                    context_constraint
                        .chain(pattern_constraint)
                        .fold(quote! {}, |mut acc, x| {
                            acc.extend(quote! {&& #x});
                            acc
                        })
            })
        });
        let addr_type = &disassembler.addr_type;
        let context_struct = &disassembler.context.name;
        let globalset_struct = &disassembler.context.globalset.name;
        for constructor in constructors_structs {
            constructor.to_tokens(tokens, disassembler);
        }
        tokens.extend(quote! {
            #[derive(Clone, Debug)]
            enum #enum_name {
                #(#constructor_enum_name_1(#constructor_struct)),*
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
                        #(Self::#constructor_enum_name_2(x) => x.#variant_display_fun(
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
                          #variant_structs::#variants_parser_fun(
                            tokens_param,
                            &mut context_current,
                            inst_start,
                        ) {
                            *context_param = context_current;
                            return Some((inst_len, Self::#variant_names(parsed)));
                        }
                    })*
                    None
                }
            }
        })
    }
}
