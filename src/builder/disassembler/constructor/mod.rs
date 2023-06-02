use indexmap::IndexMap;
use std::cell::RefCell;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};

use crate::builder::formater::from_sleigh;
use crate::builder::{token_field_display, DisassemblyGenerator};

use super::Disassembler;

mod disassembly;
pub use disassembly::*;

mod pattern;
pub use pattern::*;

pub struct ConstructorStruct {
    pub constructor_id: sleigh_rs::table::ConstructorId,
    pub table_id: sleigh_rs::TableId,
    //struct name
    pub struct_name: Ident,
    //variant name in the enum
    pub variant_name: Ident,
    //display function
    pub display_fun: Ident,
    pub disassembly_fun: Ident,
    pub parser_fun: Ident,
    pub table_fields: IndexMap<sleigh_rs::TableId, Ident>,
    pub ass_fields: IndexMap<sleigh_rs::TokenFieldId, Ident>,
    //TODO instead of having calc fields, have a constructor phase 1/2, with
    //the display/execution values in phase2 and values required by the
    //disassembly in phase1
    //Alternativally, having the having display call execute all the disassembly
    //as this will never be in the disassembly_pre
    //pub calc_fields: IndexMap<*const Variable, ParsedField<Rc<Variable>>>,
}
impl ConstructorStruct {
    pub fn new(
        sleigh: &sleigh_rs::Sleigh,
        table_id: sleigh_rs::TableId,
        constructor: &sleigh_rs::table::Constructor,
        constructor_id: sleigh_rs::table::ConstructorId,
        table_name: &str,
        number: usize,
    ) -> Self {
        //let mut calc_fields = IndexMap::new();
        let mut ass_fields: IndexMap<_, _> = IndexMap::new();
        //tables are always included to the struct, used or not
        let table_fields = constructor
            .pattern
            .produced_tables()
            .map(|produced_table| {
                let table = sleigh.table(produced_table.table);
                (
                    produced_table.table,
                    format_ident!("{}", from_sleigh(table.name())),
                )
            })
            .collect();

        //include on the enum all the required fields from the display
        for display in constructor.display.elements() {
            use sleigh_rs::display::DisplayElement::*;
            match display {
                Context(_) | InstStart(_) | InstNext(_) | Varnode(_)
                | Literal(_) | Space => (),
                TokenField(ass) => {
                    ass_fields.entry(*ass).or_insert_with(|| {
                        let ass = sleigh.token_field(*ass);
                        format_ident!("{}", from_sleigh(ass.name()))
                    });
                }
                // disassembly is all done durint the display call
                Disassembly(_var) => {}
                //table is added independed if it shows up on display
                Table(_display_table) => {}
            }
        }
        //TODO only add fields required by the display, not all fields required
        //by the disassembly
        for field in constructor
            .pattern
            .blocks()
            .iter()
            .flat_map(|block| match block {
                sleigh_rs::pattern::Block::And { pre, pos, .. } => {
                    pre.iter().chain(pos.iter())
                }
                sleigh_rs::pattern::Block::Or { pos, .. } => {
                    pos.iter().chain([/*LOL*/].iter())
                }
            })
            .chain(constructor.pattern.disassembly_pos_match())
        {
            use sleigh_rs::disassembly;
            match field {
                disassembly::Assertation::GlobalSet(
                    disassembly::GlobalSet { .. },
                ) => (),
                disassembly::Assertation::Assignment(
                    disassembly::Assignment { left: _, right },
                ) => {
                    let fields = right.elements().iter().filter_map(|ele| {
                        use sleigh_rs::disassembly::ExprElement::*;
                        use sleigh_rs::disassembly::ReadScope::*;
                        match ele {
                            Value {
                                value: TokenField(ass),
                                location: _,
                            } => Some(*ass),
                            _ => None,
                        }
                    });
                    for ass in fields {
                        ass_fields.entry(ass).or_insert_with(|| {
                            let ass = sleigh.token_field(ass);
                            format_ident!("{}", from_sleigh(ass.name()))
                        });
                    }
                }
            }
        }

        let struct_name =
            if let Some(mneumonic) = &constructor.display.mneumonic {
                format_ident!(
                    "{}_{}Var{}",
                    from_sleigh(mneumonic),
                    table_name,
                    number
                )
            } else {
                format_ident!("{}Var{}", table_name, number)
            };

        Self {
            variant_name: format_ident!("Var{}", number),
            struct_name,
            display_fun: format_ident!("display_extend"),
            disassembly_fun: format_ident!("disassembly"),
            parser_fun: format_ident!("parse"),
            ass_fields,
            table_fields,
            constructor_id,
            table_id,
        }
    }

    pub fn gen_display(&self, disassembler: &Disassembler) -> TokenStream {
        let Self {
            display_fun,
            struct_name: _,
            variant_name: _,
            disassembly_fun: _,
            parser_fun: _,
            constructor_id,
            table_id,
            table_fields: _,
            ass_fields: _,
        } = self;
        let display_param = format_ident!("display");
        let context_param = format_ident!("context");
        let inst_start = format_ident!("inst_start");
        let inst_next = format_ident!("inst_next");
        let global_set_param = format_ident!("global_set");
        let display_struct = &disassembler.display.name;
        let register_enum = &disassembler.registers.name;

        use sleigh_rs::display::DisplayElement as DisplayScope;
        let mut disassembly = DisassemblyDisplay {
            constructor: self,
            display_param: &display_param,
            context_param: &context_param,
            inst_start: &inst_start,
            inst_next: &inst_next,
            global_set_param: &global_set_param,
            vars: RefCell::new(IndexMap::new()),
            disassembler,
        };
        let constructor = disassembler
            .sleigh
            .table(*table_id)
            .constructor(*constructor_id);
        let mut disassembly_body: TokenStream = constructor
            .pattern
            .disassembly_vars()
            .iter()
            .enumerate()
            .map(|(i, var)| {
                disassembly
                    .new_variable(&sleigh_rs::disassembly::VariableId(i), var)
            })
            .collect();
        disassembly_body.extend(disassembly.to_token_stream());
        let add_mneumonic =
            constructor.display.mneumonic.as_ref().map(|mneumonic| {
                let display_element = &disassembler.display.name;
                let literal = &disassembler.display.literal_var;
                quote! { #display_param.push(#display_element::#literal(#mneumonic)); }
            });
        let elements: Vec<_> = constructor.display.elements().collect();
        let displays = elements
            .split_inclusive(|ele| matches!(ele, DisplayScope::Table(_)))
            .map(|eles| {
                let (ele, table) = match eles {
                    [ele @ .., DisplayScope::Table(table)] => {
                        (ele, Some(table))
                    }
                    _ => (eles, None),
                };
                let extend = (!ele.is_empty()).then(|| {
                    let display = ele.iter().map(|ele| match ele {
                        DisplayScope::Varnode(varnode) => {
                            let reg_var =
                                disassembler.registers.register(*varnode);
                            quote! {
                                <#display_struct>::Register(
                                    #register_enum::#reg_var
                                )
                            }
                        }
                        DisplayScope::Context(context) => {
                            disassembler.context.display_call(
                                disassembler,
                                &context_param,
                                *context,
                            )
                        }
                        DisplayScope::TokenField(ass) => {
                            let var_name = self.ass_fields.get(ass).unwrap();
                            let token_field =
                                disassembler.sleigh.token_field(*ass);
                            token_field_display(
                                quote! {self.#var_name},
                                token_field,
                                &disassembler.meanings,
                            )
                        }
                        DisplayScope::Disassembly(var) => {
                            let vars = disassembly.vars.borrow();
                            let var = vars.get(var).unwrap();
                            quote! {<#display_struct>::Number(true, #var)}
                        }
                        DisplayScope::Space => {
                            quote! {<#display_struct>::Literal(" ")}
                        }
                        DisplayScope::Literal(literal) => {
                            quote! {<#display_struct>::Literal(#literal)}
                        }
                        DisplayScope::Table(_) => unreachable!(),
                        DisplayScope::InstStart(_) => {
                            inst_start.to_token_stream()
                        }
                        DisplayScope::InstNext(_) => {
                            inst_next.to_token_stream()
                        }
                    });
                    let display_out_len = ele.len();
                    quote! {
                        let extend: [#display_struct; #display_out_len] = [
                            #(#display),*
                        ];
                        #display_param.extend_from_slice(&extend);
                    }
                });
                let build_table = table.map(|table_id| {
                    let field_name = self.table_fields.get(table_id).unwrap();
                    let table = disassembler.table_struct(*table_id);
                    let produced_table = constructor
                        .pattern
                        .produced_tables()
                        .find(|prod| prod.table == *table_id)
                        .unwrap();
                    let display_fun = &table.display_fun;
                    if produced_table.always {
                        quote! {
                            self.#field_name.#display_fun(
                                #display_param,
                                #context_param,
                                #inst_start,
                                #inst_next,
                                #global_set_param,
                            );
                        }
                    } else {
                        quote! {
                            self.#field_name.as_ref().map(|table| {
                                table.#display_fun(
                                    #display_param,
                                    #context_param,
                                    #inst_start,
                                    #inst_next,
                                    #global_set_param,
                                );
                            });
                        }
                    }
                });
                quote! {
                    #extend
                    #build_table
                }
            });
        let context_struct = &disassembler.context.name;
        let globalset_struct = &disassembler.context.globalset.name;
        let addr_type = &disassembler.addr_type;
        quote! {
            fn #display_fun(
                &self,
                #display_param: &mut Vec<#display_struct>,
                #context_param: &#context_struct,
                #inst_start: #addr_type,
                #inst_next: #addr_type,
                #global_set_param: &mut #globalset_struct,
            ) {
                #disassembly_body
                #add_mneumonic
                #(#displays)*
            }
        }
    }

    pub fn to_tokens(
        &self,
        tokens: &mut TokenStream,
        disassembler: &Disassembler,
    ) {
        let Self {
            struct_name,
            table_fields,
            ass_fields,
            parser_fun,
            variant_name: _,
            display_fun: _,
            disassembly_fun: _,
            constructor_id,
            table_id,
        } = self;
        let constructor = disassembler
            .sleigh
            .table(*table_id)
            .constructor(*constructor_id);
        let doc = format!("Constructor at {}", &constructor.location);
        let ass_fields = ass_fields.iter().map(|(field_id, name)| {
            let data_type =
                &disassembler.token_field_function(*field_id).read_type;
            quote! { #name: #data_type }
        });
        let table_fields = table_fields.iter().map(|(table_id, name)| {
            let produced_table = constructor
                .pattern
                .produced_tables()
                .find(|produced| produced.table == *table_id)
                .unwrap();
            let table_struct_name = &disassembler.table_struct(*table_id).name;
            let mut table_data = table_struct_name.into_token_stream();
            if produced_table.recursive {
                table_data = quote! {Box<#table_data>};
            }
            if !produced_table.always {
                table_data = quote! {Option<#table_data>};
            }
            quote! { #name: #table_data }
        });
        let display_impl = self.gen_display(disassembler);
        let parser_function =
            root_pattern_function(parser_fun, self, disassembler);
        tokens.extend(quote! {
            #[doc = #doc]
            #[derive(Clone, Debug)]
            struct #struct_name {
                #(#ass_fields,)*
                #(#table_fields,)*
            }
            impl #struct_name {
                #display_impl
                #parser_function
            }
        })
    }
}
