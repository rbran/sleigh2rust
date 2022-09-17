use std::collections::HashMap;
use std::rc::Rc;
use std::{cell::RefCell, rc::Weak};

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};

use super::{
    Constructor, ConstructorDisplay, Disassembler, DisassemblerConstructor,
};
use crate::builder::formater::{from_sleigh, snake_case, upper_cammel_case};

const TABLE_PARSE_NAME: &str = "parse";
#[derive(Debug, Clone)]
pub struct Table {
    ///enum declaration ident
    name: Ident,
    ///parse function ident
    parse: Ident,
    ///disassembly (pos) function ident
    disassembly: Option<Ident>,
    ///display_extend function ident
    display_extend: Ident,
    ///constructors are mapped from the sleigh_rs constructors by index
    constructors: RefCell<Vec<Constructor>>,
    me: Weak<Self>,
    sleigh: Rc<sleigh_rs::Table>,
}
impl Table {
    pub fn new_empty(sleigh: Rc<sleigh_rs::Table>) -> Rc<Self> {
        Rc::new_cyclic(|me| {
            let name = format_ident!(
                "{}",
                upper_cammel_case(from_sleigh(sleigh.name.as_ref()))
            );
            let parse =
                format_ident!("{}", snake_case([TABLE_PARSE_NAME].into_iter()));
            let constructors = RefCell::default();
            let disassembly =
                (!sleigh.is_root()).then(|| format_ident!("disassembly"));
            let display_extend = format_ident!("display_extend");
            Self {
                me: me.clone(),
                name,
                parse,
                disassembly,
                display_extend,
                constructors,
                sleigh,
            }
        })
    }
    pub fn me(&self) -> Rc<Self> {
        //I'm, therefore, I'm valid
        self.me.upgrade().unwrap()
    }
    pub fn add_constructors(&self, disassembly: &Disassembler) {
        let constructors = self
            .sleigh
            .constructors
            .iter()
            .enumerate()
            .map(|(i, cons)| Constructor::new(&cons, self.me(), i, disassembly))
            .collect();
        *self.constructors.borrow_mut() = constructors;
    }
    pub fn name(&self) -> &Ident {
        &self.name
    }
    pub fn raw_name(&self) -> &str {
        &self.sleigh.name
    }
    pub fn parse_name(&self) -> &Ident {
        &self.parse
    }
    pub fn disassembly_name(&self) -> Option<&Ident> {
        self.disassembly.as_ref()
    }
    pub fn display_extend_name(&self) -> &Ident {
        &self.display_extend
    }
    pub fn constructor_index(
        &self,
        index: usize,
    ) -> std::cell::Ref<'_, Constructor> {
        std::cell::Ref::map(self.constructors.borrow(), |x| {
            x.get(index).unwrap()
        })
    }
    pub fn constructors<'a>(&'a self) -> std::cell::Ref<'_, [Constructor]> {
        std::cell::Ref::map(self.constructors.borrow(), Vec::as_slice)
    }
    pub fn sleigh(&self) -> &Rc<sleigh_rs::Table> {
        &self.sleigh
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
        let table_enum = &self.name;
        let table_creator = &self.parse;
        quote! {
            #table_enum::#table_creator(
                #tokens,
                #context,
                #inst_start,
                #global_set,
            )
        }
    }
    pub fn gen_enum(&self) -> TokenStream {
        let name = &self.name;
        let variants = self.gen_variants();
        let doc = format!("Table {}", &self.sleigh.name);
        quote! {
            #[doc = #doc]
            #[derive(Clone, Copy, Debug)]
            pub enum #name {
                #variants
            }
        }
    }
    pub fn gen_variants(&self) -> TokenStream {
        let constructors = self.constructors();
        constructors
            .iter()
            .enumerate()
            .map(|(i, cons)| {
                let sleigh = self.sleigh.constructors.get(i).unwrap();
                let name = cons.name();
                let doc = format!("Constructor at {}", &sleigh.src);
                let fields = cons.gen_declare_fields();
                quote! {
                    #[doc = #doc]
                    #name {#(#fields),*}
                }
            })
            .reduce(|acc, x| quote! {#acc, #x})
            .unwrap_or_default()
    }
    pub fn gen_parse(&self, disassembler: &Disassembler) -> TokenStream {
        let name = &self.name;
        let parse = &self.parse;
        let constructors = self.constructors();
        let global_set_enum = disassembler.global_set.trait_name();
        let parsers = constructors.iter().map(|cons| {
            cons.gen_parse(
                &disassembler.token,
                &disassembler.global_set,
                &disassembler.context_trait,
                &disassembler.tables,
                &disassembler.inst_work_type,
            )
        });
        let context_trait_name = disassembler.context_trait.name();
        let inst_work_type = &disassembler.inst_work_type;
        let disassembly = self.disassembly_name().map(|name| {
            let context_param = format_ident!("context_param");
            let global_set_param = format_ident!("global_set");
            let inst_start = format_ident!("inst_start");
            let inst_next = Some(format_ident!("inst_next"));
            //pattern for the match statements of constructor
            let pattern = constructors.iter().map(|cons| {
                let name = cons.name();
                let pattern = cons.gen_match_fields();
                quote! {
                    Self::#name { #(#pattern),* }
                }
            });
            //disassembly code
            let disassembly = constructors.iter().map(|cons| {
                DisassemblerConstructor::disassembly(
                    &disassembler.context_trait,
                    &disassembler.global_set,
                    &inst_start,
                    &inst_next,
                    inst_work_type,
                    &global_set_param,
                    &context_param,
                    true,
                    &cons.ass_fields,
                    true,
                    &cons.calc_fields,
                    &mut HashMap::new(),
                    &cons.sleigh().disassembly,
                )
            });
            //call disassembly for all the sub tables
            let sub_disassembly = constructors.iter().map(|cons| {
                cons.table_fields()
                    .map(|(name, _table)| {
                        quote! {
                            #name.disassembly(
                                #context_param,
                                #inst_start,
                                #inst_next,
                                #global_set_param,
                            );
                        }
                    })
                    .collect::<TokenStream>()
            });
            quote! {
                fn #name<'a, T>(
                    &mut self,
                    #context_param: &mut T,
                    #inst_start: #inst_work_type,
                    #inst_next: #inst_work_type,
                    #global_set_param: &mut impl #global_set_enum,
                ) where T: #context_trait_name + Clone
                {
                    match self {
                        #(#pattern => {
                            #disassembly
                            #sub_disassembly
                        })*
                    }
                }
            }
        });
        let display_param = format_ident!("display");
        let context_param = format_ident!("context");
        let display_enum = &disassembler.display.name();
        let display_extend = self.display_extend_name();
        let displays = constructors.iter().enumerate().map(|(i, cons)| {
            let constructor = self.sleigh.constructors.get(i).unwrap();
            let parser = ConstructorDisplay::new(
                //disassembler,
                &display_param,
                &disassembler.display,
                &context_param,
                &disassembler.context_trait,
                &disassembler.registers,
                &disassembler.meanings,
                //&self,
                cons,
                constructor,
            );
            parser.gen_display_match()
        });
        //parse function for each variant
        let constructors = self.constructors();
        let parser_functions = constructors.iter().map(|cons| cons.parse());
        quote! {
            impl #name {
                pub fn #display_extend<T>(
                    &self,
                    #display_param: &mut Vec<#display_enum>,
                    #context_param: &T,
                ) where T: #context_trait_name + Clone {
                    match self {
                        #(#displays)*
                    }
                }
                #disassembly
                #(#parsers)*
                pub fn #parse<'a, T>(
                    tokens_param: &'a [u8],
                    context_param: &mut T,
                    inst_start: #inst_work_type,
                    global_set_param: &mut impl #global_set_enum,
                ) -> Option<(&'a [u8], Self)>
                    where T: #context_trait_name + Clone
                {
                    //try to parse each of the constructors, return if success
                    #(if let parsed @ Some(_) = Self::#parser_functions(
                        tokens_param,
                        context_param,
                        inst_start,
                        global_set_param,
                    ) {
                        return parsed;
                    })*
                    None
                }
            }
        }
    }
}
