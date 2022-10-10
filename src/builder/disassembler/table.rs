use std::cell::RefCell;
use std::rc::Rc;
use std::rc::Weak;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};

use super::{Constructor, Disassembler};
use crate::builder::formater::*;

#[derive(Debug, Clone)]
pub struct Table<'a> {
    //enum declaration ident
    name: Ident,
    //parse function ident
    parse: Ident,
    //disassembly (pos) function ident
    disassembly: Option<Ident>,
    //display_extend function ident
    display_extend: Ident,
    //constructors are mapped from the sleigh_rs constructors by index
    constructors: RefCell<Vec<Constructor<'a>>>,
    me: Weak<Self>,
    sleigh: &'a sleigh_rs::Table,
}
impl<'a> Table<'a> {
    pub fn new_empty(sleigh: &'a sleigh_rs::Table) -> Rc<Self> {
        Rc::new_cyclic(|me| {
            let name = format_ident!("{}", from_sleigh(sleigh.name.as_ref()));
            let parse = format_ident!("parse");
            let constructors = RefCell::default();
            //only non root tables have a disassembly function
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
    pub fn add_constructors(&self) {
        let constructors = self
            .sleigh
            .constructors
            .iter()
            .enumerate()
            .map(|(i, cons)| Constructor::new(&cons, self, i))
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
    pub fn constructors(&self) -> std::cell::Ref<'_, [Constructor<'a>]> {
        std::cell::Ref::map(self.constructors.borrow(), Vec::as_slice)
    }
    pub fn sleigh(&self) -> &'a sleigh_rs::Table {
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
}
impl<'a> Disassembler<'a> {
    pub fn gen_struct_and_variants(&self, table: &Table<'a>) -> TokenStream {
        let name = &table.name;
        let constructors = table.constructors();
        let structs = constructors
            .iter()
            .map(|cons| self.gen_constructor_variant(cons));
        let variants = constructors.iter().map(|cons| {
            let variant_name = cons.variant_name();
            let struct_name = cons.struct_name();
            quote! {
                #variant_name(#struct_name)
            }
        });
        let doc = format!("Table {}", &table.sleigh().name);
        quote! {
            #(#structs)*
            #[doc = #doc]
            #[derive(Clone, Debug)]
            pub enum #name {
                #(#variants),*
            }
        }
    }
    pub fn gen_display_extend(&self, table: &Table<'a>) -> TokenStream {
        let name = table.display_extend_name();
        let display_struct = self.display.name();
        let context_trait = self.context_trait.name();
        let constructors = table.constructors();
        let variants = constructors.iter().map(Constructor::variant_name);
        let displays = constructors.iter().map(Constructor::display);
        quote! {
            pub fn #name<T>(
                &self,
                display: &mut Vec<#display_struct>,
                context: &T,
            ) where T: #context_trait + Clone {
                match self {
                    #(Self::#variants(x) => x.#displays(display, context)),*
                }
            }
        }
    }
    //disassembly don't exist for root table
    pub fn gen_disassembly(&self, table: &Table<'a>) -> Option<TokenStream> {
        //if root table, return None
        let disassembly_name = table.disassembly_name()?;
        //call disassembly for each variant
        let constructors = table.constructors();
        let variants = constructors.iter().map(Constructor::variant_name);
        let inst_work_type = &self.inst_work_type;
        let global_set_enum = self.global_set.trait_name();
        let context_trait_name = self.context_trait.name();
        Some(quote! {
            fn #disassembly_name<'a, T>(
                &mut self,
                context_param: &mut T,
                inst_start: #inst_work_type,
                inst_next: #inst_work_type,
                global_set_param: &mut impl #global_set_enum,
            ) where T: #context_trait_name + Clone
            {
                match self {
                    #(Self::#variants(x) => x.disassembly(
                        context_param,
                        inst_start,
                        inst_next,
                        global_set_param,
                    )),*
                }
            }
        })
    }
    pub fn gen_parse(&self, table: &Table<'a>) -> TokenStream {
        let parse = &table.parse;
        let inst_work_type = &self.inst_work_type;
        let global_set_enum = self.global_set.trait_name();
        let context_trait_name = self.context_trait.name();
        let constructors = table.constructors();
        let variant_names = constructors.iter().map(Constructor::struct_name);
        let variant_enum_names =
            constructors.iter().map(Constructor::variant_name);
        let variant_parsers = constructors.iter().map(Constructor::parse);
        quote! {
            pub fn #parse<'a, T>(
                tokens_param: &'a [u8],
                context_param: &mut T,
                inst_start: #inst_work_type,
                global_set_param: &mut impl #global_set_enum,
            ) -> Option<(#inst_work_type, Self)>
                where T: #context_trait_name + Clone
            {
                //clone context, so we updated it only if we found the correct
                //match variant
                let mut context_current = context_param.clone();
                //try to parse each of the constructors, return if success
                #(if let Some((inst_next, parsed)) = #variant_names::#variant_parsers(
                    tokens_param,
                    &mut context_current,
                    inst_start,
                    global_set_param,
                ) {
                    *context_param = context_current;
                    return Some((inst_next, Self::#variant_enum_names(parsed)));
                })*
                None
            }
        }
    }
    pub fn gen_table_and_bitches(&self, table: &Table<'a>) -> TokenStream {
        let name = &table.name;
        //struct and parse/disassembly/display functions for all variants
        let table_struct_and_variants = self.gen_struct_and_variants(table);
        //table main parse-disassembly/display_functions
        let disassembly = self.gen_disassembly(table);
        let display_extend = self.gen_display_extend(table);
        let parse = self.gen_parse(table);
        quote! {
            #table_struct_and_variants
            impl #name {
                #display_extend
                #disassembly
                #parse
            }
        }
    }
}
