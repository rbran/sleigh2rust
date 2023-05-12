use indexmap::IndexMap;
use std::cell::RefCell;
use std::rc::{Rc, Weak};

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use sleigh_rs::disassembly::GlobalSet;

use crate::builder::formater::from_sleigh;
use crate::builder::{
    token_field_display, token_field_final_type, DisassemblyGenerator, WorkType,
};

use super::{Disassembler, TableEnum};

mod disassembly;
pub use disassembly::*;

mod pattern;
pub use pattern::*;

#[derive(Debug, Clone)]
pub struct ParsedField<T> {
    name: Ident,
    sleigh: T,
}
impl<T> ParsedField<T> {
    pub fn new(name: Ident, sleigh: T) -> Self {
        Self { name, sleigh }
    }
    pub fn value_type(&self) -> &T {
        &self.sleigh
    }
    pub fn name(&self) -> &Ident {
        &self.name
    }
}

impl<T> std::ops::Deref for ParsedField<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.value_type()
    }
}

#[derive(Clone)]
pub struct TableField {
    pub name: Ident,
    pub table: Rc<TableEnum>,
    pub always: bool,
    pub recursive: bool,
}

impl ToTokens for TableField {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            name,
            table,
            always,
            recursive,
        } = self;
        let mut value = table.enum_name.to_token_stream();
        if *recursive {
            value = quote! { Box<#value> };
        }
        if !*always {
            value = quote! { Option<#value> };
        }
        tokens.extend(quote! {
            #name: #value
        })
    }
}

#[derive(Clone, Debug)]
pub struct TokenFieldField {
    pub name: Ident,
    pub data_type: WorkType,
}

impl ToTokens for TokenFieldField {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { name, data_type } = self;
        tokens.extend(quote! {
            #name: #data_type
        })
    }
}

pub struct ConstructorStruct {
    pub sleigh: Rc<sleigh_rs::Constructor>,
    disassembler: Weak<Disassembler>,
    //struct name
    pub struct_name: Ident,
    //variant name in the enum
    pub variant_name: Ident,
    //display function
    pub display_fun: Ident,
    pub disassembly_fun: Ident,
    pub parser_fun: Ident,
    pub table_fields: IndexMap<*const sleigh_rs::Table, TableField>,
    pub ass_fields: IndexMap<*const sleigh_rs::TokenField, TokenFieldField>,
    //TODO instead of having calc fields, have a constructor phase 1/2, with
    //the display/execution values in phase2 and values required by the
    //disassembly in phase1
    //Alternativally, having the having display call execute all the disassembly
    //as this will never be in the disassembly_pre
    //pub calc_fields: IndexMap<*const Variable, ParsedField<Rc<Variable>>>,
}
impl ConstructorStruct {
    pub fn new(
        sleigh: Rc<sleigh_rs::Constructor>,
        tables: &IndexMap<*const sleigh_rs::Table, Rc<TableEnum>>,
        disassembler: Weak<Disassembler>,
        table_name: &str,
        number: usize,
    ) -> Self {
        let mut calc_fields = IndexMap::new();
        let mut ass_fields: IndexMap<*const sleigh_rs::TokenField, _> =
            IndexMap::new();
        //tables are always included to the struct, used or not
        let table_fields = sleigh
            .pattern
            .tables()
            .map(|field| {
                let name =
                    format_ident!("{}", from_sleigh(field.table().name()));
                let field = sleigh
                    .pattern
                    .tables()
                    .find(|prod_table| prod_table.table() == field.table())
                    .expect("Source of display field is unknown");
                let table = tables.get(&field.table().element_ptr()).unwrap();
                TableField {
                    name,
                    table: Rc::clone(table),
                    always: field.always(),
                    recursive: field.recursive(),
                }
            })
            .map(|field| (field.table.sleigh.element_ptr(), field))
            .collect();

        //include on the enum all the required fields from the display
        for display in sleigh.display.elements() {
            use sleigh_rs::display::DisplayScopeElements::*;
            match display {
                Context(_) | InstStart(_) | InstNext(_) | Varnode(_)
                | Literal(_) | Mneumonic(_) | Space => (),
                TokenField(ass) => {
                    ass_fields.entry(ass.element_ptr()).or_insert(ass);
                }
                Disassembly(var) => {
                    let ptr = Rc::as_ptr(var);
                    calc_fields.entry(ptr).or_insert_with(|| {
                        let name = format_ident!("{}", from_sleigh(var.name()));
                        ParsedField::new(name, Rc::clone(var))
                    });
                }
                //table is added independed if it shows up on display
                Table(_display_table) => (),
            }
        }
        //also include all the fields requied by the pos disassembly
        for field in sleigh
            .pattern
            .blocks()
            .iter()
            .map(|block| match block {
                sleigh_rs::Block::And { pre, pos, .. } => {
                    pre.iter().chain(pos.iter())
                }
                sleigh_rs::Block::Or { pos, .. } => {
                    pos.iter().chain([/*LOL*/].iter())
                }
            })
            .flatten()
            .chain(sleigh.pattern.disassembly_pos_match())
        {
            use sleigh_rs::disassembly;
            match field {
                disassembly::Assertation::GlobalSet(GlobalSet {
                    address: _,
                    context: _,
                }) => (),
                disassembly::Assertation::Assignment(
                    disassembly::Assignment { left: _, right },
                ) => {
                    for ele in right.elements().iter() {
                        use sleigh_rs::disassembly::ExprElement::*;
                        use sleigh_rs::disassembly::ReadScope::*;
                        match ele {
                            Value(value) => match value {
                                Context(_) | InstStart(_) | InstNext(_)
                                | Integer(_) | Local(_) => (),
                                TokenField(ass) => {
                                    ass_fields
                                        .entry(ass.element_ptr())
                                        .or_insert(ass);
                                }
                            },
                            Op(_) | OpUnary(_) => (),
                        }
                    }
                }
            }
        }
        //verify that the pattern produces those fields
        let ass_fields = ass_fields
            .into_iter()
            .map(|(k, v)| {
                let name = format_ident!("{}", from_sleigh(v.name()));
                let field = TokenFieldField {
                    name,
                    data_type: token_field_final_type(&v.element()),
                };
                (k, field)
            })
            .collect();

        let struct_name = if let Some(mneumonic) = sleigh.display.mneumonic() {
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
            sleigh,
            variant_name: format_ident!("Var{}", number),
            struct_name,
            display_fun: format_ident!("display_extend"),
            disassembly_fun: format_ident!("disassembly"),
            parser_fun: format_ident!("parse"),
            table_fields,
            ass_fields,
            disassembler,
        }
    }
    pub fn gen_match_fields<'b>(&'b self) -> impl Iterator<Item = &Ident> + 'b {
        let tables = self.table_fields.values().map(|field| &field.name);
        let ass = self.ass_fields.values().map(|field| &field.name);
        tables.chain(ass)
    }
    pub fn gen_display(&self) -> TokenStream {
        let Self {
            disassembler,
            display_fun,
            sleigh: _,
            struct_name: _,
            variant_name: _,
            disassembly_fun: _,
            table_fields: _,
            ass_fields: _,
            parser_fun: _,
        } = self;
        let disassembler = disassembler.upgrade().unwrap();
        let display_param = format_ident!("display");
        let context_param = format_ident!("context");
        let inst_start = format_ident!("inst_start");
        let inst_next = format_ident!("inst_next");
        let global_set_param = format_ident!("global_set");
        let display_struct = &disassembler.display_element().name;
        let register_enum = disassembler.register().name();

        use sleigh_rs::display::DisplayScopeElements as DisplayScope;
        let mut disassembly = DisassemblyDisplay {
            constructor: self,
            display_param: &display_param,
            context_param: &context_param,
            inst_start: &inst_start,
            inst_next: &inst_next,
            global_set_param: &global_set_param,
            vars: RefCell::new(IndexMap::new()),
        };
        let mut disassembly_body: TokenStream = self
            .sleigh
            .pattern
            .disassembly_vars()
            .iter()
            .map(|var| disassembly.new_variable(var))
            .collect();
        disassembly_body.extend(disassembly.to_token_stream());
        let elements: Vec<_> = self.sleigh.display.elements().collect();
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
                            let reg_var = disassembler
                                .register()
                                .register(varnode.element_ptr())
                                .unwrap();
                            quote! {
                                <#display_struct>::Register(
                                    #register_enum::#reg_var
                                )
                            }
                        }
                        DisplayScope::Context(context) => disassembler
                            .context()
                            .display_call(&context.element(), &context_param),
                        DisplayScope::TokenField(ass) => {
                            let ass = ass.element();
                            let var = self
                                .ass_fields
                                .get(&ass.element_ptr())
                                .unwrap();
                            let var_name = &var.name;
                            token_field_display(
                                quote! {self.#var_name},
                                &ass,
                                disassembler.meanings(),
                            )
                        }
                        DisplayScope::Disassembly(var) => {
                            let ptr = Rc::as_ptr(var);
                            let vars = disassembly.vars.borrow();
                            let var = &vars.get(&ptr).unwrap().name;
                            quote! {<#display_struct>::Number(true, #var)}
                        }
                        DisplayScope::Space => {
                            quote! {<#display_struct>::Literal(" ")}
                        }
                        DisplayScope::Literal(literal) => {
                            quote! {<#display_struct>::Literal(#literal)}
                        }
                        DisplayScope::Mneumonic(literal) => {
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
                let build_table = table.map(|table_sleigh| {
                    let field = self
                        .table_fields
                        .get(&table_sleigh.element_ptr())
                        .unwrap();
                    let field_name = &field.name;
                    let table = disassembler
                        .table(field.table.sleigh.element_ptr())
                        .unwrap();
                    let display_fun = &table.display_fun;
                    if field.always {
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
        let context_struct = &disassembler.context().name;
        let globalset_struct = &disassembler.context().globalset.name;
        let addr_type = disassembler.addr_type();
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
                #(#displays)*
            }
        }
    }
}

impl ToTokens for ConstructorStruct {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            sleigh,
            struct_name,
            table_fields,
            ass_fields,
            parser_fun,
            disassembler: _,
            variant_name: _,
            display_fun: _,
            disassembly_fun: _,
        } = self;
        let doc = format!("Constructor at {}", &sleigh.src);
        let ass_fields = ass_fields.values();
        let table_fields = table_fields.values();
        let display_impl = self.gen_display();
        let parser_function = root_pattern_function(parser_fun, self);
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
