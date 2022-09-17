use std::collections::HashMap;
use std::rc::Rc;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

mod table;
pub use table::*;

mod constructor;
pub use constructor::*;

use super::{
    ContextStruct, ContextTrait, DisplayElement, GlobalSet, Meaning,
    RegistersEnum, TokenParser, WorkType,
};

#[derive(Debug, Clone)]
pub struct Disassembler {
    context_trait: ContextTrait,
    context_struct: ContextStruct,
    global_set: GlobalSet,
    display: Rc<DisplayElement>,
    registers: Rc<RegistersEnum>,
    tables: HashMap<*const sleigh_rs::Table, Rc<Table>>,
    meanings: HashMap<*const sleigh_rs::semantic::Meaning, Meaning>,
    token: TokenParser,
    inst_work_type: WorkType,
}
impl Disassembler {
    pub fn new(sleigh: &sleigh_rs::Sleigh) -> Self {
        let context_trait = ContextTrait::new(sleigh);
        let context_struct = ContextStruct::new(sleigh);

        let token = TokenParser::new(sleigh);
        let registers = Rc::new(RegistersEnum::from_printable(
            format_ident!("Register"),
            sleigh,
        ));
        let tables = sleigh
            .tables()
            .map(|table| {
                let ptr = Rc::as_ptr(&table);
                let table = Table::new_empty(table);
                (ptr, table)
            })
            .collect();
        //TODO make sleigh to include all the meanings on the struct?
        //TODO removing the borrow in attach will simplifi this a lot
        let varnodes_attachs = sleigh.varnodes().filter_map(|varnode| {
            use sleigh_rs::semantic::varnode;
            use sleigh_rs::semantic::varnode::VarnodeType::*;
            match &varnode.varnode_type {
                Memory(_) | BitRange(_) => None,
                Context(varnode::Context { attach, .. }) => {
                    let attach = attach.borrow();
                    attach.as_ref().map(|attach| Rc::clone(attach))
                }
            }
        });
        let assembly_attachs = sleigh.token_fields().filter_map(|ass| {
            let field = &ass.field()?;
            let attach = field.attach.borrow();
            attach.as_ref().map(|attach| Rc::clone(attach))
        });
        let display =
            Rc::new(DisplayElement::new(format_ident!("DisplayElement")));
        let mut meanings = HashMap::new();
        for attach in varnodes_attachs.chain(assembly_attachs) {
            let ptr = Rc::as_ptr(&attach);
            meanings.entry(ptr).or_insert_with(|| {
                Meaning::new(attach, Rc::clone(&registers), Rc::clone(&display))
            });
        }
        let inst_work_type =
            match &sleigh.global_scope.get("inst_start").unwrap() {
                sleigh_rs::semantic::GlobalScope::Assembly(ass) => {
                    match &ass.assembly_type {
                        sleigh_rs::semantic::assembly::AssemblyType::Start(
                            size,
                        ) => WorkType::unsigned_from_bits(
                            size.get()
                                .final_value()
                                .unwrap()
                                .get()
                                .try_into()
                                .unwrap(),
                        ),
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            };
        let global_set = GlobalSet::new(format_ident!("GlobalSet"), sleigh);
        let value = Self {
            context_trait,
            context_struct,
            global_set,
            display,
            registers,
            tables,
            meanings,
            token,
            inst_work_type,
        };
        value.tables.values().for_each(|table| {
            table.add_constructors(&value);
        });
        value
    }
    pub fn pattern(&self, table: &Rc<sleigh_rs::Table>) -> &Rc<Table> {
        self.tables.get(&Rc::as_ptr(table)).unwrap()
    }
    pub fn generate(&self) -> TokenStream {
        let meaning_solvers = self.meanings.values().map(|x| x.generate());
        let token = self.token.gen_struct();
        let context_trait_def = self.context_trait.gen_trait();
        let context_struct_def = self.context_struct.gen_struct();
        let context_struct_impl_context_trait =
            self.context_struct.impl_context_trait(&self.context_trait);
        let enums = self.tables.values().map(|table| {
            let def = table.gen_enum();
            let parse = table.gen_parse(&self);
            quote! {#def #parse}
        });
        let registers_enum = self.registers.generate();
        let display_element =
            self.display.gen_display_element_enum(self.registers.name());
        let global_set = self.global_set.generate(&self.inst_work_type);
        quote! {
            #(#meaning_solvers)*
            #token
            #global_set
            #context_trait_def
            #context_struct_def
            #context_struct_impl_context_trait
            #registers_enum
            #display_element
            #(#enums)*
        }
    }
}
