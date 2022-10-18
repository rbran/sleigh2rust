use std::collections::HashMap;
use std::rc::Rc;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

mod table;
use sleigh_rs::NonZeroTypeU;
pub use table::*;

mod constructor;
pub use constructor::*;

use super::{
    gen_token_parsers, ContextStruct, ContextTrait, DisplayElement,
    GlobalSetTrait, Meaning, RegistersEnum, TokenParser, WorkType,
};

#[derive(Debug, Clone)]
pub struct Disassembler<'a> {
    //enum with all the registers used (or possibly used) by the to display
    registers: Rc<RegistersEnum<'a>>,
    //all the interger -> interger/name/register translations,
    //AKA `attach values/names/variables`
    meanings: HashMap<*const sleigh_rs::semantic::Meaning, Meaning<'a>>,
    //all structs able to parse tokens
    token_parsers: HashMap<NonZeroTypeU, TokenParser<'a>>,
    //a trait can can read/write to all context variables from all spaces
    context_trait: ContextTrait<'a>,
    //abstraction for the `global_set` call that happen in disassembly_pos
    global_set: GlobalSetTrait<'a>,
    //a basic struct that store all the context variables from all spaces, plus
    //implments context_trait
    context_struct: ContextStruct<'a>,
    //all possible display elements: Literal/Register/Value
    display: Rc<DisplayElement>,
    //all tables, that will implement parser/disassembly/display
    tables: HashMap<*const sleigh_rs::Table, Rc<Table<'a>>>,
    inst_work_type: WorkType,
}
impl<'a> Disassembler<'a> {
    pub fn new(sleigh: &'a sleigh_rs::Sleigh) -> Self {
        let context_trait = ContextTrait::new(sleigh);
        let context_struct = ContextStruct::new(sleigh);

        let token_parsers = gen_token_parsers(sleigh)
            .map(|parser| {
                let len = parser.token_len();
                (len, parser)
            })
            .collect();
        let registers =
            Rc::new(RegistersEnum::from_all(format_ident!("Register"), sleigh));
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
                Meaning::new(
                    &attach,
                    Rc::clone(&registers),
                    Rc::clone(&display),
                )
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
        let global_set = GlobalSetTrait::new(sleigh);
        let value = Self {
            context_trait,
            context_struct,
            global_set,
            display,
            registers,
            tables,
            meanings,
            token_parsers,
            inst_work_type,
        };
        value.tables.values().for_each(|table| {
            table.add_constructors();
        });
        value
    }
    pub fn pattern(&self, table: *const sleigh_rs::Table) -> &Table<'a> {
        self.tables.get(&table).unwrap()
    }
    pub fn token_parser(&self, ass: &sleigh_rs::Assembly) -> &TokenParser {
        let len = ass.field().unwrap().token.size;
        self.token_parsers.get(&len).unwrap()
    }
    pub fn generate(&self) -> TokenStream {
        let meaning_solvers = self.meanings.values().map(|x| x.generate());
        let token_parsers = self
            .token_parsers
            .values()
            .map(TokenParser::gen_struct_and_impl);
        let context_trait_def = self.context_trait.gen_trait();
        let context_struct_def = self.context_struct.gen_struct();
        let context_struct_impl_context_trait =
            self.context_struct.impl_context_trait(&self.context_trait);
        let tables = self
            .tables
            .values()
            .map(|table| self.gen_table_and_bitches(table));
        let registers_enum = self.registers.generate();
        let registers_enum_impl_display =
            self.registers.generate_impl_display();
        let display_element =
            self.display.gen_display_element_enum(self.registers.name());
        let display_element_impl_display = self.display.gen_impl_display();
        let global_set = self.global_set.generate(&self.inst_work_type);
        quote! {
            #(#meaning_solvers)*
            #(#token_parsers)*
            #global_set
            #context_trait_def
            #context_struct_def
            #context_struct_impl_context_trait
            #registers_enum
            #registers_enum_impl_display
            #display_element
            #display_element_impl_display
            #(#tables)*
        }
    }
}
