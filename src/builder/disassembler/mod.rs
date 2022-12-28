use std::rc::Rc;
use std::rc::Weak;

use indexmap::IndexMap;

use proc_macro2::Ident;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};

use super::{
    BitrangeRW, DisassemblerMemory, DisplayElement, GlobalSetTrait, Meanings,
    RegistersEnum, TokenParser, WorkType,
};

mod table;
pub use table::*;

mod constructor;
pub use constructor::*;

#[derive(Debug)]
pub struct Disassembler {
    _me: Weak<Disassembler>,
    //enum with all the registers used (or possibly used) by the to display
    pub registers: Rc<RegistersEnum>,
    //all the interger -> interger/name/register translations,
    //AKA `attach values/names/variables`
    pub meanings: Rc<Meanings>,
    //the functions that are able to convert bitranges from array into ints
    pub bitrange_rw: Rc<BitrangeRW>,
    //all structs able to parse tokens
    pub token_parser: Rc<TokenParser>,
    //a basic trait/struct that store all the context variables from all spaces
    pub memory: DisassemblerMemory,
    //abstraction for the `global_set` call that happen in disassembly_pos
    pub global_set: GlobalSetTrait,
    //all possible display elements: Literal/Register/Value
    pub display: Rc<DisplayElement>,
    //all tables, that will implement parser/disassembly/display
    pub tables: IndexMap<*const sleigh_rs::Table, Rc<TableEnum>>,
    pub addr_type: Ident,
    pub inst_work_type: WorkType,
    //make sure sleigh is not droped, so the inner references are not dropped
    pub sleigh: Rc<sleigh_rs::Sleigh>,
}
impl Disassembler {
    pub fn new(sleigh: Rc<sleigh_rs::Sleigh>) -> Rc<Self> {
        let bitrange_rw = Rc::new(BitrangeRW::new());

        let registers = Rc::new(RegistersEnum::from_all(
            format_ident!("Register"),
            &sleigh,
        ));
        let display = DisplayElement::new(
            format_ident!("DisplayElement"),
            Rc::clone(&registers),
        );
        //TODO make sleigh to include all the meanings on the struct?
        //TODO removing the borrow in attach will simplifi this a lot
        let varnodes_attachs =
            sleigh.contexts().map(|varnode| &varnode.meaning);
        let assembly_attachs = sleigh.token_fields().map(|ass| &ass.meaning);
        let meanings = Rc::new(Meanings::new(
            varnodes_attachs.chain(assembly_attachs),
            &registers,
            &display,
        ));
        let memory =
            DisassemblerMemory::new(&bitrange_rw, &display, &meanings, &sleigh);
        let token_parser = Rc::new(TokenParser::new(
            &sleigh,
            &display,
            &meanings,
            &bitrange_rw,
        ));
        let inst_work_type =
            WorkType::new_int_bytes(sleigh.addr_len_bytes(), false);
        let global_set = GlobalSetTrait::new(&sleigh);

        let me = Rc::new_cyclic(|me| {
            let tables: IndexMap<*const _, Rc<TableEnum>> = sleigh
                .tables()
                .map(|table| {
                    let ptr = table.element_ptr();
                    let table_ref = table.reference();
                    let table =
                        TableEnum::new_empty(&table_ref, Weak::clone(&me));
                    (ptr, table)
                })
                .collect();
            Self {
                _me: Weak::clone(&me),
                addr_type: format_ident!("AddrType"),
                bitrange_rw,
                memory,
                global_set,
                display,
                registers,
                tables,
                meanings,
                token_parser,
                inst_work_type,
                sleigh,
            }
        });
        me.tables
            .values()
            .for_each(|table| table.populate(&me.tables));
        me
    }
    pub fn pattern(&self, table: *const sleigh_rs::Table) -> &TableEnum {
        self.tables.get(&table).unwrap()
    }
    pub fn token_parser(&self) -> &TokenParser {
        &self.token_parser
    }
}
impl<'a> ToTokens for Disassembler {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            registers,
            meanings,
            bitrange_rw,
            token_parser,
            memory,
            global_set,
            display,
            tables,
            inst_work_type,
            sleigh,
            _me: _,
            addr_type,
        } = self;
        let tables_enum = tables.values();
        let display_enum_name = display.name();
        let context_trait_name = &memory.spaces_trait.name;
        let instruction_table = sleigh
            .global_scope
            .get("instruction")
            .unwrap()
            .table()
            .unwrap();
        let instruction_table =
            tables.get(&instruction_table.element_ptr()).unwrap();
        let instruction_table_name = &instruction_table.enum_name;
        let instruction_table_parse = &instruction_table.parse_fun;
        let instruction_table_display = &instruction_table.display_fun;
        let global_set_enum_name = global_set.trait_name();
        //.map(|table| self.gen_table_and_bitches(table));
        tokens.extend(quote! {
            pub type #addr_type = #inst_work_type;
            #bitrange_rw
            #global_set
            #memory
            #meanings
            #token_parser
            #registers
            #display
            #(#tables_enum)*
            pub fn parse_instruction<T>(
                tokens: &[u8],
                context: &mut T,
                inst_start: #inst_work_type,
                global_set: &mut impl #global_set_enum_name,
            ) -> Option<(#inst_work_type, Vec<#display_enum_name>)>
            where
                T: #context_trait_name + Clone,
            {
                let (inst_len, instruction) =
                    #instruction_table_name::#instruction_table_parse(
                        tokens,
                        context,
                        inst_start,
                )?;
                let inst_next = inst_start + inst_len;
                let mut display = vec![];
                instruction.#instruction_table_display(
                    &mut display,
                    context,
                    inst_start,
                    inst_next,
                    global_set,
                );
                Some((inst_next, display))
            }
        });
    }
}
