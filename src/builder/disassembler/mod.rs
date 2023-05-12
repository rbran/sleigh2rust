use std::rc::{Rc, Weak};

use indexmap::IndexMap;

use proc_macro2::{Ident, TokenStream};

use quote::{format_ident, quote, ToTokens};

use super::{
    ContextMemory, DisplayElement, Meanings, RegistersEnum, TokenFieldStruct,
    Tokens, WorkType,
};

mod table;
pub use table::*;

mod constructor;
pub use constructor::*;

pub struct Disassembler {
    _me: Weak<Disassembler>,
    pub debug: bool,
    //enum with all the registers used (or possibly used) by the to display
    pub registers: Rc<RegistersEnum>,
    //all the interger -> interger/name/register translations,
    //AKA `attach values/names/variables`
    pub meanings: Rc<Meanings>,
    //all possible display elements: Literal/Register/Value
    pub display: Rc<DisplayElement>,
    //all tables, that will implement parser/disassembly/display
    pub tables: IndexMap<*const sleigh_rs::Table, Rc<TableEnum>>,
    pub tokens: Tokens,
    pub addr_type: Ident,
    pub inst_work_type: WorkType,
    pub context: ContextMemory,
    //make sure sleigh is not droped, so the inner references are not dropped
    pub sleigh: Rc<sleigh_rs::Sleigh>,
}

impl Disassembler {
    pub fn new(sleigh: Rc<sleigh_rs::Sleigh>, debug: bool) -> Rc<Self> {
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
        let inst_work_type = WorkType::unsigned_from_bytes(
            sleigh.addr_len_bytes().get().try_into().unwrap(),
        );

        let me = Rc::new_cyclic(|me: &Weak<Self>| {
            let assembly_attachs =
                sleigh.token_fields().map(|ass| &ass.meaning);
            let varnodes_attachs =
                sleigh.contexts().map(|varnode| &varnode.meaning);
            let meanings = Rc::new(Meanings::new(
                Weak::clone(me),
                varnodes_attachs.chain(assembly_attachs),
            ));
            let tables: IndexMap<*const _, Rc<TableEnum>> = sleigh
                .tables()
                .map(|table| {
                    let ptr = table.element_ptr();
                    let table_ref = table.reference();
                    let table =
                        TableEnum::new_empty(&table_ref, Weak::clone(me));
                    (ptr, table)
                })
                .collect();
            let tokens = Tokens::new(Weak::clone(me), &sleigh);
            let context = ContextMemory::new(
                Weak::clone(me),
                &sleigh,
                format_ident!("ContextMemory"),
            );
            Self {
                _me: Weak::clone(&me),
                addr_type: format_ident!("AddrType"),
                display,
                registers,
                tables,
                meanings,
                tokens,
                inst_work_type,
                sleigh,
                context,
                debug,
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
    pub fn addr_type(&self) -> &Ident {
        &self.addr_type
    }

    pub fn register(&self) -> &RegistersEnum {
        &self.registers
    }

    pub fn meanings(&self) -> &Meanings {
        &self.meanings
    }

    pub fn display_element(&self) -> &DisplayElement {
        &self.display
    }

    pub fn sleigh(&self) -> &sleigh_rs::Sleigh {
        &self.sleigh
    }

    pub fn table(
        &self,
        table: *const sleigh_rs::Table,
    ) -> Option<&Rc<TableEnum>> {
        self.tables.get(&table)
    }

    pub fn context(&self) -> &ContextMemory {
        &self.context
    }

    pub fn tokens(&self) -> &Tokens {
        &self.tokens
    }
    pub fn token_field(
        &self,
        token_field: *const sleigh_rs::TokenField,
    ) -> Option<&Rc<TokenFieldStruct>> {
        self.tokens().field_structs.get(&token_field)
    }
}
impl<'a> ToTokens for Disassembler {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            registers,
            meanings,
            display,
            tables,
            inst_work_type,
            sleigh,
            _me: _,
            addr_type,
            context,
            tokens: tokens_struct,
            debug,
        } = self;
        let tables_enum = tables.values();
        let display_data_type = &display.name;
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
        let context_struct = &self.context().name;
        let globalset_struct = &self.context().globalset.name;
        tokens.extend(quote! {
            pub type #addr_type = #inst_work_type;
            #registers
            #meanings
            #display
            #tokens_struct
            #context
            #(#tables_enum)*
            pub fn parse_instruction(
                tokens: &[u8],
                context: &mut #context_struct,
                inst_start: #addr_type,
                global_set: &mut #globalset_struct,
            ) -> Option<(#inst_work_type, Vec<#display_data_type>)> {
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
