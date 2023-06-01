use proc_macro2::{Ident, TokenStream};

use quote::{format_ident, quote, ToTokens};

use super::{
    ContextMemory, DisplayElement, Meanings, RegistersEnum, TokenFieldFunction,
    TokenFieldFunctions, WorkType,
};

mod table;
pub use table::*;

mod constructor;
pub use constructor::*;

pub struct Disassembler {
    pub debug: bool,
    //enum with all the registers used (or possibly used) by the to display
    pub registers: RegistersEnum,
    //all the interger -> interger/name/register translations,
    //AKA `attach values/names/variables`
    pub meanings: Meanings,
    //all possible display elements: Literal/Register/Value
    pub display: DisplayElement,
    //all tables, that will implement parser/disassembly/display
    pub tables: Vec<TableEnum>,
    pub token_field_functions: TokenFieldFunctions,
    pub addr_type: Ident,
    pub inst_work_type: WorkType,
    pub context: ContextMemory,
    //make sure sleigh is not droped, so the inner references are not dropped
    pub sleigh: sleigh_rs::Sleigh,
}

impl Disassembler {
    pub fn new(sleigh: sleigh_rs::Sleigh, debug: bool) -> Self {
        let registers =
            RegistersEnum::from_all(format_ident!("Register"), &sleigh);
        //TODO make sleigh to include all the meanings on the struct?
        //TODO removing the borrow in attach will simplifi this a lot
        let inst_work_type = WorkType::unsigned_from_bytes(
            sleigh.addr_bytes().get().try_into().unwrap(),
        );

        let display = DisplayElement::new(format_ident!("DisplayElement"));

        let meanings = Meanings::new(&sleigh);
        let tables: Vec<TableEnum> = sleigh
            .tables()
            .iter()
            .enumerate()
            .map(|(i, table)| {
                let table_id = sleigh_rs::TableId(i);
                TableEnum::new(&sleigh, table, table_id)
            })
            .collect();
        let field_structs = TokenFieldFunctions::new(&sleigh);
        let context =
            ContextMemory::new(&sleigh, format_ident!("ContextMemory"));
        Self {
            addr_type: format_ident!("AddrType"),
            display,
            registers,
            tables,
            meanings,
            inst_work_type,
            sleigh,
            context,
            debug,
            token_field_functions: field_structs,
        }
    }

    pub fn table_struct(&self, table: sleigh_rs::TableId) -> &TableEnum {
        &self.tables[table.0]
    }

    pub fn token_field_function(
        &self,
        id: sleigh_rs::TokenFieldId,
    ) -> &TokenFieldFunction {
        self.token_field_functions.read_function(&self.sleigh, id)
    }
}

impl ToTokens for Disassembler {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let display_data_type = &self.display.name;
        let instruction_table =
            self.table_struct(self.sleigh.instruction_table);
        let instruction_table_name = &instruction_table.name;
        let instruction_table_parse = &instruction_table.parse_fun;
        let instruction_table_display = &instruction_table.display_fun;
        let context_struct = &self.context.name;
        let globalset_struct = &self.context.globalset.name;

        let addr_type = &self.addr_type;
        let inst_work_type = &self.inst_work_type;
        tokens.extend(quote! {
            pub type #addr_type = #inst_work_type;
        });
        self.registers.to_tokens(tokens, self);
        self.meanings.to_tokens(tokens, self);
        self.display.to_tokens(tokens, self);
        self.token_field_functions.to_tokens(tokens, self);
        self.context.to_tokens(tokens, self);
        for tables in self.tables.iter() {
            tables.to_tokens(tokens, self);
        }
        tokens.extend(quote! {
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
