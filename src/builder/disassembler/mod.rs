use std::rc::{Rc, Weak};

use indexmap::IndexMap;

use proc_macro2::{Ident, TokenStream};

use quote::{format_ident, quote, ToTokens};

use super::{
    ContextMemory, DisplayElement, Meanings, RegistersEnum, TokenFieldStruct,
    WorkType,
};

mod table;
pub use table::*;

mod constructor;
pub use constructor::*;

pub trait DisassemblerGlobal {
    fn addr_type(&self) -> &Ident;
    fn register(&self) -> &RegistersEnum;
    fn meanings(&self) -> &Meanings;
    fn display_element(&self) -> &DisplayElement;
    fn sleigh(&self) -> &sleigh_rs::Sleigh;
    fn table(&self, table: *const sleigh_rs::Table) -> Option<&Rc<TableEnum>>;
    fn token_field(
        &self,
        token_field: *const sleigh_rs::TokenField,
    ) -> Option<&Rc<TokenFieldStruct>>;
    fn context(&self) -> Option<&ContextMemory>;
    fn context_len(&self) -> usize {
        self.context()
            .map(|context| context.context_len())
            .unwrap_or(0)
    }
    fn context_struct(&self) -> TokenStream {
        self.context()
            .map(|context| (&context.name).into_token_stream())
            .unwrap_or_else(|| quote! {()})
    }
    fn globalset_struct(&self) -> TokenStream {
        self.context()
            .map(|context| (&context.globalset.name).into_token_stream())
            .unwrap_or_else(|| quote! {()})
    }
}

pub struct DisassemblerDebugger {
    _me: Weak<DisassemblerDebugger>,
    //enum with all the registers used (or possibly used) by the to display
    pub registers: Rc<RegistersEnum>,
    //all the interger -> interger/name/register translations,
    //AKA `attach values/names/variables`
    pub meanings: Rc<Meanings>,
    //all possible display elements: Literal/Register/Value
    pub display: Rc<DisplayElement>,
    //all tables, that will implement parser/disassembly/display
    pub tables: IndexMap<*const sleigh_rs::Table, Rc<TableEnum>>,
    pub field_structs:
        IndexMap<*const sleigh_rs::TokenField, Rc<TokenFieldStruct>>,
    pub addr_type: Ident,
    pub inst_work_type: WorkType,
    pub context: Option<ContextMemory>,
    //make sure sleigh is not droped, so the inner references are not dropped
    pub sleigh: Rc<sleigh_rs::Sleigh>,
}

impl DisassemblerGlobal for DisassemblerDebugger {
    fn addr_type(&self) -> &Ident {
        &self.addr_type
    }

    fn register(&self) -> &RegistersEnum {
        &self.registers
    }

    fn meanings(&self) -> &Meanings {
        &self.meanings
    }

    fn display_element(&self) -> &DisplayElement {
        &self.display
    }

    fn sleigh(&self) -> &sleigh_rs::Sleigh {
        &self.sleigh
    }

    fn table(&self, table: *const sleigh_rs::Table) -> Option<&Rc<TableEnum>> {
        self.tables.get(&table)
    }

    fn context(&self) -> Option<&ContextMemory> {
        self.context.as_ref()
    }

    fn token_field(
        &self,
        token_field: *const sleigh_rs::TokenField,
    ) -> Option<&Rc<TokenFieldStruct>> {
        self.field_structs.get(&token_field)
    }
}

impl DisassemblerDebugger {
    pub fn new(sleigh: Rc<sleigh_rs::Sleigh>) -> Rc<Self> {
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
            let dyn_me: Weak<dyn DisassemblerGlobal> = Weak::<Self>::clone(me);
            let assembly_attachs =
                sleigh.token_fields().map(|ass| &ass.meaning);
            let varnodes_attachs =
                sleigh.contexts().map(|varnode| &varnode.meaning);
            let meanings = Rc::new(Meanings::new(
                Weak::clone(&dyn_me),
                varnodes_attachs.chain(assembly_attachs),
            ));
            let tables: IndexMap<*const _, Rc<TableEnum>> = sleigh
                .tables()
                .map(|table| {
                    let ptr = table.element_ptr();
                    let table_ref = table.reference();
                    let table =
                        TableEnum::new_empty(&table_ref, Weak::clone(&dyn_me));
                    (ptr, table)
                })
                .collect();
            let field_structs: IndexMap<*const _, Rc<TokenFieldStruct>> =
                sleigh
                    .token_fields()
                    .map(|field| {
                        let ptr = field.element_ptr();
                        let field = Rc::new(TokenFieldStruct::new(
                            Weak::clone(&dyn_me),
                            field,
                        ));
                        (ptr, field)
                    })
                    .collect();
            let context = ContextMemory::new(
                Weak::clone(&dyn_me),
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
                field_structs,
                inst_work_type,
                sleigh,
                context,
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
}
impl<'a> ToTokens for DisassemblerDebugger {
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
            field_structs,
        } = self;
        let field_structs = field_structs.values();
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
        let context_struct = self.context_struct();
        let globalset_struct = self.globalset_struct();
        tokens.extend(quote! {
            pub type #addr_type = #inst_work_type;
            #registers
            #meanings
            #display
            #(#field_structs)*
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
