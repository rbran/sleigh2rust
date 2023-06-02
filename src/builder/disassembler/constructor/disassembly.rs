use indexmap::IndexMap;
use std::cell::RefCell;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use sleigh_rs::disassembly::{Assertation, GlobalSet, Variable, VariableId};

use crate::builder::formater::from_sleigh;
use crate::builder::{Disassembler, DisassemblyGenerator, ToLiteral, WorkType};

use super::ConstructorStruct;

pub const DISASSEMBLY_WORK_TYPE: WorkType =
    WorkType::new_int_bits(crate::DisassemblyType::BITS, true);
pub struct DisassemblyDisplay<'a> {
    pub disassembler: &'a Disassembler,
    pub constructor: &'a ConstructorStruct,
    pub display_param: &'a Ident,
    pub context_param: &'a Ident,
    pub inst_start: &'a Ident,
    pub inst_next: &'a Ident,
    pub global_set_param: &'a Ident,
    pub vars: RefCell<IndexMap<VariableId, Ident>>,
}

impl DisassemblyDisplay<'_> {
    fn inst_start(&self) -> TokenStream {
        let inst_start = &self.inst_start;
        quote! {#DISASSEMBLY_WORK_TYPE::try_from(#inst_start).unwrap()}
    }
    fn inst_next(&self) -> TokenStream {
        let inst_next = &self.inst_next;
        quote! {#DISASSEMBLY_WORK_TYPE::try_from(#inst_next).unwrap()}
    }
    //get var name on that contains the this assembly field value
    fn ass_field(&self, ass: sleigh_rs::TokenFieldId) -> TokenStream {
        //can't create new ass fields during disassembly, all
        //fields used need to be declared on the pattern
        let field = self.constructor.ass_fields.get(&ass).unwrap();
        let token_field = self.disassembler.sleigh.token_field(ass);
        let field = self.disassembler.meanings.disassembly_function_call(
            token_field.bits.len().get().try_into().unwrap(),
            quote! {self.#field},
            token_field.meaning(),
        );
        quote! {#DISASSEMBLY_WORK_TYPE::try_from(#field).unwrap()}
    }

    fn context_field(&self, context: &sleigh_rs::ContextId) -> TokenStream {
        let read_call = self
            .disassembler
            .context
            .read_call(*context, self.context_param);
        quote! { #DISASSEMBLY_WORK_TYPE::try_from(#read_call).unwrap()}
    }
    //get var name on that contains the this assembly field value
    fn table_field(&self, table: &sleigh_rs::TableId) -> Option<TokenStream> {
        let field = self.constructor.table_fields.get(table).unwrap();
        use sleigh_rs::table::ExecutionExport;
        let table = self.disassembler.sleigh.table(*table);
        match table.export {
            ExecutionExport::Const(_len) => {
                //TODO allow table to export value with addr diff from addr_len
                //and auto convert using try_from?
                //assert_eq!(len, addr_type.len_bytes())
            }
            ExecutionExport::None
            | ExecutionExport::Value(_)
            | ExecutionExport::Reference(_)
            | ExecutionExport::Multiple(_) => return None,
        }
        Some(quote! {self.#field})
    }
}

impl ToTokens for DisassemblyDisplay<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let constructor = self
            .disassembler
            .sleigh
            .table(self.constructor.table_id)
            .constructor(self.constructor.constructor_id);
        let mut asses = constructor
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
            .chain(constructor.pattern.disassembly_pos_match());
        tokens.extend(self.disassembly(&mut asses));
    }
}

impl<'a> DisassemblyGenerator for DisassemblyDisplay<'a> {
    fn global_set(&self, global_set: &GlobalSet) -> TokenStream {
        let addr_type = &self.disassembler.addr_type;
        use sleigh_rs::disassembly::AddrScope::*;
        let address = match &global_set.address {
            Integer(value) => {
                let value = value.unsuffixed();
                quote! { Some(#value) }
            }
            Local(var) => {
                let name = self.var_name(var);
                quote! { Some(#addr_type::try_from(#name).unwrap()) }
            }
            Table(table) => {
                //TODO is None required?
                self.table_field(table)
                    .map(|table| quote! {Some(#table)})
                    .unwrap_or(quote! {None})
            }
            InstNext(_) => {
                let name = self.inst_next;
                quote! { Some(#name) }
            }
            InstStart(_) => {
                let name = self.inst_start;
                quote! { Some(#name) }
            }
        };
        let global_set_param = self.global_set_param;
        let context = &self.disassembler.context;
        let set_fun = &context.globalset.set_fun;
        let context_param = format_ident!("context");
        let value = self.context_field(&global_set.context);
        let write_call = context.write_call(
            self.disassembler,
            &context_param,
            global_set.context,
            &value,
        );
        quote! {
            #global_set_param.#set_fun(#address, |#context_param| #write_call);
        }
    }

    fn value(&self, value: &sleigh_rs::disassembly::ReadScope) -> TokenStream {
        use sleigh_rs::disassembly::ReadScope;
        match value {
            ReadScope::Integer(value) => {
                value.signed_super().suffixed().into_token_stream()
            }
            ReadScope::Context(context) => {
                self.context_field(context).to_token_stream()
            }
            ReadScope::TokenField(ass) => self.ass_field(*ass),
            ReadScope::Local(var) => self.var_name(var),
            ReadScope::InstStart(_) => self.inst_start().to_token_stream(),
            ReadScope::InstNext(_) => self.inst_next().to_token_stream(),
        }
    }

    fn set_context(
        &self,
        _context: &sleigh_rs::ContextId,
        _value: TokenStream,
    ) -> TokenStream {
        //TODO what if we modify the context and the result is used in the
        //global_set? check for that and find solutions!
        //for now, we just ignore context writes
        quote! {}
    }

    fn new_variable(
        &mut self,
        var_id: &VariableId,
        var: &Variable,
    ) -> TokenStream {
        let mut vars = self.vars.borrow_mut();
        use indexmap::map::Entry::*;
        let Vacant(entry) = vars.entry(*var_id) else {
            unreachable!("Variable duplicated")
        };
        let var_name = format_ident!("calc_{}", from_sleigh(var.name()));
        let name = entry.insert(var_name);
        quote! {let mut #name: #DISASSEMBLY_WORK_TYPE = 0;}
    }

    fn var_name(&self, var: &VariableId) -> TokenStream {
        let vars = self.vars.borrow();
        let Some(name) = vars.get(var) else {
            unreachable!("Variable not created")
        };
        name.into_token_stream()
    }
}

pub struct DisassemblyPattern<'a> {
    pub disassembler: &'a Disassembler,
    pub context_instance: &'a Ident,
    pub tokens: &'a Ident,
    pub inst_start: &'a Ident,
    pub root_tables: &'a IndexMap<sleigh_rs::TableId, Ident>,
    pub root_token_fields: &'a IndexMap<sleigh_rs::TokenFieldId, Ident>,
    pub vars: &'a mut IndexMap<VariableId, Ident>,
}

impl DisassemblyPattern<'_> {
    fn inst_start(&self) -> TokenStream {
        let inst_start = &self.inst_start;
        quote! {#DISASSEMBLY_WORK_TYPE::try_from(#inst_start).unwrap()}
    }
    //get var name on that contains the this assembly field value
    fn ass_field(&self, ass: &sleigh_rs::TokenFieldId) -> TokenStream {
        let tokens = self.tokens;
        let token_field_new =
            &self.disassembler.token_field_function(*ass).read;
        quote! { #DISASSEMBLY_WORK_TYPE::try_from(#token_field_new(#tokens)).unwrap() }
    }
    //get var name on that contains the this context value
    fn context_field(&self, context: &sleigh_rs::ContextId) -> TokenStream {
        let read_call = self
            .disassembler
            .context
            .read_call(*context, self.context_instance);
        quote! { #DISASSEMBLY_WORK_TYPE::try_from(#read_call).unwrap()}
    }
    fn can_execute(&self, expr: &sleigh_rs::disassembly::Expr) -> bool {
        use sleigh_rs::disassembly::ExprElement::*;
        use sleigh_rs::disassembly::ReadScope::*;
        expr.elements().iter().all(|element| match element {
            Op(_) | OpUnary(_) => true,
            Value { value, location: _ } => match value {
                Integer(_) | Context(_) | InstStart(_) | Local(_)
                | TokenField(_) => true,
                InstNext(_) => false,
            },
        })
    }
}

impl DisassemblyGenerator for DisassemblyPattern<'_> {
    //TODO identify disassembly that can't be executed separated between pre/pos
    fn disassembly(
        &self,
        assertations: &mut dyn Iterator<Item = &Assertation>,
    ) -> TokenStream {
        let mut tokens = TokenStream::new();
        for ass in assertations {
            use sleigh_rs::disassembly::Assertation::*;
            match ass {
                GlobalSet(_) => (),
                Assignment(ass) => {
                    if !self.can_execute(&ass.right) {
                        break;
                    }
                    tokens.extend(self.assignment(ass));
                }
            }
        }
        tokens
    }
    fn global_set(&self, _global_set: &GlobalSet) -> TokenStream {
        //global set is not done yet, only in display
        unreachable!()
    }

    fn value(&self, value: &sleigh_rs::disassembly::ReadScope) -> TokenStream {
        use sleigh_rs::disassembly::ReadScope;
        match value {
            ReadScope::Integer(value) => {
                value.signed_super().suffixed().into_token_stream()
            }
            ReadScope::Context(context) => {
                self.context_field(context).to_token_stream()
            }
            ReadScope::TokenField(ass) => self.ass_field(ass),
            ReadScope::Local(var) => self.var_name(var),
            ReadScope::InstStart(_) => self.inst_start().to_token_stream(),
            ReadScope::InstNext(_) => unreachable!(),
        }
    }

    fn set_context(
        &self,
        context: &sleigh_rs::ContextId,
        value: TokenStream,
    ) -> TokenStream {
        let write = self.disassembler.context.write_call(
            self.disassembler,
            self.context_instance,
            *context,
            &value,
        );
        quote! { #write; }
    }

    fn new_variable(
        &mut self,
        var_id: &VariableId,
        var: &Variable,
    ) -> TokenStream {
        use indexmap::map::Entry::*;
        let Vacant(entry) = self.vars.entry(*var_id) else {
            unreachable!("Variable duplicated")
        };
        let var_name = format_ident!("calc_{}", from_sleigh(var.name()));
        let name = entry.insert(var_name);
        quote! {let mut #name: #DISASSEMBLY_WORK_TYPE = 0;}
    }

    fn var_name(&self, var: &VariableId) -> TokenStream {
        let var = self.vars.get(var).unwrap();
        var.to_token_stream()
    }
}
