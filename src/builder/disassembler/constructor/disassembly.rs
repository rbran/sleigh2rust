use indexmap::IndexMap;
use std::cell::RefCell;
use std::rc::Rc;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use sleigh_rs::disassembly::{Assertation, GlobalSet, Variable};
use sleigh_rs::{GlobalAnonReference, GlobalElement, GlobalReference};

use crate::builder::formater::from_sleigh;
use crate::builder::{Disassembler, DisassemblyGenerator, ToLiteral, WorkType};

use super::{ConstructorStruct, ParsedField};

pub const DISASSEMBLY_WORK_TYPE: WorkType = WorkType::DISASSEMBLY_TYPE;
pub struct DisassemblyDisplay<'a> {
    pub constructor: &'a ConstructorStruct,
    pub display_param: &'a Ident,
    pub context_param: &'a Ident,
    pub inst_start: &'a Ident,
    pub inst_next: &'a Ident,
    pub global_set_param: &'a Ident,
    pub vars: RefCell<IndexMap<*const Variable, ParsedField<Rc<Variable>>>>,
}

impl<'a> DisassemblyDisplay<'a> {
    fn inst_start(&self) -> TokenStream {
        let inst_start = &self.inst_start;
        quote! {#DISASSEMBLY_WORK_TYPE::try_from(#inst_start).unwrap()}
    }
    fn inst_next(&self) -> TokenStream {
        let inst_next = &self.inst_next;
        quote! {#DISASSEMBLY_WORK_TYPE::try_from(#inst_next).unwrap()}
    }
    //get var name on that contains the this assembly field value
    fn ass_field(
        &self,
        ass: &GlobalReference<sleigh_rs::TokenField>,
    ) -> TokenStream {
        //can't create new ass fields during disassembly, all
        //fields used need to be declared on the pattern
        let field =
            self.constructor.ass_fields.get(&ass.element_ptr()).unwrap();
        let name = &field.name;
        quote! {#DISASSEMBLY_WORK_TYPE::try_from(self.#name).unwrap()}
    }

    fn context_field(&self, context: &sleigh_rs::Context) -> TokenStream {
        let disassembler = self.constructor.disassembler.upgrade().unwrap();
        let read_call = disassembler
            .context()
            .read_call(context, &self.context_param);
        quote! { #DISASSEMBLY_WORK_TYPE::try_from(#read_call).unwrap()}
    }
    //get var name on that contains the this assembly field value
    fn table_field(
        &self,
        table: &GlobalElement<sleigh_rs::Table>,
    ) -> Option<TokenStream> {
        let field = self
            .constructor
            .table_fields
            .get(&table.element_ptr())
            .unwrap();
        let field_name = &field.name;
        use sleigh_rs::table::ExecutionExport;
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
        Some(quote! {self.#field_name})
    }
}

impl<'a> ToTokens for DisassemblyDisplay<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut asses = self
            .constructor
            .sleigh
            .pattern
            .blocks()
            .iter()
            .flat_map(|block| match block {
                sleigh_rs::Block::And { pre, pos, .. } => {
                    pre.iter().chain(pos.iter())
                }
                sleigh_rs::Block::Or { pos, .. } => {
                    pos.iter().chain([/*LOL*/].iter())
                }
            })
            .chain(self.constructor.sleigh.pattern.disassembly_pos_match());
        tokens.extend(self.disassembly(&mut asses));
    }
}

impl<'a, 'b> DisassemblyGenerator<'b> for DisassemblyDisplay<'a> {
    fn global_set(&self, global_set: &'b GlobalSet) -> TokenStream {
        let disassembler = self.constructor.disassembler.upgrade().unwrap();
        let sleigh_context = global_set.context().element();
        let addr_type = &disassembler.addr_type();
        use sleigh_rs::disassembly::AddrScope::*;
        let address = match &global_set.address {
            Integer(value) => {
                let value = value.unsuffixed();
                quote! { Some(#value) }
            }
            Varnode(_varnode) => {
                quote! { None }
            }
            Local(var) => {
                let name = self.var_name(var);
                quote! { Some(#addr_type::try_from(#name).unwrap()) }
            }
            Table(table) => {
                let table = table.element();
                self.table_field(&table)
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
        let context = disassembler.context();
        let set_fun = &context.globalset.set_fun;
        let context_param = format_ident!("context");
        let value = self.context_field(&sleigh_context);
        let write_call =
            context.write_call(&sleigh_context, &context_param, &value);
        quote! {
            #global_set_param.#set_fun(#address, |#context_param| #write_call);
        }
    }

    fn value(
        &self,
        value: &'b sleigh_rs::disassembly::ReadScope,
    ) -> TokenStream {
        use sleigh_rs::disassembly::ReadScope;
        match value {
            ReadScope::Integer(value) => {
                value.signed_super().suffixed().into_token_stream()
            }
            ReadScope::Context(varnode) => {
                let sleigh_context = varnode.element();
                self.context_field(&sleigh_context).to_token_stream()
            }
            ReadScope::TokenField(ass) => self.ass_field(ass),
            ReadScope::Local(var) => self.var_name(var),
            ReadScope::InstStart(_) => self.inst_start().to_token_stream(),
            ReadScope::InstNext(_) => self.inst_next().to_token_stream(),
        }
    }

    fn set_context(
        &self,
        _context: &GlobalAnonReference<sleigh_rs::Context>,
        _value: TokenStream,
    ) -> TokenStream {
        //TODO what if we modify the context and the result is used in the
        //global_set? check for that and find solutions!
        //for now, we just ignore context writes
        quote! {}
    }

    fn new_variable(&mut self, var: &'b Rc<Variable>) -> TokenStream {
        let mut vars = self.vars.borrow_mut();
        let ptr: *const Variable = Rc::as_ptr(var);
        use indexmap::map::Entry::*;
        match vars.entry(ptr) {
            Occupied(_) => unreachable!("Variable duplicated"),
            Vacant(entry) => {
                let var_name =
                    format_ident!("calc_{}", from_sleigh(var.name()));
                let entry =
                    entry.insert(ParsedField::new(var_name, Rc::clone(var)));
                let name = &entry.name;
                quote! {let mut #name: #DISASSEMBLY_WORK_TYPE = 0;}
            }
        }
    }

    fn var_name(&self, var: &'b Variable) -> TokenStream {
        let ptr: *const Variable = var;
        let vars = self.vars.borrow_mut();
        let var = vars.get(&ptr).unwrap();
        var.name().to_token_stream()
    }
}

pub struct DisassemblyPattern<'a> {
    pub disassembler: &'a Disassembler,
    pub context_instance: &'a Ident,
    pub tokens: &'a Ident,
    pub inst_start: &'a Ident,
    pub root_tables: &'a IndexMap<*const sleigh_rs::Table, Ident>,
    pub root_token_fields: &'a IndexMap<*const sleigh_rs::TokenField, Ident>,
    pub vars: &'a mut IndexMap<*const Variable, ParsedField<Rc<Variable>>>,
}

impl<'a> DisassemblyPattern<'a> {
    fn inst_start(&self) -> TokenStream {
        let inst_start = &self.inst_start;
        quote! {#DISASSEMBLY_WORK_TYPE::try_from(#inst_start).unwrap()}
    }
    //get var name on that contains the this assembly field value
    fn ass_field(
        &self,
        ass: &GlobalReference<sleigh_rs::TokenField>,
    ) -> TokenStream {
        let token_field =
            self.disassembler.token_field(ass.element_ptr()).unwrap();
        let value = token_field.inline_value(self.tokens);
        quote! { #DISASSEMBLY_WORK_TYPE::try_from(#value).unwrap() }
    }
    //get var name on that contains the this context value
    fn context_field(&self, context: &sleigh_rs::Context) -> TokenStream {
        let read_call = self
            .disassembler
            .context()
            .read_call(context, &self.context_instance);
        quote! { #DISASSEMBLY_WORK_TYPE::try_from(#read_call).unwrap()}
    }
    fn can_execute(&self, expr: &sleigh_rs::disassembly::Expr) -> bool {
        use sleigh_rs::disassembly::ExprElement::*;
        use sleigh_rs::disassembly::ReadScope::*;
        expr.elements().iter().all(|element| match element {
            Op(_) | OpUnary(_) => true,
            Value(value) => match value {
                Integer(_) | Context(_) | InstStart(_) | Local(_)
                | TokenField(_) => true,
                InstNext(_) => false,
            },
        })
    }
}

impl<'a, 'b> DisassemblyGenerator<'b> for DisassemblyPattern<'a> {
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
                    if !self.can_execute(ass.right()) {
                        break;
                    }
                    tokens.extend(self.assignment(ass));
                }
            }
        }
        tokens
    }
    fn global_set(&self, _global_set: &'b GlobalSet) -> TokenStream {
        //global set is not done yet, only in display
        unreachable!()
    }

    fn value(
        &self,
        value: &'b sleigh_rs::disassembly::ReadScope,
    ) -> TokenStream {
        use sleigh_rs::disassembly::ReadScope;
        match value {
            ReadScope::Integer(value) => {
                value.signed_super().suffixed().into_token_stream()
            }
            ReadScope::Context(varnode) => {
                let sleigh_context = varnode.element();
                self.context_field(&sleigh_context).to_token_stream()
            }
            ReadScope::TokenField(ass) => self.ass_field(ass),
            ReadScope::Local(var) => self.var_name(var),
            ReadScope::InstStart(_) => self.inst_start().to_token_stream(),
            ReadScope::InstNext(_) => unreachable!(),
        }
    }

    fn set_context(
        &self,
        context: &GlobalAnonReference<sleigh_rs::Context>,
        value: TokenStream,
    ) -> TokenStream {
        let write = self.disassembler.context().write_call(
            &context.element(),
            &self.context_instance,
            &value,
        );
        quote! { #write; }
    }

    fn new_variable(&mut self, var: &'b Rc<Variable>) -> TokenStream {
        let ptr: *const Variable = Rc::as_ptr(var);
        use indexmap::map::Entry::*;
        match self.vars.entry(ptr) {
            Occupied(_) => unreachable!("Variable duplicated"),
            Vacant(entry) => {
                let var_name =
                    format_ident!("calc_{}", from_sleigh(var.name()));
                let entry =
                    entry.insert(ParsedField::new(var_name, Rc::clone(var)));
                let name = &entry.name;
                quote! {let mut #name: #DISASSEMBLY_WORK_TYPE = 0;}
            }
        }
    }

    fn var_name(&self, var: &'b Variable) -> TokenStream {
        let ptr: *const Variable = var;
        let var = self.vars.get(&ptr).unwrap();
        var.name().to_token_stream()
    }
}
