use indexmap::IndexMap;
use std::cell::RefCell;
use std::rc::Rc;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use sleigh_rs::semantic::disassembly::{GlobalSet, Variable};
use sleigh_rs::semantic::{
    GlobalAnonReference, GlobalElement, GlobalReference,
};

use crate::builder::formater::from_sleigh;
use crate::builder::{Disassembler, DisassemblyGenerator, WorkType};

use super::{ConstructorStruct, ParsedField};

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
        let int_type = WorkType::int_type(true);
        quote! {#int_type::try_from(#inst_start).unwrap()}
    }
    fn inst_next(&self) -> TokenStream {
        let inst_next = &self.inst_next;
        let int_type = WorkType::int_type(true);
        quote! {#int_type::try_from(#inst_next).unwrap()}
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
        let disassembly_fun = &field.token_field_struct.disassembly_fun;
        quote! {self.#name.#disassembly_fun()}
    }
    //get var name on that contains the this context value
    fn context_field(&self, context: &sleigh_rs::Context) -> TokenStream {
        //otherwise create it
        let disassembler = self.constructor.disassembler.upgrade().unwrap();
        disassembler
            .memory
            .spaces_trait
            .build_context_disassembly_read_call(&self.context_param, &context)
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
        let disassembler = self.constructor.disassembler.upgrade().unwrap();
        let addr_type = &disassembler.inst_work_type;
        use sleigh_rs::semantic::table::ExecutionExport;
        match table.export {
            ExecutionExport::Const(len) => {
                //TODO allow table to export value with addr diff from addr_len
                //and auto convert using try_from?
                assert_eq!(len, addr_type.len_bytes())
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
        tokens.extend(self.disassembly(
            &self.constructor.sleigh.disassembly.vars,
            &self.constructor.sleigh.disassembly.assertations,
        ));
    }
}

impl<'a, 'b> DisassemblyGenerator<'b> for DisassemblyDisplay<'a> {
    fn global_set(&self, global_set: &'b GlobalSet) -> TokenStream {
        let sleigh_context = global_set.context().element();
        let disassembler = self.constructor.disassembler.upgrade().unwrap();
        let context = disassembler.global_set.context(sleigh_context.element());
        let addr_type = &disassembler.inst_work_type;
        use sleigh_rs::semantic::disassembly::AddrScope::*;
        let address = match &global_set.address {
            Integer(value) => quote! {(#value as #addr_type)},
            Varnode(_varnode) => {
                //TODO solve IntTypeS instead of i64
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
        let set_function = context.function();
        let global_set_param = self.global_set_param;
        let value = self.context_field(&sleigh_context);
        quote! {
            #global_set_param.#set_function(#address, #value);
        }
    }

    fn value(
        &self,
        value: &'b sleigh_rs::semantic::disassembly::ReadScope,
    ) -> TokenStream {
        use sleigh_rs::semantic::disassembly::ReadScope;
        match value {
            ReadScope::Integer(value) => quote! {(#value as i64)},
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

    fn new_variable(&self, var: &'b Rc<Variable>) -> TokenStream {
        let mut vars = self.vars.borrow_mut();
        let ptr: *const Variable = Rc::as_ptr(var);
        let var_name = format_ident!("{}", from_sleigh(var.name()));
        use indexmap::map::Entry::*;
        match vars.entry(ptr) {
            Occupied(_) => unreachable!("Variable duplicated"),
            Vacant(entry) => {
                let entry =
                    entry.insert(ParsedField::new(var_name, Rc::clone(var)));
                let name = &entry.name;
                let work_type = WorkType::int_type(true);
                quote! {let mut #name: #work_type = 0;}
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
    pub token_parser: Option<&'a Ident>,
    pub context_param: &'a Ident,
    pub inst_start: &'a Ident,
    pub root_tables: &'a IndexMap<*const sleigh_rs::Table, Ident>,
    pub root_token_fields: &'a IndexMap<*const sleigh_rs::TokenField, Ident>,
    pub vars: RefCell<IndexMap<*const Variable, ParsedField<Rc<Variable>>>>,
}

impl<'a> DisassemblyPattern<'a> {
    fn inst_start(&self) -> TokenStream {
        let inst_start = &self.inst_start;
        let int_type = WorkType::int_type(true);
        quote! {#int_type::try_from(#inst_start).unwrap()}
    }
    //get var name on that contains the this assembly field value
    fn ass_field(
        &self,
        ass: &GlobalReference<sleigh_rs::TokenField>,
    ) -> TokenStream {
        self.disassembler.token_parser.gen_disassembly_read_call(
            self.token_parser.as_ref().unwrap(),
            ass.element_ptr(),
        )
    }
    //get var name on that contains the this context value
    fn context_field(&self, context: &sleigh_rs::Context) -> TokenStream {
        //otherwise create it
        self.disassembler
            .memory
            .spaces_trait
            .build_context_disassembly_read_call(&self.context_param, &context)
    }
    fn can_execute(
        &self,
        expr: &sleigh_rs::semantic::disassembly::Expr,
    ) -> bool {
        use sleigh_rs::semantic::disassembly::ExprElement::*;
        use sleigh_rs::semantic::disassembly::ReadScope::*;
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
        vars: &'b [Rc<Variable>],
        assertations: &'b [sleigh_rs::semantic::disassembly::Assertation],
    ) -> TokenStream {
        let mut tokens = TokenStream::new();
        tokens.extend(vars.iter().map(|var| self.new_variable(var)));
        for ass in assertations {
            use sleigh_rs::semantic::disassembly::Assertation::*;
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
        value: &'b sleigh_rs::semantic::disassembly::ReadScope,
    ) -> TokenStream {
        use sleigh_rs::semantic::disassembly::ReadScope;
        match value {
            ReadScope::Integer(value) => quote! {(#value as i64)},
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
        let tmp_value = format_ident!("tmp");
        let write = self
            .disassembler
            .memory
            .spaces_trait
            .build_context_disassembly_write_call(
                &self.context_param,
                &context.element(),
                &tmp_value,
            );
        quote! {
            let #tmp_value = #value;
            #write;
        }
    }

    fn new_variable(&self, var: &'b Rc<Variable>) -> TokenStream {
        let mut vars = self.vars.borrow_mut();
        let ptr: *const Variable = Rc::as_ptr(var);
        let var_name = format_ident!("{}", from_sleigh(var.name()));
        use indexmap::map::Entry::*;
        match vars.entry(ptr) {
            Occupied(_) => unreachable!("Variable duplicated"),
            Vacant(entry) => {
                let entry =
                    entry.insert(ParsedField::new(var_name, Rc::clone(var)));
                let name = &entry.name;
                let work_type = WorkType::int_type(true);
                quote! {let mut #name: #work_type = 0;}
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
