use std::collections::HashMap;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use sleigh_rs::semantic::disassembly::{GlobalSet, Variable};
use sleigh_rs::{Assembly, IntTypeS, Varnode};

use crate::builder::formater::from_sleigh;
use crate::builder::{ContextTrait, DisassemblyGenerator, WorkType};

use super::{Constructor, ParsedField};

pub struct DisassemblyConstructor<'a, 'b> {
    context_trait: &'b ContextTrait<'a>,
    global_set: &'b crate::builder::GlobalSetTrait<'a>,

    inst_start: &'b Ident,
    inst_next: &'b Ident,
    inst_work_type: &'b WorkType,
    global_set_param: &'b Ident,
    context_param: &'b Ident,

    //things that need to be done before the disassembly
    build_pre_disassembly: Vec<TokenStream>,
    //things that need to be done after the disassembly
    build_pos_disassembly: Vec<TokenStream>,

    //vars that where declared outside the disassembly and can be used without
    //declaration
    deref_ass: bool,
    deref_var: bool,
    constructor: &'b Constructor<'a>,

    //variables created during the execution of this disassembly
    calc_fields: &'b HashMap<*const Variable, ParsedField<&'a Variable>>,
    ass_vars: &'b HashMap<*const Assembly, Ident>,
    existing_var: HashMap<*const Variable, ParsedField<&'a Variable>>,
    context_vars: HashMap<*const Varnode, (Ident, WorkType, &'a Varnode)>,
}

impl<'a, 'b> DisassemblyConstructor<'a, 'b> {
    pub fn disassembly(
        context_trait: &'b ContextTrait<'a>,
        global_set: &'b crate::builder::GlobalSetTrait<'a>,

        inst_start: &'b Ident,
        inst_next: &'b Ident,
        inst_work_type: &'b WorkType,
        global_set_param: &'b Ident,
        context_param: &'b Ident,

        deref_ass: bool,
        deref_var: bool,
        constructor: &'b Constructor<'a>,

        calc_fields: &'b HashMap<*const Variable, ParsedField<&'a Variable>>,
        ass_vars: &'b HashMap<*const Assembly, Ident>,
        disassembly: &'a sleigh_rs::Disassembly,
    ) -> TokenStream {
        let mut helper = Self {
            context_trait,
            global_set,

            inst_start,
            inst_next,
            inst_work_type,
            global_set_param,
            context_param,

            //things that need to be done before the disassembly
            build_pre_disassembly: vec![],
            //things that need to be done after the disassembly
            build_pos_disassembly: vec![],

            deref_ass,
            deref_var,
            constructor,
            ass_vars,

            calc_fields,
            context_vars: HashMap::new(),
            existing_var: HashMap::new(),
        };
        helper.generate(disassembly)
    }

    pub fn generate(
        &mut self,
        disassembly: &'a sleigh_rs::Disassembly,
    ) -> TokenStream {
        let disassembly =
            self.disassembly(&disassembly.vars, &disassembly.assertations);

        let pre_build_disassembly =
            std::mem::take(&mut self.build_pre_disassembly);
        let pos_build_disassembly =
            std::mem::take(&mut self.build_pos_disassembly);
        quote! {
            #(#pre_build_disassembly)*
            #disassembly
            #(#pos_build_disassembly)*
        }
    }
    //get var name on that contains the this assembly field value
    fn ass_field(
        &mut self,
        ass: &'a sleigh_rs::Assembly,
    ) -> (TokenStream, WorkType) {
        match &ass.assembly_type {
            sleigh_rs::semantic::assembly::AssemblyType::Epsilon => {
                unreachable!("epsilon field?")
            }
            sleigh_rs::semantic::assembly::AssemblyType::Start(_) => {
                let inst_start = &self.inst_start;
                (quote! {#inst_start}, *self.inst_work_type)
            }
            sleigh_rs::semantic::assembly::AssemblyType::Next(_) => {
                let inst_next = &self.inst_next;
                (quote! {#inst_next}, *self.inst_work_type)
            }
            sleigh_rs::semantic::assembly::AssemblyType::Field(_) => {
                //can't create new ass fields during disassembly, all
                //fields used need to be declared on the pattern
                let ptr: *const sleigh_rs::Assembly = ass;
                let field_name = self.ass_vars.get(&ptr).unwrap();
                let deref = self.deref_ass.then(|| quote! {*});
                let return_type = WorkType::from_ass(ass);
                (quote! {#deref #field_name}, return_type)
            }
        }
    }
    //get var name on that contains the this context value
    fn context_field(&mut self, varnode: &'a Varnode) -> (&Ident, &WorkType) {
        let ptr: *const Varnode = varnode;
        //if the variable exists, return it
        if self.context_vars.contains_key(&ptr) {
            let (ident, context_type, _) = self.context_vars.get(&ptr).unwrap();
            return (ident, context_type);
        }
        //otherwise create it
        let context = self.context_trait.varnode(ptr);
        let context_func = context.read();
        let context_name = format_ident!("{}", from_sleigh(&varnode.name));
        let context_param = &self.context_param;
        let context_type = *context.return_type();

        self.build_pre_disassembly.push(quote! {
            let mut #context_name = #context_param.#context_func();
        });
        self.context_vars
            .insert(ptr, (context_name, context_type, varnode))
            .map(|_| unreachable!());
        let (ident, context_type, _) = self.context_vars.get(&ptr).unwrap();
        (ident, context_type)
    }
}

impl<'a, 'b> DisassemblyGenerator<'a> for DisassemblyConstructor<'a, 'b> {
    fn global_set(&mut self, global_set: &'a GlobalSet) -> TokenStream {
        let context = self.global_set.context(&global_set.context);
        let addr_type = self.inst_work_type;
        use sleigh_rs::semantic::disassembly::AddrScope::*;
        let address = match &global_set.address {
            Int(value) => quote! {(#value as #addr_type)},
            Varnode(varnode) => {
                let (name, _var_type) = self.context_field(varnode);
                //TODO solve IntTypeS instead of i64
                quote! { #addr_type::try_from(#name).unwrap() }
            }
            Assembly(ass) => {
                let (name, _ass_type) = self.ass_field(ass);
                quote! { #addr_type::try_from(#name).unwrap() }
            }
            Local(var) => {
                let name = self.var_name(var);
                quote! {#addr_type::try_from(#name).unwrap()}
            }
            Table(_) => todo!(),
        };
        let set_function = context.function();
        let value_type = context.value_type();
        let global_set_param = self.global_set_param;
        let (value, _value_type) = self.context_field(&global_set.context);
        quote! {
            #global_set_param.#set_function(
                #address,
                #value_type::try_into(#value).unwrap()
            );
        }
    }
    fn value(
        &mut self,
        value: &'a sleigh_rs::semantic::disassembly::ReadScope,
    ) -> TokenStream {
        use sleigh_rs::semantic::disassembly::ReadScope;
        match value {
            ReadScope::Integer(value) => quote! {(#value as i64)},
            ReadScope::Varnode(varnode) => {
                let (name, _var_type) = self.context_field(varnode);
                //TODO solve IntTypeS instead of i64
                quote! { i64::try_from(#name).unwrap() }
            }
            ReadScope::Assembly(ass) => {
                let (name, _ass_type) = self.ass_field(ass);
                //TODO solve IntTypeS instead of i64
                quote! { i64::try_from(#name).unwrap() }
            }
            ReadScope::Local(var) => self.var_name(var),
        }
    }

    fn set_context(
        &mut self,
        varnode: &'a sleigh_rs::Varnode,
        value: TokenStream,
    ) -> TokenStream {
        let context_write_func =
            self.context_trait.varnode(varnode).write().unwrap();
        let context_param = self.context_param;
        //this context variable need to be written back into the context trait
        //after the disassembly is finished
        let (name, context_type) = self.context_field(varnode);
        let (name, context_type) = (name.clone(), *context_type);
        self.build_pos_disassembly.push(quote! {
            #context_param.#context_write_func(#name);
        });
        //update the variable during the disassembly
        quote! { #name = #context_type::try_from(#value).unwrap(); }
    }

    fn new_variable(&mut self, var: &'a Variable) {
        let ptr: *const Variable = var;
        //if this variable is already declare, does nothing
        if self.calc_fields.contains_key(&ptr) {
            return;
        }
        let var_name = format_ident!("{}", from_sleigh(var.name.as_ref()));
        let zero: IntTypeS = 0;
        //variable used used during the disassembly, initialized it with zero
        self.build_pre_disassembly.push(quote! {
            let mut #var_name = #zero;
        });
        self.existing_var
            .insert(ptr, ParsedField::new(var_name, var))
            .map(|_| unreachable!("Variable duplicated"));
    }

    fn var_name(&mut self, var: &Variable) -> TokenStream {
        let ptr: *const Variable = var;
        self.existing_var
            .get(&ptr)
            .map(|field| field.name().to_token_stream())
            .or_else(|| {
                let name = self.calc_fields.get(&ptr).map(ParsedField::name)?;
                let deref = self.deref_var.then(|| quote! {*});
                Some(quote! {#deref #name})
            })
            .unwrap()
    }
}
