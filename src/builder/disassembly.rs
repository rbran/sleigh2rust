use std::collections::HashMap;
use std::rc::Rc;

use proc_macro2::TokenStream;
use quote::quote;

use sleigh_rs::semantic::disassembly::{
    Assertation, Assignment, Expr, ExprElement, GlobalSet, Op, OpUnary,
    ReadScope, Variable,
};
use sleigh_rs::Varnode;

fn disassembly_op(op: &Op) -> TokenStream {
    match op {
        Op::Add => quote! {+},
        Op::Sub => quote! {-},
        Op::Mul => quote! {*},
        Op::Div => quote! {/},
        Op::Asr => quote! {>>},
        Op::Lsl => quote! {<<},
        Op::And => quote! {&},
        Op::Or => quote! {|},
        Op::Xor => quote! {^},
    }
}
pub trait DisassemblyGenerator<'a> {
    fn global_set(&mut self, global_set: &'a GlobalSet) -> TokenStream;
    fn value(&mut self, value: &'a ReadScope) -> TokenStream;
    fn set_context(
        &mut self,
        context: &'a Varnode,
        value: TokenStream,
    ) -> TokenStream;
    fn new_variable(&mut self, var: &'a Variable);
    fn var_name(&mut self, var: &'a Variable) -> TokenStream;
    fn op_unary(&mut self, op: &'a OpUnary) -> TokenStream {
        match op {
            OpUnary::Negation => quote! {!},
            OpUnary::Negative => quote! {-},
        }
    }
    fn expr(&mut self, expr: &'a Expr) -> TokenStream {
        let mut work_stack: Vec<_> = Vec::with_capacity(2);
        for ele in expr.rpn.iter() {
            //the rpn stack that result in a work_stack bigger then 2 is invalid
            match (ele, work_stack.len()) {
                (ExprElement::Value(value), _) => {
                    work_stack.push(self.value(value))
                }
                (ExprElement::Op(op), 2..) => {
                    let op = disassembly_op(op);
                    let y = work_stack.pop().unwrap();
                    let x = work_stack.pop().unwrap();
                    work_stack.push(quote! {(#x #op #y)});
                }
                (ExprElement::OpUnary(op), 1..) => {
                    let op = self.op_unary(op);
                    let x = work_stack.pop().unwrap();
                    work_stack.push(quote! {(#op #x)});
                }
                _ => unreachable!(),
            }
        }
        assert_eq!(work_stack.len(), 1);
        match work_stack.pop() {
            Some(out) => out,
            _ => unreachable!(),
        }
    }
    fn set_variable(
        &mut self,
        var: &'a Variable,
        value: TokenStream,
    ) -> TokenStream {
        let var_name = self.var_name(var);
        quote! { #var_name = #value; }
    }
    fn assignment(&mut self, ass: &'a Assignment) -> TokenStream {
        use sleigh_rs::semantic::disassembly::WriteScope::*;
        let value = self.expr(&ass.right);
        match &ass.left {
            Varnode(varnode) => self.set_context(varnode, value),
            Local(variable) => self.set_variable(variable, value),
        }
    }
    fn disassembly(
        &mut self,
        vars: &'a HashMap<Rc<str>, Rc<Variable>>,
        assertations: &'a Vec<Assertation>,
    ) -> TokenStream {
        for var in vars.values() {
            self.new_variable(var)
        }
        assertations
            .iter()
            .map(|ass| {
                use sleigh_rs::semantic::disassembly::Assertation::*;
                match ass {
                    GlobalSet(global) => self.global_set(global),
                    Assignment(ass) => self.assignment(ass),
                }
            })
            .collect()
    }
}
