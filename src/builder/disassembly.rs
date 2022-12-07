use std::rc::Rc;

use proc_macro2::TokenStream;
use quote::quote;

use sleigh_rs::semantic::disassembly::{
    Assertation, Assignment, Expr, ExprElement, GlobalSet, Op, OpUnary,
    ReadScope, Variable,
};
use sleigh_rs::semantic::GlobalAnonReference;
use sleigh_rs::Context;

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
    fn global_set(&self, global_set: &'a GlobalSet) -> TokenStream;
    fn value(&self, value: &'a ReadScope) -> TokenStream;
    fn set_context(
        &self,
        context: &GlobalAnonReference<Context>,
        value: TokenStream,
    ) -> TokenStream;
    fn new_variable(&self, var: &'a Rc<Variable>) -> TokenStream;
    fn var_name(&self, var: &'a Variable) -> TokenStream;
    fn op_unary(&self, op: &'a OpUnary) -> TokenStream {
        match op {
            OpUnary::Negation => quote! {!},
            OpUnary::Negative => quote! {-},
        }
    }
    fn expr(&self, expr: &'a Expr) -> TokenStream {
        let mut work_stack: Vec<_> = Vec::with_capacity(2);
        for ele in expr.elements().iter() {
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
        &self,
        var: &'a Variable,
        value: TokenStream,
    ) -> TokenStream {
        let var_name = self.var_name(var);
        quote! { #var_name = #value; }
    }
    fn assignment(&self, ass: &'a Assignment) -> TokenStream {
        use sleigh_rs::semantic::disassembly::WriteScope::*;
        let value = self.expr(ass.right());
        match ass.left() {
            Context(context) => {
                let context = context.clone().into();
                self.set_context(&context, value)
            }
            Local(variable) => self.set_variable(variable, value),
        }
    }
    fn disassembly(
        &self,
        vars: &'a [Rc<Variable>],
        assertations: &'a [Assertation],
    ) -> TokenStream {
        let vars_iter = vars.iter().map(|var| self.new_variable(var));
        let assertations_iter = assertations.iter().map(|ass| {
            use sleigh_rs::semantic::disassembly::Assertation::*;
            match ass {
                GlobalSet(global) => self.global_set(global),
                Assignment(ass) => self.assignment(ass),
            }
        });
        vars_iter.chain(assertations_iter).collect()
    }
}
