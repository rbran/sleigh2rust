use proc_macro2::TokenStream;

use quote::{quote, ToTokens};

use sleigh_rs::disassembly::{
    Assertation, Assignment, Expr, ExprElement, GlobalSet, Op, OpUnary,
    ReadScope, Variable, VariableId,
};

fn disassembly_op(x: impl ToTokens, op: &Op, y: impl ToTokens) -> TokenStream {
    match (crate::DISASSEMBLY_ALLOW_OVERFLOW, op) {
        (true, Op::Add) => quote! {#x.wrapping_add(#y)},
        (true, Op::Sub) => quote! {#x.wrapping_sub(#y)},
        (true, Op::Mul) => quote! {#x.wrapping_mul(#y)},
        (true, Op::Div) => quote! {#x.wrapping_div(#y)},
        (true, Op::Asr) => quote! {
            u32::try_from(#y).ok().and_then(|shr| #x.checked_shr(shr)).unwrap_or(0)
        },
        (true, Op::Lsl) => quote! {
            u32::try_from(#y).ok().and_then(|shl| #x.checked_shl(shl)).unwrap_or(0)
        },
        (false, Op::Add) => quote! {(#x + #y)},
        (false, Op::Sub) => quote! {(#x - #y)},
        (false, Op::Mul) => quote! {(#x * #y)},
        (false, Op::Div) => quote! {(#x / #y)},
        (false, Op::Asr) => quote! {(#x >> #y)},
        (false, Op::Lsl) => quote! {(#x << #y)},
        //bit op, works the same way unsigned/signed, so use unsigned
        (_, Op::And) => quote! {(#x & #y)},
        (_, Op::Or) => quote! {(#x | #y)},
        (_, Op::Xor) => quote! {(#x ^ #y)},
    }
}
fn op_unary(op: &OpUnary, x: impl ToTokens) -> TokenStream {
    match op {
        OpUnary::Negation => quote! {(!#x)},
        OpUnary::Negative => quote! {(-#x)},
    }
}
pub trait DisassemblyGenerator {
    fn global_set(&self, global_set: &GlobalSet) -> TokenStream;
    fn value(&self, value: &ReadScope) -> TokenStream;
    fn set_context(
        &self,
        id: &sleigh_rs::ContextId,
        value: TokenStream,
    ) -> TokenStream;
    fn new_variable(
        &mut self,
        var_id: &VariableId,
        variable: &Variable,
    ) -> TokenStream;
    fn var_name(&self, var: &VariableId) -> TokenStream;
    fn expr(&self, expr: &Expr) -> TokenStream {
        let mut work_stack: Vec<_> = Vec::with_capacity(2);
        for ele in expr.elements().iter() {
            //the rpn stack that result in a work_stack bigger then 2 is invalid
            match (ele, work_stack.len()) {
                (ExprElement::Value { value, location: _ }, _) => {
                    work_stack.push(self.value(value))
                }
                (ExprElement::Op(op), 2..) => {
                    let y = work_stack.pop().unwrap();
                    let x = work_stack.pop().unwrap();
                    let op = disassembly_op(x, op, y);
                    work_stack.push(op);
                }
                (ExprElement::OpUnary(op), 1..) => {
                    let x = work_stack.pop().unwrap();
                    let op = op_unary(op, x);
                    work_stack.push(op);
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
        var: &VariableId,
        value: TokenStream,
    ) -> TokenStream {
        let var_name = self.var_name(var);
        quote! { #var_name = #value; }
    }
    fn assignment(&self, ass: &Assignment) -> TokenStream {
        use sleigh_rs::disassembly::WriteScope::*;
        let value = self.expr(&ass.right);
        match &ass.left {
            Context(context) => self.set_context(context, value),
            Local(variable) => self.set_variable(variable, value),
        }
    }
    fn disassembly(
        &self,
        assertations: &mut dyn Iterator<Item = &Assertation>,
    ) -> TokenStream {
        assertations
            .map(|ass| {
                use sleigh_rs::disassembly::Assertation::*;
                match ass {
                    GlobalSet(global) => self.global_set(global),
                    Assignment(ass) => self.assignment(ass),
                }
            })
            .collect()
    }
}
