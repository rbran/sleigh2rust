use std::rc::Rc;

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

use sleigh_rs::semantic::disassembly::{
    Assertation, Assignment, Expr, ExprElement, GlobalSet, Op, OpUnary,
    ReadScope, Variable,
};
use sleigh_rs::semantic::GlobalAnonReference;
use sleigh_rs::Context;

fn disassembly_op(x: impl ToTokens, op: &Op, y: impl ToTokens) -> TokenStream {
    if crate::DISASSEMBLY_ALLOW_OVERFLOW {
        match op {
            Op::Add => quote! {#x.wrapping_add(#y)},
            Op::Sub => quote! {#x.wrapping_sub(#y)},
            Op::Mul => quote! {#x.wrapping_mul(#y)},
            Op::Div => quote! {#x.wrapping_div(#y)},
            Op::Asr => quote! {
                #x.checked_shr(u32::try_from(#y).unwrap()).unwrap_or(0)
            },
            Op::Lsl => quote! {
                #x.checked_shl(u32::try_from(#y).unwrap()).unwrap_or(0)
            },
            Op::And => quote! {(#x & #y)},
            Op::Or => quote! {(#x | #y)},
            Op::Xor => quote! {(#x ^ #y)},
        }
    } else {
        match op {
            Op::Add => quote! {(#x + #y)},
            Op::Sub => quote! {(#x - #y)},
            Op::Mul => quote! {(#x * #y)},
            Op::Div => quote! {(#x / #y)},
            Op::Asr => quote! {(#x >> #y)},
            Op::Lsl => quote! {(#x << #y)},
            Op::And => quote! {(#x & #y)},
            Op::Or => quote! {(#x | #y)},
            Op::Xor => quote! {(#x ^ #y)},
        }
    }
}
fn op_unary(op: &OpUnary, x: impl ToTokens) -> TokenStream {
    match op {
        OpUnary::Negation => quote! {(!#x)},
        OpUnary::Negative => quote! {(-#x)},
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
    fn new_variable(&mut self, var: &'a Rc<Variable>) -> TokenStream;
    fn var_name(&self, var: &'a Variable) -> TokenStream;
    fn expr(&self, expr: &'a Expr) -> TokenStream {
        let mut work_stack: Vec<_> = Vec::with_capacity(2);
        for ele in expr.elements().iter() {
            //the rpn stack that result in a work_stack bigger then 2 is invalid
            match (ele, work_stack.len()) {
                (ExprElement::Value(value), _) => {
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
    fn disassembly(&self, assertations: &mut dyn Iterator<Item=&'a Assertation>) -> TokenStream {
        assertations
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
