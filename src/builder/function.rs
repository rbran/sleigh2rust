//use std::collections::HashMap;
//use std::rc::Rc;
//
//use proc_macro2::Ident;
//use quote::format_ident;
//
//use super::{from_sleigh, snake_case, SLEIGH_IDENT};
//
//pub struct Functions {
//    macros: HashMap<*const sleigh_rs::PcodeMacro, PcodeMacro>,
//    functions: HashMap<*const sleigh_rs::UserFunction, UserFunction>,
//}
//
//impl Functions {
//    pub fn new(sleigh: &sleigh_rs::Sleigh) -> Self {
//        let macros = sleigh
//            .pcode_macros()
//            .map(PcodeMacro::new)
//            .map(|fuck| (Rc::as_ptr(&fuck.sleigh), fuck))
//            .collect();
//        let functions = sleigh
//            .user_functions()
//            .map(UserFunction::new)
//            .map(|function| (Rc::as_ptr(&function.sleigh), function))
//            .collect();
//        Self { macros, functions }
//    }
//}
//
//const MACRO_PREFIX: [&str; 2] = [SLEIGH_IDENT, "macro"];
//struct PcodeMacro {
//    ident: Ident,
//    sleigh: Rc<sleigh_rs::PcodeMacro>,
//}
//impl PcodeMacro {
//    pub fn new(sleigh: Rc<sleigh_rs::PcodeMacro>) -> Self {
//        let ident = MACRO_PREFIX.into_iter().chain(from_sleigh(&sleigh.name));
//        let ident = format_ident!("{}", &snake_case(ident));
//        Self { ident, sleigh }
//    }
//}
//
//const USER_FUNCTION_PREFIX: [&str; 3] = [SLEIGH_IDENT, "user", "function"];
//struct UserFunction {
//    ident: Ident,
//    sleigh: Rc<sleigh_rs::UserFunction>,
//}
//impl UserFunction {
//    pub fn new(sleigh: Rc<sleigh_rs::UserFunction>) -> Self {
//        let ident = USER_FUNCTION_PREFIX
//            .into_iter()
//            .chain(from_sleigh(&sleigh.name));
//        let ident = format_ident!("{}", &snake_case(ident));
//        Self { ident, sleigh }
//    }
//}

//struct PcodeMacroBuilder<'a, 'b> {
//    builder: &'a mut Builder<'b>,
//    pcode_macro: &'a PcodeMacro,
//    params: HashMap<Rc<str>, Ident>,
//    vars: HashMap<Rc<str>, Ident>,
//}
//
//impl<'a, 'b> PcodeMacroBuilder<'a, 'b> {
//    pub fn new(
//        builder: &'a mut Builder<'b>,
//        pcode_macro: &'a PcodeMacro,
//    ) -> Self {
//        Self {
//            builder,
//            pcode_macro,
//            params: HashMap::new(),
//            vars: HashMap::new(),
//        }
//    }
//    pub fn impl_function(&mut self) -> TokenStream {
//        let pcode_macros = self.builder.pcode_macros.insert(HashMap::new());
//        let function_name = Rc::clone(&self.pcode_macro.name);
//        let function_ident = pcode_macros
//            .entry(Rc::clone(&function_name))
//            .or_insert(format_ident!("macro_{}", *function_name))
//            .clone();
//        let params = self.pcode_macro.params.borrow();
//        self.params = params
//            .keys()
//            .map(|name| (Rc::clone(name), format_ident!("{}", **name)))
//            .collect();
//        let body = self.impl_body();
//        let params = self.params.values();
//        let ident = &self.builder.space_struct().ident;
//        quote! {
//            impl #ident {
//                fn #function_ident(&mut self, #(#params : usize),*) {
//                    #body
//                }
//            }
//        }
//    }
//
//    pub fn impl_body(&mut self) -> TokenStream {
//        let execution = self.pcode_macro.execution.borrow();
//
//        self.vars = execution
//            .vars
//            .values()
//            .map(|var| (Rc::clone(&var.name), format_ident!("{}", *var.name)))
//            .collect();
//        let vars = execution.vars.values().map(|var| {
//            let var = self.vars.get(&var.name).unwrap();
//            quote! {
//                let #var;
//            }
//        });
//
//        let entry_next = &execution.entry_block.next.borrow();
//        let entry_next = entry_next
//            .as_ref()
//            .map(|x| {
//                let next = x.name.as_ref().unwrap();
//                quote! {todo!{#next};}
//            })
//            .unwrap_or(quote! {todo!("Return");});
//        let entry_statements = execution.entry_block.statements.borrow();
//        let entry_statements = entry_statements.iter().map(|st| {
//            use sleigh_rs::parser::semantic::execution::Statement::*;
//            match st {
//                Delayslot(x) => quote! {todo!("delayslot({})", #x);},
//                Export(_) => quote! {todo!("export");},
//                CpuBranch(_) => quote! {todo!("cpubranch");},
//                LocalGoto(_) => quote! {todo!("localgoto");},
//                Call(_) => quote! {todo!("call");},
//                Build(_) => unreachable!(),
//                Declare(x) => {
//                    let var = self.vars.get(&x.name).unwrap();
//                    let var_type = todo!();
//                    quote! {let #var: #var_type;}
//                }
//                Assignment(_) => quote! {todo!();},
//            }
//        });
//        quote! {
//            #(#vars)*
//            #(#entry_statements)*
//            #entry_next
//        }
//    }
//}
//
//pub fn do_the_thing<'a, 'b>(
//    builder: &'a mut Builder<'b>,
//    pcode_macro: &'a PcodeMacro,
//) -> TokenStream {
//    let mut builder = PcodeMacroBuilder::new(builder, pcode_macro);
//    builder.impl_function()
//}
