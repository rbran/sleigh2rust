use indexmap::IndexMap;
use std::rc::{Rc, Weak};

use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use sleigh_rs::Number;
use sleigh_rs::{GlobalReference, PrintBase, PrintFmt};

use super::{DisassemblerGlobal, WorkType, DISPLAY_WORK_TYPE};
use crate::{NonZeroTypeU, NumberSuperSigned};

type VarMeaningSleigh = [(usize, GlobalReference<sleigh_rs::Varnode>)];
#[derive(Debug, Clone)]
pub struct VarMeaning {
    disassembler: Weak<dyn DisassemblerGlobal>,
    sleigh: Rc<VarMeaningSleigh>,
    pub display_func: Ident,
    pub value_func: Ident,
    pub index_type: WorkType,
    pub varnode_bytes: NonZeroTypeU,
}
impl VarMeaning {
    pub fn new(
        disassembler: Weak<dyn DisassemblerGlobal>,
        sleigh: Rc<VarMeaningSleigh>,
        fun_count: usize,
    ) -> Self {
        let index_max =
            sleigh.iter().map(|(index, _varnode)| *index).max().unwrap();
        let index_bits = (usize::BITS - index_max.leading_zeros()) + 1;
        let index_type = WorkType::new_int_bits(index_bits, false);
        let varnode_bytes = sleigh
            .first()
            .map(|(_index, varnode)| varnode.element().len_bytes())
            .unwrap();
        Self {
            disassembler,
            display_func: format_ident!("meaning_{}_display", fun_count),
            value_func: format_ident!("meaning_{}_value", fun_count),
            index_type,
            varnode_bytes,
            sleigh,
        }
    }
}
impl ToTokens for VarMeaning {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            disassembler,
            display_func,
            value_func,
            index_type,
            varnode_bytes: _,
            sleigh,
        } = self;
        let disassembler = disassembler.upgrade().unwrap();
        let display_element = &disassembler.display_element().name;
        let ele_index =
            sleigh.iter().map(|(i, _v)| Literal::usize_unsuffixed(*i));
        let ele_value = self.sleigh.iter().map(|(_i, v)| {
            let regs = disassembler.register().name();
            let variant =
                disassembler.register().register(v.element_ptr()).unwrap();
            quote! { #regs::#variant }
        });
        let registers_enum = disassembler.register().name();
        tokens.extend(quote! {
            fn #value_func<T>(num: T) -> #registers_enum
            where
                #index_type: TryFrom<T>,
                <#index_type as TryFrom<T>>::Error: core::fmt::Debug,
            {
                match #index_type::try_from(num).unwrap() {
                    #(#ele_index => #ele_value,)*
                    _ => unreachable!("Invalid Attach Value"),
                }
            }
            fn #display_func<T>(num: T) -> #display_element
            where
                #index_type: TryFrom<T>,
                <#index_type as TryFrom<T>>::Error: core::fmt::Debug,
            {
                let value = #value_func(num.try_into().unwrap());
                <#display_element>::Register(value)
            }
        });
    }
}

type NameMeaningSleigh = [(usize, String)];
#[derive(Debug, Clone)]
pub struct NameMeaning {
    disassembler: Weak<dyn DisassemblerGlobal>,
    sleigh: Rc<NameMeaningSleigh>,
    pub display_func: Ident,
    pub index_type: WorkType,
}
impl NameMeaning {
    pub fn new(
        disassembler: Weak<dyn DisassemblerGlobal>,
        sleigh: Rc<NameMeaningSleigh>,
        fun_count: usize,
    ) -> Self {
        let index_max =
            sleigh.iter().map(|(index, _varnode)| *index).max().unwrap();
        let index_bits = (usize::BITS - index_max.leading_zeros()) + 1;
        let index_type = WorkType::new_int_bits(index_bits, false);
        Self {
            sleigh,
            disassembler,
            index_type,
            display_func: format_ident!("meaning_{}_display", fun_count),
        }
    }
}
impl ToTokens for NameMeaning {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let param_type = &self.index_type;
        let disassembler = self.disassembler.upgrade().unwrap();
        let display_element = &disassembler.display_element().name;
        let ele_index = self
            .sleigh
            .iter()
            .map(|(i, _v)| Literal::usize_unsuffixed(*i));
        let ele_value = self.sleigh.iter().map(|(_i, v)| v);
        let display_func = &self.display_func;
        let index_type = &self.index_type;
        tokens.extend(quote! {
            fn #display_func<T>(num: T) -> #display_element
            where
                #param_type: TryFrom<T>,
                <#param_type as TryFrom<T>>::Error: core::fmt::Debug,
            {
                match #index_type::try_from(num).unwrap() {
                    #(#ele_index => <#display_element>::Literal(#ele_value),)*
                    _ => unreachable!("Invalid Attach Value"),
                }
            }
        });
    }
}
type ValueMeaningSleigh = [(usize, Number)];
#[derive(Debug, Clone)]
pub struct ValueMeaning {
    disassembler: Weak<dyn DisassemblerGlobal>,
    sleigh: Rc<ValueMeaningSleigh>,
    pub display_func: Ident,
    pub value_func: Ident,
    pub index_type: WorkType,
    pub value_type: WorkType,
}
impl ValueMeaning {
    pub fn new(
        disassembler: Weak<dyn DisassemblerGlobal>,
        sleigh: Rc<ValueMeaningSleigh>,
        fun_count: usize,
    ) -> Self {
        let index_max =
            sleigh.iter().map(|(index, _varnode)| *index).max().unwrap();
        let index_bits = (usize::BITS - index_max.leading_zeros()) + 1;
        let index_type = WorkType::new_int_bits(index_bits, false);
        let (min, max) = sleigh.iter().map(|(_i, value)| *value).fold(
            (0, 0),
            |(min, max), value| {
                let min = min.min(value.signed_super());
                let max = max.max(value.signed_super());
                (min, max)
            },
        );
        //TODO this is not TOTALLY true, but good enough for now
        let mut value_bits =
            NumberSuperSigned::BITS - min.abs().max(max.abs()).leading_zeros();
        let signed = min.is_negative();
        if min.is_negative() {
            value_bits += 1;
        }
        let value_type = WorkType::new_int_bits(value_bits, signed);
        Self {
            disassembler,
            sleigh,
            display_func: format_ident!("meaning_{}_display", fun_count),
            value_func: format_ident!("meaning_{}_value", fun_count),
            index_type,
            value_type,
        }
    }
}

impl ToTokens for ValueMeaning {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let disassembler = self.disassembler.upgrade().unwrap();
        let param_type = &self.index_type;
        let display_element = &disassembler.display_element().name;
        let ele_index = self
            .sleigh
            .iter()
            .map(|(i, _v)| Literal::usize_unsuffixed(*i));
        let ele_value = self
            .sleigh
            .iter()
            .map(|(_i, v)| Literal::i128_unsuffixed(v.signed_super()));
        let display_func = &self.display_func;
        let value_func = &self.value_func;
        let value_type = &self.value_type;
        let index_type = &self.index_type;
        tokens.extend(quote! {
            fn #value_func<T>(num: T) -> #value_type
            where
                #param_type: TryFrom<T>,
                <#param_type as TryFrom<T>>::Error: core::fmt::Debug,
            {
                match #index_type::try_from(num).unwrap() {
                    #(#ele_index => #ele_value,)*
                    _ => unreachable!("Invalid Attach Value"),
                }
            }
            fn #display_func<T>(hex: bool, num: T) -> #display_element
            where
                #param_type: TryFrom<T>,
                <#param_type as TryFrom<T>>::Error: core::fmt::Debug,
            {
                let value = #value_func(num);
                let value = #DISPLAY_WORK_TYPE::try_from(value).unwrap();
                <#display_element>::Number(hex, value)
            }
        });
    }
}

#[derive(Debug, Clone)]
pub enum MeaningExecutionType {
    Register(Rc<VarMeaning>),
    Value(WorkType),
}

impl ToTokens for MeaningExecutionType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            MeaningExecutionType::Register(regs) => regs
                .disassembler
                .upgrade()
                .unwrap()
                .register()
                .name()
                .to_tokens(tokens),
            MeaningExecutionType::Value(value) => value.to_tokens(tokens),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Meaning {
    Literal(PrintFmt),
    Variables(Rc<VarMeaning>),
    Names(Rc<NameMeaning>),
    Values(PrintBase, Rc<ValueMeaning>),
}
impl Meaning {
    pub fn execution_type(&self) -> Option<MeaningExecutionType> {
        match self {
            Meaning::Literal(_) | Meaning::Names(_) => None,
            Meaning::Variables(vars) => {
                Some(MeaningExecutionType::Register(Rc::clone(vars)))
            }
            Meaning::Values(_, values) => {
                Some(MeaningExecutionType::Value(values.value_type))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Meanings {
    disassembler: Weak<dyn DisassemblerGlobal>,
    pub literal_display: Ident,
    pub vars: IndexMap<*const VarMeaningSleigh, Rc<VarMeaning>>,
    pub names: IndexMap<*const NameMeaningSleigh, Rc<NameMeaning>>,
    pub values: IndexMap<*const ValueMeaningSleigh, Rc<ValueMeaning>>,
}

impl Meanings {
    pub fn new<'a>(
        disassembler: Weak<dyn DisassemblerGlobal>,
        meanings: impl Iterator<Item = &'a sleigh_rs::Meaning>,
    ) -> Self {
        let mut vars = IndexMap::new();
        let mut names = IndexMap::new();
        let mut values = IndexMap::new();
        let mut counter = 0usize;
        let mut counter_value = || {
            let value = counter;
            counter += 1;
            value
        };
        for meaning in meanings {
            match meaning {
                sleigh_rs::Meaning::Literal(_print_fmt) => {}
                sleigh_rs::Meaning::Variable(list) => {
                    vars.entry(Rc::as_ptr(list)).or_insert_with(|| {
                        Rc::new(VarMeaning::new(
                            Weak::clone(&disassembler),
                            Rc::clone(list),
                            counter_value(),
                        ))
                    });
                }
                sleigh_rs::Meaning::Name(list) => {
                    names.entry(Rc::as_ptr(list)).or_insert_with(|| {
                        Rc::new(NameMeaning::new(
                            Weak::clone(&disassembler),
                            Rc::clone(list),
                            counter_value(),
                        ))
                    });
                }
                sleigh_rs::Meaning::Value(_print_base, list) => {
                    values.entry(Rc::as_ptr(list)).or_insert_with(|| {
                        Rc::new(ValueMeaning::new(
                            Weak::clone(&disassembler),
                            Rc::clone(list),
                            counter_value(),
                        ))
                    });
                }
            }
        }
        Self {
            disassembler,
            vars,
            names,
            values,
            literal_display: format_ident!("meaning_number"),
        }
    }
    pub fn from_sleigh(&self, meaning: &sleigh_rs::Meaning) -> Meaning {
        match meaning {
            sleigh_rs::Meaning::Literal(print_fmt) => {
                Meaning::Literal(*print_fmt)
            }
            sleigh_rs::Meaning::Variable(var) => self
                .vars
                .get(&Rc::as_ptr(var))
                .map(Rc::clone)
                .map(Meaning::Variables)
                .unwrap(),
            sleigh_rs::Meaning::Name(var) => self
                .names
                .get(&Rc::as_ptr(var))
                .map(Rc::clone)
                .map(Meaning::Names)
                .unwrap(),
            sleigh_rs::Meaning::Value(print_base, var) => self
                .values
                .get(&Rc::as_ptr(var))
                .map(Rc::clone)
                .map(|x| Meaning::Values(*print_base, x))
                .unwrap(),
        }
    }
    pub fn display_function_call(
        &self,
        value: impl ToTokens,
        meaning: &sleigh_rs::Meaning,
    ) -> TokenStream {
        match meaning {
            sleigh_rs::Meaning::Literal(print_fmt) => {
                let function = &self.literal_display;
                let hex = print_fmt.base().is_hex();
                quote! { #function(#hex, #value) }
            }
            sleigh_rs::Meaning::Variable(vars) => {
                let meaning = self.vars.get(&Rc::as_ptr(vars)).unwrap();
                let function = &meaning.display_func;
                quote! { #function(#value) }
            }
            sleigh_rs::Meaning::Name(vars) => {
                let meaning = self.names.get(&Rc::as_ptr(vars)).unwrap();
                let function = &meaning.display_func;
                quote! { #function(#value) }
            }
            sleigh_rs::Meaning::Value(print_fmt, vars) => {
                let meaning = self.values.get(&Rc::as_ptr(vars)).unwrap();
                let function = &meaning.display_func;
                let hex = print_fmt.is_hex();
                quote! {
                    #function(#hex, #value)
                }
            }
        }
    }
}

impl ToTokens for Meanings {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            disassembler,
            literal_display,
            vars,
            names,
            values,
        } = self;
        let disassembler = disassembler.upgrade().unwrap();
        let variables = self.vars.values();
        let names = self.names.values();
        let values = self.values.values();
        let literal_func = &self.literal_display;
        let display_type = &disassembler.display_element().name;
        tokens.extend(quote! {
            fn #literal_func<T>(hex: bool, num: T) -> #display_type
            where
                #DISPLAY_WORK_TYPE: TryFrom<T>,
                <#DISPLAY_WORK_TYPE as TryFrom<T>>::Error: core::fmt::Debug,
            {
                #display_type::Number(
                    hex,
                    #DISPLAY_WORK_TYPE::try_from(num).unwrap(),
                )
            }
            #(#variables)*
            #(#names)*
            #(#values)*
        })
    }
}
