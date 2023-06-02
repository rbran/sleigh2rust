use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};

use super::{Disassembler, WorkType, DISPLAY_WORK_TYPE};
use crate::{NonZeroTypeU, NumberSuperSigned};

#[derive(Debug, Clone)]
pub struct VarMeaning {
    id: sleigh_rs::AttachVarnodeId,
    pub display_func: Ident,
    pub value_func: Ident,
    pub index_type: WorkType,
    pub varnode_bytes: NonZeroTypeU,
}
impl VarMeaning {
    pub fn new(
        sleigh: &sleigh_rs::Sleigh,
        id: sleigh_rs::AttachVarnodeId,
        attach: &sleigh_rs::meaning::AttachVarnode,
        fun_count: usize,
    ) -> Self {
        let index_max = attach
            .0
            .iter()
            .map(|(index, _varnode)| *index)
            .max()
            .unwrap();
        let index_bits = (usize::BITS - index_max.leading_zeros()) + 1;
        let index_type = WorkType::new_int_bits(index_bits, false);
        let varnode_bytes = attach
            .0
            .first()
            .map(|(_index, varnode)| sleigh.varnode(*varnode).len_bytes)
            .unwrap();
        Self {
            display_func: format_ident!("meaning_{}_display", fun_count),
            value_func: format_ident!("meaning_{}_value", fun_count),
            index_type,
            varnode_bytes,
            id,
        }
    }

    pub fn to_tokens(
        &self,
        tokens: &mut TokenStream,
        disassembler: &Disassembler,
    ) {
        let Self {
            id,
            display_func,
            value_func,
            index_type,
            varnode_bytes: _,
        } = self;
        let sleigh = disassembler.sleigh.attach_varnode(*id);
        let display_element = &disassembler.display.name;
        let ele_index =
            sleigh.0.iter().map(|(i, _v)| Literal::usize_unsuffixed(*i));
        let ele_value = sleigh.0.iter().map(|(_i, v)| {
            let regs = disassembler.registers.name();
            let variant = disassembler.registers.register(*v);
            quote! { #regs::#variant }
        });
        let registers_enum = disassembler.registers.name();
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

#[derive(Debug, Clone)]
pub struct NameMeaning {
    id: sleigh_rs::AttachLiteralId,
    pub display_func: Ident,
    pub index_type: WorkType,
}
impl NameMeaning {
    pub fn new(
        id: sleigh_rs::AttachLiteralId,
        attach: &sleigh_rs::meaning::AttachLiteral,
        fun_count: usize,
    ) -> Self {
        let index_max = attach
            .0
            .iter()
            .map(|(index, _varnode)| *index)
            .max()
            .unwrap();
        let index_bits = (usize::BITS - index_max.leading_zeros()) + 1;
        let index_type = WorkType::new_int_bits(index_bits, false);
        Self {
            id,
            display_func: format_ident!("meaning_{}_display", fun_count),
            index_type,
        }
    }
    pub fn to_tokens(
        &self,
        tokens: &mut TokenStream,
        disassembler: &Disassembler,
    ) {
        let param_type = &self.index_type;
        let display_element = &disassembler.display.name;
        let attach = disassembler.sleigh.attach_literal(self.id);
        let ele_index =
            attach.0.iter().map(|(i, _v)| Literal::usize_unsuffixed(*i));
        let ele_value = attach.0.iter().map(|(_i, v)| v);
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
#[derive(Debug, Clone)]
pub struct ValueMeaning {
    id: sleigh_rs::AttachNumberId,
    pub display_func: Ident,
    pub value_func: Ident,
    pub index_type: WorkType,
    pub value_type: WorkType,
}
impl ValueMeaning {
    pub fn new(
        id: sleigh_rs::AttachNumberId,
        attach: &sleigh_rs::meaning::AttachNumber,
        fun_count: usize,
    ) -> Self {
        let index_max = attach
            .0
            .iter()
            .map(|(index, _varnode)| *index)
            .max()
            .unwrap();
        let index_bits = (usize::BITS - index_max.leading_zeros()) + 1;
        let index_type = WorkType::new_int_bits(index_bits, false);
        let (min, max) = attach.0.iter().map(|(_i, value)| *value).fold(
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
            id,
            display_func: format_ident!("meaning_{}_display", fun_count),
            value_func: format_ident!("meaning_{}_value", fun_count),
            index_type,
            value_type,
        }
    }

    pub fn to_tokens(
        &self,
        tokens: &mut TokenStream,
        disassembler: &Disassembler,
    ) {
        let param_type = &self.index_type;
        let display_element = &disassembler.display.name;
        let sleigh = disassembler.sleigh.attach_number(self.id);
        let ele_index =
            sleigh.0.iter().map(|(i, _v)| Literal::usize_unsuffixed(*i));
        let ele_value = sleigh
            .0
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
pub struct Meanings {
    pub literal_display: Ident,
    pub vars: Vec<VarMeaning>,
    pub names: Vec<NameMeaning>,
    pub values: Vec<ValueMeaning>,
}

impl Meanings {
    pub fn new(sleigh: &sleigh_rs::Sleigh) -> Self {
        let mut counter = 0usize;
        let mut counter_value = || {
            let value = counter;
            counter += 1;
            value
        };
        let vars = sleigh
            .attach_varnodes()
            .iter()
            .enumerate()
            .map(|(i, var)| {
                VarMeaning::new(
                    sleigh,
                    sleigh_rs::AttachVarnodeId(i),
                    var,
                    counter_value(),
                )
            })
            .collect();
        let names = sleigh
            .attach_literals()
            .iter()
            .enumerate()
            .map(|(i, var)| {
                NameMeaning::new(
                    sleigh_rs::AttachLiteralId(i),
                    var,
                    counter_value(),
                )
            })
            .collect();
        let values = sleigh
            .attach_numbers()
            .iter()
            .enumerate()
            .map(|(i, var)| {
                ValueMeaning::new(
                    sleigh_rs::AttachNumberId(i),
                    var,
                    counter_value(),
                )
            })
            .collect();
        Self {
            vars,
            names,
            values,
            literal_display: format_ident!("meaning_number"),
        }
    }
    pub fn display_function_call(
        &self,
        len_bits: u32,
        value: impl ToTokens,
        meaning: sleigh_rs::meaning::Meaning,
    ) -> TokenStream {
        use sleigh_rs::meaning::Meaning;
        match meaning {
            Meaning::NoAttach(print_fmt) => {
                let function = &self.literal_display;
                let value = if !print_fmt.signed {
                    value.into_token_stream()
                } else {
                    let final_type = WorkType::new_int_bits(len_bits, true);
                    crate::builder::helper::sign_from_value(
                        len_bits, final_type, value,
                    )
                };
                let hex = print_fmt.base.is_hex();
                quote! { #function(#hex, #value) }
            }
            Meaning::Varnode(vars) => {
                let function = &self.vars[vars.0].display_func;
                quote! { #function(#value) }
            }
            Meaning::Literal(vars) => {
                let function = &self.names[vars.0].display_func;
                quote! { #function(#value) }
            }
            Meaning::Number(base, vars) => {
                let function = &self.values[vars.0].display_func;
                let hex = base.is_hex();
                quote! {
                    #function(#hex, #value)
                }
            }
        }
    }

    pub fn disassembly_function_call(
        &self,
        len_bits: u32,
        value: impl ToTokens,
        meaning: sleigh_rs::meaning::Meaning,
    ) -> TokenStream {
        use sleigh_rs::meaning::Meaning;
        match meaning {
            Meaning::NoAttach(print_fmt) if print_fmt.signed => {
                let final_type = WorkType::new_int_bits(len_bits, true);
                crate::builder::helper::sign_from_value(
                    len_bits, final_type, value,
                )
            }
            Meaning::Number(_base, vars) => {
                let function = &self.values[vars.0].value_func;
                quote! { #function(#value) }
            }
            _ => value.into_token_stream(),
        }
    }

    //TODO
    //pub fn execution_function_call(
    //  todo!();
    //}

    pub fn to_tokens(
        &self,
        tokens: &mut TokenStream,
        disassembler: &Disassembler,
    ) {
        let literal_func = &self.literal_display;
        let display_type = &disassembler.display.name;
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
        });
        for variable in self.vars.iter() {
            variable.to_tokens(tokens, disassembler);
        }
        for name in self.names.iter() {
            name.to_tokens(tokens, disassembler);
        }
        for value in self.values.iter() {
            value.to_tokens(tokens, disassembler);
        }
    }
}
