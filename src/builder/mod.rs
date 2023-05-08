use sleigh_rs::Number;

use ethnum::u256;

pub mod formater;

pub(crate) mod helper;

use proc_macro2::{Literal, TokenStream};

use quote::{quote, ToTokens};

mod context;
mod disassembler;
mod disassembly;
mod display;
mod meaning;
mod register;
mod token;

pub use context::*;
pub use disassembler::*;
pub use disassembly::*;
pub use display::*;
pub use meaning::*;
pub use register::*;
pub use token::*;

trait ToLiteral {
    fn suffixed(&self) -> Literal;
    fn unsuffixed(&self) -> Literal;
}

impl ToLiteral for Number {
    fn suffixed(&self) -> Literal {
        self.signed()
            .map(Literal::i64_suffixed)
            .unwrap_or_else(|| self.unsuffixed())
    }

    fn unsuffixed(&self) -> Literal {
        Literal::i128_unsuffixed(self.signed_super())
    }
}

macro_rules! impl_to_literal {
    ($type:ty, $suffixed:ident, $unsuffixed:ident) => {
        impl ToLiteral for $type {
            fn suffixed(&self) -> Literal {
                Literal::$suffixed(*self)
            }
            fn unsuffixed(&self) -> Literal {
                Literal::$unsuffixed(*self)
            }
        }
    };
}
impl_to_literal!(u8, u8_suffixed, u8_unsuffixed);
impl_to_literal!(u16, u16_suffixed, u16_unsuffixed);
impl_to_literal!(u32, u32_suffixed, u32_unsuffixed);
impl_to_literal!(u64, u64_suffixed, u64_unsuffixed);
impl_to_literal!(u128, u128_suffixed, u128_unsuffixed);
impl_to_literal!(usize, usize_suffixed, usize_unsuffixed);
impl_to_literal!(i8, i8_suffixed, i8_unsuffixed);
impl_to_literal!(i16, i16_suffixed, i16_unsuffixed);
impl_to_literal!(i32, i32_suffixed, i32_unsuffixed);
impl_to_literal!(i64, i64_suffixed, i64_unsuffixed);
impl_to_literal!(i128, i128_suffixed, i128_unsuffixed);
impl_to_literal!(isize, isize_suffixed, isize_unsuffixed);

//TODO remove the signed/unsigned
#[derive(Clone, Copy, Debug)]
pub enum WorkType {
    U8,
    U16,
    U32,
    U64,
    U128,
    U256,
    I8,
    I16,
    I32,
    I64,
    I128,
    I256,
}

impl WorkType {
    pub const NUMBER_UNSIGNED: Self =
        Self::new_int_bits(crate::NumberUnsigned::BITS, false);
    pub const NUMBER_SIGNED: Self =
        Self::new_int_bits(crate::NumberSigned::BITS, true);
    pub const NUMBER_SUPER_SIGNED: Self =
        Self::new_int_bits(crate::NumberSuperSigned::BITS, true);

    pub const NUMBER_NON_ZERO_UNSIGNED: Self =
        Self::new_int_bits(crate::NumberNonZeroUnsigned::BITS, false);
    pub const NUMBER_NON_ZERO_SIGNED: Self =
        Self::new_int_bits(crate::NumberNonZeroSigned::BITS, true);
    pub const NUMBER_NON_ZERO_SUPER_SIGNED: Self =
        Self::new_int_bits(crate::NumberNonZeroSuperSigned::BITS, true);
    pub const DISASSEMBLY_TYPE: Self =
        Self::new_int_bits(crate::DisassemblyType::BITS, true);

    pub const fn new_int_bytes(bytes: u32, signed: bool) -> Self {
        match bytes {
            1 if signed => Self::I8,
            2 if signed => Self::I16,
            3..=4 if signed => Self::I32,
            5..=8 if signed => Self::I64,
            9..=16 if signed => Self::I128,
            17..=32 if signed => Self::I256,
            1 /*if !signed*/ => Self::U8,
            2 /*if !signed*/ => Self::U16,
            3..=4 /*if !signed*/ => Self::U32,
            5..=8 /*if !signed*/ => Self::U64,
            9..=16 /*if !signed*/ => Self::U128,
            17..=32 /*if !signed*/ => Self::U256,
            _x => unreachable!(),
        }
    }
    pub const fn new_int_bits(bits: u32, signed: bool) -> Self {
        Self::new_int_bytes((bits + 7) / 8, signed)
    }
    pub const fn unsigned_from_bytes(bytes: u32) -> Self {
        Self::new_int_bytes(bytes, false)
    }
    pub const fn unsigned_from_bits(bits: u32) -> Self {
        Self::new_int_bits(bits, false)
    }
    pub const fn is_signed(&self) -> bool {
        match self {
            WorkType::U8
            | WorkType::U16
            | WorkType::U32
            | WorkType::U64
            | WorkType::U128
            | WorkType::U256 => false,
            WorkType::I8
            | WorkType::I16
            | WorkType::I32
            | WorkType::I64
            | WorkType::I128
            | WorkType::I256 => true,
        }
    }
    pub const fn len_bytes(&self) -> u32 {
        match self {
            WorkType::I8 | WorkType::U8 => u8::BITS / 8,
            WorkType::I16 | WorkType::U16 => u16::BITS / 8,
            WorkType::I32 | WorkType::U32 => u32::BITS / 8,
            WorkType::I64 | WorkType::U64 => u64::BITS / 8,
            WorkType::I128 | WorkType::U128 => u128::BITS / 8,
            WorkType::I256 | WorkType::U256 => u256::BITS / 8,
        }
    }
}

impl ToTokens for WorkType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            WorkType::U8 => tokens.extend(quote! {u8}),
            WorkType::U16 => tokens.extend(quote! {u16}),
            WorkType::U32 => tokens.extend(quote! {u32}),
            WorkType::U64 => tokens.extend(quote! {u64}),
            WorkType::U128 => tokens.extend(quote! {u128}),
            WorkType::U256 => tokens.extend(quote! {ethnum::u256}),
            WorkType::I8 => tokens.extend(quote! {i8}),
            WorkType::I16 => tokens.extend(quote! {i16}),
            WorkType::I32 => tokens.extend(quote! {i32}),
            WorkType::I64 => tokens.extend(quote! {i64}),
            WorkType::I128 => tokens.extend(quote! {i128}),
            WorkType::I256 => tokens.extend(quote! {ethnum::i256}),
        }
    }
}
