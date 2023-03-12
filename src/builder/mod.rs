use sleigh_rs::Number;

use ethnum::u256;

pub mod formater;

mod memory;
pub use memory::*;

mod token;
pub use token::*;

mod disassembly;
pub use disassembly::*;

mod disassembler;
pub use disassembler::*;

mod display;
pub use display::*;

mod register;
pub use register::*;

mod globalset;
pub use globalset::*;

mod meaning;
pub use meaning::*;

use proc_macro2::{Literal, TokenStream};
use quote::{quote, ToTokens};
use sleigh4rust::{
    IntTypeU, NonZeroTypeU, NumberNonZeroSigned, NumberNonZeroSuperSigned,
    NumberNonZeroUnsigned, NumberSigned, NumberSuperSigned, NumberUnsigned,
};

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
    Array(NonZeroTypeU),
}

impl WorkType {
    pub const NUMBER_UNSIGNED: Self =
        Self::const_creator(NumberUnsigned::BITS, false);
    pub const NUMBER_SIGNED: Self =
        Self::const_creator(NumberSigned::BITS, true);
    pub const NUMBER_SUPER_SIGNED: Self =
        Self::const_creator(NumberSuperSigned::BITS, true);

    pub const NUMBER_NON_ZERO_UNSIGNED: Self =
        Self::const_creator(NumberNonZeroUnsigned::BITS, false);
    pub const NUMBER_NON_ZERO_SIGNED: Self =
        Self::const_creator(NumberNonZeroSigned::BITS, true);
    pub const NUMBER_NON_ZERO_SUPER_SIGNED: Self =
        Self::const_creator(NumberNonZeroSuperSigned::BITS, true);

    const fn const_creator(bits: u32, signed: bool) -> Self {
        let Some(bits) = NonZeroTypeU::new((bits as NumberUnsigned + 7) / 8) else { unreachable!() };
        Self::new_int_bytes(bits, signed)
    }

    pub fn new_array(bytes: NonZeroTypeU) -> Self {
        Self::Array(bytes)
    }
    pub const fn new_int_bytes(bytes: NonZeroTypeU, signed: bool) -> Self {
        match bytes.get() {
            0 => unreachable!(),
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
            _x => todo!(),
        }
    }
    pub const fn new_int_bits(bits: NonZeroTypeU, signed: bool) -> Self {
        let Some(bytes) = NonZeroTypeU::new((bits.get() + 7) / 8) else { unreachable!() };
        Self::new_int_bytes(bytes, signed)
    }
    pub fn unsigned_from_bytes(bytes: NonZeroTypeU) -> Self {
        Self::new_int_bytes(bytes, false)
    }
    pub fn unsigned_from_bits(bits: NonZeroTypeU) -> Self {
        Self::new_int_bits(bits, false)
    }
    pub fn from_token_field(field: &sleigh_rs::TokenField) -> Self {
        let bits = field.range().len_bits().get().try_into().unwrap();
        Self::new_int_bits(bits, field.meaning().is_signed())
    }
    pub fn from_varnode(varnode: &sleigh_rs::Varnode) -> Self {
        Self::unsigned_from_bytes(varnode.len_bytes())
    }
    pub fn from_bitrange(bitrange: &sleigh_rs::Bitrange) -> Self {
        Self::new_int_bits(bitrange.range.len_bits(), false)
    }
    pub fn from_context(context: &sleigh_rs::Context) -> Self {
        let bit_len: NonZeroTypeU =
            context.range.len_bits().get().try_into().unwrap();
        Self::new_int_bits(bit_len, context.meaning().is_signed())
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
            WorkType::Array(_) => todo!(),
        }
    }
    pub const fn len_bytes(&self) -> NonZeroTypeU {
        let bits = match self {
            WorkType::I8 | WorkType::U8 => u8::BITS / 8,
            WorkType::I16 | WorkType::U16 => u16::BITS / 8,
            WorkType::I32 | WorkType::U32 => u32::BITS / 8,
            WorkType::I64 | WorkType::U64 => u64::BITS / 8,
            WorkType::I128 | WorkType::U128 => u128::BITS / 8,
            WorkType::I256 | WorkType::U256 => u256::BITS / 8,
            WorkType::Array(x) => return *x,
        };
        unsafe { NonZeroTypeU::new_unchecked(bits as IntTypeU) }
    }
    pub fn to_literal(&self, value: IntTypeU) -> TokenStream {
        match self {
            WorkType::U8 => Literal::u8_suffixed(value.try_into().unwrap())
                .to_token_stream(),
            WorkType::U16 => Literal::u16_suffixed(value.try_into().unwrap())
                .to_token_stream(),
            WorkType::U32 => Literal::u32_suffixed(value.try_into().unwrap())
                .to_token_stream(),
            WorkType::U64 => Literal::u64_suffixed(value.try_into().unwrap())
                .to_token_stream(),
            WorkType::U128 => Literal::u128_suffixed(value.try_into().unwrap())
                .to_token_stream(),
            WorkType::U256 => {
                let value = Literal::u64_suffixed(value);
                quote! {
                    ethnum::u256::from(#value)
                }
            }
            WorkType::I8 => Literal::i8_suffixed(value.try_into().unwrap())
                .to_token_stream(),
            WorkType::I16 => Literal::i16_suffixed(value.try_into().unwrap())
                .to_token_stream(),
            WorkType::I32 => Literal::i32_suffixed(value.try_into().unwrap())
                .to_token_stream(),
            WorkType::I64 => Literal::i64_suffixed(value.try_into().unwrap())
                .to_token_stream(),
            WorkType::I128 => Literal::i128_suffixed(value.try_into().unwrap())
                .to_token_stream(),
            WorkType::I256 => {
                let value = Literal::u64_suffixed(value);
                quote! {
                    ethnum::i256::from(#value)
                }
            }
            WorkType::Array(_) => todo!(),
        }
    }
    fn sleigh4rust_read_memory(&self) -> TokenStream {
        match self {
            WorkType::U8 => quote! {read_u8},
            WorkType::U16 => quote! {read_u16},
            WorkType::U32 => quote! {read_u32},
            WorkType::U64 => quote! {read_u64},
            WorkType::U128 => quote! {read_u128},
            WorkType::U256 => quote! {read_u256},
            WorkType::I8 => quote! {read_i8},
            WorkType::I16 => quote! {read_i16},
            WorkType::I32 => quote! {read_i32},
            WorkType::I64 => quote! {read_i64},
            WorkType::I128 => quote! {read_i128},
            WorkType::I256 => quote! {read_i256},
            WorkType::Array(_) => todo!(),
        }
    }
    fn sleigh4rust_write_memory(&self) -> TokenStream {
        match self {
            WorkType::U8 => quote! {write_u8},
            WorkType::U16 => quote! {write_u16},
            WorkType::U32 => quote! {write_u32},
            WorkType::U64 => quote! {write_u64},
            WorkType::U128 => quote! {write_u128},
            WorkType::U256 => quote! {write_u256},
            WorkType::I8 => quote! {write_i8},
            WorkType::I16 => quote! {write_i16},
            WorkType::I32 => quote! {write_i32},
            WorkType::I64 => quote! {write_i64},
            WorkType::I128 => quote! {write_i128},
            WorkType::I256 => quote! {write_i256},
            WorkType::Array(_) => todo!(),
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
            WorkType::Array(len) => {
                let len: usize = len.get().try_into().unwrap();
                tokens.extend(quote! {[u8; #len]})
            }
        }
    }
}
