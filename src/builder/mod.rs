pub mod formater;

mod memory;
use std::rc::Rc;

use ethnum::u256;
pub use memory::*;

mod token;
use sleigh_rs::Number;
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

use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use sleigh4rust::{
    IntTypeU, NonZeroTypeU, NumberNonZeroSigned,
    NumberNonZeroSuperSigned, NumberNonZeroUnsigned, NumberSigned,
    NumberSuperSigned, NumberUnsigned,
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
    const NUMBER_UNSIGNED: Self =
        Self::const_creator(NumberUnsigned::BITS, false);
    const NUMBER_SIGNED: Self = Self::const_creator(NumberSigned::BITS, true);
    const NUMBER_SUPER_SIGNED: Self =
        Self::const_creator(NumberSuperSigned::BITS, true);

    const NUMBER_NON_ZERO_UNSIGNED: Self =
        Self::const_creator(NumberNonZeroUnsigned::BITS, false);
    const NUMBER_NON_ZERO_SIGNED: Self =
        Self::const_creator(NumberNonZeroSigned::BITS, true);
    const NUMBER_NON_ZERO_SUPER_SIGNED: Self =
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

//TODO reimplement that using a trait instead of this shit
pub struct BitrangeFromMemory {
    pub big_endian: bool,
    pub bitrange_rw: Rc<BitrangeRW>,
    pub mem_start: IntTypeU,
    pub read_bytes: NonZeroTypeU,
    pub work_type: WorkType,
    pub work_start: IntTypeU,
    pub bit_start: IntTypeU,
    pub bit_len: NonZeroTypeU,
    pub return_type: WorkType,
}

impl BitrangeFromMemory {
    pub fn new(
        big_endian: bool,
        bitrange_rw: Rc<BitrangeRW>,
        varnode_addr: IntTypeU,
        varnode_bytes: NonZeroTypeU,
        bit_start: IntTypeU,
        bit_len: NonZeroTypeU,
        signed: bool,
    ) -> Self {
        let return_type = WorkType::new_int_bits(bit_len, signed);
        let varnode_bytes = varnode_bytes.get();
        let len_bits = bit_len.get();
        let bit_start_start_byte = bit_start % 8;
        let read_bytes = (len_bits + bit_start_start_byte + 7) / 8;
        let start_byte = if big_endian {
            (varnode_bytes - read_bytes) - (bit_start / 8)
        } else {
            bit_start / 8
        };
        assert!(varnode_bytes >= start_byte + read_bytes);
        let mem_start = varnode_addr + start_byte;
        let read_bytes = unsafe { NonZeroTypeU::new_unchecked(read_bytes) };
        let work_type = WorkType::new_int_bytes(read_bytes, signed);
        let work_bytes = work_type.len_bytes();
        let work_start = if big_endian {
            work_bytes.get() - read_bytes.get()
        } else {
            0
        };
        Self {
            big_endian,
            bitrange_rw,
            mem_start,
            read_bytes,
            work_type,
            work_start,
            bit_start: bit_start_start_byte,
            bit_len,
            return_type,
        }
    }
    pub fn return_type(&self) -> &WorkType {
        &self.return_type
    }
    pub fn read_value<T>(&self, read: T) -> TokenStream
    where
        T: FnOnce(&Ident, NonZeroTypeU, IntTypeU, IntTypeU) -> TokenStream,
    {
        let Self {
            big_endian,
            bitrange_rw,
            mem_start,
            read_bytes,
            work_type,
            work_start,
            bit_start,
            bit_len,
            return_type,
        } = self;
        let work_value = format_ident!("work_value");
        let read_const =
            read(&work_value, *read_bytes, *mem_start, *work_start);
        let work_type_bytes = work_type.len_bytes().get();
        let bitrange_read = bitrange_rw.read_function(&self.work_type);
        let bit_len = bit_len.get();
        quote! {
            let mut #work_value = [0u8; #work_type_bytes as usize];
            #read_const
            let value = #bitrange_read::<#big_endian>(
                #work_value,
                #bit_start as usize,
                #bit_len as usize,
            );
            Ok(#return_type::try_from(value).unwrap())
        }
    }
}

//TODO make this generic and remove this bullshit
#[derive(Debug)]
pub struct BitrangeRW {
    pub read_u8: Ident,
    pub read_i8: Ident,
    pub read_u16: Ident,
    pub read_i16: Ident,
    pub read_u32: Ident,
    pub read_i32: Ident,
    pub read_u64: Ident,
    pub read_i64: Ident,
    pub read_u128: Ident,
    pub read_i128: Ident,
    pub read_u256: Ident,
    pub read_i256: Ident,
    pub write_u8: Ident,
    pub write_i8: Ident,
    pub write_u16: Ident,
    pub write_i16: Ident,
    pub write_u32: Ident,
    pub write_i32: Ident,
    pub write_u64: Ident,
    pub write_i64: Ident,
    pub write_u128: Ident,
    pub write_i128: Ident,
    pub write_u256: Ident,
    pub write_i256: Ident,
}
impl BitrangeRW {
    pub fn new() -> Self {
        Self {
            read_u8: format_ident!("read_u8"),
            read_i8: format_ident!("read_i8"),
            read_u16: format_ident!("read_u16"),
            read_i16: format_ident!("read_i16"),
            read_u32: format_ident!("read_u32"),
            read_i32: format_ident!("read_i32"),
            read_u64: format_ident!("read_u64"),
            read_i64: format_ident!("read_i64"),
            read_u128: format_ident!("read_u128"),
            read_i128: format_ident!("read_i128"),
            read_u256: format_ident!("read_u256"),
            read_i256: format_ident!("read_i256"),
            write_u8: format_ident!("write_u8"),
            write_i8: format_ident!("write_i8"),
            write_u16: format_ident!("write_u16"),
            write_i16: format_ident!("write_i16"),
            write_u32: format_ident!("write_u32"),
            write_i32: format_ident!("write_i32"),
            write_u64: format_ident!("write_u64"),
            write_i64: format_ident!("write_i64"),
            write_u128: format_ident!("write_u128"),
            write_i128: format_ident!("write_i128"),
            write_u256: format_ident!("write_u256"),
            write_i256: format_ident!("write_i256"),
        }
    }
    pub fn read_function(&self, work_type: &WorkType) -> &Ident {
        match work_type {
            WorkType::U8 => &self.read_u8,
            WorkType::U16 => &self.read_u16,
            WorkType::U32 => &self.read_u32,
            WorkType::U64 => &self.read_u64,
            WorkType::U128 => &self.read_u128,
            WorkType::U256 => &self.read_u256,
            WorkType::I8 => &self.read_i8,
            WorkType::I16 => &self.read_i16,
            WorkType::I32 => &self.read_i32,
            WorkType::I64 => &self.read_i64,
            WorkType::I128 => &self.read_i128,
            WorkType::I256 => &self.read_i256,
            WorkType::Array(_) => todo!(),
        }
    }
    pub fn write_function(&self, work_type: &WorkType) -> &Ident {
        match work_type {
            WorkType::U8 => &self.write_u8,
            WorkType::U16 => &self.write_u16,
            WorkType::U32 => &self.write_u32,
            WorkType::U64 => &self.write_u64,
            WorkType::U128 => &self.write_u128,
            WorkType::U256 => &self.write_u256,
            WorkType::I8 => &self.write_i8,
            WorkType::I16 => &self.write_i16,
            WorkType::I32 => &self.write_i32,
            WorkType::I64 => &self.write_i64,
            WorkType::I128 => &self.write_i128,
            WorkType::I256 => &self.write_i256,
            WorkType::Array(_) => todo!(),
        }
    }
}

impl ToTokens for BitrangeRW {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let BitrangeRW {
            read_u8,
            read_i8,
            read_u16,
            read_i16,
            read_u32,
            read_i32,
            read_u64,
            read_i64,
            read_u128,
            read_i128,
            read_u256,
            read_i256,
            write_u8,
            write_i8,
            write_u16,
            write_i16,
            write_u32,
            write_i32,
            write_u64,
            write_i64,
            write_u128,
            write_i128,
            write_u256,
            write_i256,
        } = self;
        //FUTURE, instead of impl for all types, use generics with const impl
        //AKA: `~const Add + ~const Sub`
        tokens.extend(quote! {
            macro_rules! impl_read_to_type {
                (
                    $unsigned_type:ty,
                    $signed_type:ty,
                    $len:literal,
                    $read_unsigned:ident,
                    $read_signed:ident,
                    $write_unsigned:ident,
                    $write_signed:ident
                ) => {
                    fn $read_unsigned<const BIG_ENDIAN: bool>(
                        data: [u8; $len],
                        start_bit: usize,
                        len_bits: usize,
                    ) -> $unsigned_type {
                        const TYPE_BITS: usize = <$unsigned_type>::BITS as usize;
                        assert!(TYPE_BITS / 8 == $len);
                        assert!(len_bits > 0);
                        assert!(len_bits + start_bit <= TYPE_BITS);
                        let mut data = if BIG_ENDIAN {
                            <$unsigned_type>::from_be_bytes(data)
                        } else {
                            <$unsigned_type>::from_le_bytes(data)
                        };
                        let value_mask = <$unsigned_type>::MAX >> (TYPE_BITS - len_bits);
                        data = data >> start_bit;
                        data = data & value_mask;
                        data
                    }
                    fn $read_signed<const BIG_ENDIAN: bool>(
                        data: [u8; $len],
                        start_bit: usize,
                        len_bits: usize,
                    ) -> $signed_type {
                        const TYPE_BITS: usize = <$signed_type>::BITS as usize;
                        assert!(len_bits > 1);
                        assert!(TYPE_BITS / 8 == $len);
                        let data = $read_unsigned::<BIG_ENDIAN>(data, start_bit, len_bits);
                        let value_mask =
                            <$unsigned_type>::try_from(<$signed_type>::MAX).unwrap()  >> (TYPE_BITS - len_bits);
                        let sign_mask = !value_mask;
                        let value_part = data & value_mask;
                        let sign_part = data & sign_mask;
                        //TODO: make better makeshift `as` that also works with u256
                        if sign_part != 0 {
                            let neg_value = (!value_part + 1) & value_mask;
                            <$signed_type>::try_from(neg_value).unwrap().checked_neg().unwrap()
                        } else {
                            <$signed_type>::try_from(value_part).unwrap()
                        }
                    }
                    fn $write_unsigned<const BIG_ENDIAN: bool>(
                        value: $unsigned_type,
                        mem: $unsigned_type,
                        start_bit: usize,
                        len_bits: usize,
                    ) -> [u8; $len] {
                        const TYPE_BITS: usize = <$unsigned_type>::BITS as usize;
                        assert!(len_bits > 0);
                        assert!(len_bits + start_bit <= TYPE_BITS);
                        let value_max = <$unsigned_type>::MAX >> (TYPE_BITS - len_bits);
                        let mask = value_max << start_bit;
                        let mut value = value;
                        //NOTE context write can overflow
                        //assert!(value <= value_max);
                        value <<= start_bit;
                        value = (mem & !mask) | value;
                        if BIG_ENDIAN {
                            value.to_be_bytes()
                        } else {
                            value.to_le_bytes()
                        }
                    }
                    fn $write_signed<const BIG_ENDIAN: bool>(
                        value: $signed_type,
                        mem: $signed_type,
                        start_bit: usize,
                        len_bits: usize,
                    ) -> [u8; $len] {
                        const TYPE_BITS: usize = <$unsigned_type>::BITS as usize;
                        assert!(len_bits > 0);
                        assert!(len_bits + start_bit <= TYPE_BITS);
                        //NOTE context write can overflow
                        //let value_max = <$signed_type>::MAX >> (TYPE_BITS - len_bits);
                        //let value_min = <$signed_type>::MIN >> (TYPE_BITS - len_bits);
                        //assert!(value <= value_max);
                        //assert!(value >= value_min);
                        //TODO: make better makeshift `as` that also works with u256
                        let value: $unsigned_type = if value < 0 {
                            <$unsigned_type>::MAX - <$unsigned_type>::try_from(value.abs() - 1).unwrap()
                        } else {
                            <$unsigned_type>::try_from(value).unwrap()
                        };
                        let mem: $unsigned_type = if mem < 0 {
                            <$unsigned_type>::MAX - <$unsigned_type>::try_from(mem.abs() - 1).unwrap()
                        } else {
                            <$unsigned_type>::try_from(value).unwrap()
                        };

                        let mask = <$unsigned_type>::MAX >> (TYPE_BITS - len_bits);
                        let value = value & mask;
                        $write_unsigned::<BIG_ENDIAN>(value, mem, start_bit, len_bits)
                    }
                };
            }
            impl_read_to_type!(u8, i8, 1, #read_u8, #read_i8, #write_u8, #write_i8);
            impl_read_to_type!(u16, i16, 2, #read_u16, #read_i16, #write_u16, #write_i16);
            impl_read_to_type!(u32, i32, 4, #read_u32, #read_i32, #write_u32, #write_i32);
            impl_read_to_type!(u64, i64, 8, #read_u64, #read_i64, #write_u64, #write_i64);
            impl_read_to_type!(u128, i128, 16, #read_u128, #read_i128, #write_u128, #write_i128);
            impl_read_to_type!(ethnum::u256, ethnum::i256, 32, #read_u256, #read_i256, #write_u256, #write_i256);
        });
    }
}

//pub struct Emulator<'a> {
//    sleigh: &'a Sleigh,
//    memory: Memory,
//    function: Function,
//}
//
//impl<'a> Emulator<'a> {
//    pub fn new(sleigh: &'a Sleigh) -> Self {
//        let memory = Memory::new(sleigh);
//        let function = Function::new(sleigh);
//        Self {
//            sleigh,
//            memory,
//            function,
//        }
//    }
//    pub fn memory_struct(&self) -> TokenStream {
//        self.memory.gen_memory_struct()
//    }
//    pub fn memory_trait(&self) -> TokenStream {
//        self.memory.gen_memory_trait()
//    }
//    pub fn context_struct(&self) -> TokenStream {
//        todo!()
//    }
//    pub fn context_trait(&self) -> TokenStream {
//        todo!()
//    }
//    pub fn memory_impl_trait(&self) -> TokenStream {
//        self.memory.struct_impl_trait()
//    }
//
//    //pub fn impl_memory_addr_function(&mut self) -> TokenStream {
//    //    let mut memory_addr_function = IndexMap::new();
//
//    //    let data = format_ident!("data");
//    //    let addr = format_ident!("addr");
//    //    let read = format_ident!("read");
//    //    let funs = self
//    //        .sleigh
//    //        .global_scope
//    //        .values()
//    //        .filter_map(|x| x.unwrap_space())
//    //        .map(|space| {
//    //            match space.space_type {
//    //                SpaceType::Const => quote!{},
//    //                SpaceType::Unique => quote!{},
//    //                SpaceType::Rom(_) => {
//    //            let chunks = self
//    //                .space_struct()
//    //                .chunks.iter()
//    //                .filter(|(chunk_space, _, _)| chunk_space.name == space.name)
//    //                .map(|(_space, ident, range)| {
//    //                    let chunk_start = range.start;
//    //                    let chunk_end = range.end - 1;
//    //                    quote!{
//    //                        #chunk_start..=#chunk_end => {
//    //                            if !#read {
//    //                                unreachable!("Rom can't be written")
//    //                            }
//    //                            let start = #addr - #chunk_start;
//    //                            let end = start + #data.len();
//    //                            #data.copy_from_slice(&self.#ident[start..end]);
//    //                        }
//    //                    }
//    //            });
//    //            let fn_name = format_ident!("rw_{}_addr", *space.name);
//    //            memory_addr_function
//    //                .insert(Rc::clone(&space.name), fn_name.clone());
//    //            quote! {
//    //                pub fn #fn_name(&mut self, #addr: usize, #data: &mut [u8]) {
//    //                    match #addr {
//    //                        #(#chunks),*
//    //                        _ => unreachable!(),
//    //                    }
//    //                }
//    //            }
//    //                },
//    //                SpaceType::Ram(_) |
//    //                SpaceType::Register(_) => {
//    //            let chunks = self
//    //                .space_struct()
//    //                .chunks.iter()
//    //                .filter(|(chunk_space, _, _)| chunk_space.name == space.name)
//    //                .map(|(_space, ident, range)| {
//
//    //                    let chunk_start = range.start;
//    //                    let chunk_end = range.end - 1;
//    //                    quote!{
//    //                        #chunk_start..=#chunk_end => {
//    //                            let start = #addr - #chunk_start;
//    //                            let end = start + #data.len();
//    //                            if read {
//    //                                #data.copy_from_slice(&self.#ident[start..end]);
//    //                            } else {
//    //                                self.#ident[start..end].copy_from_slice(#data);
//    //                            }
//    //                        }
//    //                    }
//    //            });
//    //            let fn_name = format_ident!("rw_{}_addr", *space.name);
//    //            memory_addr_function
//    //                .insert(Rc::clone(&space.name), fn_name.clone());
//    //            quote! {
//    //                pub fn #fn_name(&mut self, #read:bool, #addr: usize, #data: &mut [u8]) {
//    //                    match #addr {
//    //                        #(#chunks),*
//    //                        _ => unreachable!(),
//    //                    }
//    //                }
//    //            }
//    //                }
//    //            }
//    //        });
//    //    let memory_struct = &self.space_struct().ident;
//    //    quote! {
//    //        impl #memory_struct {
//    //            #(#funs)*
//    //        }
//    //    }
//    //}
//    pub fn impl_varnode_function(&mut self) -> TokenStream {
//        todo!();
//        //let mut varnode_functions = IndexMap::new();
//        //let funs = self.sleigh
//        //.global_scope
//        //.values()
//        //.filter_map(|x| x.unwrap_varnode())
//        //.map(|varnode| {
//        //    let fun_read_name = format_ident!("varnode_read_{}", *varnode.name);
//        //    let fun_write_name = format_ident!("varnode_write_{}", *varnode.name);
//        //    varnode_functions.insert(
//        //        Rc::clone(&varnode.name),
//        //        VarnodeFunction {
//        //            read: fun_read_name.clone(),
//        //            write: fun_write_name.clone(),
//        //        },
//        //    );
//        //    //TODO impl only read function if spacetype is Rom
//        //    let (chunk_name, chunk_offset) = self
//        //        .memory_struct
//        //        .as_ref()
//        //        .unwrap()
//        //        .chunk_from_memory(varnode.memory())
//        //        .unwrap();
//        //    match &varnode.varnode_type {
//        //        VarnodeType::Memory(memory) => {
//        //            let type_bytes: usize = memory.size.get().try_into().unwrap();
//        //            let addr: usize = memory.offset.try_into().unwrap();
//        //            let chunk_start: usize = addr - chunk_offset;
//        //            let chunk_end: usize = chunk_start + type_bytes;
//        //            quote! {
//        //                fn #fun_read_name(&self) -> [u8; #type_bytes] {
//        //                  let mut data = [0u8; #type_bytes];
//        //                  data.copy_from_slice(&self.#chunk_name[#chunk_start..#chunk_end]);
//        //                  data
//        //                }
//        //                fn #fun_write_name(&mut self, data: [u8; #type_bytes]) {
//        //                  self.#chunk_name[#chunk_start..#chunk_end].copy_from_slice(&data);
//        //                }
//        //            }
//        //        }
//        //        VarnodeType::BitRange(bitrange)
//        //            | VarnodeType::Context(Context{bitrange, ..}) => {
//        //                let bit_start = bitrange.range.lsb_bit % 8;
//        //                let bit_len: usize = bitrange.range.n_bits.get().try_into().unwrap();
//        //                let bit_mask: usize = (1 << bit_len) - 1;
//
//        //                let byte_offset: usize = usize::try_from(bitrange.range.lsb_bit).unwrap() / 8;
//        //                let byte_start: usize = chunk_offset + byte_offset;
//        //                let byte_len: usize = ((bit_len - 1) / 8) + 1;
//        //                let byte_end: usize = byte_start + byte_len;
//
//        //                let (work_len, work_type) = match bit_len {
//        //                    0 => unreachable!(),
//        //                    1..=8 => (u8::BITS/8, format_ident!("u8")),
//        //                    0..=16 => (u16::BITS/8, format_ident!("u16")),
//        //                    17..=32 => (u32::BITS/8, format_ident!("u32")),
//        //                    33..=64 => (u64::BITS/8, format_ident!("u64")),
//        //                    69..=128 => (u128::BITS/8, format_ident!("u128")),
//        //                    x => todo!("bitrange size {}", x),
//        //                };
//        //                let work_len = work_len as usize;
//
//        //                quote! {
//        //                    fn #fun_read_name(&self) -> [u8; #byte_len] {
//        //                        let mut tmp = [0u8; #work_len];
//        //                        tmp[0..#byte_len].copy_from_slice(
//        //                            &self.#chunk_name[#byte_start..#byte_end]
//        //                        );
//        //                        let mut tmp = #work_type::from_be_bytes(tmp);
//        //                        tmp >>= #bit_start;
//        //                        tmp &= #bit_mask as #work_type;
//        //                        tmp.to_be_bytes()
//        //                    }
//        //                    fn #fun_write_name(&mut self, value: [u8; #byte_len]) {
//        //                        let mut tmp = [0u8; #work_len];
//        //                        tmp[0..#byte_len].copy_from_slice(&value);
//        //                        let mut tmp = #work_type::from_be_bytes(tmp);
//        //                        tmp &= #bit_mask as #work_type;
//        //                        tmp <<= #bit_start;
//
//        //                        let mut raw_data = [0u8; #work_len];
//        //                        raw_data[0..#byte_len].copy_from_slice(
//        //                            &self.#chunk_name[#byte_start..#byte_end]
//        //                        );
//        //                        let mut raw_data = #work_type::from_be_bytes(raw_data);
//        //                        raw_data &= !((#bit_mask as #work_type) << #bit_start);
//
//        //                        let result = (raw_data | tmp).to_be_bytes();
//        //                        self.#chunk_name[#byte_start..#byte_end].copy_from_slice(
//        //                            &result[0..#byte_len]
//        //                        );
//        //                    }
//        //                }
//        //        },
//        //    }
//        //});
//        //let memory_struct = &self.space_struct().ident;
//        //let out = quote! {
//        //    impl #memory_struct {
//        //        #(#funs)*
//        //    }
//        //};
//        //self.varnode_functions = Some(varnode_functions);
//        //out
//    }
//    pub fn impl_user_function(&mut self) -> TokenStream {
//        todo!()
//        //let mut user_functions = IndexMap::new();
//        //let user_functions = self
//        //    .sleigh
//        //    .global_scope
//        //    .values()
//        //    .filter_map(|x| x.unwrap_pcode_macro())
//        //    .map(|user_function| {
//        //        let function_name = user_functions
//        //            .entry(Rc::clone(&user_function.name))
//        //            .or_insert(format_ident!(
//        //                "user_function_{}",
//        //                *user_function.name
//        //            ));
//        //        quote! {
//        //            fn #function_name(&self) {
//        //                todo!()
//        //            }
//        //        }
//        //    });
//        //let memory_struct = &self.space_struct().ident;
//        //quote! {
//        //    impl #memory_struct {
//        //        #(#user_functions)*
//        //    }
//        //}
//    }
//
//    pub fn impl_pcode_macro_function(&mut self) -> TokenStream {
//        todo!();
//        //let pcode_macros = self
//        //    .sleigh
//        //    .global_scope
//        //    .values()
//        //    .filter_map(|x| x.unwrap_pcode_macro())
//        //    .map(|pcode_macro| function::do_the_thing(self, &pcode_macro));
//        //quote! {
//        //    #(#pcode_macros)*
//        //}
//    }
//
//    //pub fn impl_space_functions(&mut self) -> TokenStream {
//    //    let addr_fun = self.space_function_addr();
//    //    let varnode_read_write = self.varnodes_function_access();
//    //    let user_functions = self.functions_user_function();
//    //    let pcode_macros = self.functions_pcode_macro();
//
//    //    quote! {
//    //        impl AddressSpaces {
//    //            #addr_fun
//    //            #varnode_read_write
//    //            #user_functions
//    //            #pcode_macros
//    //        }
//    //    }
//    //}
//}
