use std::ops::Range;

use proc_macro2::{Ident, TokenStream};

use quote::{format_ident, quote, ToTokens};

use sleigh_rs::{pattern::BitConstraint, Endian};

use super::{ToLiteral, WorkType};

// each bit have four possible states:
// mask 1, value X: bit is restricted to `value`
// mask 0, value 0: bit is unrestricted
// mask 0, value 1: bit is restricted but value can't be defined at compile time
#[derive(Clone, Copy, Default, Debug)]
pub struct PatternByte {
    pub(crate) value: u8,
    pub(crate) mask: u8,
}

impl PatternByte {
    pub(crate) fn from_bit_constraints(bits: &[BitConstraint]) -> Vec<Self> {
        let context_bytes = (usize::try_from(bits.len()).unwrap() + 7) / 8;
        let mut bytes: Vec<PatternByte> =
            vec![PatternByte::default(); context_bytes];
        for (bit, bit_constraint) in bits.iter().enumerate() {
            match bit_constraint {
                BitConstraint::Unrestrained => {}
                BitConstraint::Restrained => {
                    bytes[bit / 8].value |= 1 << (bit % 8);
                }
                BitConstraint::Defined(value) => {
                    bytes[bit / 8].value |= (*value as u8) << (bit % 8);
                    bytes[bit / 8].mask |= 1 << (bit % 8);
                }
            }
        }
        bytes
    }
    pub fn defined_value(&self) -> u8 {
        self.value & self.mask
    }

    pub fn defined_bits(&self) -> u8 {
        self.mask
    }
}

pub fn bytes_from_value(
    endian: Endian,
    type_bytes: u32,
    value_bytes: u32,
) -> (u32, u32) {
    assert!(type_bytes >= value_bytes);
    match endian {
        // in le, the lsB is first, so we copy from 0 to the value len
        Endian::Little => (0, value_bytes),
        //in be, the msB is first, so we copy from (len-1) to (len-1-value_len)
        Endian::Big => (type_bytes - 1, (type_bytes - 1) - value_bytes),
    }
}

pub fn rotation_and_mask_from_range(bits: Range<u32>) -> (u128, u128) {
    let bit_lsb = bits.start;
    let field_len = bits.len() as u32;
    let mask = u128::MAX >> (u128::BITS - field_len);
    (bit_lsb.into(), mask)
}

pub fn bitrange_from_value(
    bits: Range<u32>,
    final_type: WorkType,
    raw_value: impl ToTokens,
) -> TokenStream {
    let (rotation, mask) = rotation_and_mask_from_range(bits);
    let mask = mask.unsuffixed();
    let rotation = rotation.unsuffixed();
    quote! {(((#raw_value >> #rotation) & #mask) as #final_type)}
}

pub fn sign_from_value(
    len_bits: u32,
    final_type: WorkType,
    raw_value: impl ToTokens,
) -> TokenStream {
    let sign_bit = 1u128 << (len_bits - 1);
    let mask = sign_bit - 1;
    quote! {(
        // if negative add the negative part
        if #raw_value & #sign_bit != 0 { -1 & !#mask } else { 0 }
        // or with the value part
        | #raw_value as #final_type
    )}
}

pub fn from_endian_bytes(endian: Endian) -> Ident {
    match endian {
        Endian::Little => format_ident!("from_le_bytes"),
        Endian::Big => format_ident!("from_be_bytes"),
    }
}
