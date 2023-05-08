use proc_macro2::{Ident, TokenStream};

use quote::{format_ident, quote, ToTokens};

use sleigh_rs::{BitRange, Endian};

use super::{ToLiteral, WorkType};

pub fn mask_and_rotation_from_range(range: &BitRange) -> (u128, u128) {
    let bit_lsb = u32::try_from(range.start()).unwrap();
    let field_len = u32::try_from(range.len().get()).unwrap();
    let mask = (u128::MAX >> (u128::BITS - field_len)) << bit_lsb;
    (bit_lsb.into(), mask)
}

pub fn bytes_from_varnode(
    big_endian: bool,
    varnode_addr: u64,
    varnode_len: u64,
    varnode_lsb: u64,
    data_bits: u64,
) -> (u64, u64) {
    assert!(data_bits > 0);
    let data_lsb = varnode_lsb % 8;
    let read_bytes = (data_bits + data_lsb + 7) / 8;
    let data_addr_offset = if big_endian {
        (varnode_len - read_bytes) - (varnode_lsb / 8)
    } else {
        varnode_lsb / 8
    };
    let data_lsb = varnode_lsb % 8;
    (varnode_addr + data_addr_offset, data_lsb)
}

pub fn bitrange_from_value(
    final_value: &Ident,
    final_type: WorkType,
    raw_value: impl ToTokens,
    range: &BitRange,
    signed: bool,
) -> TokenStream {
    let (rotation, mask) = mask_and_rotation_from_range(range);
    let mask = mask.unsuffixed();
    let rotation = rotation.unsuffixed();
    let len_bits: u32 = range.len().get().try_into().unwrap();
    let tmp_type = WorkType::unsigned_from_bits(len_bits);
    if signed {
        let signed_bit = (1u128 << range.len().get()).unsuffixed();
        quote! {
            let tmp_value = ((#raw_value & #mask) >> #rotation) as #tmp_type;
            let #final_value = if tmp_value & #signed_bit != 0 {
                -((tmp_value & !#signed_bit) as #final_type)
            } else {
                tmp_value as #final_type
            };
        }
    } else {
        quote! {
            let #final_value = ((#raw_value & #mask) >> #rotation) as #final_type;
        }
    }
}

pub fn from_endian_bytes(endian: Endian) -> Ident {
    match endian {
        Endian::Little => format_ident!("from_le_bytes"),
        Endian::Big => format_ident!("from_be_bytes"),
    }
}
