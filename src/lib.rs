use std::path::Path;

use quote::ToTokens;

use proc_macro2::TokenStream;

use sleigh_rs::{file_to_sleigh, SleighError};

mod builder;
use builder::Disassembler;

pub(crate) use sleigh_rs::{DisassemblyType, NonZeroTypeU, NumberSuperSigned};
pub(crate) const DISASSEMBLY_ALLOW_OVERFLOW: bool = true;

fn disassembler(
    file: impl AsRef<Path>,
    debug: bool,
) -> Result<TokenStream, SleighError> {
    let sleigh = file_to_sleigh(file.as_ref())?;
    Ok(Disassembler::new(sleigh, debug).into_token_stream())
}

pub fn generate_disassembler(
    file: impl AsRef<Path>,
) -> Result<TokenStream, SleighError> {
    disassembler(file, false)
}

pub fn generate_debug_disassembler(
    file: impl AsRef<Path>,
) -> Result<TokenStream, SleighError> {
    disassembler(file, true)
}
