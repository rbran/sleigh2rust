use std::path::Path;
use std::rc::Rc;

use sleigh_rs::{file_to_sleigh, SleighError};

mod builder;
use builder::Disassembler;

pub(crate) use sleigh_rs::{
    DisassemblyType, IntTypeU, NonZeroTypeU, NumberNonZeroSigned,
    NumberNonZeroSuperSigned, NumberNonZeroUnsigned, NumberSigned,
    NumberSuperSigned, NumberUnsigned,
};
pub(crate) const DISASSEMBLY_ALLOW_OVERFLOW: bool = true;

pub fn parse_debugger(
    file: impl AsRef<Path>,
    debug_mode: bool,
) -> Result<Rc<Disassembler>, SleighError> {
    let sleigh = file_to_sleigh(file.as_ref())?;
    Ok(Disassembler::new(Rc::new(sleigh), debug_mode))
}
