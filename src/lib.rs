use std::path::Path;
use std::rc::Rc;

use sleigh_rs::{file_to_sleigh, SleighError};

mod builder;
use builder::Disassembler;

pub use sleigh2macro::*;
pub use sleigh4rust::*;

pub(crate) const DISASSEMBLY_ALLOW_OVERFLOW: bool = true;

pub fn parse_disassembler(
    file: &Path,
) -> Result<Rc<Disassembler>, SleighError> {
    let sleigh = file_to_sleigh(file)?;
    Ok(Disassembler::new(Rc::new(sleigh)))
}
