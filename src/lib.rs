use std::{rc::Rc, path::Path};

use builder::Disassembler;

use sleigh_rs::{Sleigh, SleighError, file_to_sleigh};

pub const DISASSEMBLY_ALLOW_OVERFLOW: bool = true;

pub mod builder;

//pub fn emu<'a>(sleigh: &'a Sleigh) -> impl ToTokens + 'a {
//    let memory_trait = builder::SpacesTrait::new_all(sleigh);
//    let context_trait = builder::ContextTrait::new(sleigh);
//    let memory_struct = builder::SpacesStruct::new(sleigh);
//
//    let memory_trait_def = memory_trait.gen_trait();
//    let context_trait_def = context_trait.gen_trait();
//    let memory_trait_impl_context =
//        memory_trait.impl_context_trait(&context_trait);
//    let mem_struct_def = memory_struct.gen_struct();
//    let mem_struct_impl_memory = memory_struct.impl_memory_trait(&memory_trait);
//    quote! {
//        #mem_struct_def
//        #memory_trait_def
//        #context_trait_def
//        #mem_struct_impl_memory
//        #memory_trait_impl_context
//    }
//}

pub fn dis(sleigh: Rc<Sleigh>) -> Rc<Disassembler> {
    Disassembler::new(sleigh)
}

pub fn parse(file: &Path) -> Result<Rc<Disassembler>, SleighError> {
    let sleigh = file_to_sleigh(file)?;
    Ok(Disassembler::new(Rc::new(sleigh)))
}
