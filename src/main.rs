use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::rc::Rc;

use quote::ToTokens;
use sleigh2rust;

const ARCH_FILES: &[(&str, &[&str])] = &[
    (
        "68000",
        &[
            //TODO What is `export 0:0;`?
            //"68040",
            //"68030",
            //"coldfire",
            //"68020",
        ],
    ),
    (
        "HCS12",
        &[
            //TODO: Assign values with diferent sizes, eg 8bit value into 16bit variable
            "HCS12"
        ],
    ),
    (
        "Atmel",
        &[
            //TODO: disassembly pointing to non context varnode???
            //"avr32a",
            //"avr8xmega",
            //"avr8eind",
            //TODO: bitrange auto adapt to an arbitrary size
            //TODO: Assign values with diferent sizes, eg 8bit value into 16bit variable
            "avr8",
            "avr8e",
        ],
    ),
    (
        "8048",
        &[
            //TODO: disassembly pointing to non context varnode???
            //"8048",
        ],
    ),
    (
        "PA-RISC",
        &[
            //TODO: sometimes the dst addr is 32, other time 64
            "pa-risc32be",
        ],
    ),
    (
        "RISCV",
        &[
            //TODO: try to assign a 32bits value into a 64bits varnode
            "riscv.ilp32d",
            "riscv.lp64d",
        ],
    ),
    (
        "V850",
        &[
            //TODO: bitrange auto adapt to an arbitrary size
            //TODO: Assign values with diferent sizes, eg 8bit value into 16bit variable
            "V850",
        ],
    ),
    (
        "6502",
        &[
            //TODO: bitrange auto adapt to an arbitrary size
            //TODO: Assign values with diferent sizes, eg 8bit value into 16bit variable
            "6502",
            "65c02",
        ],
    ),
    (
        "CR16",
        &[
            //TODO: bitrange auto adapt to an arbitrary size
            //TODO: Assign values with diferent sizes, eg 8bit value into 16bit variable
            "CR16B",
            "CR16C",
        ],
    ),
    (
        "Z80",
        &[
            //TODO: bitrange auto adapt to an arbitrary size
            //TODO: Assign values with diferent sizes, eg 8bit value into 16bit variable
            "z80",
            "z180",
        ],
    ),
    (
        "HCS08",
        &[
            //TODO: bitrange auto adapt to an arbitrary size
            //TODO: Assign values with diferent sizes, eg 8bit value into 16bit variable
            "HC08",
            "HCS08",
            "HC05",
        ],
    ),
    (
        "tricore",
        &[
            //TODO: bitrange auto adapt to an arbitrary size
            //TODO: Assign values with diferent sizes, eg 8bit value into 16bit variable
            "tricore",
        ],
    ),
    (
        "MC6800",
        &[
            //TODO: bitrange auto adapt to an arbitrary size
            //TODO: Assign values with diferent sizes, eg 8bit value into 16bit variable
            "6809",
            "6805",
            "H6309",
        ],
    ),
    (
        "MCS96",
        &[
            //TODO: bitrange auto adapt to an arbitrary size
            //TODO: Assign values with diferent sizes, eg 8bit value into 16bit variable
            "MCS96",
        ],
    ),
    (
        "TI_MSP430",
        &[
            //TODO: bitrange auto adapt to an arbitrary size
            //TODO: Assign values with diferent sizes, eg 8bit value into 16bit variable
            //"TI_MSP430",
            //"TI_MSP430X",
        ],
    ),
    (
        "CP1600",
        &[
            //TODO: bitrange auto adapt to an arbitrary size
            //TODO: Assign values with diferent sizes, eg 8bit value into 16bit variable
            "CP1600",
        ],
    ),
    (
        "M8C",
        &[
            //TODO: bitrange auto adapt to an arbitrary size
            //TODO: Assign values with diferent sizes, eg 8bit value into 16bit variable
            "m8c",
        ],
    ),
    (
        "8051",
        &[
            //TODO: bitrange auto adapt to an arbitrary size
            //TODO: Assign values with diferent sizes, eg 8bit value into 16bit variable
            "80251",
            "80390",
            "8051",
            "mx51",
        ],
    ),
    (
        "8085",
        &[
            //TODO: jmp into 16/8bit address
            "8085",
        ],
    ),
    (
        "MIPS",
        &[
            //TODO: use value from non export table
            //"mips32be",
            //"mips32le",
            //"mips32R6be",
            //"mips32R6le",
            //"mips64be",
            //"mips64le",
        ],
    ),
    (
        "AARCH64",
        &[
            //TODO: re-export from a table that also export const
            "AARCH64",
            "AARCH64BE",
            "AARCH64_AppleSilicon",
        ],
    ),
    (
        "JVM",
        &[
            //TODO: Cpool
            //"JVM",
        ],
    ),
    (
        "Dalvik",
        &[
            //TODO: Cpool
            "Dalvik_Base",
            "Dalvik_ODEX_KitKat",
            "Dalvik_DEX_KitKat",
            "Dalvik_DEX_Lollipop",
            "Dalvik_DEX_Marshmallow",
            "Dalvik_DEX_Nougat",
            "Dalvik_DEX_Oreo",
            "Dalvik_DEX_Pie",
            "Dalvik_DEX_Android10",
            "Dalvik_DEX_Android11",
            "Dalvik_DEX_Android12",
        ],
    ),
    (
        "PowerPC",
        &[
            //TODO: AND-OP a 64bit value with a 32bit variable, outputing a 32bit value
            "ppc_32_be",
            "ppc_32_le",
            "ppc_32_quicciii_be",
            "ppc_32_quicciii_le",
            "ppc_32_4xx_be",
            "ppc_32_4xx_le",
            "ppc_64_be",
            "ppc_64_le",
            "ppc_64_isa_be",
            "ppc_64_isa_le",
            "ppc_64_isa_altivec_be",
            "ppc_64_isa_altivec_le",
            "ppc_64_isa_altivec_vle_be",
            "ppc_64_isa_vle_be",
        ],
    ),
    (
        "x86",
        &[
            //TODO: Jmp into a 16bit address
            //"x86",
            //"x86-64",
        ],
    ),
    (
        "Sparc",
        &[
            //TODO: Op 32bits value with Int greater then 32bits
            "SparcV9_32",
            "SparcV9_64",
        ],
    ),
    (
        "ARM",
        &[
            "ARM4_be", "ARM4_le", "ARM4t_be", "ARM4t_le", "ARM5_be", "ARM5_le",
            "ARM5t_be", "ARM5t_le", "ARM6_be", "ARM6_le", "ARM7_be", "ARM7_le",
            "ARM8_be", "ARM8_le",
        ],
    ),
    ("DATA", &["data-be-64", "data-le-64"]),
    ("SuperH4", &["SuperH4_be", "SuperH4_le"]),
    (
        "SuperH",
        &[
            //TODO: bitrange auto adapt to an arbitrary size
            //TODO: Assign values with diferent sizes, eg 8bit value into 16bit variable
            "sh-1",
            "sh-2",
            "sh-2a",
        ],
    ),
    (
        "Toy",
        &[
            "toy_builder_be_align2",
            "toy_builder_le_align2",
            "toy_builder_le",
            "toy_be_posStack",
            "toy_builder_be",
            "toy_wsz_be",
            "toy_wsz_le",
            "toy_be",
            "toy_le",
            "toy64_be",
            "toy64_le",
            "toy64_be_harvard",
        ],
    ),
    (
        "PIC",
        &[
            "pic12c5xx",
            "pic16c5x",
            //TODO: bitrange auto adapt to an arbitrary size
            //TODO: Assign values with diferent sizes, eg 8bit value into 16bit variable
            "pic16",
            "pic16f",
            "pic17c7xx",
            "pic18",
            "PIC24E",
            "PIC24F",
            "PIC24H",
            "dsPIC30F",
            "dsPIC33C",
            "dsPIC33E",
            "dsPIC33F",
        ],
    ),
];

fn parse(arch: &str, variant: &str) -> Result<(), Box<dyn std::error::Error>> {
    let in_file_name = format!(
        "/home/rbran/src/ghidra/Ghidra/Processors/{}/data/languages/{}.slaspec",
        arch, variant
    );
    let sleigh = sleigh_rs::file_to_sleigh(&in_file_name)?;
    let sleigh = Rc::new(sleigh);

    let out_file_path = Path::new("/home/rbran/src/sleigh3rust")
        .join(variant.to_lowercase().replace('-', "_"))
        .join("src");
    std::fs::create_dir_all(&out_file_path).unwrap();
    let out_file_name = out_file_path.join("disassembler.rs");

    let mut file = File::create(&out_file_name)?;
    let emu = sleigh2rust::dis(sleigh);
    file.write_all(emu.into_token_stream().to_string().as_bytes())?;
    drop(file);

    let fmt_success = std::process::Command::new("rustfmt")
        .arg(out_file_name)
        .status()
        .unwrap()
        .success();
    assert!(fmt_success);
    Ok(())
}

fn main() {
    let _ = std::fs::create_dir("/tmp/sleigh/src");
    for (arch, variants) in ARCH_FILES.iter() {
        for variant in variants.iter() {
            println!("arch {} {}", arch, variant);
            match parse(arch, variant) {
                Ok(_) => (),
                Err(e) => {
                    println!("unable: {}", e);
                    return;
                }
            }
        }
    }
}
