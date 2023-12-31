/// See https://cirosantilli.com/elf-hello-world
use std::io::{self, Read, Write};

use std::mem;
use std::collections::HashMap;

pub struct Elf {
    e_hdr: Elf64Ehdr,
    p_hdr: Vec<Elf64Phdr>,
    program: Vec<u8>,
    data: Vec<Vec<u8>>,
    shstrtab: Vec<u8>,
    s_hdr: Vec<Elf64Shdr>,
}

impl Elf {
    // data is a list of constants
    // rewrites is a map of indexes into the program to data indexes
    pub fn new(isa: ISA, mut program: Vec<u8>, mut data: Vec<Vec<u8>>, rewrites: HashMap<usize, usize>) -> Self {
        // TODO
        assert!(program.len() < 0x200000);
        assert!(data.len() < 0x100000);
        let program_offset = (mem::size_of::<Elf64Ehdr>() + (2 * mem::size_of::<Elf64Phdr>())) as u64;
        // 8-byte align
        if program.len() % 8 != 0 {
            program.push(0);
            program.push(0);
            program.push(0);
            program.push(0);
        }
        let data_offset = program_offset + program.len() as u64;

        // Read memory positions of each data entry
        let mut data_position = Vec::new();
        let mut pos = DATA_LOCATION + data_offset;
        let mut data_len = 0;
        for id in &mut data {
            // 8-byte align
            let align = (pos+id.len() as u64) % 8;
            if align > 0 {
                for _ in 0..8-align {
                    id.push(0);
                }
            }
            data_position.push(pos);
            pos += id.len() as u64;
            data_len += id.len();
        }
        // Perform rewrites
        match isa {
            ISA::Riscv => for (p, i) in rewrites {
                // TODO
                let offset = data_position[i] as u32;
                let mut buf = [0; 4];
                (&program[p..]).read_exact(&mut buf).unwrap();
                let lui = u32::from_le_bytes(buf);
                (&program[p+4..]).read_exact(&mut buf).unwrap();
                let addi = u32::from_le_bytes(buf);

                // https://patchwork.kernel.org/project/linux-riscv/patch/20220131182145.236005-3-kernel@esmil.dk/
                let imm20 = (offset + 0x800) >> 12;
                let imm12 = offset & 0xfff;
                let lui = imm20 << 12 | lui;
                let addi = imm12 << 20 | addi;
                (&mut program[p..]).write_all(&lui.to_le_bytes()).unwrap();
                (&mut program[p+4..]).write_all(&addi.to_le_bytes()).unwrap();
            }
            _ => unimplemented!(),
        }

        Elf {
            e_hdr: Elf64Ehdr::new(isa, 2),
            p_hdr: vec![Elf64Phdr::text(0, data_offset),
                        Elf64Phdr::data(data_offset, data_len as u64)],
            program: program,
            data: data,
            shstrtab: Vec::new(),
            s_hdr: Vec::new(),
        }
    }

    pub fn new_debug(isa: ISA, program: Vec<u8>, data: Vec<Vec<u8>>, rewrites: HashMap<usize, usize>) -> Self {
        let mut elf = Self::new(isa, program, data, rewrites);

        let shstrtab = b"\0.text\0.data\0.shstrtab\0";
        let shstrtab_offset = elf.p_hdr[1].p_offset + elf.p_hdr[1].p_filesz;
        let sh_off = shstrtab_offset + shstrtab.len() as u64;

        let s_hdr = vec![Elf64Shdr::null(),
                        Elf64Shdr::text(elf.program.len() as u64, elf.e_hdr.e_entry),
                        Elf64Shdr::data(elf.p_hdr[1].p_filesz, elf.p_hdr[1].p_offset),
                        Elf64Shdr::shstrtab(shstrtab.len() as u64, shstrtab_offset)];

        elf.e_hdr.e_shoff = sh_off;
        elf.e_hdr.e_shnum = 4;
        elf.e_hdr.e_shstrndx = 3;
        elf.shstrtab = shstrtab.to_vec();
        elf.s_hdr = s_hdr;

        elf
    }

    pub fn write<W: Write>(self, mut w: W) -> io::Result<()> {
        let Elf { e_hdr, p_hdr, program, data, shstrtab, s_hdr } = self;

        w.write_all(e_hdr.to_slice())?;
        for p in p_hdr {
            w.write_all(p.to_slice())?;
        }
        w.write_all(&program)?;
        for d in data {
            w.write_all(&d)?;
        }
        w.write_all(&shstrtab)?;
        for s in s_hdr {
            w.write_all(s.to_slice())?;
        }
        Ok(())
    }
}

type Elf64Addr = u64;
type Elf64Off = u64;
type Elf64Half = u16;
type Elf64Word = u32;
type Elf64Xword = u64;

const EI_NIDENT: usize = 16; // Number of bytes in e_ident
const ENTRY_LOCATION: u64 = 0x400000;
const DATA_LOCATION: u64 = 0x600000;
const RODATA_LOCATION: u64 = 0x700000;

pub enum ISA {
    Amd64 = 0x3e,
    Riscv = 0xf3,
}

#[repr(packed)]
struct Elf64Ehdr {
    e_ident: [u8; EI_NIDENT],
    e_type: Elf64Half,
    e_machine: Elf64Half,
    e_version: Elf64Word,
    e_entry: Elf64Addr,
    e_phoff: Elf64Off,
    e_shoff: Elf64Off,
    e_flags: Elf64Word,
    e_ehsize: Elf64Half,
    e_phentsize: Elf64Half,
    e_phnum: Elf64Half,
    e_shentsize: Elf64Half,
    e_shnum: Elf64Half,
    e_shstrndx: Elf64Half,
}

impl Elf64Ehdr {
    fn new(isa: ISA, program_headers: u16) -> Self {
        Elf64Ehdr {
            e_ident: [0x7f, b'E', b'L', b'F', // magic number
                      2, // 1 for 32 bit, 2 for 64bit
                      1, // endianness - 1 is little-endian
                      1, // version
                      0, // abi version
                      0, 0, 0, 0, 0, 0, 0, 0], // unused padding
            // object file type: 2 if executable
            e_type: 2,
            // target isa: 0x3e is amd64
            e_machine: isa as u16,
            // set to 1 for the original version of elf
            e_version: 1,
            // memory address of the entry point. right after elf header + program headers
            e_entry: ENTRY_LOCATION + mem::size_of::<Elf64Ehdr>() as u64 + (program_headers as usize * mem::size_of::<Elf64Phdr>()) as u64,
            // phoff: points to start of the program header table
            e_phoff: mem::size_of::<Elf64Ehdr>() as u64,
            // shoff: points to start of the section header table
            e_shoff: 0,
            // flags
            e_flags: 0,
            // size of this header, usually 64 bytes on 64bit
            e_ehsize: mem::size_of::<Elf64Ehdr>() as u16,
            // size of a program header table entry
            e_phentsize: mem::size_of::<Elf64Phdr>() as u16,
            // number of entries in the program header table
            e_phnum: program_headers,
            // size of a section header table entry
            e_shentsize: mem::size_of::<Elf64Shdr>() as u16,
            // number of entries in the section header table
            e_shnum: 0,
            // index of the section header table entry that contains the section names
            e_shstrndx: 0,
        }
    }

    fn to_slice(self) -> &'static [u8] {
        let ehdr_ptr: *const Elf64Ehdr = &self;
        let ehdr_raw = unsafe { mem::transmute::<_, *const u8>(ehdr_ptr) };
        let ehdr_slice = unsafe { std::slice::from_raw_parts(ehdr_raw, mem::size_of::<Elf64Ehdr>()) };
        ehdr_slice
    }
}

#[repr(packed)]
struct Elf64Phdr {
    p_type: Elf64Word,
    p_flags: Elf64Word,
    p_offset: Elf64Off,
    p_vaddr: Elf64Addr,
    p_paddr: Elf64Addr,
    p_filesz: Elf64Xword,
    p_memsz: Elf64Xword,
    p_align: Elf64Xword,
}

impl Elf64Phdr {
    fn text(offset: u64, size: u64) -> Self {
        Elf64Phdr {
            // 1 is PT_LOAD
            p_type: 1,
            // Execute and read permissions
            p_flags: 5,
            // offset from beginning of segments
            p_offset: offset,
            // Initial virtual memory address to load this segment to
            p_vaddr: ENTRY_LOCATION,
            p_paddr: ENTRY_LOCATION,
            p_filesz: size,
            p_memsz: size,
            p_align: 4096,
        }
    }

    fn rodata(offset: u64, size: u64) -> Self {
        Elf64Phdr {
            // 1 is PT_LOAD
            p_type: 1,
            // read and write permissions
            p_flags: 4,
            // offset from beginning of segments
            p_offset: offset,
            // Initial virtual memory address to load this segment to
            p_vaddr: RODATA_LOCATION+offset,
            p_paddr: RODATA_LOCATION+offset,
            p_filesz: size,
            p_memsz: size,
            p_align: 4096,
        }
    }

    fn data(offset: u64, size: u64) -> Self {
        Elf64Phdr {
            // 1 is PT_LOAD
            p_type: 1,
            // read and write permissions
            p_flags: 6,
            // offset from beginning of segments
            p_offset: offset,
            // Initial virtual memory address to load this segment to
            p_vaddr: DATA_LOCATION+offset,
            p_paddr: DATA_LOCATION+offset,
            p_filesz: size,
            p_memsz: size,
            p_align: 4096,
        }
    }

    fn to_slice(self) -> &'static [u8] {
        let phdr_ptr: *const Elf64Phdr = &self;
        let phdr_raw = unsafe { mem::transmute::<_, *const u8>(phdr_ptr) };
        let phdr_slice = unsafe { std::slice::from_raw_parts(phdr_raw, mem::size_of::<Elf64Phdr>()) };
        phdr_slice
    }
}

#[repr(packed)]
struct Elf64Shdr {
    sh_name: Elf64Word,
    sh_type: Elf64Word,
    sh_flags: Elf64Xword,
    sh_addr: Elf64Addr,
    sh_offset: Elf64Off,
    sh_size: Elf64Xword,
    sh_link: Elf64Word,
    sh_info: Elf64Word,
    sh_addralign: Elf64Xword,
    sh_entsize: Elf64Xword,
}

impl Elf64Shdr {
    fn null() -> Self {
        Elf64Shdr {
            sh_name: 0,
            sh_type: 0,
            sh_flags: 0,
            sh_addr: 0,
            sh_offset: 0,
            sh_size: 0,
            sh_link: 0,
            sh_info: 0,
            sh_addralign: 0,
            sh_entsize: 0,
        }
    }

    fn text(sh_size: u64, sh_offset: u64) -> Self {
        Elf64Shdr {
            sh_name: 0x01,
            sh_type: 1,
            sh_flags: 6,
            sh_addr: sh_offset,
            sh_offset: sh_offset - ENTRY_LOCATION,
            sh_size: sh_size,
            sh_link: 0,
            sh_info: 0,
            sh_addralign: 4,
            sh_entsize: 0,
        }
    }

    fn data(sh_size: u64, sh_offset: u64) -> Self {
        Elf64Shdr {
            sh_name: 0x07,
            sh_type: 1,
            sh_flags: 3,
            sh_addr: DATA_LOCATION + sh_offset,
            sh_offset: sh_offset,
            sh_size: sh_size,
            sh_link: 0,
            sh_info: 0,
            sh_addralign: 8,
            sh_entsize: 0,
        }
    }

    fn rodata(sh_size: u64, sh_offset: u64) -> Self {
        Elf64Shdr {
            sh_name: 0x07,
            sh_type: 1,
            sh_flags: 2,
            sh_addr: DATA_LOCATION + sh_offset,
            sh_offset: sh_offset,
            sh_size: sh_size,
            sh_link: 0,
            sh_info: 0,
            sh_addralign: 8,
            sh_entsize: 0,
        }
    }

    fn shstrtab(sh_size: u64, sh_offset: u64) -> Self {
        Elf64Shdr {
            sh_name: 0x0d,
            sh_type: 3,
            sh_flags: 0,
            sh_addr: 0,
            sh_offset: sh_offset,
            sh_size: sh_size,
            sh_link: 0,
            sh_info: 0,
            sh_addralign: 1,
            sh_entsize: 0,
        }
    }

    fn to_slice(self) -> &'static [u8] {
        let shdr_ptr: *const Elf64Shdr = &self;
        let shdr_raw = unsafe { mem::transmute::<_, *const u8>(shdr_ptr) };
        let shdr_slice = unsafe { std::slice::from_raw_parts(shdr_raw, mem::size_of::<Elf64Shdr>()) };
        shdr_slice
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn size() {
        assert_eq!(mem::size_of::<Elf64Ehdr>(), 64);
        assert_eq!(mem::size_of::<Elf64Phdr>(), 56);
        assert_eq!(mem::size_of::<Elf64Shdr>(), 64);
    }

    /*
    #[test]
    fn to_bytes() {
        let ehdr = Elf64Ehdr::new(ISA::Riscv, 2).to_vec();
        let ehdr2 = Elf64Ehdr::new(ISA::Riscv, 2);
        let ehdr_ptr: *const Elf64Ehdr = &ehdr2;
        let ehdr_raw = unsafe { mem::transmute::<_, *const u8>(ehdr_ptr) };
        let ehdr_slice = unsafe { std::slice::from_raw_parts(ehdr_raw, mem::size_of::<Elf64Ehdr>()) };
        assert_eq!(ehdr_slice, &ehdr);

    }
    */
}
