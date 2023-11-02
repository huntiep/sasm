extern crate elf;

use elf::{Elf, ISA};

use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

fn main() {
    let program = vec![0xb8, 0x01, 0, 0, 0,
                       0xbf, 0x01, 0, 0, 0,
                       //0x48, 0xbe, 0xd9, 0, 0x60, 0, 0, 0, 0, 0,
                       0x48, 0xbe, 0, 0, 0, 0, 0, 0, 0, 0,
                       0xba, 0x0e, 0, 0, 0,
                       0x0f, 0x05,
                       0xb8, 0x3c, 0, 0, 0,
                       0xbf, 0, 0, 0, 0,
                       0x0f, 0x05,
    ];
    let data = vec![b"Hello, world!\n".to_vec()];
    let mut rewrites = HashMap::new();
    rewrites.insert(12, 0);
    let e = Elf::new(ISA::Amd64, program, data, rewrites);
    let mut f = File::create("test.out").unwrap();
    f.write_all(&e.to_vec()).unwrap();
}
