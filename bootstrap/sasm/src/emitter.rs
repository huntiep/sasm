use std::io::{Read, Write};

pub struct Emitter(Vec<u8>);

impl Emitter {
    pub fn new() -> Self {
        Emitter(vec![])
    }

    pub fn append(&mut self, mut other: Vec<u8>) {
        self.0.append(&mut other);
    }

    pub fn emit_u32(&mut self, b: u32) {
        self.0.write_all(&b.to_le_bytes()).unwrap();
    }

    pub fn read_u32_at_offset(&mut self, offset: usize) -> u32 {
        let mut buf = [0; 4];
        (&self.0[offset..]).read_exact(&mut buf).unwrap();
        u32::from_le_bytes(buf)
    }

    pub fn replace_u32_at_offset(&mut self, offset: usize, b: u32) {
        (&mut self.0[offset..]).write_all(&b.to_le_bytes()).unwrap();
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn code(self) -> Vec<u8> {
        self.0
    }
}
