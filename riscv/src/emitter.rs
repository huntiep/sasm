use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};

pub struct Emitter(Vec<u8>);

impl Emitter {
    pub fn new() -> Self {
        Emitter(vec![])
    }

    pub fn append(&mut self, mut other: Vec<u8>) {
        self.0.append(&mut other);
    }

    pub fn emit_u32(&mut self, b: u32) {
        self.0.write_u32::<LittleEndian>(b).unwrap();
    }

    pub fn read_u32_at_offset(&mut self, offset: usize) -> u32 {
        (&self.0[offset..]).read_u32::<LittleEndian>().unwrap()
    }

    pub fn replace_u32_at_offset(&mut self, offset: usize, b: u32) {
        (&mut self.0[offset..]).write_u32::<LittleEndian>(b).unwrap();
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn code(self) -> Vec<u8> {
        self.0
    }
}
