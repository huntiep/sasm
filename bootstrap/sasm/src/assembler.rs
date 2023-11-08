use {Emitter, Register};

use std::collections::HashMap;

macro_rules! r {
    ($instruction:ident, $funct3:expr, $funct7:expr) => {
        pub fn $instruction(&mut self, rd: Register, rs1: Register, rs2: Register) {
            self.r(rd, rs1, rs2, $funct3, $funct7);
        }
    };
}

macro_rules! i {
    ($instruction:ident, $funct3:expr) => {
        pub fn $instruction(&mut self, rd: Register, rs: Register, imm: i32) {
            self.i(rd, rs, $funct3, imm, 0b0010011);
        }
    };
}

macro_rules! i2 {
    ($instruction:ident, $funct3:expr) => {
        pub fn $instruction(&mut self, rd: Register, rs: Register, imm: i32) {
            self.i(rd, rs, $funct3, imm, 0b0000011);
        }
    };
}

macro_rules! s {
    ($instruction:ident, $funct3:expr) => {
        pub fn $instruction(&mut self, rd: Register, rs: Register, imm: i32) {
            self.s(rd, rs, $funct3, imm);
        }
    };
}

macro_rules! b {
    ($instruction:ident, $funct3:expr) => {
        pub fn $instruction(&mut self, rs1: Register, rs2: Register, label: &str) {
            self.b(rs1, rs2, $funct3, label);
        }
    };
}

enum JumpType {
    Branch,
    Jump
}

pub struct Assembler {
    labels: HashMap<String, usize>,
    jumps: Vec<(String, usize, JumpType)>,
    emitter: Emitter,
}

impl Assembler {
    pub fn new() -> Self {
        Assembler {
            labels: HashMap::new(),
            jumps: Vec::new(),
            emitter: Emitter::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.emitter.len()
    }

    pub fn code(self) -> Vec<u8> {
        self.emitter.code()
    }

    pub fn append(&mut self, other: Vec<u8>) {
        self.emitter.append(other);
    }

    pub fn finish(mut self) -> Vec<u8> {
        let mut jumps = Vec::new();
        std::mem::swap(&mut jumps, &mut self.jumps);
        for (label, i, ty) in jumps {
            let p = if let Some(p) = self.labels.get(&label) {
                *p
            } else {
                panic!("Unknown label `{}`", label);
            };

            let offset = (p as isize - i as isize) as i32;
            match ty {
                JumpType::Branch => self.rewrite_b(i, offset),
                JumpType::Jump => self.rewrite_j(i, offset),
            }
        }

        self.code()
    }

    pub fn label<S: Into<String>>(&mut self, label: S) {
        self.labels.insert(label.into(), self.emitter.len());
    }

    #[inline]
    fn r(&mut self, rd: Register, rs1: Register, rs2: Register, funct3: u32, funct7: u32) {
        let instr = (funct7 << 25) | (rs2.as_u32() << 20) | (rs1.as_u32() << 15) | (funct3 << 12) | (rd.as_u32() << 7) | 0b0110011;
        self.emitter.emit_u32(instr);
    }

    r!(add, 0x0, 0x00);
    r!(sub, 0x0, 0x20);
    r!(xor, 0x4, 0x00);
    r!(or, 0x6, 0x00);
    r!(and, 0x7, 0x00);
    r!(sll, 0x1, 0x00);
    r!(srl, 0x5, 0x00);
    r!(sra, 0x5, 0x20);
    r!(slt, 0x2, 0x00);
    r!(sltu, 0x3, 0x00);
    r!(mul, 0x0, 0x01);
    r!(div, 0x4, 0x01);
    r!(rem, 0x6, 0x01);

    #[inline]
    fn i(&mut self, rd: Register, rs: Register, funct3: u32, imm: i32, opcode: u32) {
        assert!(imm < 2048 && imm >= -2048);
        let instr = ((imm as u32) << 20) | (rs.as_u32() << 15) | (funct3 << 12) | (rd.as_u32() << 7) | opcode;
        self.emitter.emit_u32(instr);
    }

    i!(addi, 0x00);
    pub fn subi(&mut self, rd: Register, rs: Register, imm: i32) {
        self.addi(rd, rs, -imm);
    }
    i!(xori, 0x04);
    i!(ori, 0x06);
    i!(andi, 0x07);
    //TODO
    pub fn slli(&mut self, rd: Register, rs: Register, imm: i32) {
        assert!(imm <= 0x3F);
        self.i(rd, rs, 0x01, imm, 0b0010011);
    }
    //TODO
    pub fn srli(&mut self, rd: Register, rs: Register, imm: i32) {
        assert!(imm <= 0x3F);
        self.i(rd, rs, 0x05, imm, 0b0010011);
    }
    //TODO
    pub fn srai(&mut self, rd: Register, rs: Register, imm: i32) {
        assert!(imm <= 0x3F);
        self.i(rd, rs, 0x05, imm | (0x20 << 5), 0b0010011);
    }
    i!(slti, 0x02);
    i!(sltiu, 0x03);

    i2!(lb, 0x0);
    i2!(lh, 0x1);
    i2!(lw, 0x2);
    i2!(ld, 0x3);
    i2!(lbu, 0x4);
    i2!(lhu, 0x5);
    i2!(lwu, 0x6);

    #[inline]
    fn s(&mut self, rd: Register, rs: Register, funct3: u32, imm: i32) {
        assert!(imm >= -2048 && imm <= 2047);
        let imm = imm as u32;
        let instruction = ((imm >> 5) << 25) | (rd.as_u32() << 20) | (rs.as_u32() << 15) | (funct3 << 12) | ((imm & 0b11111) << 7) | 0b0100011;
        self.emitter.emit_u32(instruction);
    }

    s!(sb, 0x0);
    s!(sh, 0x1);
    s!(sw, 0x2);
    s!(sd, 0x3);

    #[inline]
    fn b(&mut self, rs1: Register, rs2: Register, funct3: u32, label: &str) {
        let imm = if let Some(&i) = self.labels.get(label) {
            (i as isize - self.emitter.len() as isize) as i32
        } else {
            self.jumps.push((label.to_string(), self.emitter.len(), JumpType::Branch));
            0
        };
        assert!(imm >= -4096 && imm <= 4095);
        let imm = imm as u32;
        let imm1 =  (imm & 0x1e) | ((imm >> 11) & 1);
        let imm2 = ((imm & 0x10_00) >> 6) | ((imm >> 5) & 0x3F);
        let instruction = (imm2 << 25) | (rs2.as_u32() << 20) | (rs1.as_u32() << 15) | (funct3 << 12) | (imm1 << 7) | 0b1100011;
        self.emitter.emit_u32(instruction);
    }

    fn rewrite_b(&mut self, offset: usize, imm: i32) {
        assert!(imm >= -4096 && imm <= 4095);
        let imm = imm as u32;
        let instruction = self.emitter.read_u32_at_offset(offset);
        let imm1 =  (imm & 0x1e) | ((imm >> 11) & 1);
        let imm2 = ((imm & 0x10_00) >> 6) | ((imm >> 5) & 0x3F);
        let instruction = (imm2 << 25) | (imm1 << 7) | instruction;
        self.emitter.replace_u32_at_offset(offset, instruction);
    }

    b!(beq, 0x0);
    b!(bne, 0x1);
    b!(blt, 0x4);
    b!(bge, 0x5);
    b!(bltu, 0x6);
    b!(bgeu, 0x7);

    pub fn jal(&mut self, rd: Register, label: &str) {
        let imm = if let Some(&i) = self.labels.get(label) {
            (i as isize - self.emitter.len() as isize) as i32
        } else {
            self.jumps.push((label.to_string(), self.emitter.len(), JumpType::Jump));
            0
        };
        assert!(imm >= -1048576 && imm <= 1048575);
        let imm = imm as u32;
        let imm2 = ((imm & 0x10_00_00) >> 1) | (((imm >> 1) & 0x3_FF) << 9) | (((imm >> 11) & 1) << 8) | ((imm >> 12) & 0xFF);
        let instruction = (imm2 << 12) | (rd.as_u32() << 7) | 0b1101111;
        self.emitter.emit_u32(instruction);
    }

    fn rewrite_j(&mut self, offset: usize, imm: i32) {
        assert!(imm >= -1048576 && imm <= 1048575);
        let imm = imm as u32;
        let instruction = self.emitter.read_u32_at_offset(offset);
        let imm2 = ((imm & 0x10_00_00) >> 1) | (((imm >> 1) & 0x3_FF) << 9) | (((imm >> 11) & 1) << 8) | ((imm >> 12) & 0xFF);
        let instruction = (imm2 << 12) | instruction;
        self.emitter.replace_u32_at_offset(offset, instruction);
    }

    //TODO
    pub fn jalr(&mut self, rd: Register, rs1: Register, imm: i32) {
        self.i(rd, rs1, 0x0, imm, 0b1100111);
    }

    pub fn lui(&mut self, rd: Register, imm: u32) {
        assert!(imm <= 0xF_FF_FF);
        let instruction = (imm << 12) | (rd.as_u32() << 7) | 0b0110111;
        self.emitter.emit_u32(instruction);
    }

    pub fn auipc(&mut self, rd: Register, imm: u32) {
        assert!(imm <= 0xF_FF_FF);
        let instruction = (imm << 12) | (rd.as_u32() << 7) | 0b0010111;
        self.emitter.emit_u32(instruction);
    }

    pub fn ecall(&mut self) {
        self.i(Register::X0, Register::X0, 0x00, 0x0, 0b1110011);
    }

    pub fn ebreak(&mut self) {
        self.i(Register::X0, Register::X0, 0x00, 0x1, 0b1110011);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn basic() {
        let code = vec![0x02, 0x00, 0x00, 0x6f,
                        0x00, 0x10, 0x0e, 0x63,
                        0x00, 0x00, 0x05, 0x33,
                        0x00, 0x20, 0x85, 0x13,
                        0xff, 0xe0, 0x85, 0x13,
                        0x7f, 0xf0, 0x85, 0x13,
                        0x80, 0x00, 0x85, 0x13,
                        //0x80, 0x00, 0x85, 0x13,
                        0x02, 0x01, 0x04, 0x13,
                        0xfe, 0x1f, 0xf0, 0x6f,
                        0xfc, 0x10, 0x0e, 0xe3,
                        0x00, 0x00, 0x00, 0x73, // ecall
                        0x00, 0x05, 0x0e, 0x03,
                        0x00, 0x55, 0x0e, 0x03,
                        0xff, 0xb5, 0x0e, 0x03,
                        0x01, 0xc5, 0x00, 0x23,
                        0x01, 0xc5, 0x02, 0xa3,
                        0xff, 0xc5, 0x0d, 0xa3,
                        0x00, 0x00, 0x80, 0x67,
                        0x00, 0x50, 0x80, 0x67,
                        0xff, 0xb0, 0x80, 0x67,
                        0x00, 0x00, 0x50, 0xb7,
                        0x00, 0x00, 0x50, 0x97,
        ];
        let mut asm = Assembler::new();
        asm.label("begin");
        asm.jal(Register::X0, "end");
        asm.beq(Register::X0, Register::X1, "end");
        asm.add(Register::X10, Register::X0, Register::X0);
        asm.addi(Register::X10, Register::X1, 2);
        asm.addi(Register::X10, Register::X1, -2);
        asm.addi(Register::X10, Register::X1, 2047);
        asm.addi(Register::X10, Register::X1, -2048);
        //asm.subi(Register::X10, Register::X1, 2048);
        asm.addi(Register::X8, Register::X2, 32);
        asm.label("end");
        asm.jal(Register::X0, "begin");
        asm.beq(Register::X0, Register::X1, "begin");
        asm.ecall();
        asm.lb(Register::X28, Register::X10, 0);
        asm.lb(Register::X28, Register::X10, 5);
        asm.lb(Register::X28, Register::X10, -5);
        asm.sb(Register::X10, Register::X28, 0);
        asm.sb(Register::X10, Register::X28, 5);
        asm.sb(Register::X10, Register::X28, -5);
        asm.jalr(Register::X0, Register::X1, 0);
        asm.jalr(Register::X0, Register::X1, 5);
        asm.jalr(Register::X0, Register::X1, -5);
        asm.lui(Register::X1, 5);
        asm.auipc(Register::X1, 5);
        assert_eq!(code, asm.finish());
    }
}
