extern crate byteorder;
extern crate elf;
extern crate tokenizer;

#[macro_use]
mod macros;

mod assembler;
mod emitter;
mod register;

use emitter::Emitter;

use tokenizer::Token;

use std::env;
use std::collections::HashMap;
use std::fs::{self, OpenOptions};
use std::io::Write;
use std::os::unix::fs::OpenOptionsExt;

pub use assembler::Assembler;
pub use register::Register;

fn main() {
    let input_file = env::args().nth(1).unwrap();
    let input = fs::read_to_string(&input_file).unwrap();
    let tokens = tokenizer::Tokenizer::tokenize(&input).unwrap();
    let (program, data, rewrites) = Asm::new(tokens, &input);
    let e = elf::Elf::new(elf::ISA::Riscv, program, data, rewrites);
    let mut output_file = input_file.split(".").next().unwrap();
    if output_file == input_file {
        output_file = "bin.elf";
    }
    let mut f = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .mode(0o777)
        .open(output_file)
        .unwrap();
    f.write_all(&e.to_vec()).unwrap();

}

struct Asm {
    asm: Assembler,
    data: Vec<Vec<u8>>,
    rewrites: HashMap<usize, usize>,
    constants: HashMap<String, i32>,
    globals: HashMap<String, usize>,
    register_aliases: HashMap<String, Register>,
}

macro_rules! r {
    ($op:ident, $self:ident, $tokens:ident, $input:ident) => {
        {
            let rd = $self.unwrap_register($tokens, $input);
            let rs1 = $self.unwrap_register($tokens, $input);
            let rs2 = $self.unwrap_register($tokens, $input);
            assert!($tokens.next().unwrap().closerp());
            $self.asm.$op(rd, rs1, rs2);
        }
    };
}

macro_rules! i {
    ($op:ident, $self:ident, $tokens:ident, $input:ident) => {
        {
            let rd = $self.unwrap_register($tokens, $input);
            let rs1 = $self.unwrap_register($tokens, $input);
            let imm = $self.read_imm($tokens, $input);
            assert!($tokens.next().unwrap().closerp());
            $self.asm.$op(rd, rs1, imm);
        }
    };
}

macro_rules! i2 {
    ($op:ident, $self:ident, $tokens:ident, $input:ident) => {
        {
            let rd = $self.unwrap_register($tokens, $input);
            let (rs1, imm) = $self.offset($tokens, $input);
            assert!($tokens.next().unwrap().closerp());
            $self.asm.$op(rd, rs1, imm);
        }
    };
}

macro_rules! s {
    ($op:ident, $self:ident, $tokens:ident, $input:ident) => {
        {
            let (rs1, imm) = $self.offset($tokens, $input);
            let rd = $self.unwrap_register($tokens, $input);
            assert!($tokens.next().unwrap().closerp());
            $self.asm.$op(rd, rs1, imm);
        }
    };
}

macro_rules! b {
    ($op:ident, $self:ident, $tokens:ident, $input:ident) => {
        {
            let rs1 = $self.unwrap_register($tokens, $input);
            let rs2 = $self.unwrap_register($tokens, $input);
            let label = if let Some(Token::Symbol(i)) = $tokens.next() {
                Token::Symbol(*i).as_str($input)
            } else {
                unreachable!();
            };
            assert!($tokens.next().unwrap().closerp());
            $self.asm.$op(rs1, rs2, label);
        }
    };
}

impl Asm {
    fn new(tokens: Vec<Token>, input: &str) -> (Vec<u8>, Vec<Vec<u8>>, HashMap<usize, usize>) {
        let mut asm = Asm {
            asm: Assembler::new(),
            data: Vec::new(),
            rewrites: HashMap::new(),
            constants: HashMap::new(),
            globals: HashMap::new(),
            register_aliases: HashMap::new(),
        };

        asm.assemble(tokens, input);

        (asm.asm.finish(), asm.data, asm.rewrites)
    }
    fn assemble(&mut self, tokens: Vec<Token>, input: &str) {
        let mut tokens = tokens.iter().filter(|t| !t.commentp());
        while let Some(t) = tokens.next() {
            match t {
                s @ Token::Symbol(_) => { self.asm.label(s.as_str(input)); },
                Token::LParen(_) => if let Some(Token::Symbol(i)) = tokens.next() {
                    let s = Token::Symbol(*i).as_str(input);
                    if "include!" == s {
                        self.handle_include(&mut tokens, input);
                    } else if "define" == s {
                        self.handle_define(&mut tokens, input);
                    } else {
                        self.handle_opcode(s, &mut tokens, input);
                    }
                } else {
                    unreachable!();
                },
                _ => unreachable!(),
            }
        }
    }

    fn handle_include<'b,  I: Iterator<Item = &'b Token>>(&mut self, tokens: &mut I, input: &str) {
        let filename = tokens.next().unwrap();
        assert!(filename.is_string());
        let filename = filename.as_str(input);
        let filename = &filename[1..filename.len()-1];
        assert!(tokens.next().unwrap().closerp());
        let include_input = fs::read_to_string(filename).unwrap();
        let include_tokens = tokenizer::Tokenizer::tokenize(&include_input).unwrap();
        self.assemble(include_tokens, &include_input);
    }

    fn handle_define<'b,  I: Iterator<Item = &'b Token>>(&mut self, tokens: &mut I, input: &str) {
        let var = if let Some(Token::Symbol(i)) = tokens.next() {
            Token::Symbol(*i).as_str(input).to_string()
        } else {
            unreachable!();
        };

        match tokens.next().unwrap() {
            s @ Token::Symbol(_) => {
                let r = Register::from_str(s.as_str(input)).unwrap();
                self.register_aliases.insert(var, r);
            }
            s @ Token::Integer(_) => {
                let i = s.as_str(input).parse().unwrap();
                self.constants.insert(var, i);
            }
            s @ Token::Char(_) => {
                let s = s.as_str(input);
                let s = &s[2..(s.len() - 1)].as_bytes();
                let i = match s[0] {
                    b'\\' => match s[1] {
                        b'\\' | b'\'' => s[1] as i32,
                        b'r' => b'\r' as i32,
                        b'n' => b'\n' as i32,
                        b't' => b'\t' as i32,
                        b'0' => b'\0' as i32,
                        _ => unreachable!(),
                    },
                    _ => s[0] as i32,
                };
                self.constants.insert(var, i);
            }
            s @ Token::String(_) => {
                let s = s.as_str(input);
                let s = s[1..s.len()-1].as_bytes();
                let mut v = Vec::with_capacity(s.len());
                let mut i = 0;
                while i < s.len() {
                    match s[i] {
                        b'\\' => {
                            v.push(match s[i+1] {
                                b'"' | b'\\' => s[i+1],
                                b'r' => b'\r',
                                b'n' => b'\n',
                                b't' => b'\t',
                                b'0' => b'\0',
                                _ => unreachable!(),
                            });
                            i += 1;
                        }
                        _ => v.push(s[i]),
                    }
                    i += 1;
                }
                //self.globals.insert(var, (self.data.len(), v.len()));
                self.globals.insert(var, self.data.len());
                self.data.push(v);
            }
            Token::Pound(_) => {
                assert!(tokens.next().unwrap().openerp());
                let mut v = Vec::new();
                loop {
                    match tokens.next().unwrap() {
                        s @ Token::Integer(_) => {
                            let i = s.as_str(input).parse().unwrap();
                            v.push(i);
                        }
                        t if t.closerp() => break,
                        t if t.commentp() => continue,
                        _ => unreachable!(),
                    }
                }
                //self.globals.insert(var, (self.data.len(), v.len()));
                self.globals.insert(var, self.data.len());
                self.data.push(v);
            }
            _ => unreachable!(),
        }

        assert!(tokens.next().unwrap().closerp());
    }

    fn unwrap_register<'b, I: Iterator<Item = &'b Token>>(&self, tokens: &mut I, input: &str) -> Register {
        if let Some(Token::Symbol(i)) = tokens.next() {
            let reg = Token::Symbol(*i).as_str(input);
            if let Some(r) = Register::from_str(reg) {
                r
            } else {
                *self.register_aliases.get(reg).unwrap()
            }
        } else {
            unreachable!();
        }
    }

    fn offset<'b, I: Iterator<Item = &'b Token>>(&self, tokens: &mut I, input: &str) -> (Register, i32) {
        match tokens.next().unwrap() {
            s @ Token::Symbol(_) => {
                let s = s.as_str(input);
                if let Some(r) = Register::from_str(s) {
                    (r, 0)
                } else {
                    (*self.register_aliases.get(s).unwrap(), 0)
                }
            }
            Token::LParen(_) => {
                let negate = match tokens.next().unwrap() {
                    s @ Token::Symbol(_) => match s.as_str(input) {
                        "+" => false,
                        "-" => true,
                        _ => unreachable!(),
                    }
                    _ => unreachable!(),
                };

                let (r, i): (_, i32) = match tokens.next().unwrap() {
                    s @ Token::Symbol(_) => {
                        let s = s.as_str(input);
                        let (r, i) = if let Some(r) = Register::from_str(s) {
                            (Some(r), None)
                        } else if let Some(r) = self.register_aliases.get(s) {
                            (Some(*r), None)
                        } else {
                            (None, Some(*self.constants.get(s).unwrap()))
                        };

                        if let Some(r) = r {
                            match tokens.next().unwrap() {
                                i @ Token::Integer(_) => (r, i.as_str(input).parse().unwrap()),
                                i @ Token::Symbol(_) => (r, *self.constants.get(i.as_str(input)).unwrap()),
                                _ => unreachable!(),
                            }
                        } else {
                            (self.unwrap_register(tokens, input), i.unwrap())
                        }
                    }
                    i @ Token::Integer(_) => (self.unwrap_register(tokens, input), i.as_str(input).parse().unwrap()),
                    _ => unreachable!(),
                };
                assert!(tokens.next().unwrap().closerp());
                // TODO
                if negate {
                    (r, -i)
                } else {
                    (r, i)
                }
            },
            _ => unreachable!(),
        }
    }


    fn read_imm<'b, I: Iterator<Item = &'b Token>>(&self, tokens: &mut I, input: &str) -> i32 {
        match tokens.next().unwrap() {
            t if t.openerp() => {
                let s = tokens.next().unwrap();
                assert!(s.is_symbol());
                let s = s.as_str(input);
                assert_eq!(s, "len");
                let i = match tokens.next().unwrap() {
                    s @ Token::Symbol(_) => {
                        let s = s.as_str(input);
                        *self.globals.get(s).unwrap()
                    },
                    s @ Token::String(_) => {
                        let s = s.as_str(input).as_bytes();
                        let mut j = 0;
                        let mut i = 1;
                        while i < s.len()-1 {
                            match s[i] {
                                b'\\' => match s[i+1] {
                                    b'"' | b'\\' | b'r' | b'n' | b't' | b'0' => {
                                        j += 1;
                                        i += 1;
                                    }
                                    _ => unreachable!(),
                                },
                                _ => j += 1,
                            }
                            i += 1;
                        }
                        j
                    },
                    _ => unreachable!(),
                };
                assert!(tokens.next().unwrap().closerp());
                i as i32
            }
            s @ Token::Integer(_) => s.as_str(input).parse::<i32>().unwrap(),
            s @ Token::Char(_) => {
                let s = s.as_str(input);
                let s = &s[2..(s.len() - 1)].as_bytes();
                match s[0] {
                    b'\\' => match s[1] {
                        b'"' | b'\\' | b'\'' => s[1] as i32,
                        b'r' => b'\r' as i32,
                        b'n' => b'\n' as i32,
                        b't' => b'\t' as i32,
                        b'0' => b'\0' as i32,
                        _ => unreachable!(),
                    },
                    _ => s[0] as i32,
                }
            }
            s @ Token::Symbol(_) => {
                let s = s.as_str(input);
                *self.constants.get(s).unwrap()
            },
            s @ _ => unreachable!("{:?}", s),
        }
    }

    fn handle_opcode<'b, I: Iterator<Item = &'b Token>>(&mut self, opcode: &str, tokens: &mut I, input: &str) {
        match opcode {
            "add" => r!(add, self, tokens, input),
            "sub" => r!(sub, self, tokens, input),
            "xor" => r!(xor, self, tokens, input),
            "or" => r!(or, self, tokens, input),
            "and" => r!(and, self, tokens, input),
            "sll" => r!(sll, self, tokens, input),
            "srl" => r!(srl, self, tokens, input),
            "sra" => r!(sra, self, tokens, input),
            "slt" => r!(slt, self, tokens, input),
            "sltu" => r!(sltu, self, tokens, input),
            "mul" => r!(mul, self, tokens, input),

            "addi" => i!(addi, self, tokens, input),
            "subi" => i!(subi, self, tokens, input),
            "xori" => i!(xori, self, tokens, input),
            "ori" => i!(ori, self, tokens, input),
            "andi" => i!(andi, self, tokens, input),
            "slli" => i!(slli, self, tokens, input),
            "srli" => i!(srli, self, tokens, input),
            "srai" => i!(srai, self, tokens, input),
            "slti" => i!(slti, self, tokens, input),
            "sltiu" => i!(sltiu, self, tokens, input),

            "lb" => i2!(lb, self, tokens, input),
            "lh" => i2!(lh, self, tokens, input),
            "lw" => i2!(lw, self, tokens, input),
            "ld" => i2!(ld, self, tokens, input),
            "lbu" => i2!(lbu, self, tokens, input),
            "lhu" => i2!(lhu, self, tokens, input),
            "lwu" => i2!(lwu, self, tokens, input),
            "la" => {
                let rd = self.unwrap_register(tokens, input);
                // TODO: offsets?
                let symbol = if let Some(Token::Symbol(i)) = tokens.next() {
                    Token::Symbol(*i).as_str(input)
                } else {
                    unreachable!();
                };
                assert!(tokens.next().unwrap().closerp());
                let addr = self.globals.get(symbol).unwrap();
                self.rewrites.insert(self.asm.len(), *addr);
                self.asm.lui(rd, 0);
                self.asm.addi(rd, rd, 0);
            }

            "sb" => s!(sb, self, tokens, input),
            "sh" => s!(sh, self, tokens, input),
            "sw" => s!(sw, self, tokens, input),
            "sd" => s!(sd, self, tokens, input),

            "beq" => b!(beq, self, tokens, input),
            "bne" => b!(bne, self, tokens, input),
            "blt" => b!(blt, self, tokens, input),
            "bge" => b!(bge, self, tokens, input),
            "bltu" => b!(bltu, self, tokens, input),
            "bgeu" => b!(bgeu, self, tokens, input),

            "jal" => {
                let rd = self.unwrap_register(tokens, input);
                let label = if let Some(Token::Symbol(i)) = tokens.next() {
                    Token::Symbol(*i).as_str(input)
                } else {
                    unreachable!();
                };
                assert!(tokens.next().unwrap().closerp());
                self.asm.jal(rd, label);
            }
            "jalr" => {
                let rd = self.unwrap_register(tokens, input);
                let (rs, imm) = self.offset(tokens, input);
                assert!(tokens.next().unwrap().closerp());
                // TODO
                self.asm.jalr(rd, rs, imm);
            }
            "lui" => {
                let rd = self.unwrap_register(tokens, input);
                let imm = self.read_imm(tokens, input);
                assert!(tokens.next().unwrap().closerp());
                self.asm.lui(rd, imm as u32);
            }
            "auipc" => {
                let rd = self.unwrap_register(tokens, input);
                let imm = self.read_imm(tokens, input);
                assert!(tokens.next().unwrap().closerp());
                self.asm.auipc(rd, imm as u32);
            }
            "ecall" => {
                assert!(tokens.next().unwrap().closerp());
                self.asm.ecall();
            }
            "ebreak" => {
                assert!(tokens.next().unwrap().closerp());
                self.asm.ebreak();
            }
            _ => unreachable!("{}", opcode),
        }
    }
}
