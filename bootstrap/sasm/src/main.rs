mod elf;
mod symbols;
mod tokenizer;

use tokenizer::{get_symbol, get_value, Token};

use std::env;
use std::collections::HashMap;
use std::fs::{self, OpenOptions};
use std::io::ErrorKind as IOError;
use std::os::unix::fs::OpenOptionsExt;
use std::process::exit;

fn main() {
    let mut args = env::args();
    let prog = args.next().unwrap();
    let mut input_file = None;
    let mut output_file = None;
    let mut debug = false;
    for arg in args {
        if arg == "-d" {
            debug = true;
        } else if arg.starts_with("-") {
            eprintln!("Unknown flag `{}`.", arg);
            eprintln!("USAGE:\n\t{} <INPUT> [OUTPUT]", prog);
            eprintln!("\t-d: Include debug info.");
        } else if input_file.is_none() {
            input_file = Some(arg);
        } else if output_file.is_none() {
            output_file = Some(arg);
        } else {
            eprintln!("Too many arguments.");
            eprintln!("USAGE:\n\t{} <INPUT> [OUTPUT]", prog);
            eprintln!("\t-d: Include debug info.");
            exit(1);
        }
    }
    let input_file = if let Some(arg) = input_file {
        arg
    } else {
        eprintln!("USAGE:\n\t{} <INPUT> [OUTPUT]", prog);
        eprintln!("\t-d: Include debug info.");
        exit(1);
    };
    let output_file = if let Some(arg) = output_file {
        arg
    } else {
        "bin.elf".to_string()
    };

    symbols::init();
    let input = match fs::read(&input_file) {
        Ok(v) => v,
        Err(e) => match e.kind() {
            IOError::NotFound => panic!("No such file `{}`", input_file),
            IOError::PermissionDenied => panic!("Permission denied trying to open file `{}`", input_file),
            IOError::OutOfMemory => panic!("Not enough memory to read file `{}`", input_file),
            _ => panic!("Unknown error reading file `{}`: {}", input_file, e),
        },
    };
    let tokenizer::Tokenizer { input, tokens, err, filename, .. } = tokenizer::tokenize(input, input_file);
    let (program, data, rewrites) = assemble(input, filename, tokens, err);
    let e = if debug {
        elf::Elf::new(program, data, rewrites)
    } else {
        elf::Elf::new_debug(program, data, rewrites)
    };
    /*
    let name = input_file.rsplitn(2, '.').last().unwrap().to_string();
    let asm = Asm::new(tokens, &input, name.clone());
    let (program, data, rewrites) = asm.finish();
    let e = elf::Elf::new(elf::ISA::Riscv, program, data, rewrites);
    //let e = elf::Elf::new_debug(elf::ISA::Riscv, program, data, rewrites);
    */
    let f = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .mode(0o777)
        .open(output_file)
        .unwrap();
    e.write(f).unwrap();

    /*
    let program = Program {
        modules: vec![Module::new(None), Module::new(Some(1))],
        stack: Vec::new(),
        current: 1,
    };
    program.modules[0].children.insert(name, 1);
    */
}

fn assemble(input: Vec<u8>, filename: String, tokens: Vec<Token>, err: bool) -> (Vec<u32>, Vec<u8>, Vec<(usize, usize)>) {
    let mut root = Module::new(None, false);
    let name = filename.rsplitn(2, '.').last().unwrap().as_bytes().to_vec();
    root.children.insert(get_symbol(name), Unit::Module(1));

    let mut asm = Asm {
        input: input,
        filename: filename,
        tokens: tokens,
        position: 0,
        err: err,
        modules: vec![root, Module::new(Some(0), true)],
        module: 1,
        data: Vec::new(),
    };
    loop {
        asm.assemble();
        if asm.peek().is_none() {
            break;
        }
        eprintln!("Unxepected closing parenthesis");
        asm.err = true;

    }
    let (code, data, rewrites) = asm.finish();
    if asm.err {
        exit(1);
    }
    (code, data, rewrites)
}


struct Asm {
    input: Vec<u8>,
    filename: String,
    tokens: Vec<Token>,
    position: usize,
    err: bool,
    modules: Vec<Module>,
    module: usize,
    data: Vec<u8>,
}

type Symbol = usize;
struct Module {
    parent: Option<usize>,
    children: HashMap<Symbol, Unit>,
    filep: bool,
    code: Vec<u32>,
    labels: HashMap<Symbol, usize>,
    jumps: Vec<(Symbol, usize, JumpType)>,
    rewrites: Vec<(usize, Symbol)>,
}

impl Module {
    fn new(parent: Option<usize>, filep: bool) -> Self {
        Module {
            parent: parent,
            children: HashMap::new(),
            filep: filep,
            code: Vec::new(),
            labels: HashMap::new(),
            jumps: Vec::new(),
            rewrites: Vec::new(),
        }
    }

    pub fn finish(&mut self) -> Vec<(Symbol, usize, JumpType)> {
        let mut jumps = Vec::new();
        for (label, i, ty) in &self.jumps {
            let p = if let Some(p) = self.labels.get(label) {
                *p
            } else {
                jumps.push((*label,*i, *ty));
                continue;
            };

            let i = *i;
            let imm = (p as isize - i as isize) as i32;
            match ty {
                JumpType::Branch => {
                    assert!(imm >= -4096 && imm <= 4095);
                    let imm = imm as u32;
                    let imm1 = (imm & 0x1e) | ((imm >> 11) & 1);
                    let imm2 = ((imm & 0x10_00) >> 6) | ((imm >> 5) & 0x3F);
                    self.code[i/4] |= (imm2 << 25) | (imm1 << 7);
                }
                JumpType::Jump => {
                    assert!(imm >= -1048576 && imm <= 1048575);
                    let imm = imm as u32;
                    let imm2 = ((imm & 0x10_00_00) >> 1) | (((imm >> 1) & 0x3_FF) << 9) | (((imm >> 11) & 1) << 8) | ((imm >> 12) & 0xFF);
                    self.code[i/4] |= imm2 << 12;
                }
            }
        }
        jumps
    }

}

#[derive(Clone, Copy, Debug, PartialEq)]
enum JumpType {
    Branch,
    Jump
}

enum Unit {
    Constant(i64),
    // position in data, len
    Bytes(usize, usize),
    //Bytes(Vec<u8>),
    Module(usize),
}

impl Asm {
    fn assemble(&mut self) {
        while let Some(token) = self.next() {
            match token {
                Token::Symbol(s) => self.add_label(s),
                Token::LParen => match self.next() {
                    Some(Token::Symbol(s)) if s == symbols::INCLUDE => self.handle_include(),
                    Some(Token::Symbol(s)) if s == symbols::DEFINE => self.handle_define(),
                    Some(Token::Symbol(s)) if s == symbols::MODULE => self.handle_module(),
                    Some(Token::Symbol(s)) if s == symbols::IMPORT => self.handle_import(),
                    Some(Token::Symbol(s)) => self.handle_opcode(s),
                    Some(Token::RParen) => {
                        // TODO
                        eprintln!("Empty expression ending at line {} and index {} in file `{}`.", 0,0,0);
                        self.err = true;
                    }
                    Some(_) => {
                        // TODO
                        eprintln!("Empty expression ending at line {} and index {} in file `{}`.", 0,0,0);
                        self.err = true;
                        // TODO: read to closer?
                    }
                    None => {
                        // TODO
                        eprintln!("Unexpected parenthesis at line {} and index {} in file `{}`.", 0,0,0);
                        self.err = true;
                    }
                },
                Token::RParen => {
                    self.position -= 1;
                    return;
                }
                _ => {
                    // TODO
                    eprintln!("Must be a label or an S-Expression.");
                    self.err = true;
                }
            }
        }

        // handle labels
    }

    fn finish(&mut self) -> (Vec<u32>, Vec<u8>, Vec<(usize, usize)>) {
        let mut code = Vec::new();
        let mut jumps = Vec::new();
        let mut labels = HashMap::new();
        let mut rewrites = Vec::new();
        for module in &mut self.modules[1..] {
            let j = module.finish();
            for (l, i, ty) in j {
                jumps.push((l, i + (code.len() * 4), ty));
            }
            for (l, i) in &module.labels {
                labels.insert(l, i + (code.len() * 4));
            }
            for (i, s) in &module.rewrites {
                let p = match module.children.get(s) {
                    Some(Unit::Bytes(p, _)) => p,
                    Some(_) => {
                        self.err = true;
                        continue;
                    },
                    None => {
                        self.err = true;
                        continue;
                    },
                };
                rewrites.push((i + code.len(), *p));
            }
            code.append(&mut module.code);
        }

        for (label, i, ty) in jumps {
            let p = if let Some(p) = labels.get(&label) {
                *p
            } else {
                eprintln!("Unknown label `{}`.", get_value(label).unwrap());
                self.err = true;
                continue;
            };

            let imm = (p as isize - i as isize) as i32;
            match ty {
                JumpType::Branch => {
                    assert!(imm >= -4096 && imm <= 4095);
                    let imm = imm as u32;
                    let imm1 = (imm & 0x1e) | ((imm >> 11) & 1);
                    let imm2 = ((imm & 0x10_00) >> 6) | ((imm >> 5) & 0x3F);
                    code[i/4] |= (imm2 << 25) | (imm1 << 7);
                }
                JumpType::Jump => {
                    assert!(imm >= -1048576 && imm <= 1048575);
                    let imm = imm as u32;
                    let imm2 = ((imm & 0x10_00_00) >> 1) | (((imm >> 1) & 0x3_FF) << 9) | (((imm >> 11) & 1) << 8) | ((imm >> 12) & 0xFF);
                    code[i/4] |= imm2 << 12;
                }
            }
        }

        println!("{:x?}", code);
        // TODO
        (code, self.data.clone(), rewrites)
    }

    fn add_label(&mut self, label: usize) {
        let m = self.get_mod();
        if m.labels.contains_key(&label) {
            eprintln!("Duplicate label `{}`.", get_value(label).unwrap());
            self.err = true;
        } else if m.children.contains_key(&label) {
            eprintln!("Label `{}` conflicts with module/definition.", get_value(label).unwrap());
            self.err = true;
        } else {
            m.labels.insert(label, m.code.len() * 4);
        }
    }

    fn handle_import(&mut self) {
    }

    fn handle_module(&mut self) {
        let ident = match self.next() {
            Some(Token::Symbol(s)) => s,
            Some(Token::LParen) => {
                self.position -= 1;
                eprintln!("Invalid module declaration, expected identifier.");
                self.err = true;
                0
            }
            Some(Token::RParen) | None => {
                eprintln!("Unfinished module declaration.");
                self.err = true;
                return;
            }
            Some(_) => {
                eprintln!("Invalid module declaration, expected identifier as module name.");
                self.err = true;
                0
            }
        };

        let module = Module::new(Some(self.module), false);
        // shitty bc
        let m_id = self.modules.len();
        if self.get_mod().children.contains_key(&ident) {
            eprintln!("Module `{}` conflicts with existing module/definition in scope", get_value(ident).unwrap());
            self.err = true;
        } else {
            self.get_mod().children.insert(ident, Unit::Module(m_id));
        }
        self.module = m_id;
        self.modules.push(module);
        self.add_label(ident);
        self.assemble();
        if self.next() != Some(Token::RParen) {
            eprintln!("Unclosed module.");
            self.err = true;
        }

        self.module = self.get_mod().parent.unwrap();
    }

    fn get_string(&mut self, start: usize, end: usize) -> Vec<u8> {
        let mut v = Vec::with_capacity(end-start);
        let mut i = start;
        while i < end {
            if self.input[i] == b'\\' {
                i += 1;
                match self.input[i] {
                    b'r' => v.push(b'\r'),
                    b'n' => v.push(b'\n'),
                    b't' => v.push(b'\t'),
                    b'\\' => v.push(b'\\'),
                    b'"' => v.push(b'"'),
                    b'0' => v.push(b'\0'),
                    c @ _ => panic!("Invariant broken in Asm::get_string `{:?}`", c),
                }
            } else {
                v.push(self.input[i]);
            }
            i += 1;
        }
        v
    }

    fn handle_include(&mut self) {
        let filename = match self.next() {
            Some(Token::String(s, e)) => match String::from_utf8(self.get_string(s, e)) {
                Ok(s) => s,
                Err(_) => {
                    // TODO
                    eprintln!("Invalid string at line {} and index {} in file `{}`.", 0,0,0);
                    self.err = true;
                    self.skip_opcode();
                    return;
                }
            },
            Some(Token::RParen) => {
                // TODO
                eprintln!("Unexpected parenthesis at line {} and index {} in file `{}`.", 0,0,0);
                self.err = true;
                return;
            },
            Some(_) => {
                // TODO
                eprintln!("include! file must be a string at line {} and index {} in file `{}`.", 0,0,0);
                self.err = true;
                self.skip_opcode();
                return;
            },
            None => {
                // TODO
                eprintln!("Unexpected parenthesis at line {} and index {} in file `{}`.", 0,0,0);
                self.err = true;
                return;
            }
        };
        match self.next() {
            Some(Token::RParen) => (),
            Some(Token::String(_, _)) => {
                eprintln!("Each file needs its own include statement at line {} and index {} in file `{}`.", 0,0,0);
                self.err = true;
                self.skip_opcode();
            },
            Some(_) => {
                eprintln!("Unexpected expression in include statement at line {} and index {} in file `{}`.", 0,0,0);
                self.err = true;
                self.skip_opcode();
            }
            None => {
                eprintln!("Unexpected EOF at line {} and index {} in file `{}`.", 0,0,0);
                self.err = true;
            }
        }
        let include_input = fs::read(&filename).unwrap();
        let tokenizer::Tokenizer { mut input, mut tokens, err, mut filename, .. } = tokenizer::tokenize(include_input, filename);
        self.err |= err;
        std::mem::swap(&mut self.input, &mut input);
        std::mem::swap(&mut self.tokens, &mut tokens);
        std::mem::swap(&mut self.filename, &mut filename);
        let position = self.position;
        self.position = 0;
        self.assemble();
        self.position = position;
        std::mem::swap(&mut self.input, &mut input);
        std::mem::swap(&mut self.tokens, &mut tokens);
        std::mem::swap(&mut self.filename, &mut filename);
    }

    fn handle_define(&mut self) {
        let ident = match self.next() {
            Some(Token::Symbol(s)) => s,
            Some(Token::RParen) => {
                // TODO
                eprintln!("Definition must have an identifier and value.");
                self.err = true;
                return;
            }
            Some(_) => {
                // TODO
                eprintln!("Expected identifier.");
                self.err = true;
                0
            }
            None => {
                // TODO
                eprintln!("Unexpected EOF.");
                self.err = true;
                return;
            }
        };

        // TODO
        match self.next() {
            // retarded match behaviour
            Some(Token::Integer(i)) => if self.get_mod().children.contains_key(&ident) {
                eprintln!("Definition `{}` conflicts with existing module/definition in scope", get_value(ident).unwrap());
                self.err = true;
            } else {
                self.get_mod().children.insert(ident, Unit::Constant(i));
            },
            Some(Token::Char(c)) => if self.get_mod().children.contains_key(&ident) {
                eprintln!("Definition `{}` conflicts with existing module/definition in scope", get_value(ident).unwrap());
                self.err = true;
            } else {
                self.get_mod().children.insert(ident, Unit::Constant(c as i64));
            },
            Some(Token::Pound) => {
                match self.next() {
                    Some(Token::LParen) => (),
                    Some(Token::RParen) => {
                        eprintln!("Unexepected `#` in definition, definition incomplete.");
                        self.err = true;
                        return;
                    }
                    Some(_) => {
                        eprintln!("Unexepected `#` in definition.");
                        self.err = true;
                        self.skip_opcode();
                        return;
                    }
                    None => {
                        eprintln!("Unexepected `#` at end of file.");
                        self.err = true;
                        return;
                    }
                }
                let mut v = Vec::new();
                loop {
                    match self.next() {
                        Some(Token::RParen) => break,
                        Some(Token::Integer(i)) => {
                            if i < 0 || i >= 256 {
                                eprintln!("Array literals must consist of u8 integers, `{}` is out of range.", i);
                                self.err = true;
                            }
                            v.push(i as u8);
                        }
                        Some(Token::Symbol(_)) => {
                            eprintln!("Array literals must consist of u8 integers, cannot use variables.");
                            self.err = true;
                        }
                        Some(Token::Char(_)) => {
                            eprintln!("Array literals must consist of u8 integers.");
                            self.err = true;
                        }
                        Some(_) => {
                            eprintln!("Array literals must consist of u8 integers.");
                            self.err = true;
                        }
                        None => {
                            eprintln!("Unexepected EOF in array literal.");
                            self.err = true;
                            return;
                        }
                    }
                }
                if v.is_empty() {
                    eprintln!("Definition of empty array literal.");
                }
                let start = self.data.len();
                if self.get_mod().children.contains_key(&ident) {
                    eprintln!("Definition `{}` conflicts with existing module/definition in scope", get_value(ident).unwrap());
                    self.err = true;
                } else {
                    self.get_mod().children.insert(ident, Unit::Bytes(start, v.len()));
                }
                while self.data.len() % 8 != 0 {
                    self.data.push(0);
                }
                self.data.append(&mut v);
            }
            Some(Token::String(start, end)) => {
                let mut s = self.get_string(start, end);
                let start = self.data.len();
                if self.get_mod().children.contains_key(&ident) {
                    eprintln!("Definition `{}` conflicts with existing module/definition in scope", get_value(ident).unwrap());
                    self.err = true;
                } else {
                    self.get_mod().children.insert(ident, Unit::Bytes(start, s.len()));
                }
                while self.data.len() % 8 != 0 {
                    self.data.push(0);
                }
                self.data.append(&mut s);
            }
            Some(Token::RParen) => {
                // TODO
                eprintln!("Definition must have a value.");
                self.err = true;
                return;
            }
            Some(_) => {
                // TODO
                eprintln!("Define must be a constant.");
                self.err = true;
            }
            None => {
                // TODO
                eprintln!("Unexpected EOF.");
                self.err = true;
                return;
            }
        }

        if Some(Token::RParen) != self.next() {
            eprintln!("Missing closing parenthesis.");
            self.err = true;
        }
    }

    fn get_mod(&mut self) -> &mut Module {
        &mut self.modules[self.module]
    }

    fn skip_opcode(&mut self) {
        while let Some(t) = self.next() {
            match t {
                Token::RParen => return,
                Token::LParen => {
                    // TODO
                    self.position -= 1;
                    return;
                }
                _ => (),
            }
        }
        // TODO
        eprintln!("Unclosed opcode.");
        self.err = true;
    }

    fn handle_opcode(&mut self, symbol: usize) {
        const FUNCT3: [u32; 41] = [0, 0, 4, 6, 7, 1, 5, 5, 2, 3, 0, 4, 6,   // r
                                   0, 0, 4, 6, 7, 1, 5, 5, 2, 3,            // i
                                   0, 1, 2, 3, 4, 5, 6, 0,                  // i2 - last item is gap for la
                                   0, 1, 2, 3,                              // s
                                   0, 1, 4, 5, 6, 7];                       // b
        let i = if symbol < symbols::ADD {
            // TODO
            eprintln!("Cannot use register as opcode");
            self.err = true;
            return self.skip_opcode();
        // R instructions
        } else if symbol <= symbols::REM {
            let funct7 = match symbol {
                symbols::SUB | symbols::SRAI => 0x20,
                symbols::MUL | symbols::DIV | symbols::REM => 0x01,
                _ => 0x00,
            };
            let rd = self.unwrap_register();
            let rs1 = self.unwrap_register();
            let rs2 = self.unwrap_register();
            (funct7 << 25) | (rs2 << 20) | (rs1 << 15) | (FUNCT3[symbol - symbols::ADD] << 12) | (rd << 7) | 0b0110011
        // I instructions
        } else if symbol <= symbols::SLTIU {
            let rd = self.unwrap_register();
            let rs1 = self.unwrap_register();
            let mut imm = self.unwrap_imm();
            if symbol == symbols::SUBI {
                imm = (-(imm as i32)) as u32;
            } else if symbol == symbols::SRAI {
                imm = imm | (0x20 << 5);
            }
            (imm << 20) | (rs1 << 15) | (FUNCT3[symbol - symbols::ADD] << 12) | (rd << 7) | 0b0010011
        // I2 instructions
        } else if symbol <= symbols::LWU {
            let rd = self.unwrap_register();
            let (rs1, imm) = self.unwrap_offset();
            (imm << 20) | (rs1 << 15) | (FUNCT3[symbol - symbols::ADD] << 12) | (rd << 7) | 0b0000011
        } else if symbol == symbols::LA {
            let rd = self.unwrap_register();
            // TODO: lookup global/rewrite
            let ident = match self.next() {
                Some(Token::Symbol(s)) => s,
                Some(Token::LParen) => {
                    todo!();
                },
                Some(Token::RParen) => {
                    eprintln!("Expected identifier in instruction.");
                    self.err = true;
                    return;
                },
                Some(_) => {
                    eprintln!("Unexpected identifier in instruction.");
                    self.err = true;
                    return self.skip_opcode();
                }
                None => {
                    eprintln!("Unexpected EOF in instruction.");
                    self.err = true;
                    return;
                }
            };
            let p = self.get_mod().code.len();
            self.get_mod().rewrites.push((p, ident));
            // lui
            let i = (rd << 7) | 0b0110111;
            self.get_mod().code.push(i);
            // addi
            (rd << 15) | (rd << 7) | 0b0010011
        // S instructions
        } else if symbol <= symbols::SD {
            let (rs1, imm) = self.unwrap_offset();
            let rd = self.unwrap_register();
            ((imm >> 5) << 25) | (rd << 20) | (rs1 << 15) | (FUNCT3[symbol - symbols::ADD] << 12) | ((imm & 0b11111) << 7) | 0b0100011

        // B instructions
        } else if symbol <= symbols::BGEU {
            // TODO
            let rs1 = self.unwrap_register();
            let rs2 = self.unwrap_register();
            let mut i = (rs2 << 20) | (rs1 << 15) | (FUNCT3[symbol - symbols::ADD] << 12) | 0b1100011;
            if let Some(imm) = self.unwrap_label(true) {
                assert!(imm >= -4096 && imm <= 4095);
                let imm = imm as u32;
                let imm1 = (imm & 0x1e) | ((imm >> 11) & 1);
                let imm2 = ((imm & 0x10_00) >> 6) | ((imm >> 5) & 0x3F);
                i = (imm2 << 25) | (imm1 << 7) | i;
            }
            i
        // Other instructions
        } else if symbol == symbols::JAL {
            // TODO
            let rd = self.unwrap_register();
            let mut i = (rd << 7) | 0b1101111;
            if let Some(imm) = self.unwrap_label(false) {
                assert!(imm >= -1048576 && imm <= 1048575);
                let imm = imm as u32;
                let imm2 = ((imm & 0x10_00_00) >> 1) | (((imm >> 1) & 0x3_FF) << 9) | (((imm >> 11) & 1) << 8) | ((imm >> 12) & 0xFF);
                i = (imm2 << 12) | i;
            }
            i
        } else if symbol == symbols::JALR {
            let rd = self.unwrap_register();
            let (rs, imm) = self.unwrap_offset();
            (imm << 20) | (rs << 15) | (rd << 7) | 0b1100111
        } else if symbol == symbols::LUI || symbol == symbols::AUIPC {
            let rd = self.unwrap_register();
            let imm = self.unwrap_imm();
            (imm << 12) | (rd << 7) | if symbol == symbols::LUI { 0b0110111 } else { 0b0010111 }
        } else if symbol == symbols::ECALL {
            0b1110011
        } else if symbol == symbols::EBREAK {
            1 << 12 | 0b1110011
        } else {
            // TODO
            eprintln!("Unknown opcode");
            self.err = true;
            return self.skip_opcode();
        };
        self.get_mod().code.push(i);

        match self.next() {
            Some(Token::RParen) => (),
            Some(Token::LParen) => {
                self.position -= 1;
                eprintln!("Unclosed instruction.");
                self.err = true;
            }
            Some(_) => {
                eprintln!("Too many arguments or missing parenthesis in instruction.");
                self.err = true;
                self.skip_opcode();
            }
            None => {
                eprintln!("Unclosed instruction at end of file.");
                self.err = true;
            }
        }

    }

    fn unwrap_register(&mut self) -> u32 {
        match self.next() {
            Some(Token::Symbol(s)) => if s <= symbols::X31 {
                s as u32 - 1
            } else {
                // TODO
                eprintln!("Expected register in instruction, got identifier `{}`", get_value(s).unwrap());
                self.err = true;
                0
            },
            Some(Token::RParen) | Some(Token::LParen) => {
                self.position -= 1;
                eprintln!("Expected register in instruction");
                self.err = true;
                0
            }
            Some(_) => {
                // TODO
                eprintln!("Expected register in instruction");
                self.err = true;
                0
            }
            None => {
                // TODO
                eprintln!("Missing register in instruction");
                self.err = true;
                0
            }
        }
    }

    fn unwrap_imm(&mut self) -> u32 {
        match self.next() {
            Some(Token::LParen) => {
                match self.next() {
                    Some(Token::Symbol(s)) if s == symbols::LEN => (),
                    Some(Token::Symbol(s)) => {
                        // TODO PATH
                    }
                    Some(_) => {
                        // TODO
                        self.err = true;
                    }
                    None => {
                        // TODO
                        self.err = true;
                    }
                }
                match self.next() {
                    Some(Token::Symbol(s)) => {
                        // TODO: lookup global and get length
                        0
                    },
                    Some(_) => {
                        // TODO
                        self.err = true;
                        0
                    }
                    None => {
                        // TODO
                        self.err = true;
                        0
                    }
                }
            }
            Some(Token::Symbol(s)) => {
                match self.get_mod().children.get(&s) {
                    Some(Unit::Constant(i)) => *i as i32 as u32,
                    Some(_) => {
                        eprintln!("Variable must be a constant.");
                        self.err = true;
                        0
                    }
                    None => {
                        eprintln!("Unknown variable.");
                        self.err = true;
                        0
                    }
                }
            }
            Some(Token::Char(c)) => c as u32,
            Some(Token::Integer(i)) => {
                // TODO
                i as i32 as u32
            }
            Some(Token::RParen) => {
                // TODO
                eprintln!("Expected immediate argument in instruction.");
                self.err = true;
                0
            }
            Some(_) => {
                // TODO
                self.err = true;
                0
            }
            None => {
                // TODO
                self.err = true;
                0
            }
        }
    }

    fn unwrap_offset(&mut self) -> (u32, u32) {
        match self.next() {
            Some(Token::Symbol(_)) => {
                self.position -= 1;
                return (self.unwrap_register(), 0);
            }
            Some(Token::LParen) => (),
            Some(Token::RParen) => {
                // TODO
                self.position -= 1;
                self.err = true;
                return (0, 0);
            }
            Some(_) => {
                // TODO
                self.err = true;
                return (0, 0);
            }
            None => {
                // TODO
                self.err = true;
                return (0, 0);
            }
        }
        let neg = match self.next() {
            Some(Token::Symbol(s)) => if s == symbols::PLUS {
                false
            } else if s == symbols::NEG {
                true
            } else {
                // TODO
                self.err = true;
                return (0, 0);
            },
            Some(Token::RParen) => {
                // TODO
                self.err = true;
                return (0, 0);
            }
            Some(_) => {
                // TODO
                self.err = true;
                self.skip_opcode();
                return (0, 0);
            }
            None => {
                // TODO
                self.err = true;
                return (0, 0);
            }
        };

        let (mut imm, reg) = match self.peek() {
            Some(Token::Symbol(s)) => if s <= symbols::X31 {
                (self.unwrap_imm(), s as u32 - 1)
            } else {
                (self.unwrap_imm(), self.unwrap_register())
            },
            Some(Token::Integer(_)) => {
                (self.unwrap_imm(), self.unwrap_register())
            }
            Some(Token::LParen) => {
                //TODO read path
                todo!();
            }
            Some(Token::RParen) => {
                // TODO
                self.err = true;
                return (0, 0);
            }
            Some(_) => {
                // TODO
                self.err = true;
                (0, 0)
            }
            None => {
                // TODO
                self.err = true;
                return (0, 0);
            }
        };

        if self.next() != Some(Token::RParen) {
            self.err = true;
        }
        if neg {
            imm = -(imm as i32) as u32;
        }
        (reg, imm)
    }

    fn unwrap_label(&mut self, branchp: bool) -> Option<i32> {
        let s = match self.next() {
            Some(Token::Symbol(s)) => s,
            // TODO
            Some(Token::LParen) => {
                todo!();
            },
            Some(Token::RParen) => {
                self.position -= 1;
                eprintln!("Expected label in instruction.");
                self.err = true;
                return None;
            },
            Some(_) => {
                eprintln!("Expected label in instruction.");
                self.err = true;
                return None;
            },
            None => {
                eprintln!("Expected label in instruction, got EOF.");
                self.err = true;
                return None;
            }
        };
        let p = self.get_mod().code.len() * 4;
        match self.get_mod().labels.get(&s) {
            Some(s) => Some((*s as isize - p as isize) as i32),
            None => {
                self.get_mod().jumps.push((s, p, if branchp { JumpType::Branch } else { JumpType::Jump }));
                None
            }
        }
    }

    fn next(&mut self) -> Option<Token> {
        if self.position < self.tokens.len() {
            self.position += 1;
            Some(self.tokens[self.position - 1])
        } else {
            None
        }
    }

    fn peek(&self) -> Option<Token> {
        if self.position < self.tokens.len() {
            Some(self.tokens[self.position])
        } else {
            None
        }
    }
}

/*
struct Asm {
    path: Vec<String>,
    module: Module,
    data: Vec<Vec<u8>>,
    rewrites: HashMap<usize, usize>,
    modules: HashMap<String, Module>,
    module_stack: Vec<Module>,
    imports: Vec<String>,
}

struct Program {
    modules: Vec<Module>,
    stack: Vec<usize>,
    current: usize,
}

struct Module {
    parent: Option<usize>,
    code: Assembler,
    children: HashMap<String, Unit>,
}

impl Module {
    fn new(parent: Option<usize>) -> Self {
        Module {
            parent: parent,
            code: Assembler::new(),
            children: HashMap::new(),
        }
    }
}

enum Unit {
    Constant(isize),
    //String(String),
    Bytes(Vec<u8>),
    Module(usize),
}

macro_rules! r {
    ($op:ident, $self:ident, $tokens:ident, $input:ident) => {
        {
            let rd = $self.unwrap_register($tokens, $input);
            let rs1 = $self.unwrap_register($tokens, $input);
            let rs2 = $self.unwrap_register($tokens, $input);
            assert!($tokens.next().unwrap().closerp());
            $self.module.asm.$op(rd, rs1, rs2);
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
            $self.module.asm.$op(rd, rs1, imm);
        }
    };
}

macro_rules! i2 {
    ($op:ident, $self:ident, $tokens:ident, $input:ident) => {
        {
            let rd = $self.unwrap_register($tokens, $input);
            let (rs1, imm) = $self.offset($tokens, $input);
            assert!($tokens.next().unwrap().closerp());
            $self.module.asm.$op(rd, rs1, imm);
        }
    };
}

macro_rules! s {
    ($op:ident, $self:ident, $tokens:ident, $input:ident) => {
        {
            let (rs1, imm) = $self.offset($tokens, $input);
            let rd = $self.unwrap_register($tokens, $input);
            assert!($tokens.next().unwrap().closerp());
            $self.module.asm.$op(rd, rs1, imm);
        }
    };
}

macro_rules! b {
    ($op:ident, $self:ident, $tokens:ident, $input:ident) => {
        {
            let rs1 = $self.unwrap_register($tokens, $input);
            let rs2 = $self.unwrap_register($tokens, $input);
            let label = $self.unwrap_label($tokens, $input);
            assert!($tokens.next().unwrap().closerp());
            $self.module.asm.$op(rs1, rs2, &label);
        }
    };
}

impl Program {
    fn new(tokens: Vec<Token>, input: &str, module_name: String) -> Self {
        let mut asm = Asm {
            path: vec![module_name],
            module: Module::new(),
            data: Vec::new(),
            rewrites: HashMap::new(),
            modules: HashMap::new(),
            module_stack: Vec::new(),
            imports: Vec::new(),
        };

        asm.assemble(tokens, input);
        asm
    }

    fn finish(mut self) -> (Vec<u8>, Vec<Vec<u8>>, HashMap<usize, usize>) {
        for (_label, module) in self.modules {
            self.module.asm.append(module.asm);
        }
        (self.module.asm.finish(), self.data, self.rewrites)
    }

    fn assemble_file(&mut self, path: &str) {
        let input = fs::read_to_string(path.to_string() + ".sasm").unwrap();
        let tokens = tokenizer::tokenize(&input.as_bytes()).unwrap();
        self.modules[self.current].children.insert(path.to_string(), Unit::Module(self.modules.len()));
        self.modules.push(Module::new(Some(self.current)));
        self.stack.push(self.current);
        self.current = self.modules.len() - 1;

        let mut tokens = tokens.iter().filter(|t| !t.commentp());
        self.assemble(&mut tokens, &input);
        self.current = self.stack.pop().unwrap();

    }

    fn assemble<'b,  I: Iterator<Item = &'b Token>>(&mut self, tokens: &mut I, input: &str) {
    //fn assemble(&mut self, tokens: Vec<Token>, input: &str) {
        while let Some(t) = tokens.next() {
            match t {
                Token::RParen => return,
                s @ Token::Symbol(_) => {
                    //let label = self.path_to_string() + " " + s.as_str(input);
                    let label = s.as_str(input);
                    self.modules[self.current].code.label(label);
                    //self.module.asm.label(&label);
                },
                Token::LParen => if let Some(Token::Symbol(i)) = tokens.next() {
                    let s = Token::Symbol(*i).as_str(input);
                    if "include!" == s {
                        self.handle_include(tokens, input);
                    } else if "module" == s {
                        self.handle_module(tokens, input);
                    } else if "import" == s {
                        self.handle_import(tokens, input);
                    } else if "define" == s {
                        self.handle_define(tokens, input);
                    } else {
                        self.handle_opcode(s, tokens, input);
                    }
                } else {
                    unreachable!();
                },
                /*
                Token::RParen => if self.path.len() > 1 {
                    let module_name = self.path_to_string();
                    self.path.pop();
                    let mut module = self.module_stack.pop().unwrap();
                    std::mem::swap(&mut module, &mut self.module);
                    self.modules.insert(module_name, module);
                } else {
                    unreachable!();
                },
                */
                _ => unreachable!(),
            }
        }

        /*
        assert!(self.path.len() == 1);
        for path in self.imports.clone() {
            let import = fs::read_to_string(path.clone() + ".sasm").unwrap();
            let import_tokens = tokenizer::Tokenizer::tokenize(&import).unwrap();
            self.path.push(path);
            let mut module = Module::new();
            std::mem::swap(&mut module, &mut self.module);
            self.assemble(import_tokens, &import);
            std::mem::swap(&mut module, &mut self.module);
            self.modules.insert(self.path_to_string(), module);
            self.path.pop();
        }
        */
    }

        /*
    fn path_to_string(&self) -> String {
        self.path[1..].iter().fold(self.path[0].clone(), |acc, i| acc + " " + i)
    }
        */

    fn handle_include<'b,  I: Iterator<Item = &'b Token>>(&mut self, tokens: &mut I, input: &str) {
        let filename = tokens.next().unwrap();
        assert!(filename.is_string());
        let filename = filename.as_str(input);
        let filename = &filename[1..filename.len()-1];
        assert!(tokens.next().unwrap().closerp());
        let include_input = fs::read_to_string(filename).unwrap();
        let include_tokens = tokenizer::tokenize(&include_input.as_bytes()).unwrap();
        let mut include_tokens = include_tokens.iter().filter(|t| !t.commentp());
        self.assemble(&mut include_tokens, &include_input);
    }

    fn handle_module<'b,  I: Iterator<Item = &'b Token>>(&mut self, tokens: &mut I, input: &str) {
        let var = if let Some(Token::Symbol(i)) = tokens.next() {
            Token::Symbol(*i).as_str(input).to_string()
        } else {
            unreachable!();
        };
        //let label = self.path_to_string() + " " + &var;

        let mut module = Module::new(Some(self.current));
        module.code.label(&var);
        self.stack.push(self.current);
        self.modules[self.current].children.insert(var, Unit::Module(self.modules.len()));
        self.current = self.modules.len();
        self.modules.push(module);
        self.assemble(tokens, input);

    }

    fn handle_import<'b,  I: Iterator<Item = &'b Token>>(&mut self, tokens: &mut I, input: &str) {
        /*
        let mut path = Vec::new();
        loop {
            match tokens.next().unwrap() {
                Token::RParen => break,
                Token::Symbol(i) => path.push(Token::Symbol(*i).as_str(input).to_string()),
                _ => unreachable!(),
            }
        }
        assert!(!path.is_empty());
        */

        let path = if let Some(Token::Symbol(i)) = tokens.next() {
            Token::Symbol(*i).as_str(input).to_string()
        } else {
            unreachable!();
        };
        assert!(tokens.next().unwrap().closerp());
        if self.modules[self.current].children.get(path).is_some() {
        } else if let Some(Unit::Module(ptr)) = self.modules[self.modules[self.current].parent.unwrap()].children.get(path) {
            self.modules[self.current].children.insert(path, Unit::Module(ptr));
        } else {
            self.new_module(path);
        }
    }

    fn get_module(&self, path: String) -> Option<usize> {
        let m = self.modules[self.current];
        if let Some(Unit::Module(u)) = m.children.get(path) {
            Some(u)
        } else if let Some(Unit::Module(u)) = self.modules[m.parent.unwrap()].children.get(path) {
            Some(u)
        } else {
            None
        }
    }

    fn module(&mut self) -> &mut Module {
        &mut self.modules[self.current]
    }

    fn handle_define<'b,  I: Iterator<Item = &'b Token>>(&mut self, tokens: &mut I, input: &str) {
        let var = if let Some(Token::Symbol(i)) = tokens.next() {
            //self.path_to_string() + " " + Token::Symbol(*i).as_str(input)
            Token::Symbol(*i).as_str(input).to_string()
        } else {
            unreachable!();
        };

        match tokens.next().unwrap() {
            s @ Token::Integer(_) => {
                let i = s.as_str(input).parse().unwrap();
                self.module().children.insert(var, Unit::Constant(i));
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
                self.module().children.insert(var, Unit::Constant(i as isize));
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
                self.module().children.insert(var, Unit::Bytes(v));
                //self.module.globals.insert(var, self.data.len());
                //self.data.push(v);
            }
            Token::Pound => {
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
                self.module().children.insert(var, Unit::Bytes(v));
                //self.module.globals.insert(var, self.data.len());
                //self.data.push(v);
            }
            _ => unreachable!(),
        }

        assert!(tokens.next().unwrap().closerp());
    }

    fn unwrap_register<'b, I: Iterator<Item = &'b Token>>(&self, tokens: &mut I, input: &str) -> Register {
        if let Some(Token::Symbol(i)) = tokens.next() {
            let reg = Token::Symbol(*i).as_str(input);
            Register::from_str(reg).unwrap()
        } else {
            unreachable!();
        }
    }

    fn unwrap_label<'b, I: Iterator<Item = &'b Token>>(&self, tokens: &mut I, input: &'b str) -> String {
        match tokens.next().unwrap() {
            s @ Token::Symbol(_) => {
                self.path_to_string() + " " + s.as_str(input)
            }
            t if t.openerp() => {
                let mut label = String::new();
                loop {
                    match tokens.next().unwrap() {
                        s @ Token::Symbol(_) => {
                            label.push_str(s.as_str(input));
                            label.push_str(" ");
                        },
                        t if t.closerp() => break,
                        _ => unreachable!(),
                    }
                }

                label.pop();
                assert!(!label.is_empty());
                self.path_to_string() + " " + &label
            },
            _ => unreachable!(),
        }
    }

    fn offset<'b, I: Iterator<Item = &'b Token>>(&self, tokens: &mut I, input: &str) -> (Register, i32) {
        match tokens.next().unwrap() {
            s @ Token::Symbol(_) => {
                let s = s.as_str(input);
                (Register::from_str(s).unwrap(), 0)
            }
            Token::LParen => {
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
                        } else {
                            (None, Some(*self.module.constants.get(s).unwrap()))
                        };

                        if let Some(r) = r {
                            match tokens.next().unwrap() {
                                i @ Token::Integer(_) => (r, i.as_str(input).parse().unwrap()),
                                i @ Token::Symbol(_) => (r, *self.module.constants.get(i.as_str(input)).unwrap()),
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
                        let s = self.module.globals.get(s).unwrap();
                        self.data[*s].len()
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
                *self.module.constants.get(s).unwrap()
            },
            s @ _ => unreachable!("{:?}", s),
        }
    }

    fn handle_opcode<'b, I: Iterator<Item = &'b Token>>(&mut self, opcode: &str, tokens: &mut I, input: &'b str) {
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
            "div" => r!(div, self, tokens, input),
            "rem" => r!(rem, self, tokens, input),

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
                let symbol = self.unwrap_label(tokens, input);
                assert!(tokens.next().unwrap().closerp());
                let addr = self.module.globals.get(&symbol).unwrap();
                self.rewrites.insert(self.module.asm.len(), *addr);
                self.module.asm.lui(rd, 0);
                self.module.asm.addi(rd, rd, 0);
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
                let label = self.unwrap_label(tokens, input);
                assert!(tokens.next().unwrap().closerp());
                self.module.asm.jal(rd, &label);
            }
            "jalr" => {
                let rd = self.unwrap_register(tokens, input);
                let (rs, imm) = self.offset(tokens, input);
                assert!(tokens.next().unwrap().closerp());
                // TODO
                self.module.asm.jalr(rd, rs, imm);
            }
            "lui" => {
                let rd = self.unwrap_register(tokens, input);
                let imm = self.read_imm(tokens, input);
                assert!(tokens.next().unwrap().closerp());
                self.module.asm.lui(rd, imm as u32);
            }
            "auipc" => {
                let rd = self.unwrap_register(tokens, input);
                let imm = self.read_imm(tokens, input);
                assert!(tokens.next().unwrap().closerp());
                self.module.asm.auipc(rd, imm as u32);
            }
            "ecall" => {
                assert!(tokens.next().unwrap().closerp());
                self.module.asm.ecall();
            }
            "ebreak" => {
                assert!(tokens.next().unwrap().closerp());
                self.module.asm.ebreak();
            }
            _ => unreachable!("{}", opcode),
        }
    }
}
*/
