mod elf;
mod symbols;
mod tokenizer;

use tokenizer::{get_symbol, get_value, Token, TokenInfo};

use std::{env, mem};
use std::collections::HashMap;
use std::fs::{self, OpenOptions};
use std::io::ErrorKind as IOError;
use std::os::unix::fs::OpenOptionsExt;
use std::path::PathBuf;
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
    let input = read_file(&input_file).unwrap_or_else(|| exit(1));
    let tokenizer::Tokenizer { input, tokens, token_info, err, filename, .. } = tokenizer::tokenize(input, input_file);
    let (program, data, rodata, rewrites) = assemble(input, filename, tokens, token_info, err);
    let e = if debug {
        elf::Elf::new_debug(program, data, rodata, rewrites)
    } else {
        elf::Elf::new(program, data, rodata, rewrites)
    };
    let f = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .mode(0o777)
        .open(output_file)
        .unwrap();
    e.write(f).unwrap();
}

fn read_file<P: AsRef<std::path::Path>>(path: P) -> Option<Vec<u8>> {
    let path = path.as_ref();
    match fs::read(path) {
        Ok(v) => Some(v),
        Err(e) => {
            match e.kind() {
                IOError::NotFound => {
                    eprintln!("No such file `{}`", path.display());
                }
                IOError::PermissionDenied => {
                    eprintln!("Permission denied trying to open file `{}`", path.display());
                }
                IOError::OutOfMemory => {
                    eprintln!("Not enough memory to read file `{}`", path.display());
                }
                _ => {
                    eprintln!("Unknown error reading file `{}`: {}", path.display(), e);
                }
            };
            None
        }
    }
}

fn assemble(input: Vec<u8>, filename: String, tokens: Vec<Token>, token_info: Vec<TokenInfo>, err: bool)
    -> (Vec<u32>, Vec<u8>, Vec<u8>, Vec<(usize, usize, bool)>)
{
    let mut root = Module::new(0, None, false);
    let mut path: PathBuf = filename.clone().into();
    path.pop();
    root.path = path;
    let name = get_symbol(filename.rsplitn(2, '.').last().unwrap().as_bytes().to_vec());
    root.children.insert(name, (false, Unit::Module(1)));

    let mut asm = Asm {
        input: input,
        filename: filename,
        tokens: tokens,
        token_info: token_info,
        position: 0,
        err: err,
        modules: vec![root, Module::new(1, Some(0), true)],
        module: 1,
        data: Vec::new(),
        rodata: Vec::new(),
        import_files: HashMap::new(),
    };
    asm.add_label(name);
    loop {
        asm.assemble();
        if asm.peek().is_none() {
            break;
        }
        eprintln!("Unxepected closing parenthesis in file `{}` at line {}.", asm.filename, asm.token_info[asm.position].line);
        asm.err = true;
        asm.position += 1;
    }
    let (code, data, rodata, rewrites) = asm.finish();
    if asm.err {
        exit(1);
    }
    (code, data, rodata, rewrites)
}


struct Asm {
    input: Vec<u8>,
    filename: String,
    tokens: Vec<Token>,
    token_info: Vec<TokenInfo>,
    position: usize,
    err: bool,
    modules: Vec<Module>,
    module: usize,
    data: Vec<u8>,
    rodata: Vec<u8>,
    import_files: HashMap<PathBuf, Symbol>,
}

type Symbol = usize;
struct Module {
    id: usize,
    parent: Option<usize>,
    children: HashMap<Symbol, (bool, Unit)>,
    filep: bool,
    path: PathBuf,
    location: usize,
    code: Vec<u32>,
    labels: HashMap<Symbol, usize>,
    jumps: Vec<(Symbol, usize, JumpType)>,
    refs: Vec<(Vec<Symbol>, usize, JumpType)>,
    rewrites: Vec<(usize, usize, bool)>,
}

impl Module {
    fn new(id: usize, parent: Option<usize>, filep: bool) -> Self {
        Module {
            id: id,
            parent: parent,
            children: HashMap::new(),
            filep: filep,
            path: PathBuf::new(),
            location: 0,
            code: Vec::new(),
            labels: HashMap::new(),
            jumps: Vec::new(),
            refs: Vec::new(),
            rewrites: Vec::new(),
        }
    }

    pub fn finish(&mut self) -> Vec<(Symbol, usize, JumpType)> {
        let mut jumps = Vec::new();
        for (label, i, ty) in &self.jumps {
            let p = if let Some(p) = self.labels.get(label) {
                *p
            } else {
                jumps.push((*label, *i, *ty));
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

#[derive(Clone, Copy, Debug, PartialEq)]
enum Unit {
    Constant(i64),
    // position in data, len
    Bytes(usize, usize, bool),
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
                    Some(Token::Symbol(s)) if s == symbols::DEFCON || s == symbols::DEFVAR => self.handle_defcon_var(s == symbols::DEFCON),
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
    }

    fn finish(&mut self) -> (Vec<u32>, Vec<u8>, Vec<u8>, Vec<(usize, usize, bool)>) {
        let mut code = Vec::new();
        let mut rewrites = Vec::new();
        let mut refs = Vec::new();
        for module in &mut self.modules[1..] {
            let j = module.finish();
            for (l, i, ty) in j {
                refs.push((module.id, vec![l], i + (code.len() * 4), ty));
            }
            for (path, i, ty) in &module.refs {
                refs.push((module.id, path.clone(), i + (code.len() * 4), *ty));
            }
            self.module = module.id;
            for (i, p, constant) in &module.rewrites {
                rewrites.push((i + code.len(), *p, *constant));
            }
            module.location = code.len() * 4;
            code.append(&mut module.code);
        }

        for (id, path, i, ty) in refs {
            self.module = id;
            let p = match self.follow_path(&path) {
                Some(Unit::Module(m)) => self.modules[m].location,
                Some(_) => {
                    eprintln!("Expected module.");
                    self.err = true;
                    continue;
                }
                None => {
                    eprint!("Unknown module/label `");
                    for p in path {
                        eprint!("{} ", get_value(p));
                    }
                    eprintln!("`.");
                    self.err = true;
                    continue;
                }
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

        // TODO
        (code, self.data.clone(), self.rodata.clone(), rewrites)
    }

    fn add_label(&mut self, label: usize) {
        let m = self.get_mod();
        if m.labels.contains_key(&label) {
            eprintln!("Duplicate label `{}`.", get_value(label));
            self.err = true;
        } else if m.children.contains_key(&label) {
            eprintln!("Label `{}` conflicts with module/definition.", get_value(label));
            self.err = true;
        } else {
            m.labels.insert(label, m.code.len() * 4);
        }
    }

    fn handle_import(&mut self) {
        let path = match self.next() {
            Some(Token::Symbol(s)) => vec![s],
            Some(Token::LParen) => self.unwrap_path(),
            Some(Token::String(_, _)) => {
                eprintln!("Import path should consist of identifiers, not strings.");
                self.err = true;
                return self.skip_opcode();
            }
            Some(Token::RParen) => {
                eprintln!("Empty import statement.");
                self.err = true;
                return;
            }
            Some(_) => {
                eprintln!("Bad arguments in import statement.");
                self.err = true;
                return self.skip_opcode();
            }
            None => {
                eprintln!("Unexpected EOF in import statement.");
                self.err = true;
                return;
            }
        };
        match self.next() {
            Some(Token::RParen) => (),
            Some(Token::LParen) => {
                self.position -= 1;
                eprintln!("Forgot closing parenthesis in import statement.");
                eprintln!("\tNote: each import requires its own import statement.");
                self.err = true;
            }
            Some(_) => {
                eprintln!("Forgot closing parenthesis in import statement.");
                eprintln!("\tNote: each import requires its own import statement.");
                self.err = true;
                self.skip_opcode();
            }
            None => {
                eprintln!("Unexpected EOF in import statement.");
                self.err = true;
            }
        }
        if path.is_empty() {
            eprintln!("Empty path in import statement.");
            self.err = true;
            return;
        }

        if path.len() == 1 {
            if path[0] == symbols::CARAT || path[0] == symbols::STAR {
                eprintln!("Invalid path in import statement.");
                self.err = true;
                return;
            }
            let mut existp = false;
            let mut m = &self.modules[self.module];
            while !m.filep {
                if m.children.contains_key(&path[0]) {
                    existp = true;
                }
                // shitty bc
                let p = m.parent.unwrap();
                m = &self.modules[p];
            }
            let p = m.parent.unwrap();
            let mut file_path = self.modules[p].path.clone();
            file_path.push(get_value(path[0]));

            if file_path.is_dir() {
                self.import_dir(path[0], file_path, None);
                return;
            } else if { file_path.set_extension("sasm"); !file_path.is_file() } {
                if existp {
                    eprintln!("Unnecessary import statement");
                    self.err = true;
                    return;
                } else {
                    eprintln!("File `{}` does not exist.", file_path.display());
                    self.err = true;
                    return;
                }
            }

            self.import_file(path[0], file_path, None);
            return;
        }

        let mut m = &self.modules[self.module];
        let mut i = 0;
        while i < path.len() {
            let p = path[i];
            if p == symbols::CARAT {
                i += 1;
                let p = if let Some(p) = m.parent {
                    p
                } else {
                    eprintln!("Path jumped past root.");
                    self.err = true;
                    return;
                };
                m = &self.modules[p];
            } else {
                break;
            }
        }

        while i < path.len() {
            let p = path[i];
            i += 1;
            if p == symbols::CARAT {
                eprintln!("Bad path, can only move up (`^`) at beginning of path.");
                self.err = true;
                return;
            }

            if p == symbols::STAR && i != path.len() {
                eprintln!("Bad path, `*` can only appear as the final element.");
                self.err = true;
                return;
            } else if p == symbols::STAR {
                for (k, (importp, v)) in m.children.clone() {
                    if self.get_mod().children.contains_key(&k) {
                        eprintln!("`{}` is already defined in this scope.", get_value(k));
                        self.err = true;
                    } else if !importp {
                        self.get_mod().children.insert(k, (true, v));
                    }
                }
                return;
            }

            match m.children.get(&p).copied() {
                v @ Some((_, Unit::Bytes(_, _, _))) | v @ Some((_, Unit::Constant(_))) => {
                    let (importp, v) = v.unwrap();
                    // TODO
                    if importp {
                        eprintln!("Cannot import an import");
                        self.err = true;
                    } else if i == path.len() {
                        if self.get_mod().children.contains_key(&p) {
                            eprintln!("`{}` is already defined in this scope.", get_value(p));
                            self.err = true;
                        } else {
                            self.get_mod().children.insert(p, (true, v));
                        }
                    } else {
                        eprintln!("Path resolved to variable/global partway.");
                        self.err = true;
                    }
                    return;
                }
                Some((importp, Unit::Module(id))) if !importp => if i == path.len() {
                    if self.get_mod().children.contains_key(&p) {
                        eprintln!("`{}` is already defined in this scope.", get_value(p));
                        self.err = true;
                    } else {
                        self.get_mod().children.insert(p, (true, Unit::Module(id)));
                    }
                    return;
                } else {
                    m = &self.modules[id]
                },
                Some(_) | None => {
                    let mut m2 = m;
                    let mut existp = false;
                    while !m2.filep {
                        if m2.children.contains_key(&p) {
                            existp = true;
                        }
                        // shitty bc
                        let p = m2.parent.unwrap();
                        m2 = &self.modules[p];
                    }
                    let ptr = m2.parent.unwrap();
                    let mut file_path = self.modules[ptr].path.clone();
                    file_path.push(get_value(p));
                    if file_path.is_dir() {
                        if let Some(m_id) = self.import_dir(p, file_path, Some(ptr)) {
                            m = &self.modules[m_id];
                        } else {
                            self.err = true;
                            return;
                        }
                    } else if { file_path.set_extension("sasm"); !file_path.is_file() } {
                        // TODO: i = path.len
                        if existp {
                            eprintln!("Unnecessary import statement");
                            self.err = true;
                            return;
                        } else {
                            eprintln!("File `{}` does not exist.", file_path.display());
                            self.err = true;
                            return;
                        }
                    } else {
                        if let Some(m_id) = self.import_file(p, file_path, Some(ptr)) {
                            m = &self.modules[m_id];
                        } else {
                            self.err = true;
                            return;
                        }
                    }
                }
            }
        }
    }

    fn import_dir(&mut self, path: Symbol, file_path: PathBuf, parent: Option<usize>) -> Option<usize> {
        if let Some(id) = self.import_files.get(&file_path).copied() {
            if parent.is_some() {
                return Some(id);
            } else {
                match self.get_mod().children.get(&path) {
                    Some((_, Unit::Module(i))) if *i == id => eprintln!("Double import statement"),
                    Some(_) => {
                        eprintln!("Module `{}` conflicts with existing module/definition in scope", get_value(path));
                        self.err = true;
                    }
                    None => {
                        self.get_mod().children.insert(path, (true, Unit::Module(id)));
                        return Some(id);
                    }
                }
                return None;
            }
        }

        let m_id = self.modules.len();
        if parent.is_none() && self.get_mod().children.contains_key(&path) {
            eprintln!("Module `{}` conflicts with existing module/definition in scope", get_value(path));
            self.err = true;
        } else if parent.is_none() {
            self.get_mod().children.insert(path, (true, Unit::Module(m_id)));
        }
        self.import_files.insert(file_path.clone(), m_id);
        // TODO: should filep be set?
        let parent = parent.unwrap_or(self.module);
        let mut module = Module::new(m_id, Some(parent), false);
        module.path = file_path.clone();
        self.modules.push(module);
        let old_id = self.module;
        self.module = m_id;

        let dir = if let Ok(d) = fs::read_dir(&file_path) {
            d
        } else {
            eprintln!("Unable to read directory `{}`", file_path.display());
            self.err = true;
            return Some(m_id);
        };
        for f in dir {
            // TODO
            let f = f.unwrap();
            if f.file_type().unwrap().is_dir() {
                let f = f.path();
                let mut p = file_path.clone();
                p.push(f.file_name().unwrap());
                let path = get_symbol(f.file_stem().unwrap().as_encoded_bytes().to_vec());
                self.import_dir(path, p, None);
            } else {
                let f = f.path();
                if Some("sasm".as_ref()) != f.extension() {
                    continue;
                }
                let mut p = file_path.clone();
                p.push(f.file_name().unwrap());
                let path = get_symbol(f.file_stem().unwrap().as_encoded_bytes().to_vec());
                self.import_file(path, p, None);
            }
        }

        self.module = old_id;
        Some(m_id)
    }

    fn import_file(&mut self, path: Symbol, file_path: PathBuf, parent: Option<usize>) -> Option<usize> {
        if let Some(id) = self.import_files.get(&file_path).copied() {
            if parent.is_some() {
                return Some(id);
            } else {
                match self.get_mod().children.get(&path) {
                    Some((_, Unit::Module(i))) if *i == id => eprintln!("Double import statement"),
                    Some(_) => {
                        eprintln!("Module `{}` conflicts with existing module/definition in scope", get_value(path));
                        self.err = true;
                    }
                    None => {
                        self.get_mod().children.insert(path, (true, Unit::Module(id)));
                        return Some(id);
                    }
                }
                return None;
            }
        }

        // shitty bc
        let m_id = self.modules.len();
        if parent.is_none() && self.get_mod().children.contains_key(&path) {
            eprintln!("Module `{}` conflicts with existing module/definition in scope", get_value(path));
            self.err = true;
        } else if parent.is_none() {
            self.get_mod().children.insert(path, (true, Unit::Module(m_id)));
        }
        self.import_files.insert(file_path.clone(), m_id);
        let parent = parent.unwrap_or(self.module);
        let module = Module::new(m_id, Some(parent), true);
        let old_id = self.module;
        self.module = m_id;
        self.modules.push(module);
        self.add_label(path);

        let include_input = if let Some(i) = read_file(&file_path) {
            i
        } else {
            self.module = self.get_mod().parent.unwrap();
            self.err = true;
            return Some(m_id);
        };

        let tokenizer::Tokenizer { mut input, mut tokens, mut token_info, err, mut filename, .. } = tokenizer::tokenize(include_input, file_path.display().to_string());
        self.err |= err;
        mem::swap(&mut self.input, &mut input);
        mem::swap(&mut self.tokens, &mut tokens);
        mem::swap(&mut self.token_info, &mut token_info);
        mem::swap(&mut self.filename, &mut filename);
        let position = self.position;
        self.position = 0;
        self.assemble();
        self.position = position;
        mem::swap(&mut self.input, &mut input);
        mem::swap(&mut self.tokens, &mut tokens);
        mem::swap(&mut self.token_info, &mut token_info);
        mem::swap(&mut self.filename, &mut filename);

        self.module = old_id;
        Some(m_id)
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

        // shitty bc
        let m_id = self.modules.len();
        let module = Module::new(m_id, Some(self.module), false);
        if self.get_mod().children.contains_key(&ident) {
            eprintln!("Module `{}` conflicts with existing module/definition in scope", get_value(ident));
            self.err = true;
        } else {
            self.get_mod().children.insert(ident, (false, Unit::Module(m_id)));
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
        let include_input = if let Some(i) = read_file(&filename) {
            i
        } else {
            self.err = true;
            return;
        };

        let tokenizer::Tokenizer { mut input, mut tokens, mut token_info, err, mut filename, .. } = tokenizer::tokenize(include_input, filename);
        self.err |= err;
        mem::swap(&mut self.input, &mut input);
        mem::swap(&mut self.tokens, &mut tokens);
        mem::swap(&mut self.token_info, &mut token_info);
        mem::swap(&mut self.filename, &mut filename);
        let position = self.position;
        self.position = 0;
        self.assemble();
        self.position = position;
        mem::swap(&mut self.input, &mut input);
        mem::swap(&mut self.tokens, &mut tokens);
        mem::swap(&mut self.token_info, &mut token_info);
        mem::swap(&mut self.filename, &mut filename);
    }

    fn unwrap_ident(&mut self) -> Option<Symbol> {
        match self.next() {
            Some(Token::Symbol(s)) => Some(s),
            Some(Token::RParen) => {
                // TODO
                eprintln!("Definition must have an identifier and value.");
                self.err = true;
                None
            }
            Some(_) => {
                // TODO
                eprintln!("Expected identifier.");
                self.err = true;
                Some(0)
            }
            None => {
                // TODO
                eprintln!("Unexpected EOF.");
                self.err = true;
                None
            }
        }
    }

    fn unwrap_array(&mut self) -> Option<Vec<u8>> {
        match self.next() {
            Some(Token::LParen) => (),
            Some(Token::RParen) => {
                eprintln!("Unexepected `#` in definition, definition incomplete.");
                self.err = true;
                return None;
            }
            Some(_) => {
                eprintln!("Unexepected `#` in definition.");
                self.err = true;
                self.skip_opcode();
                return None;
            }
            None => {
                eprintln!("Unexepected `#` at end of file.");
                self.err = true;
                return None;
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
                    return None;
                }
            }
        }
        Some(v)
    }

    fn handle_define(&mut self) {
        let ident = if let Some(i) = self.unwrap_ident() {
            i
        } else {
            return;
        };

        // TODO
        match self.next() {
            // retarded match behaviour
            Some(Token::Integer(i)) => if self.get_mod().children.contains_key(&ident) {
                eprintln!("Definition `{}` conflicts with existing module/definition in scope", get_value(ident));
                self.err = true;
            } else {
                self.get_mod().children.insert(ident, (false, Unit::Constant(i)));
            },
            Some(Token::Char(c)) => if self.get_mod().children.contains_key(&ident) {
                eprintln!("Definition `{}` conflicts with existing module/definition in scope", get_value(ident));
                self.err = true;
            } else {
                self.get_mod().children.insert(ident, (false, Unit::Constant(c as i64)));
            },
            Some(Token::Pound) | Some(Token::String(_, _)) => {
                eprintln!("Define is only for build-time constants, did you mean `defcon`/`defvar`?");
                self.err = true;
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

    fn handle_defcon_var(&mut self, constant: bool) {
        let ident = if let Some(i) = self.unwrap_ident() {
            i
        } else {
            return;
        };

        match self.next() {
            Some(Token::Integer(i)) => if self.get_mod().children.contains_key(&ident) {
                eprintln!("Definition `{}` conflicts with existing module/definition in scope", get_value(ident));
                self.err = true;
            } else {
                let d = if constant {
                    &mut self.rodata
                } else {
                    &mut self.data
                };
                while d.len() % 8 != 0 {
                    d.push(0);
                }
                let start = d.len();
                d.extend_from_slice(&i.to_le_bytes());
                self.get_mod().children.insert(ident, (false, Unit::Bytes(start, 8, constant)));
            },
            Some(Token::Char(c)) => if self.get_mod().children.contains_key(&ident) {
                eprintln!("Definition `{}` conflicts with existing module/definition in scope", get_value(ident));
                self.err = true;
            } else {
                let d = if constant {
                    &mut self.rodata
                } else {
                    &mut self.data
                };
                while d.len() % 8 != 0 {
                    d.push(0);
                }
                let start = d.len();
                d.push(c);
                self.get_mod().children.insert(ident, (false, Unit::Bytes(start, 1, constant)));
            },
            Some(Token::Pound) => {
                let mut v = if let Some(v) = self.unwrap_array() {
                    v
                } else {
                    return;
                };
                if v.is_empty() {
                    eprintln!("Definition of empty array literal.");
                }

                let len = v.len();
                let d = if constant {
                    &mut self.rodata
                } else {
                    &mut self.data
                };
                while d.len() % 8 != 0 {
                    d.push(0);
                }
                let start = d.len();
                d.append(&mut v);
                if self.get_mod().children.contains_key(&ident) {
                    eprintln!("Definition `{}` conflicts with existing module/definition in scope", get_value(ident));
                    self.err = true;
                } else {
                    self.get_mod().children.insert(ident, (false, Unit::Bytes(start, len, constant)));
                }
            }
            Some(Token::String(start, end)) => {
                let mut s = self.get_string(start, end);
                if s.is_empty() {
                    eprintln!("Definition of empty string literal.");
                }

                let len = s.len();
                let d = if constant {
                    &mut self.rodata
                } else {
                    &mut self.data
                };
                while d.len() % 8 != 0 {
                    d.push(0);
                }
                let start = d.len();
                d.append(&mut s);
                if self.get_mod().children.contains_key(&ident) {
                    eprintln!("Definition `{}` conflicts with existing module/definition in scope", get_value(ident));
                    self.err = true;
                } else {
                    self.get_mod().children.insert(ident, (false, Unit::Bytes(start, len, constant)));
                }
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
                    // TODO check for newline
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
        const FUNCT3: [u32; 40] = [0, 0, 4, 6, 7, 1, 5, 5, 2, 3, 0, 4, 6,   // r
                                   0, 0, 4, 6, 7, 1, 5, 5, 2, 3,            // i
                                   0, 1, 2, 3, 4, 5, 6,                     // i2
                                   0, 1, 2, 3,                              // s
                                   0, 1, 4, 5, 6, 7];                       // b
        let i = if symbol < symbols::ADD {
            // TODO
            eprintln!("Cannot use register as opcode");
            self.err = true;
            return self.skip_opcode();
        // R instructions
        } else if symbol <= symbols::LAST_R {
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
        } else if symbol <= symbols::LAST_I {
            let rd = self.unwrap_register();
            let rs1 = self.unwrap_register();
            let mut imm = self.unwrap_imm();
            if symbol == symbols::SUBI {
                imm = (-(imm as i32)) as u32;
            } else if symbol == symbols::SRAI {
                imm = imm | (0x20 << 5);
            }
            if (imm as i32) >= 2048 || (imm as i32) < -2048 {
                eprintln!("Immediate `{}` out of range [-2048, 2048)", imm as i32);
                self.err = true;
                imm = 0
            }
            (imm << 20) | (rs1 << 15) | (FUNCT3[symbol - symbols::ADD] << 12) | (rd << 7) | 0b0010011
        // I2 instructions
        } else if symbol <= symbols::LAST_I2 {
            let rd = self.unwrap_register();
            let (rs1, mut imm) = self.unwrap_offset();
            if (imm as i32) >= 2048 || (imm as i32) < -2048 {
                eprintln!("Offset `{}` out of range [-2048, 2048)", imm as i32);
                self.err = true;
                imm = 0
            }
            (imm << 20) | (rs1 << 15) | (FUNCT3[symbol - symbols::ADD] << 12) | (rd << 7) | 0b0000011
        } else if symbol == symbols::LA {
            let rd = self.unwrap_register();
            // TODO: lookup global/rewrite
            let path = match self.next() {
                Some(Token::Symbol(s)) => vec![s],
                Some(Token::LParen) => self.unwrap_path(),
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
            let (p, constant) = match self.follow_path(&path) {
                Some(Unit::Bytes(p, _, c)) => (p, c),
                Some(_) => {
                    eprintln!("Global not defined/imported at use.");
                    self.err = true;
                    (0, true)
                },
                None => {
                    eprintln!("Global not defined/imported at use.");
                    self.err = true;
                    (0, true)
                },
            };
            let i = self.get_mod().code.len();
            self.get_mod().rewrites.push((i, p, constant));
            // lui
            let i = (rd << 7) | 0b0110111;
            self.get_mod().code.push(i);
            // addi
            (rd << 15) | (rd << 7) | 0b0010011
        // S instructions
        } else if symbol <= symbols::LAST_S {
            let (rs1, mut imm) = self.unwrap_offset();
            if (imm as i32) >= 2048 || (imm as i32) < -2048 {
                eprintln!("Offset `{}` out of range [-2048, 2048)", imm as i32);
                self.err = true;
                imm = 0
            }
            let rd = self.unwrap_register();
            ((imm >> 5) << 25) | (rd << 20) | (rs1 << 15) | (FUNCT3[symbol - symbols::ADD] << 12) | ((imm & 0b11111) << 7) | 0b0100011

        // B instructions
        } else if symbol <= symbols::LAST_B {
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
            let mut imm = self.unwrap_imm();
            if imm > 0xF_FF_FF {
                eprintln!("Immediate `{}` out of range [-2048, 2048)", imm as i32);
                self.err = true;
                imm = 0
            }
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
                eprintln!("Expected register in instruction, got identifier `{}`", get_value(s));
                self.err = true;
                0
            },
            Some(Token::RParen) | Some(Token::LParen) => {
                self.position -= 1;
                eprintln!("Expected register in instruction in file `{}`", self.filename);
                self.err = true;
                0
            }
            Some(_) => {
                // TODO
                eprintln!("Expected register in instruction in file `{}`", self.filename);
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
                    Some(Token::Symbol(_)) => {
                        self.position -= 1;
                        let path = self.unwrap_path();
                        return match self.follow_path(&path) {
                            Some(Unit::Constant(i)) => i as i32 as u32,
                            Some(_) => {
                                eprintln!("Variable must be a constant.");
                                self.err = true;
                                0
                            }
                            None => {
                                eprintln!("Global value has not been defined.");
                                self.err = true;
                                0
                            }
                        };
                    }
                    Some(Token::RParen) => {
                        eprintln!("Expected path.");
                        self.err = true;
                        return 0;
                    }
                    Some(_) => {
                        // TODO
                        eprintln!("Expected path.");
                        self.err = true;
                        return 0;
                    }
                    None => {
                        // TODO
                        eprintln!("Unexpected EOF.");
                        self.err = true;
                        return 0;
                    }
                }
                // len
                let len = match self.next() {
                    Some(Token::Symbol(s)) => {
                        match self.follow_path(&[s]) {
                            Some(Unit::Bytes(_, l, _)) => l as u32,
                            Some(_) => {
                                eprintln!("Global value `{}` has not been defined.", get_value(s));
                                self.err = true;
                                0
                            }
                            None => {
                                eprintln!("Global value `{}` has not been defined.", get_value(s));
                                self.err = true;
                                0
                            }
                        }
                    },
                    Some(Token::LParen) => {
                        let path = self.unwrap_path();
                        match self.follow_path(&path) {
                            Some(Unit::Bytes(_, l, _)) => l as u32,
                            Some(_) => {
                                eprintln!("Global value has not been defined.");
                                self.err = true;
                                0
                            }
                            None => {
                                eprintln!("Global value has not been defined.");
                                self.err = true;
                                0
                            }
                        }
                    }
                    Some(_) => {
                        // TODO
                        self.err = true;
                        0
                    }
                    None => {
                        // TODO
                        eprintln!("Unexpected EOF.");
                        self.err = true;
                        0
                    }
                };

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
                len
            }
            Some(Token::Symbol(s)) => {
                match self.follow_path(&[s]) {
                    Some(Unit::Constant(i)) => i as i32 as u32,
                    Some(_) => {
                        eprintln!("Variable must be a constant.");
                        self.err = true;
                        0
                    }
                    None => {
                        eprintln!("Unknown variable in file `{}`.", self.filename);
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
                self.next();
                (self.unwrap_imm(), s as u32 - 1)
            } else {
                (self.unwrap_imm(), self.unwrap_register())
            },
            Some(Token::Integer(_)) | Some(Token::LParen) => {
                (self.unwrap_imm(), self.unwrap_register())
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
                let path = self.unwrap_path();
                if path.is_empty() {
                    return None;
                }
                let p = self.get_mod().code.len() * 4;
                self.get_mod().refs.push((path, p, if branchp { JumpType::Branch } else { JumpType::Jump }));
                return None;
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

    fn unwrap_path(&mut self) -> Vec<Symbol> {
        let mut path = Vec::new();
        loop {
            match self.next() {
                Some(Token::RParen) => return path,
                Some(Token::Symbol(s)) => path.push(s),
                Some(_) => {
                    eprintln!("Unexpected token in path");
                    self.err = true;
                    self.skip_opcode();
                    return Vec::new();
                }
                None => {
                    eprintln!("Unexpected EOF");
                    self.err = true;
                    return Vec::new();
                }
            }
        }
    }

    fn follow_path(&mut self, path: &[Symbol]) -> Option<Unit> {
        if path.len() == 1 {
            let mut m = &self.modules[self.module];
            loop {
                if let Some((_, u)) = m.children.get(&path[0]) {
                    return Some(*u);
                } else if m.filep {
                    return None;
                } else {
                    // shitty bc
                    let p = m.parent.unwrap();
                    m = &self.modules[p];
                }
            }
        }

        let mut m = &self.modules[self.module];
        let mut i = 0;
        while i < path.len() {
            let p = path[i];
            if p == symbols::CARAT {
                i += 1;
                let p = if let Some(p) = m.parent {
                    p
                } else {
                    eprintln!("Path jumped past root.");
                    self.err = true;
                    return None;
                };
                m = &self.modules[p];
            } else {
                break;
            }
        }

        while i < path.len() {
            let p = path[i];
            i += 1;
            if p == symbols::CARAT {
                eprintln!("Bad path, can only move up (`^`) at beginning of path.");
                self.err = true;
                return None;
            }
            match m.children.get(&p) {
                v @ Some((_, Unit::Bytes(_, _, _))) | v @ Some((_, Unit::Constant(_))) => {
                    if i == path.len() {
                        return Some(v.unwrap().1);
                    } else {
                        return None;
                    }
                }
                Some((_, Unit::Module(id))) => m = &self.modules[*id],
                None => {
                    if m.filep {
                        self.err = true;
                        return None;
                    } else {
                        let p = m.parent.unwrap();
                        m = &self.modules[p];
                        i -= 1;
                    }
                }
            }
        }
        Some(Unit::Module(m.id))
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
