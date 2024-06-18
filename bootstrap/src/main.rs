mod elf;
mod symbols;
mod tokenizer;

use tokenizer::{get_symbol, get_value, Token, Tokenizer};

use std::{env, mem};
use std::collections::HashMap;
use std::fs::{self, OpenOptions};
use std::io::ErrorKind as IOError;
use std::os::unix::fs::OpenOptionsExt;
use std::path::PathBuf;
use std::process::exit;

fn print_usage(prog: &str) {
    eprintln!("USAGE: {} <INPUT> [OUTPUT]", prog);
    eprintln!("    -d: Include debug info.");
}

fn main() {
    let mut args = env::args();
    let prog = args.next().unwrap();
    let mut input_file = None;
    let mut output_file = None;
    let mut debug = false;
    let mut usage = false;
    for arg in args {
        if arg == "-d" {
            debug = true;
        } else if arg.starts_with("-") {
            eprintln!("Unknown flag `{}`.", arg);
            usage = true;
        } else if input_file.is_none() {
            input_file = Some(arg);
        } else if output_file.is_none() {
            output_file = Some(arg);
        } else {
            eprintln!("Too many arguments.");
            print_usage(&prog);
            exit(1);
        }
    }
    let input_file = if let Some(arg) = input_file {
        arg
    } else {
        print_usage(&prog);
        exit(1);
    };
    let output_file = if let Some(arg) = output_file {
        arg
    } else {
        "bin.elf".to_string()
    };

    if usage {
        print_usage(&prog);
        eprintln!();
    }

    symbols::init();
    if (std::path::Path::new(&input_file)).is_dir() {
            eprintln!("Error: Expected file, got directory: {}", input_file);
            exit(1);
    }
    let input = match read_file(&input_file) {
        Ok(i) => i,
        Err(e) => {
            eprintln!("Error: {}", e);
            exit(1);
        }
    };
    let tokenizer = Tokenizer::new(input, input_file);
    let (program, data, rodata, rewrites) = assemble(tokenizer);
    let e = if debug {
        elf::Elf::new_debug(program, data, rodata, rewrites)
    } else {
        elf::Elf::new(program, data, rodata, rewrites)
    };
    let f = match OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .mode(0o777)
        .open(&output_file)
    {
        Ok(f) => f,
        Err(e) => {
            match e.kind() {
                IOError::PermissionDenied => eprintln!("Permission denied trying to open file `{}`", output_file),
                IOError::OutOfMemory => eprintln!("Not enough memory to write file `{}`", output_file),
                _ => eprintln!("Error writing file `{}`: {}", output_file, e),
            }
            exit(1);
        }
    };

    if let Err(e) = e.write(f) {
        match e.kind() {
            IOError::PermissionDenied => eprintln!("Permission denied trying to open file `{}`", output_file),
            IOError::OutOfMemory => eprintln!("Not enough memory to write file `{}`", output_file),
            _ => eprintln!("Error writing file `{}`: {}", output_file, e),
        }
        exit(1);
    }
}

fn read_file<P: AsRef<std::path::Path>>(path: P) -> Result<Vec<u8>, String> {
    let path = path.as_ref();
    match fs::read(path) {
        Ok(v) => Ok(v),
        Err(e) => Err(match e.kind() {
            IOError::NotFound => {
                format!("No such file `{}`", path.display())
            }
            IOError::PermissionDenied => {
                format!("Permission denied trying to open file `{}`", path.display())
            }
            IOError::OutOfMemory => {
                format!("Not enough memory to read file `{}`", path.display())
            }
            _ => {
                format!("Error reading file `{}`: {}", path.display(), e)
            }
        })
    }
}

fn assemble(tokenizer: Tokenizer) -> (Vec<u32>, Vec<u8>, Vec<u8>, Vec<(usize, usize, bool)>) {
    let mut root = Module::new(0, None, true);
    let mut path: PathBuf = tokenizer.filename.clone().into();
    path.pop();
    root.path = path;
    let name = get_symbol(tokenizer.filename.rsplitn(2, '.').last().unwrap().as_bytes().to_vec());
    root.children.insert(name, (false, Unit::Module(1)));

    let mut asm = Asm {
        tokenizer: tokenizer,
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
        asm.tokenizer.token_position += 1;
        asm.print_err("Unexpected closing parenthesis", "");
    }
    let (code, data, rodata, rewrites, err) = asm.finish();
    if err {
        exit(1);
    }
    (code, data, rodata, rewrites)
}


struct Asm {
    tokenizer: Tokenizer,
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

    pub fn finish(&mut self) -> (Vec<(Symbol, usize, JumpType)>, bool) {
        let mut jumps = Vec::new();
        let mut err = false;
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
                    if imm < -4096 || imm > 4095 {
                        eprintln!("Branch to `{}` too far.", get_value(*label));
                        err = true;
                    }
                    let imm = imm as u32;
                    let imm1 = (imm & 0x1e) | ((imm >> 11) & 1);
                    let imm2 = ((imm & 0x10_00) >> 6) | ((imm >> 5) & 0x3F);
                    self.code[i/4] |= (imm2 << 25) | (imm1 << 7);
                }
                JumpType::Jump => {
                    if imm < -1048576 || imm > 1048575 {
                        eprintln!("Jump to `{}` too far.", get_value(*label));
                        err = true;
                    }
                    let imm = imm as u32;
                    let imm2 = ((imm & 0x10_00_00) >> 1) | (((imm >> 1) & 0x3_FF) << 9) | (((imm >> 11) & 1) << 8) | ((imm >> 12) & 0xFF);
                    self.code[i/4] |= imm2 << 12;
                }
            }
        }
        (jumps, err)
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
    // position in data, len, constantp
    Bytes(usize, usize, bool),
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
                    Some(Token::RParen) => self.print_err("Empty expression ending", ""),
                    Some(_) => {
                        self.print_err("Unexpected value in expression", "");
                        self.skip_opcode();
                    }
                    None => self.print_err("Unexpected opening parenthesis", ""),
                },
                Token::RParen => {
                    self.backtrack();
                    return;
                }
                _ => self.print_err("Expression must be a label or an S-Expression", ""),
            }
        }
    }

    fn finish(mut self) -> (Vec<u32>, Vec<u8>, Vec<u8>, Vec<(usize, usize, bool)>, bool) {
        let mut code = Vec::new();
        let mut rewrites = Vec::new();
        let mut refs = Vec::new();
        for module in &mut self.modules[1..] {
            let (j, err) = module.finish();
            self.tokenizer.err |= err;
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
                    eprintln!("Expected module at path `{}`.", self.path_to_string(&path));
                    self.tokenizer.err = true;
                    continue;
                }
                None => {
                    eprintln!("Unknown module/label `{}`.", self.path_to_string(&path));
                    self.tokenizer.err = true;
                    continue;
                }
            };

            let imm = (p as isize - i as isize) as i32;
            match ty {
                JumpType::Branch => {
                    if imm < -4096 || imm > 4095 {
                        eprintln!("Branch to `{}` too far.", self.path_to_string(&path));
                        self.tokenizer.err = true;
                    }
                    let imm = imm as u32;
                    let imm1 = (imm & 0x1e) | ((imm >> 11) & 1);
                    let imm2 = ((imm & 0x10_00) >> 6) | ((imm >> 5) & 0x3F);
                    code[i/4] |= (imm2 << 25) | (imm1 << 7);
                }
                JumpType::Jump => {
                    if imm < -1048576 || imm > 1048575 {
                        eprintln!("Jump to `{}` too far.", self.path_to_string(&path));
                        self.tokenizer.err = true;
                    }
                    let imm = imm as u32;
                    let imm2 = ((imm & 0x10_00_00) >> 1) | (((imm >> 1) & 0x3_FF) << 9) | (((imm >> 11) & 1) << 8) | ((imm >> 12) & 0xFF);
                    code[i/4] |= imm2 << 12;
                }
            }
        }

        let err = self.tokenizer.err || code.len() == 0;
        if code.len() == 0 {
            eprintln!("Error: No instructions found, cannot build executable.");
        }
        let Asm { data, rodata, .. } = self;
        (code, data, rodata, rewrites, err)
    }

    fn add_label(&mut self, label: usize) {
        if self.get_mod().labels.contains_key(&label) {
            self.print_err(&format!("Duplicate label `{}`", get_value(label)), "");
        //} else if self.in_scope(label) {
        } else if self.get_mod().children.contains_key(&label) {
            self.print_err(&format!("Label `{}` conflicts with module/definition", get_value(label)), "");
        } else {
            let m = self.get_mod();
            m.labels.insert(label, m.code.len() * 4);
        }
    }

    fn handle_import(&mut self) {
        let path = match self.next() {
            Some(Token::Symbol(_)) | Some(Token::LParen) => self.unwrap_path(),
            Some(Token::String(_, _)) => {
                self.print_err("Import path should consist of identifiers, not strings,", "");
                return self.skip_opcode();
            }
            Some(Token::RParen) => return self.print_err("Empty import statement", ""),
            Some(_) => {
                self.print_err("Bad argument in import statement", "");
                return self.skip_opcode();
            }
            None => return self.print_err("Unexpected EOF in import statement", ""),
        };
        match self.next() {
            Some(Token::RParen) => (),
            Some(t) => {
                self.print_err("Forgot closing parenthesis in import statement",
                               "\tNote: each import requires its own import statement.");
                if t == Token::LParen {
                    self.backtrack();
                }
                self.skip_opcode();
            }
            None => self.print_err("Unexpected EOF in import statement", ""),
        }
        if path.is_empty() {
            self.print_err("Empty path in import statement", "");
            return;
        } else if path.len() == 1 && (path[0] == symbols::CARAT || path[0] == symbols::STAR) {
            self.print_err("Invalid path in import statement", "");
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
                    self.print_err(&format!("Path `{}` jumped past root in import statement", self.path_to_string(&path)), "");
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
                self.print_err(&format!("Invalid path `{}` in import statement", self.path_to_string(&path)),
                               "\tNote: can only move up (`^`) at beginning of path in expression.");
                return;
            } else if p == symbols::STAR && i != path.len() {
                self.print_err(&format!("Invalid path `{}` in import statement", self.path_to_string(&path)),
                               "\tNote: can only import all (`*`) at the end of path in expression.");
                return;
            } else if p == symbols::STAR {
                for (k, (importp, v)) in m.children.clone() {
                    if self.in_scope(k) {
                        self.print_err(&format!("`{}` is already defined in this scope from import statement", get_value(k)), "");
                    } else if !importp {
                        self.get_mod().children.insert(k, (true, v));
                    }
                }
                return;
            }

            match m.children.get(&p).copied() {
                v @ Some((_, Unit::Bytes(_, _, _))) | v @ Some((_, Unit::Constant(_))) => {
                    let (importp, v) = v.unwrap();
                    if importp {
                        self.print_err("Cannot import an import from import statement", "");
                    } else if i == path.len() {
                        if self.in_scope(p) {
                            self.print_err(&format!("`{}` is already defined in this scope from import statement", get_value(p)), "");
                        } else {
                            self.get_mod().children.insert(p, (true, v));
                        }
                    } else {
                        self.print_err(&format!("Path `{}` resolved to variable/global partway in import statement", self.path_to_string(&path)), "");
                    }
                    return;
                }
                Some((importp, Unit::Module(id))) if !importp => if i == path.len() {
                    if id == self.get_mod().id {
                        self.print_err("Cannot import self", "");
                    } else if self.in_scope(p) {
                        self.print_err(&format!("`{}` is already defined in this scope in import statement", get_value(p)), "");
                    } else {
                        self.get_mod().children.insert(p, (true, Unit::Module(id)));
                    }
                    return;
                } else {
                    if id == self.get_mod().id {
                        self.print_err("Cannot import self", "");
                        return;
                    }
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
                    let ptr = m2.parent.unwrap_or(0);
                    let mut file_path = self.modules[ptr].path.clone();
                    file_path.push(get_value(p));
                    if file_path.is_dir() {
                        if let Some(m_id) = self.import_dir(file_path, ptr) {
                            if i == path.len() {
                                self.get_mod().children.insert(p, (true, Unit::Module(m_id)));
                            }
                            m = &self.modules[m_id];
                        } else {
                            self.tokenizer.err = true;
                            return;
                        }
                    } else if { file_path.set_extension("sasm"); !file_path.is_file() } {
                        if existp {
                            self.print_err("Unnecessary import statement", "");
                            return;
                        } else {
                            self.print_err(&format!("File `{}` does not exist in import statement", file_path.display()), "");
                            return;
                        }
                    } else {
                        if let Some(m_id) = self.import_file(p, file_path, ptr) {
                            if i == path.len() {
                                self.get_mod().children.insert(p, (true, Unit::Module(m_id)));
                            }
                            m = &self.modules[m_id];
                        } else {
                            self.tokenizer.err = true;
                            return;
                        }
                    }
                }
            }
        }
    }

    fn import_dir(&mut self, file_path: PathBuf, parent: usize) -> Option<usize> {
        if let Some(id) = self.import_files.get(&file_path).copied() {
            return Some(id);
        }

        let m_id = self.modules.len();
        self.import_files.insert(file_path.clone(), m_id);
        // TODO: should filep be set?
        let mut module = Module::new(m_id, Some(parent), false);
        module.path = file_path.clone();
        self.modules.push(module);
        let old_id = self.module;
        self.module = m_id;

        let dir = match fs::read_dir(&file_path) {
            Ok(d) => d,
            Err(e) => {
                let e = match e.kind() {
                    IOError::PermissionDenied => {
                        format!("Permission denied trying to open directory `{}`", file_path.display())
                    }
                    IOError::OutOfMemory => {
                        format!("Not enough memory to read directory `{}`", file_path.display())
                    }
                    _ => {
                        format!("Error reading directory `{}`: {}", file_path.display(), e)
                    }
                };
                self.print_err("Error executing import", &format!("\t{}", e));
                return Some(m_id);
            }
        };
        for f in dir {
            let f = f.unwrap();
            if f.file_type().unwrap().is_dir() {
                let f = f.path();
                let mut fp = file_path.clone();
                fp.push(f.file_name().unwrap());
                let path = get_symbol(f.file_stem().unwrap().as_encoded_bytes().to_vec());
                if let Some(m) = self.import_dir(fp, m_id) {
                    self.get_mod().children.insert(path, (true, Unit::Module(m)));
                }
            } else {
                let f = f.path();
                if Some("sasm".as_ref()) != f.extension() {
                    continue;
                }
                let mut fp = file_path.clone();
                fp.push(f.file_name().unwrap());
                let path = get_symbol(f.file_stem().unwrap().as_encoded_bytes().to_vec());
                if let Some(m) = self.import_file(path, fp, m_id) {
                    self.get_mod().children.insert(path, (true, Unit::Module(m)));
                }
            }
        }

        self.module = old_id;
        Some(m_id)
    }

    fn import_file(&mut self, path: Symbol, file_path: PathBuf, parent: usize) -> Option<usize> {
        if let Some(id) = self.import_files.get(&file_path).copied() {
            return Some(id);
        }

        let m_id = self.modules.len();
        self.import_files.insert(file_path.clone(), m_id);
        let mut module = Module::new(m_id, Some(parent), true);
        module.labels.insert(path, 0);
        let old_id = self.module;
        self.module = m_id;
        self.modules.push(module);

        let include_input = match read_file(&file_path) {
            Ok(i) => i,
            Err(e) => {
                self.print_err("Error executing import", &format!("\t{}", e));
                self.module = old_id;
                return Some(m_id);
            }
        };

        let mut t = Tokenizer::new(include_input, file_path.display().to_string());
        mem::swap(&mut self.tokenizer, &mut t);
        self.assemble();
        t.err |= self.tokenizer.err;
        mem::swap(&mut self.tokenizer, &mut t);

        self.module = old_id;
        Some(m_id)
    }

    fn handle_module(&mut self) {
        let ident = match self.next() {
            Some(Token::Symbol(s)) => s,
            Some(Token::LParen) => {
                self.print_err("Invalid module declaration, expected identifier", "");
                self.backtrack();
                0
            }
            Some(Token::RParen) | None => return self.print_err("Unfinished module declaration", ""),
            Some(_) => {
                self.print_err("Invalid module declaration, expected identifier", "");
                0
            }
        };

        let m_id = self.modules.len();
        let mut module = Module::new(m_id, Some(self.module), false);
        module.labels.insert(ident, 0);
        if self.get_mod().children.contains_key(&ident) {
            self.print_err(&format!("Module `{}` conflicts with existing module/definition in scope", get_value(ident)), "");
        } else {
            self.get_mod().children.insert(ident, (false, Unit::Module(m_id)));
        }
        self.module = m_id;
        self.modules.push(module);
        self.assemble();
        if self.next() != Some(Token::RParen) {
            self.print_err(&format!("Unclosed module `{}`", get_value(ident)), "");
        }

        self.module = self.get_mod().parent.unwrap();
    }

    fn handle_include(&mut self) {
        let filename = match self.next() {
            Some(Token::String(s, e)) => match String::from_utf8(self.get_string(s, e)) {
                Ok(s) => s,
                Err(_) => {
                    self.print_err("Invalid filename", "");
                    self.skip_opcode();
                    return;
                }
            },
            Some(Token::RParen) =>return  self.print_err("Unexpected parenthesis", ""),
            Some(_) => {
                self.print_err("`include!` file must be a string", "");
                self.skip_opcode();
                return;
            },
            None => return self.print_err("Unexpected EOF in `include!`", ""),
        };

        if filename == self.tokenizer.filename {
            self.print_err("Recursive include", "");
            self.skip_opcode();
            return;
        }

        match self.next() {
            Some(Token::RParen) => (),
            Some(Token::String(_, _)) => {
                self.print_err("Each file needs its own include statement", "");
                self.skip_opcode();
            },
            Some(_) => {
                self.print_err("Unexpected expression in include statement", "");
                self.skip_opcode();
            }
            None => self.print_err("Unexpected EOF in `include!`", ""),
        }
        let include_input = match read_file(&filename) {
            Ok(i) => i,
            Err(e) => return self.print_err("Error executing `include!`", &format!("\t{}", e)),
        };

        let mut t = Tokenizer::new(include_input, filename);
        mem::swap(&mut self.tokenizer, &mut t);
        self.assemble();
        t.err |= self.tokenizer.err;
        mem::swap(&mut self.tokenizer, &mut t);
    }

    fn handle_define(&mut self) {
        let ident = if let Some(i) = self.unwrap_ident() {
            i
        } else {
            return;
        };

        match self.next() {
            Some(Token::Integer(i)) => if self.in_scope(ident) {
                self.print_err(&format!("Definition `{}` conflicts with existing module/definition in scope", get_value(ident)), "");
            } else {
                self.get_mod().children.insert(ident, (false, Unit::Constant(i)));
            },
            Some(Token::Char(c)) => if self.in_scope(ident) {
                self.print_err(&format!("Definition `{}` conflicts with existing module/definition in scope", get_value(ident)), "");
            } else {
                self.get_mod().children.insert(ident, (false, Unit::Constant(c as i64)));
            },
            Some(Token::Pound) | Some(Token::String(_, _)) => {
                self.print_err(&format!("Definition `{}` must be a constant", get_value(ident)),
                               "\tNote: Define is only for build-time constants, did you mean `defcon`/`defvar`?");
            }
            Some(Token::RParen) => return self.print_err(&format!("Definition `{}` must have a value", get_value(ident)), ""),
            Some(_) => self.print_err(&format!("Definition `{}` must be a constant", get_value(ident)), ""),
            None => return self.print_err(&format!("Unexpected EOF in definition `{}` at end of file", get_value(ident)), ""),
        }

        if Some(Token::RParen) != self.next() {
            self.backtrack();
            let (line, _, _) = self.info();
            self.print_err("Unclosed definition", "");
            self.next();
            if self.same_line(line) {
                self.skip_opcode();
            } else {
                self.backtrack();
            }
        }
    }

    fn handle_defcon_var(&mut self, constant: bool) {
        let ident = if let Some(i) = self.unwrap_ident() {
            i
        } else {
            return;
        };

        match self.next() {
            Some(Token::Integer(i)) => if self.in_scope(ident) {
                self.print_err(&format!("Definition `{}` conflicts with existing module/definition in scope", get_value(ident)), "");
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
            Some(Token::Char(c)) => if self.in_scope(ident) {
                self.print_err(&format!("Definition `{}` conflicts with existing module/definition in scope", get_value(ident)), "");
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
                    self.print_warn("Definition of empty array literal");
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
                if self.in_scope(ident) {
                    self.print_err(&format!("Definition `{}` conflicts with existing module/definition in scope", get_value(ident)), "");
                } else {
                    self.get_mod().children.insert(ident, (false, Unit::Bytes(start, len, constant)));
                }
            }
            Some(Token::String(start, end)) => {
                let mut s = self.get_string(start, end);
                if s.is_empty() {
                    self.print_warn("Definition of empty string literal");
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
                if self.in_scope(ident) {
                    self.print_err(&format!("Definition `{}` conflicts with existing module/definition in scope", get_value(ident)), "");
                } else {
                    self.get_mod().children.insert(ident, (false, Unit::Bytes(start, len, constant)));
                }
            }
            Some(Token::RParen) => return self.print_err(&format!("Definition `{}` must have a value", get_value(ident)), ""),
            Some(_) => self.print_err(&format!("Definition `{}` must be a constant", get_value(ident)), ""),
            None => return self.print_err(&format!("Unexpected EOF in definition `{}` at end of file", get_value(ident)), ""),
        }

        if Some(Token::RParen) != self.next() {
            self.backtrack();
            let (line, _, _) = self.info();
            self.print_err("Unclosed definition", "");
            self.next();
            if self.same_line(line) {
                self.skip_opcode();
            } else {
                self.backtrack();
            }
        }
    }

    fn handle_opcode(&mut self, symbol: usize) {
        const FUNCT3: [u32; 40] = [0, 0, 4, 6, 7, 1, 5, 5, 2, 3, 0, 4, 6,   // r
                                   0, 0, 4, 6, 7, 1, 5, 5, 2, 3,            // i
                                   0, 1, 2, 3, 4, 5, 6,                     // i2
                                   0, 1, 2, 3,                              // s
                                   0, 1, 4, 5, 6, 7];                       // b
        let i = if symbol < symbols::ADD {
            self.print_err(&format!("Cannot use register `{}` as opcode", get_value(symbol)), "");
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
                imm = -imm;
            } else if symbol == symbols::SRAI {
                imm = imm | (0x20 << 5);
            }
            if imm >= 2048 || imm < -2048 {
                self.print_err(&format!("Immediate `{}` out of range [-2048, 2048)", imm), "");
                imm = 0
            }
            ((imm as i32 as u32) << 20) | (rs1 << 15) | (FUNCT3[symbol - symbols::ADD] << 12) | (rd << 7) | 0b0010011
        // I2 instructions
        } else if symbol <= symbols::LAST_I2 {
            let rd = self.unwrap_register();
            let (rs1, mut imm) = self.unwrap_offset();
            if imm >= 2048 || imm < -2048 {
                self.print_err(&format!("Offset `{}` out of range [-2048, 2048)", imm), "");
                imm = 0
            }
            ((imm as i32 as u32) << 20) | (rs1 << 15) | (FUNCT3[symbol - symbols::ADD] << 12) | (rd << 7) | 0b0000011
        } else if symbol == symbols::LA {
            let rd = self.unwrap_register();
            let path = match self.next() {
                Some(Token::Symbol(_)) | Some(Token::LParen) => self.unwrap_path(),
                Some(Token::RParen) => return self.print_err("Expected identifier in instruction", ""),
                Some(_) => {
                    self.print_err("Expected path as argument to `la` instruction", "");
                    return self.skip_opcode();
                }
                None => return self.print_err("Unexpected EOF in instruction", ""),
            };

            if path.len() == 1 && path[0] <= symbols::X31 {
                self.print_err("Expected path as argument to `la` instruction, got register", "");
                return self.skip_opcode();
            }

            let (p, constant) = match self.follow_path(&path) {
                Some(Unit::Bytes(p, _, c)) => (p, c),
                _ => {
                    self.print_err(&format!("Global `{}` not defined/imported at use", self.path_to_string(&path)), "");
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
            if imm >= 2048 || imm < -2048 {
                self.print_err(&format!("Offset `{}` out of range [-2048, 2048)", imm), "");
                imm = 0
            }
            let rd = self.unwrap_register();
            (((imm as i32 as u32) >> 5) << 25) | (rd << 20) | (rs1 << 15) | (FUNCT3[symbol - symbols::ADD] << 12) | (((imm as i32 as u32) & 0b11111) << 7) | 0b0100011

        // B instructions
        } else if symbol <= symbols::LAST_B {
            let rs1 = self.unwrap_register();
            let rs2 = self.unwrap_register();
            let mut i = (rs2 << 20) | (rs1 << 15) | (FUNCT3[symbol - symbols::ADD] << 12) | 0b1100011;
            if let Some((imm, label)) = self.unwrap_label(true) {
                if imm < -4096 || imm > 4095 {
                    self.print_err(&format!("Branch to `{}` too far", get_value(label)), "");
                }
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
            if let Some((imm, label)) = self.unwrap_label(false) {
                if imm < -1048576 || imm > 1048575 {
                    self.print_err(&format!("Jump to `{}` too far", get_value(label)), "");
                }
                let imm = imm as u32;
                let imm2 = ((imm & 0x10_00_00) >> 1) | (((imm >> 1) & 0x3_FF) << 9) | (((imm >> 11) & 1) << 8) | ((imm >> 12) & 0xFF);
                i = (imm2 << 12) | i;
            }
            i
        } else if symbol == symbols::JALR {
            let rd = self.unwrap_register();
            let (rs, imm) = self.unwrap_offset();
            ((imm as i32 as u32) << 20) | (rs << 15) | (rd << 7) | 0b1100111
        } else if symbol == symbols::LUI || symbol == symbols::AUIPC {
            let rd = self.unwrap_register();
            let mut imm = self.unwrap_imm();
            if imm >= 524288 || imm < -524288 {
                self.print_err(&format!("Immediate `{}` out of range [-524288, 524288)", imm), "");
                imm = 0
            }
            ((imm as i32 as u32) << 12) | (rd << 7) | if symbol == symbols::LUI { 0b0110111 } else { 0b0010111 }
        } else if symbol == symbols::ECALL {
            0b1110011
        } else if symbol == symbols::EBREAK {
            1 << 12 | 0b1110011
        } else {
            self.print_err(&format!("Unknown opcode `{}`", get_value(symbol)), "");
            return self.skip_opcode();
        };
        self.get_mod().code.push(i);

        match self.next() {
            Some(Token::RParen) => (),
            Some(Token::LParen) => {
                self.backtrack();
                self.print_err("Unclosed instruction", "");
            }
            Some(_) => {
                self.print_err("Too many arguments or missing parenthesis in instruction", "");
                self.skip_opcode();
            }
            None => self.print_err("Unclosed instruction at end of file", ""),
        }

    }

    fn skip_opcode(&mut self) {
        let (line, _, _) = self.info();
        while let Some(t) = self.next() {
            match t {
                Token::RParen => return,
                Token::LParen => if !self.same_line(line) {
                    self.backtrack();
                    self.print_err("Unclosed expression", "");
                    return;
                },
                _ => (),
            }
        }
        self.print_err("Unclosed expression at end of file", "");
    }

    fn unwrap_register(&mut self) -> u32 {
        match self.next() {
            Some(Token::Symbol(s)) => if s <= symbols::X31 {
                s as u32 - 1
            } else {
                self.print_err(&format!("Expected register in instruction, got identifier `{}`", get_value(s)), "");
                0
            },
            Some(Token::RParen) | Some(Token::LParen) => {
                self.backtrack();
                self.print_err("Expected register in instruction", "");
                0
            }
            Some(_) => {
                self.print_err("Expected register in instruction", "");
                0
            }
            None => {
                self.print_err("Expected register in instruction, got EOF", "");
                0
            }
        }
    }

    fn unwrap_imm(&mut self) -> i64 {
        match self.next() {
            Some(Token::LParen) => {
                match self.next() {
                    Some(Token::RParen) => {
                        self.print_err("Empty path in expression", "");
                        return 0;
                    }
                    Some(Token::Symbol(s)) if s == symbols::LEN => (),
                    _ => {
                        self.backtrack();
                        let path = self.unwrap_path();
                        if path.is_empty() {
                            return 0;
                        }
                        return match self.follow_path(&path) {
                            Some(Unit::Constant(i)) => i,
                            Some(_) => {
                                self.print_err(&format!("Variable `{}` must be a constant", self.path_to_string(&path)), "");
                                0
                            }
                            None => {
                                self.print_err(&format!("Variable `{}` not defined/imported at use", self.path_to_string(&path)), "");
                                0
                            }
                        };
                    }
                }
                // len
                let len = match self.next() {
                    Some(Token::Symbol(_)) | Some(Token::LParen) => {
                        let path = self.unwrap_path();
                        match self.follow_path(&path) {
                            Some(Unit::Bytes(_, l, _)) => l as i64,
                            Some(_) | None => {
                                self.print_err(&format!("Global `{}` not defined/imported at use", self.path_to_string(&path)), "");
                                0
                            }
                        }
                    },
                    Some(Token::String(start, end)) => {
                        let s = self.get_string(start, end);
                        s.len() as i64
                    }
                    Some(Token::Pound) => {
                        if let Some(v) = self.unwrap_array() {
                            v.len() as i64
                        } else {
                            0
                        }
                    }
                    Some(_) => {
                        self.print_err("Expected path as argument in `len` expression", "");
                        0
                    }
                    None => 0,
                };

                match self.next() {
                    Some(Token::RParen) => (),
                    Some(t) => {
                        if Token::LParen == t {
                            self.backtrack();
                        }
                        self.print_err("Too many arguments or missing parenthesis in `len` expression", "");
                        if Token::LParen != t {
                            self.skip_opcode();
                        }
                    }
                    None => self.print_err("Unexpected EOF in `len` expression", ""),
                }
                len
            }
            Some(Token::Symbol(s)) => {
                match self.follow_path(&[s]) {
                    Some(Unit::Constant(i)) => i,
                    Some(_) => {
                        self.print_err(&format!("Variable `{}` must be a constant in expression", get_value(s)), "");
                        0
                    }
                    None => {
                        self.print_err(&format!("Unknown variable `{}` in expression", get_value(s)), "");
                        0
                    }
                }
            }
            Some(Token::Char(c)) => c as i64,
            Some(Token::Integer(i)) => i,
            Some(Token::RParen) => {
                self.print_err("Expected immediate in expression", "");
                self.backtrack();
                0
            }
            Some(_) => {
                self.print_err("Expected immediate in expression", "");
                0
            }
            None => {
                self.print_err("Unexpected EOF in expression", "");
                0
            }
        }
    }

    fn unwrap_offset(&mut self) -> (u32, i64) {
        match self.next() {
            Some(Token::Symbol(_)) => {
                self.backtrack();
                return (self.unwrap_register(), 0);
            }
            Some(Token::LParen) => (),
            Some(t) => {
                self.print_err("Expected register/offset in expression", "");
                if t == Token::RParen {
                    self.backtrack();
                }
                return (0, 0);
            }
            None => {
                self.print_err("Unexpected EOF in expression", "");
                return (0, 0);
            }
        }
        let neg = match self.next() {
            Some(Token::Symbol(s)) => if s == symbols::PLUS {
                false
            } else if s == symbols::NEG {
                true
            } else {
                self.print_err(&format!("Expected `+`/`-`, got `{}` in expression offset", get_value(s)), "");
                self.skip_opcode();
                return (0, 0);
            },
            Some(Token::RParen) => {
                self.print_err("Empty offset in expression", "");
                return (0, 0);
            }
            Some(_) => {
                self.print_err("Expected `+`/`-` in expression offset", "");
                self.skip_opcode();
                return (0, 0);
            }
            None => {
                self.print_err("Unexpected EOF in expression offset", "");
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
            Some(Token::Integer(_)) | Some(Token::Char(_)) | Some(Token::LParen) => {
                (self.unwrap_imm(), self.unwrap_register())
            }
            Some(Token::RParen) => {
                self.print_err("Expected register and immediate in expression offset", "");
                self.next();
                return (0, 0);
            }
            Some(_) => {
                self.print_err("Expected register and immediate in expression offset", "");
                self.skip_opcode();
                return (0, 0)
            }
            None => {
                self.print_err("Unexpected EOF in expression offset", "");
                return (0, 0);
            }
        };

        if self.next() != Some(Token::RParen) {
            self.backtrack();
            self.print_err("Expected closing parenthesis in expression offset", "");
            self.skip_opcode();
        }
        if neg {
            imm = -imm;
        }
        (reg, imm)
    }

    fn unwrap_label(&mut self, branchp: bool) -> Option<(i32, Symbol)> {
        let sym = match self.next() {
            Some(Token::Symbol(s)) => s,
            Some(Token::LParen) => {
                let path = self.unwrap_path();
                if path.is_empty() {
                    self.print_err("Expected non-empty path in instruction", "");
                    return None;
                }
                let p = self.get_mod().code.len() * 4;
                self.get_mod().refs.push((path, p, if branchp { JumpType::Branch } else { JumpType::Jump }));
                return None;
            },
            Some(Token::RParen) => {
                self.print_err("Expected label in instruction", "");
                self.backtrack();
                return None;
            },
            Some(_) => {
                self.print_err("Expected label in instruction", "");
                return None;
            },
            None => {
                self.print_err("Expected label in instruction, got EOF", "");
                return None;
            }
        };
        let p = self.get_mod().code.len() * 4;
        match self.get_mod().labels.get(&sym) {
            Some(s) => Some(((*s as isize - p as isize) as i32, sym)),
            None => {
                self.get_mod().jumps.push((sym, p, if branchp { JumpType::Branch } else { JumpType::Jump }));
                None
            }
        }
    }

    fn unwrap_path(&mut self) -> Vec<Symbol> {
        let mut path = Vec::new();
        self.backtrack();
        if let Some(Token::Symbol(s)) = self.next() {
            path.push(s);
            return path;
        }

        loop {
            match self.next() {
                Some(Token::RParen) => break,
                Some(Token::Symbol(s)) => path.push(s),
                Some(_) => {
                    self.print_err("Unexpected token in path", "");
                    self.skip_opcode();
                    return Vec::new();
                }
                None => {
                    self.print_err("Unexpected EOF in path", "");
                    return Vec::new();
                }
            }
        }
        if path.len() == 1 {
            self.print_warn("Unnecessary parenthesis in path");
        }
        path
    }

    fn unwrap_ident(&mut self) -> Option<Symbol> {
        match self.next() {
            Some(Token::Symbol(s)) => Some(s),
            Some(Token::RParen) => {
                self.print_err("Definition must have an identifier and value", "");
                None
            }
            Some(_) => {
                self.print_err("Expected identifier in definition", "");
                None
            }
            None => {
                self.print_err("Unexpected EOF in definition", "");
                None
            }
        }
    }

    fn unwrap_array(&mut self) -> Option<Vec<u8>> {
        match self.next() {
            Some(Token::LParen) => (),
            Some(Token::RParen) => {
                self.print_err("Unexpected `#` in definition, definition incomplete", "");
                return None;
            }
            Some(_) => {
                self.print_err("Unexpected `#` in definition", "");
                self.skip_opcode();
                return None;
            }
            None => {
                self.print_err("Unexpected `#` at end of file", "");
                return None;
            }
        }
        let mut v = Vec::new();
        loop {
            match self.next() {
                Some(Token::RParen) => break,
                Some(Token::Integer(i)) => {
                    if i < 0 || i >= 256 {
                        self.print_err(&format!("Array literals must consist of u8 integers, `{}` is out of range", i), "");
                    }
                    v.push(i as u8);
                }
                Some(Token::Char(c)) => {
                    v.push(c);
                }
                Some(Token::Symbol(_)) => self.print_err("Array literals must consist of u8 integers, cannot use variables", ""),
                Some(_) => self.print_err("Array literals must consist of u8 integers", ""),
                None => {
                    self.print_err("Unexpected EOF in array literal", "");
                    return None;
                }
            }
        }
        Some(v)
    }

    fn follow_path(&mut self, path: &[Symbol]) -> Option<Unit> {
        if path.len() == 1 && (path[0] == symbols::CARAT || path[0] == symbols::STAR) {
            self.print_err("Path cannot consist of just `^`/`*`", "");
            return None;
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
                    self.print_err("Path jumped past root in expression", "");
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
                self.print_err("Bad path, can only move up (`^`) at beginning of path in expression", "");
                return None;
            } else if p == symbols::STAR {
                self.print_err("Bad path, only import statements can use `*` in expression", "");
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
                        self.tokenizer.err = true;
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

    fn in_scope(&self, ident: Symbol) -> bool {
        let mut m = &self.modules[self.module];
        loop {
            if m.children.get(&ident).is_some() {
                return true;
            } else if m.filep {
                return false;
            } else {
                // shitty bc
                let p = m.parent.unwrap();
                m = &self.modules[p];
            }
        }
    }

    fn path_to_string(&self, path: &[Symbol]) -> String {
        if path.len() == 1 {
            return get_value(path[0]);
        }
        let mut out = String::from('(');
        for p in path {
            out.push_str(&get_value(*p));
            out.push(' ');
        }
        out.pop();
        out.push(')');
        out
    }

    fn get_string(&mut self, start: usize, end: usize) -> Vec<u8> {
        let mut v = Vec::with_capacity(end-start);
        let mut i = start;
        while i < end {
            if self.tokenizer.input[i] == b'\\' {
                i += 1;
                match self.tokenizer.input[i] {
                    b'r' => v.push(b'\r'),
                    b'n' => v.push(b'\n'),
                    b't' => v.push(b'\t'),
                    b'\\' => v.push(b'\\'),
                    b'"' => v.push(b'"'),
                    b'0' => v.push(b'\0'),
                    // TODO
                    c @ _ => panic!("Invariant broken in Asm::get_string `{:?}`", c),
                }
            } else {
                v.push(self.tokenizer.input[i]);
            }
            i += 1;
        }
        v
    }

    fn print_warn(&mut self, msg: &str) {
        let err = self.tokenizer.err;
        self.print_err(msg, "");
        self.tokenizer.err = err;
    }

    fn print_err(&mut self, msg: &str, note: &str) {
        let t = self.tokenizer.token_info[self.tokenizer.token_position-1];
        self.tokenizer.print_err(t.start, t.line, msg, note)
    }

    fn info(&self) -> (usize, usize, String) {
        let t = self.tokenizer.token_info[self.tokenizer.token_position-1];
        (t.line, self.tokenizer.idx_in_line(t.start), self.tokenizer.filename.clone())
    }

    fn same_line(&self, start: usize) -> bool {
        self.tokenizer.token_info[self.tokenizer.token_position-1].line == self.tokenizer.token_info[start].line
    }

    fn get_mod(&mut self) -> &mut Module {
        &mut self.modules[self.module]
    }

    fn next(&mut self) -> Option<Token> {
        self.tokenizer.next()
    }

    fn peek(&mut self) -> Option<Token> {
        self.tokenizer.peek()
    }

    fn backtrack(&mut self) {
        self.tokenizer.token_position -= 1;
    }
}
