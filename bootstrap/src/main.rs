mod elf;
mod scm;
mod symbols;
mod tokenizer;
mod value;

use tokenizer::{get_symbol, get_value, Token, Tokenizer};
use value::Value;
use value::heap_repr::LambdaE;

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
        } else if arg == "-V" {
            eprintln!("S-Assembler v2.0 - The Arcadian State (Rust version)");
            eprintln!("Written by Hunter Praska");
            exit(0);
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
    /*
    {
    let symbols = tokenizer::INTERNER.lock().unwrap();
    let mut total_symbols = 0;
    let mut small8 = 0;
    let mut small16 = 0;
    let mut size = 0;
    for (k, _) in symbols.iter() {
        total_symbols += 1;
        if k.len() <= 8 {
            small8 += 1;
            size += 16;
        } else if k.len() <= 11 {
            small16 += 1;
            size += 24;
        }
    }
    println!("{} total\n{} small8\n{} small16\n {} bytes saved", total_symbols, small8, small16, size);
    }
    */
    if (std::path::Path::new(&input_file)).is_dir() {
            eprintln!("Error: Expected file, got directory `{}`.", input_file);
            exit(1);
    }
    let input = match read_file(&input_file) {
        Ok(i) => i,
        Err(e) => {
            eprintln!("Error: {}.", e);
            exit(1);
        }
    };
    let tokenizer = Box::new(Tokenizer::new(input, input_file));
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
    /*
    let symbols = tokenizer::INTERNER.lock().unwrap();
    let mut total_symbols = 0;
    let mut small8 = 0;
    let mut small16 = 0;
    let mut size = 0;
    for (k, _) in symbols.iter() {
        total_symbols += 1;
        if k.len() <= 8 {
            small8 += 1;
            size += 16;
        } else if k.len() <= 11 {
            small16 += 1;
            size += 24;
        }
    }
    println!("{} total\n{} small8\n{} small16\n {} bytes saved", total_symbols, small8, small16, size);
    */
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

fn assemble(tokenizer: Box<Tokenizer>) -> (Vec<u32>, Vec<u8>, Vec<u8>, Vec<(usize, usize, bool)>) {
    let mut root = Module::new(0, None, true);
    let mut path: PathBuf = tokenizer.filename.clone().into();
    let name = get_symbol(path.file_stem().unwrap().as_encoded_bytes().to_vec());
    path.pop();
    root.path = path;
    // TODO: check for conflicts
    root.children.insert(name, (false, Unit::Module(1)));

    // Define registers
    for i in 1..=32 {
        root.children.insert(i, (false, Unit::Value(Value::Symbol(i))));
    }

    root.children.insert(get_symbol(b"read-file".to_vec()), (false, Unit::Value(Value::LambdaNative(scm::read_file))));
    root.children.insert(get_symbol(b"<".to_vec()), (false, Unit::Value(Value::LambdaNative(scm::lt))));
    root.children.insert(get_symbol(b"+".to_vec()), (false, Unit::Value(Value::LambdaNative(scm::add))));
    root.children.insert(get_symbol(b"cons".to_vec()), (false, Unit::Value(Value::LambdaNative(scm::cons))));
    root.children.insert(get_symbol(b"car".to_vec()), (false, Unit::Value(Value::LambdaNative(scm::car))));
    root.children.insert(get_symbol(b"cdr".to_vec()), (false, Unit::Value(Value::LambdaNative(scm::cdr))));
    root.children.insert(get_symbol(b"list".to_vec()), (false, Unit::Value(Value::LambdaNative(scm::list))));
    root.children.insert(get_symbol(b"make-vec".to_vec()), (false, Unit::Value(Value::LambdaNative(scm::makevec))));
    root.children.insert(get_symbol(b"vec-len".to_vec()), (false, Unit::Value(Value::LambdaNative(scm::veclen))));
    root.children.insert(get_symbol(b"vec-ref".to_vec()), (false, Unit::Value(Value::LambdaNative(scm::vecref))));
    root.children.insert(get_symbol(b"vec-push".to_vec()), (false, Unit::Value(Value::LambdaNative(scm::vecpush))));
    root.children.insert(get_symbol(b"string->symbol".to_vec()), (false, Unit::Value(Value::LambdaNative(scm::string_to_symbol))));
    root.children.insert(get_symbol(b"symbol->string".to_vec()), (false, Unit::Value(Value::LambdaNative(scm::symbol_to_string))));
    root.children.insert(get_symbol(b"string-append".to_vec()), (false, Unit::Value(Value::LambdaNative(scm::string_append))));
    root.children.insert(get_symbol(b"eq?".to_vec()), (false, Unit::Value(Value::LambdaNative(scm::eqp))));
    root.children.insert(get_symbol(b"itoa".to_vec()), (false, Unit::Value(Value::LambdaNative(scm::itoa))));
    root.children.insert(get_symbol(b"len".to_vec()), (false, Unit::Value(Value::LambdaNative(scm::veclen))));

    let mut asm = Asm {
        tokenizer: tokenizer,
        modules: vec![root, Module::new(1, Some(0), true)],
        module: 1,
        data: Vec::new(),
        rodata: Vec::new(),
        import_files: HashMap::new(),
    };

    asm.import_files.insert(asm.tokenizer.filename.clone().into(), 1);
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
    tokenizer: Box<Tokenizer>,
    modules: Vec<Module>,
    module: usize,
    data: Vec<u8>,
    rodata: Vec<u8>,
    import_files: HashMap<PathBuf, usize>,
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
    // position in data, len, constantp
    Bytes(usize, usize, bool, Value),
    Module(usize),
    Value(Value),
}

#[derive(Clone, Debug, PartialEq)]
enum Ast {
    Ident(Symbol),
    Constant(Value),
    If {
        predicate: Box<Ast>,
        consequent: Box<Ast>,
        alternative: Box<Ast>,
    },
    Application(Box<Ast>, Vec<Ast>),
    Lambda {
        variadic: bool,
        args: Vec<Symbol>,
        body: Vec<Ast>,
    },
    Set {
        ident: Symbol,
        expr: Box<Ast>,
    }
}

impl Ast {
    fn to_sexpr(&self) -> Value {
        match self {
            Ast::Ident(s) => Value::Symbol(*s),
            Ast::Constant(v) => *v,
            Ast::If { predicate, consequent, alternative } => {
                Value::Pair(Value::Symbol(symbols::IF), Value::Pair(predicate.to_sexpr(), Value::Pair(consequent.to_sexpr(), Value::Pair(alternative.to_sexpr(), Value::Nil))))
            }
            Ast::Set { ident, expr } => Value::Pair(Value::Symbol(symbols::SET), Value::Pair(Value::Symbol(*ident), Value::Pair(expr.to_sexpr(), Value::Nil))),
            Ast::Lambda { variadic, args, body } => {
                let mut sargs = Value::Nil;
                if *variadic && args.len() == 1 {
                    sargs = Value::Symbol(args[0]);
                } else {
                    let mut len = args.len();
                    if *variadic {
                        sargs = Value::Pair(Value::Symbol(args[len - 2]), Value::Symbol(args[len - 1]));
                        len -= 2;
                    }
                    for i in 0..len {
                        sargs = Value::Pair(Value::Symbol(args[len - 1 - i]), sargs);
                    }
                }

                let mut sbody = Value::Nil;
                for i in 0..body.len() {
                    sbody = Value::Pair(body[body.len() - 1 - i].to_sexpr(), sbody);
                }
                Value::Pair(Value::Symbol(symbols::LAMBDA), Value::Pair(sargs, Value::Pair(sbody, Value::Nil)))
            }
            Ast::Application(f, args) => {
                let mut sargs = Value::Nil;
                for i in 0..args.len() {
                    sargs = Value::Pair(args[args.len() - 1 - i].to_sexpr(), sargs);
                }
                Value::Pair(f.to_sexpr(), sargs)
            }
        }
    }
}

impl Asm {
    fn assemble(&mut self) {
        while let Some(token) = self.next() {
            match token {
                Token::Symbol(s) => self.add_label(s),
                Token::LParen => match self.next() {
                    Some(Token::Symbol(symbols::INCLUDE)) => self.handle_include(),
                    //Some(Token::Symbol(symbols::DEFINE)) => self.handle_define(),
                    Some(Token::Symbol(s)) if s == symbols::DEFINE || s == symbols::DEFMACRO => self.handle_define(s == symbols::DEFMACRO),
                    Some(Token::Symbol(s)) if s == symbols::DEFCON || s == symbols::DEFVAR => self.handle_defcon_var(s == symbols::DEFCON),
                    Some(Token::Symbol(symbols::MODULE)) => self.handle_module(),
                    Some(Token::Symbol(symbols::IMPORT)) => self.handle_import(),
                    Some(Token::Symbol(s)) if s < symbols::LAST_INSTRUCTION => self.handle_opcode(s),
                    Some(Token::Symbol(_)) | Some(Token::LParen) => {
                        self.backtrack();
                        self.backtrack();
                        let a = self.read_expr().unwrap();
                        let mut v = self.eval(a).unwrap();
                        if v.pairp() {
                            if !v.car().pairp() {
                                v = Value::Pair(v, Value::Nil);
                            } 
                            let mut t = Box::new(Tokenizer::new(Vec::new(), "macro".to_string()));
                            mem::swap(&mut self.tokenizer, &mut t);
                            while !v.nilp() {
                                let e = v.car();
                                v = v.cdr();
                                if !e.pairp() {
                                    // TODO: err
                                    continue;
                                }
                                self.tokenizer.tokens.clear();
                                e.to_tokens(&mut self.tokenizer.tokens);
                                self.tokenizer.token_position = 0;
                                self.assemble();
                            }
                            mem::swap(&mut self.tokenizer, &mut t);
                            // TODO: create tokenstream?
                        } else if v.vecp() {
                            let token_vec = v.to_vec();
                            let mut t = Box::new(Tokenizer::new(Vec::new(), "macro".to_string()));
                            mem::swap(&mut self.tokenizer, &mut t);

                            for expr in &token_vec.vec {
                                if expr.symbolp() {
                                    self.add_label(expr.to_symbol());
                                    continue;
                                }
                                if !expr.pairp() {
                                    // TODO: err
                                    continue;
                                }
                                self.tokenizer.tokens.clear();
                                expr.to_tokens(&mut self.tokenizer.tokens);
                                self.tokenizer.token_position = 0;
                                //println!("{:?}", self.tokenizer.tokens);
                                self.assemble();
                            }
                            let _ = Box::into_raw(token_vec);
                            mem::swap(&mut self.tokenizer, &mut t);
                        } else if !v.nilp() {
                            // TODO: err?
                        }
                    },
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
                    if !importp && self.in_scope(k) {
                        self.print_err(&format!("`{}` is already defined in this scope from import statement", get_value(k)), "");
                    } else if !importp {
                        self.get_mod().children.insert(k, (true, v));
                    }
                }
                return;
            }

            match m.children.get(&p).copied() {
                v @ Some((_, Unit::Bytes(_, _, _, _))) | v @ Some((_, Unit::Value(_))) => {
                    let (importp, v) = v.unwrap();
                    if importp {
                        self.print_err("Cannot import an import from import statement", "");
                    }
                    if i == path.len() {
                        if self.in_scope(p) {
                            self.print_err(&format!("`{}` is already defined in this scope from import statement", get_value(p)), "");
                        } else if !importp {
                            self.get_mod().children.insert(p, (true, v));
                        }
                    } else {
                        self.print_err(&format!("Path `{}` resolved to variable/global partway in import statement", self.path_to_string(&path)), "");
                    }
                    return;
                }
                Some((importp, Unit::Module(id))) if !importp => if id == self.get_mod().id {
                    self.print_err("Cannot import self", "");
                    return;
                } else if i == path.len() {
                    if self.in_scope(p) {
                        self.print_err(&format!("`{}` is already defined in this scope from import statement", get_value(p)), "");
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
                                if m_id == self.get_mod().id {
                                    self.print_err("Cannot import self", "");
                                    return;
                                }
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
                    self.get_mod().children.insert(path, (false, Unit::Module(m)));
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
                    self.get_mod().children.insert(path, (false, Unit::Module(m)));
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
        let module = Module::new(m_id, Some(parent), true);
        self.modules.push(module);

        let include_input = match read_file(&file_path) {
            Ok(i) => i,
            Err(e) => {
                self.print_err("Error executing import", &format!("\t{}", e));
                return Some(m_id);
            }
        };

        let old_id = self.module;
        self.module = m_id;
        let mut t = Box::new(Tokenizer::new(include_input, file_path.display().to_string()));
        mem::swap(&mut self.tokenizer, &mut t);
        loop {
            self.assemble();
            if self.peek().is_none() {
                break;
            }
            self.tokenizer.token_position += 1;
            self.print_err("Unexpected closing parenthesis", "");
        }
        t.err |= self.tokenizer.err;
        mem::swap(&mut self.tokenizer, &mut t);

        if self.get_mod().code.len() > 0 {
            self.get_mod().labels.insert(path, 0);
        }
        self.module = old_id;
        Some(m_id)
    }

    // TODO: use 1 or u32::MAX for ident on error?
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
        // TODO: in_scope?
        if self.get_mod().children.contains_key(&ident) {
            self.print_err(&format!("Module `{}` conflicts with existing module/definition in scope", get_value(ident)), "");
        } else {
            self.get_mod().children.insert(ident, (false, Unit::Module(m_id)));
        }
        self.module = m_id;
        self.modules.push(module);
        // TODO: maybe loop here?
        self.assemble();
        if self.next() != Some(Token::RParen) {
            if ident == 0 {
                self.print_err("Unclosed module", "");
            } else {
                self.print_err(&format!("Unclosed module `{}`", get_value(ident)), "");
            }
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

        let mut t = Box::new(Tokenizer::new(include_input, filename));
        mem::swap(&mut self.tokenizer, &mut t);
        loop {
            self.assemble();
            if self.peek().is_none() {
                break;
            }
            self.tokenizer.token_position += 1;
            self.print_err("Unexpected closing parenthesis", "");
        }
        t.err |= self.tokenizer.err;
        mem::swap(&mut self.tokenizer, &mut t);
    }

    fn handle_define(&mut self, macrop: bool) {
        let ident = if let Some(i) = self.unwrap_ident() {
            i
        } else {
            return;
        };

        // TODO: error
        let v = self.read_expr().unwrap();
        let v = self.eval(v).unwrap();
        if macrop {
            if !v.lambdap() {
                todo!();
            }
            let mut l = v.to_lambda();
            if let LambdaE::Ast { ref mut macrop, .. } = l.f {
                *macrop = true;
            }
            let _ = Box::into_raw(l);

        }

        if self.in_scope(ident) {
            self.print_err(&format!("Definition `{}` conflicts with existing module/definition in scope", get_value(ident)), "");
        } else {
            self.get_mod().children.insert(ident, (false, Unit::Value(v)));
        }

        /*
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
        */

        match self.next() {
            Some(Token::RParen) => (),
            None => {
                self.print_err("Unclosed definition", "");
            }
            Some(_) => {
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
                self.get_mod().children.insert(ident, (false, Unit::Bytes(start, 8, constant, Value::Int(i))));
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
                self.get_mod().children.insert(ident, (false, Unit::Bytes(start, 1, constant, Value::Int(c as i64))));
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
                let value = Value::bvec_from_vec(v.clone());
                let start = d.len();
                d.append(&mut v);
                if self.in_scope(ident) {
                    self.print_err(&format!("Definition `{}` conflicts with existing module/definition in scope", get_value(ident)), "");
                } else {
                    self.get_mod().children.insert(ident, (false, Unit::Bytes(start, len, constant, value)));
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
                let value = Value::bvec_from_vec(s.clone());
                let start = d.len();
                d.append(&mut s);
                if self.in_scope(ident) {
                    self.print_err(&format!("Definition `{}` conflicts with existing module/definition in scope", get_value(ident)), "");
                } else {
                    self.get_mod().children.insert(ident, (false, Unit::Bytes(start, len, constant, value)));
                }
            }
            Some(Token::RParen) => return self.print_err(&format!("Definition `{}` must have a value", get_value(ident)), ""),
            Some(_) => self.print_err(&format!("Definition `{}` must be a constant", get_value(ident)), ""),
            None => return self.print_err(&format!("Unexpected EOF in definition `{}` at end of file", get_value(ident)), ""),
        }

        match self.next() {
            Some(Token::RParen) => (),
            None => {
                self.print_err("Unclosed definition", "");
            }
            Some(_) => {
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
    }

    fn handle_opcode(&mut self, symbol: usize) {
        const FUNCT3: [u32; 40] = [0, 0, 4, 6, 7, 1, 5, 5, 2, 3, 0, 4, 6,   // r
                                   0, 0, 4, 6, 7, 2, 3, 5, 1, 5,            // i
                                   0, 1, 2, 3, 4, 5, 6,                     // i2
                                   0, 1, 2, 3,                              // s
                                   0, 1, 4, 5, 6, 7];                       // b
        let i = if symbol < symbols::ADD {
            self.print_err(&format!("Cannot use register `{}` as opcode", get_value(symbol)), "");
            return self.skip_opcode();
        // R instructions
        } else if symbol <= symbols::LAST_R {
            let funct7 = match symbol {
                symbols::SUB | symbols::SRA => 0x20,
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
            } else if symbol >= symbols::SRAI {
                if imm as u32 >= 64 {
                    self.print_err(&format!("Immediate `{}` out of range [0, 64)", imm), "");
                    imm = 0;
                }
                if symbol == symbols::SRAI {
                    imm = imm | (0x20 << 5);
                }
            }
            if imm >= 2048 || imm < -2048 {
                self.print_err(&format!("Immediate `{}` out of range [-2048, 2048)", imm), "");
                imm = 0;
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

            if path.is_empty() {
                self.print_err("Expected non-empty path as argument to `la` instruction", "");
                return self.skip_opcode();
            }
            if path.len() == 1 && path[0] <= symbols::X31 {
                self.print_err("Expected path as argument to `la` instruction, got register", "");
                return self.skip_opcode();
            }

            let (p, constant) = match self.follow_path(&path) {
                Some(Unit::Bytes(p, _, c, _)) => (p, c),
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
            if imm >= 2048 || imm < -2048 {
                self.print_err(&format!("Offset `{}` out of range [-2048, 2048)", imm), "");
            }
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

    fn read_expr(&mut self) -> Result<Ast, Option<Token>> {
        match self.next() {
            Some(Token::Symbol(s)) => Ok(Ast::Ident(s)),
            Some(Token::Integer(i)) => Ok(Ast::Constant(Value::Int(i))),
            Some(Token::Char(c)) => Ok(Ast::Constant(Value::Int(c as i64))),
            Some(Token::String(pos, len)) => Ok(Ast::Constant(Value::bvec_from_vec(self.get_string(pos, len)))),
            Some(Token::Quote) => Ok(Ast::Constant(self.read_quote(false)?)),
            Some(Token::Quasiquote) => Ok(self.read_quasiquote(false)?),
            Some(Token::LParen) => match self.peek() {
                Some(Token::Symbol(symbols::QUOTE)) => Ok(Ast::Constant(self.read_quote(true)?)),
                Some(Token::Symbol(symbols::QUASIQUOTE)) => Ok(self.read_quasiquote(true)?),
                Some(Token::Symbol(symbols::LAMBDA)) => self.read_lambda(),
                Some(Token::Symbol(symbols::IF)) => self.read_if(),
                Some(Token::Symbol(symbols::SET)) => self.read_set(),
                Some(Token::LParen) | Some(Token::Symbol(_)) => {
                    let f = match self.read_expr() {
                        Ok(f) => f,
                        Err(_) => todo!(),
                    };
                    let mut args = Vec::new();
                    loop {
                        match self.read_expr() {
                            Ok(a) => args.push(a),
                            Err(Some(Token::RParen)) => break,
                            _ => todo!(),
                        }
                    }
                    Ok(Ast::Application(Box::new(f), args))
                }
                t @ _ => todo!("{:?}", t),
            },
            Some(Token::RParen) => Err(Some(Token::RParen)),
            _ => todo!(),
        }
    }

    fn read_set(&mut self) -> Result<Ast, Option<Token>> {
        self.next();
        let ident = match self.next() {
            Some(Token::Symbol(s)) => s,
            _ => todo!(),
        };
        let e = self.read_expr().unwrap();

        // TODO: rparen
        self.next();
        Ok(Ast::Set {
            ident: ident,
            expr: Box::new(e),
        })
    }

    fn read_lambda(&mut self) -> Result<Ast, Option<Token>> {
        self.next();

        let mut args = Vec::new();
        let mut variadic = false;
        // TODO: lparen
        match self.next() {
            Some(Token::LParen) => {
                loop {
                    match self.next() {
                        Some(Token::RParen) => break,
                        Some(Token::Symbol(symbols::DOT)) => {
                            variadic = true;
                            if let Some(Token::Symbol(s)) = self.next() {
                                args.push(s);
                            } else {
                                todo!();
                            }
                            match self.next() {
                                Some(Token::RParen) => break,
                                _ => todo!(),
                            }
                        },
                        Some(Token::Symbol(s)) => args.push(s),
                        _ => todo!(),
                    }
                }
            },
            Some(Token::Symbol(s)) => {
                args.push(s);
                variadic = true;
            },
            _ => todo!(),
        }
        let mut body = Vec::new();
        loop {
            match self.read_expr() {
                Ok(a) => body.push(a),
                Err(Some(Token::RParen)) => break,
                _ => todo!(),
            }
        }
        Ok(Ast::Lambda { variadic, args, body })
    }

    fn read_if(&mut self) -> Result<Ast, Option<Token>> {
        self.next();
        let p = self.read_expr()?;
        let c = self.read_expr()?;
        let a = if self.next() == Some(Token::RParen) {
            Ast::Constant(Value::Void)
        } else {
            self.backtrack();
            let a = self.read_expr()?;
            // TODO: read closing paren
            self.next();
            a
        };
        Ok(Ast::If {
            predicate: Box::new(p),
            consequent: Box::new(c),
            alternative: Box::new(a),
        })
    }

    fn read_quasiquote(&mut self, listp: bool) -> Result<Ast, Option<Token>> {
        if listp {
            self.next();
            let expr = match self.read_quasiquote(false) {
                Ok(v) => v,
                _ => todo!(),
            };
            if Some(Token::RParen) != self.next() {
                todo!();
            }
            return Ok(expr);
        }

        // TODO
        let mut dotted = false;
        match self.next() {
            Some(Token::Unquote) => {
                self.read_expr()
            }
            Some(Token::Symbol(s)) => Ok(Ast::Constant(Value::Symbol(s))),
            Some(Token::Integer(i)) => Ok(Ast::Constant(Value::Int(i))),
            Some(Token::Char(c)) => Ok(Ast::Constant(Value::Int(c as i64))),
            Some(Token::String(start, len)) => Ok(Ast::Constant(Value::bvec_from_vec(self.get_string(start, len)))),
            Some(Token::RParen) => Err(Some(Token::RParen)),
            Some(Token::LParen) => {
                let mut args = Vec::new();
                let mut start = true;
                loop {
                    match self.read_quasiquote(false) {
                        Ok(Ast::Constant(v)) if start && v.symbolp() && v.to_symbol() == symbols::UNQUOTE => {
                            let expr = self.read_expr();
                            // TODO: closer
                            self.next();
                            return expr;
                        }
                        Ok(v) => args.push(v),
                        Err(Some(Token::RParen)) => break,
                        _ => todo!(),
                    }
                    start = false;
                }
                Ok(Ast::Application(Box::new(Ast::Ident(symbols::LIST)), args))
            }
            _ => todo!(),
        }
    }

    fn read_quote(&mut self, listp: bool) -> Result<Value, Option<Token>> {
        if listp {
            self.next();
            let v = match self.read_quote(false) {
                Ok(v) => v,
                _ => todo!(),
            };
            if Some(Token::RParen) != self.next() {
                todo!();
            }
            return Ok(v)
        }

        let mut dotted = false;
        match self.next() {
            Some(Token::Symbol(s)) => Ok(Value::Symbol(s)),
            Some(Token::Integer(i)) => Ok(Value::Int(i)),
            // TODO
            Some(Token::Char(c)) => Ok(Value::Int(c as i64)),
            Some(Token::String(start, len)) => Ok(Value::bvec_from_vec(self.get_string(start, len))),
            Some(Token::RParen) => Err(Some(Token::RParen)),
            Some(Token::LParen) => {
                let mut args = Vec::new();
                loop {
                    match self.read_quote(false) {
                        Ok(v) if v.symbolp() && v.to_symbol() == symbols::DOT => {
                            if let Ok(v) = self.read_quote(false) {
                                dotted = true;
                                args.push(v);
                            } else {
                                todo!();
                            }
                            if let Err(Some(Token::RParen)) = self.read_quote(false) {
                                break;
                            } else {
                                todo!();
                            }
                        },
                        Ok(v) => args.push(v),
                        Err(Some(Token::RParen)) => break,
                        _ => todo!(),
                    }
                }
                let mut list = Value::Nil;
                if dotted {
                    list = args.pop().unwrap();
                }
                while args.len() != 0 {
                    list = Value::Pair(args.pop().unwrap(), list);
                }
                Ok(list)
            }
            _ => todo!(),
        }
    }

    fn eval(&mut self, a: Ast) -> Result<Value, ()> {
        match a {
            Ast::Constant(v) => Ok(v),
            Ast::Ident(s) => self.lookup(s),
            Ast::Application(f, a) => self.apply(*f, a),
            Ast::If { predicate, consequent, alternative } => {
                let p = self.eval(*predicate)?;
                if p.truthy() {
                    self.eval(*consequent)
                } else {
                    self.eval(*alternative)
                }
            },
            Ast::Set { ident, expr } => {
                let v = self.eval(*expr).unwrap();
                self.set_path(&[ident], v).unwrap();
                Ok(Value::Void)
            }
            l @ Ast::Lambda { .. } => Ok(Value::Lambda(l)),
        }
    }

    fn lookup(&mut self, s: Symbol) -> Result<Value, ()> {
        let st = get_value(s);
        //println!("{}", st);
        match self.follow_path(&[s]) {
            Some(Unit::Value(v)) => Ok(v),
            Some(Unit::Bytes(_, _, _, v)) => Ok(v),
            _ => {
                println!("{}", st);
                todo!();
            }
        }
    }

    fn apply(&mut self, f: Ast, args_raw: Vec<Ast>) -> Result<Value, ()> {
        if let Ast::Ident(s) = f {
            if let Some(Unit::Module(_)) = self.follow_path(&[s]) {
                let mut path = vec![s];
                for a in args_raw {
                    if let Ast::Ident(s) = a {
                        path.push(s);
                    } else {
                        todo!();
                    }
                }
                match self.follow_path(&path) {
                    Some(Unit::Value(v)) => return Ok(v),
                    _ => todo!(),
                }
            }
        }
        let f = self.eval(f)?;
        if !f.lambdap() {
            todo!();
        }
        let f = f.to_lambda();

        let mut v = Value::Void;
        if let LambdaE::Ast { macrop, variadicp, args: ref fargs, ref body } = f.f {
            let mut args = Vec::new();
            if macrop {
                for a in args_raw {
                    args.push(a.to_sexpr());
                }
            } else {
                for a in args_raw {
                    args.push(self.eval(a)?);
                }
            }

            if args.len() < fargs.len() {
                todo!("not enough args");
            } else if args.len() > fargs.len() && !variadicp {
                todo!("not enough args");
            }
            let m_id = self.modules.len();
            let mut module = Module::new(m_id, Some(self.module), false);
            for (i, a) in fargs.iter().enumerate() {
                module.children.insert(*a, (false, Unit::Value(args[i])));
            }
            self.module = m_id;
            self.modules.push(module);

            for e in body {
                v = self.eval(e.clone()).unwrap();
            }

            self.module = self.get_mod().parent.unwrap();
            // TODO
            self.modules.pop();
            if macrop && !v.voidp() && !v.nilp() {
                let x = v.to_ast();
                v = self.eval(x).unwrap();
                //v = self.eval(v.to_ast()).unwrap();
            }
        } else if let LambdaE::Native(f) = f.f {
            let mut args = Vec::new();
            for a in args_raw {
                args.push(self.eval(a)?);
            }
            v = f(args);
        }
        // TODO: mem::forget?
        let _ = Box::into_raw(f);
        Ok(v)
    }

    fn unwrap_register(&mut self) -> u32 {
        // TODO: err
        let e = self.read_expr().unwrap();
        let v = self.eval(e).unwrap();
        let s = v.to_symbol();
        if v.symbolp() && s <= symbols::X31 {
            s as u32 - 1
        } else if v.symbolp() {
            self.print_err(&format!("Expected register in instruction, got identifier `{}`", get_value(s)), "");
            0
        } else {
            self.print_err("Expected register in instruction", "");
            0
        }

        /*
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
        */
    }

    fn unwrap_imm(&mut self) -> i64 {
        let e = self.read_expr().unwrap();
        let v = self.eval(e).unwrap();
        if v.intp() {
            v.to_integer() as i64
        } else if v.bigintp() {
            todo!();
        } else {
            todo!();
        }

        /*
        match self.next() {
            Some(Token::LParen) => {
                match self.next() {
                    None => {
                        self.print_err("Expected path in expression, got EOF", "");
                        return 0;
                    }
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
                            // TODO
                            Some(Unit::Value(v)) => if v.intp() {
                                v.to_integer() as i64
                            } else if v.bigintp() {
                                todo!();
                            } else {
                                todo!();
                            },
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
                        if path.is_empty() {
                            self.print_err("Empty path in `len` expression", "");
                            0
                        } else {
                            match self.follow_path(&path) {
                                Some(Unit::Bytes(_, l, _)) => l as i64,
                                Some(_) | None => {
                                    self.print_err(&format!("Global `{}` not defined/imported at use", self.path_to_string(&path)), "");
                                    0
                                }
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
                        self.skip_opcode();
                    }
                    None => self.print_err("Unexpected EOF in `len` expression", ""),
                }
                len
            }
            Some(Token::Symbol(s)) => {
                match self.follow_path(&[s]) {
                    // TODO
                    Some(Unit::Value(v)) => if v.intp() {
                        v.to_integer() as i64
                    } else if v.bigintp() {
                        todo!();
                    } else {
                        todo!();
                    },
                    Some(_) => {
                        self.print_err(&format!("Variable `{}` must be a constant in expression", get_value(s)), "");
                        0
                    }
                    None => {
                        self.print_err(&format!("Variable `{}` not defined/imported at use", get_value(s)), "");
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
        */
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

        match self.next() {
            Some(Token::RParen) => (),
            None => {
                self.print_err("Expected closing parenthesis in expression offset", "");
            }
            Some(_) => {
                self.backtrack();
                self.print_err("Expected closing parenthesis in expression offset", "");
                self.skip_opcode();
            }
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
                // TODO
                self.print_err("Bad path, only import statements can use `*` in expression", "");
                return None;
            }

            match m.children.get(&p) {
                v @ Some((_, Unit::Bytes(_, _, _, _))) | v @ Some((_, Unit::Value(_))) => {
                    if i == path.len() {
                        return Some(v.unwrap().1);
                    } else {
                        return None;
                    }
                }
                Some((_, Unit::Module(id))) => m = &self.modules[*id],
                None => {
                    if m.filep {
                        if i == path.len() {
                            match self.modules[0].children.get(&path[i - 1]) {
                                v @ Some((_, Unit::Bytes(_, _, _, _))) | v @ Some((_, Unit::Value(_))) => return Some(v.unwrap().1),
                                _ => (),
                            }
                        }
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

    fn set_path(&mut self, path: &[Symbol], value: Value) -> Result<(), ()> {
        if path.len() == 1 && (path[0] == symbols::CARAT || path[0] == symbols::STAR) {
            self.print_err("Path cannot consist of just `^`/`*`", "");
            todo!();
        }
        let mut id = self.module;
        let mut i = 0;
        while i < path.len() {
            let p = path[i];
            if p == symbols::CARAT {
                i += 1;
                if let Some(p) = self.modules[id].parent {
                    id = p;
                } else {
                    self.print_err("Path jumped past root in expression", "");
                    todo!();
                };
            } else {
                break;
            }
        }

        while i < path.len() {
            let p = path[i];
            i += 1;
            if p == symbols::CARAT {
                self.print_err("Bad path, can only move up (`^`) at beginning of path in expression", "");
                todo!();
            } else if p == symbols::STAR {
                // TODO
                self.print_err("Bad path, only import statements can use `*` in expression", "");
                todo!();
            }

            match self.modules[id].children.get(&p) {
                Some((_, Unit::Bytes(_, _, _, _))) => if i == path.len() {
                    return Err(());
                } else {
                    todo!();
                },
                Some((_, Unit::Value(_))) => if i == path.len() {
                    self.modules[id].children.insert(p, (false, Unit::Value(value)));
                    return Ok(());
                } else {
                    todo!();
                }
                Some((_, Unit::Module(mid))) => id = *mid,
                None => {
                    if self.modules[id].filep {
                        if i == path.len() {
                            match self.modules[0].children.get(&path[i - 1]) {
                                Some((_, Unit::Bytes(_, _, _, _))) => return Err(()),
                                Some((_, Unit::Value(_))) => {
                                    self.modules[0].children.insert(p, (false, Unit::Value(value)));
                                    return Ok(());
                                }
                                _ => (),
                            }
                        }
                        self.tokenizer.err = true;
                        todo!();
                    } else {
                        id = self.modules[id].parent.unwrap();
                        i -= 1;
                    }
                }
            }
        }
        todo!()
    }

    fn in_scope(&self, ident: Symbol) -> bool {
        let mut m = &self.modules[self.module];
        loop {
            if m.children.get(&ident).is_some() {
                return true;
            } else if m.filep {
                return self.modules[0].children.get(&ident).is_some();
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

    fn get_string(&mut self, start: usize, end: u32) -> Vec<u8> {
        let mut v = Vec::with_capacity(end as usize);
        let mut i = 0;
        while i < end as usize {
            if self.tokenizer.input[start+i] == b'\\' {
                i += 1;
                if i == end as usize {
                    self.print_err("Unfinished escape code", "");
                    break;
                }
                match self.tokenizer.input[start+i] {
                    b'r' => v.push(b'\r'),
                    b'n' => v.push(b'\n'),
                    b't' => v.push(b'\t'),
                    b'\\' => v.push(b'\\'),
                    b'"' => v.push(b'"'),
                    b'0' => v.push(b'\0'),
                    c @ _ => v.push(c),
                }
            } else {
                v.push(self.tokenizer.input[start+i]);
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

    fn same_line(&self, line: usize) -> bool {
        self.tokenizer.token_info[self.tokenizer.token_position-1].line == line
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
