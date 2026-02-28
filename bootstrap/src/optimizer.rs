use {Symbol, Value};
use vm::{ASM, GotoValue, Register, Value};

use std::collections::{HashMap, HashSet};


#[derive(Clone, Debug, PartialEq)]
pub enum IR {
    Label(Symbol),
    Return(Symbol),
    Goto(Symbol),
    GotoIf(Symbol, Symbol),
    GotoIfNot(Symbol, Symbol),
    // TODO: new PHI
    //Phi(Symbol, Symbol, Vec<IR>, Symbol, Vec<IR>),
    Move(Symbol, Symbol),
    Phi(Symbol, Symbol, Symbol, usize),
    Begin(Symbol, usize),
    End(Symbol),
    Define(Symbol, Symbol),
    Set(Symbol, Symbol),
    Primitive(Symbol, Value),
    Lookup(Symbol, Symbol),
    Copy(Symbol, Symbol),
    //Param(Symbol),
    //Call(Symbol, Symbol, usize),
    Call(Symbol, Symbol, Vec<Symbol>),
    Fn(Symbol, bool, Vec<Symbol>, Vec<IR>),
}

pub fn optimize(mut ir: Vec<IR>) -> Vec<IR> {
    optimize_lambda_formals(&mut ir);
    optimize_lookups(&mut ir);
    optimize_copies(&mut ir);
    optimize_dead_code(&mut ir);
    //optimize_tail_call(&mut ir);
    //optimize_recursion(&mut ir);
    ir
}

// TODO: make recursion jumps rather than calls
/*
fn optimize_recursion(ir: &mut Vec<IR>) {
    fn inner(f: &mut IR) {
    }

    for i in ir.iter_mut() {
        if let IR::Fn(_, _, _) = *i {
            inner(i);
        }
    }
}

// TODO
fn optimize_tail_call(ir: &mut Vec<IR>) {
    fn inner(f: &mut IR) {
    }

    for i in ir.iter_mut() {
        if let IR::Fn(_, _, _) = *i {
            inner(i);
        }
    }
}
*/

fn optimize_lambda_formals(ir: &mut Vec<IR>) {
    fn inner(f: &mut IR) {
        let (formals, ir) = if let IR::Fn(_, b, c) = f { (b, c) } else { unreachable!() };
        for i in ir.iter_mut() {
            match *i {
                IR::Fn(_, _, _) => inner(i),
                IR::Lookup(t, ident) => if formals.contains(&ident) {
                    *i = IR::Copy(t, ident);
                },
                _ => (),
            }
        }
    }
    /*
    fn inner(formals: &[Symbol], ir: &mut Vec<IR>) {
        for i in ir.iter_mut() {
            match i {
                IR::Fn(_, f, i) => inner(&f, i),
                IR::Lookup(t, ident) => if formals.contains(&ident) {
                    *i = IR::Copy(*t, *ident);
                },
                IR::Phi(_, _, ref mut cons, _, ref mut alt) => {
                    inner(formals, cons);
                    inner(formals, alt);
                },
                _ => (),
            }
        }
    }
    */

    for i in ir.iter_mut() {
        if let IR::Fn(_, _, _) = *i {
            inner(i);
        }
    }
}

fn optimize_lookups(ir: &mut Vec<IR>) {
    /*
    fn inner(ir: &mut Vec<IR>, lookups: &mut HashMap<Symbol, Symbol>) {
        for i in ir.iter_mut() {
            match i {
                IR::Lookup(target, ident) => {
                    // Shitty bc
                    let ident = *ident;
                    if let Some(t) = lookups.get(&ident) {
                        *i = IR::Copy(*target, *t);
                    } else {
                        lookups.insert(ident, *target);
                    }
                }
                IR::Fn(_, _, ir) => optimize_lookups(ir),
                IR::Phi(_, _, cons, _, alt) => {
                    inner(cons, lookups);
                    inner(alt, lookups);
                },
                _ => (),
            }
        }
    }
    */
    let mut lookups = HashMap::new();
    for i in ir.iter_mut() {
        match i {
            IR::Lookup(target, ident) => {
                // Shitty bc
                let ident = *ident;
                if let Some(t) = lookups.get(&ident) {
                    *i = IR::Copy(*target, *t);
                } else {
                    lookups.insert(ident, *target);
                }
            }
            IR::Fn(_, _, ir) => optimize_lookups(ir),
            _ => (),
        }
    }
    //inner(ir, &mut lookups);

}

fn optimize_dead_code(ir: &mut Vec<IR>) {
    /*
    fn intern(ir: &Vec<IR>, used: &mut HashSet<Symbol>) {
        for i in ir.iter().rev() {
            match i {
                IR::GotoIf(_, s) => { used.insert(*s); }
                IR::GotoIfNot(_, s) => { used.insert(*s); }
                //IR::Param(s) => { used.insert(*s); }
                IR::Return(s) => { used.insert(*s); }
                IR::Define(_, s) => { used.insert(*s); }
                IR::Call(_, s, args) => {
                    used.insert(*s);
                    for arg in args {
                        used.insert(*arg);
                    }
                }
                IR::Phi(s1, conss, cons, alts, alt) => {
                    used.insert(*s1);
                    used.insert(*conss);
                    used.insert(*alts);
                    intern(cons, used);
                    intern(alt, used);
                }
                _ => (),
            }
        }
    }
    */
    let mut used = HashSet::new();
    //intern(ir, &mut used);
    for i in ir.iter().rev() {
        match i {
            IR::GotoIf(_, s) => { used.insert(*s); }
            IR::GotoIfNot(_, s) => { used.insert(*s); }
            //IR::Param(s) => { used.insert(*s); }
            IR::Return(s) => { used.insert(*s); }
            IR::Define(_, s) => { used.insert(*s); }
            IR::Call(_, s, args) => {
                used.insert(*s);
                for arg in args {
                    used.insert(*arg);
                }
            }
            IR::Phi(s, cons, alt, _) => {
                used.insert(*s);
                used.insert(*cons);
                used.insert(*alt);
            }
            _ => (),
        }
    }

    let mut idx = 0;
    while idx < ir.len() {
        match &mut ir[idx] {
            IR::Fn(s, _, ir) => if !used.contains(s) {
                ir.remove(idx);
                continue;
            } else {
                optimize_dead_code(ir);
            },
            IR::Primitive(s, _) => if !used.contains(s) {
                ir.remove(idx);
                continue;
            },
            IR::Lookup(s, _) => if !used.contains(s) {
                ir.remove(idx);
                continue;
            },
            _ => (),
        }
        idx += 1;
    }
}

fn optimize_copies(ir: &mut Vec<IR>) {
    /*
    fn intern(ir: &mut Vec<IR>, copies: &mut HashMap<Symbol, Symbol>) {
        let mut idx = 0;
        while idx < ir.len() {
            match &mut ir[idx] {
                IR::Copy(target, s) => if let Some(&t) = copies.get(s) {
                    copies.insert(*target, t);
                    ir.remove(idx);
                    continue;
                } else {
                    copies.insert(*target, *s);
                    ir.remove(idx);
                    continue;
                },
                IR::Move(target, s) => if let Some(t) = copies.get(s) {
                    ir[idx] = IR::Move(*target, *t);
                },
                IR::Return(s) => if let Some(t) = copies.get(s) {
                    ir[idx] = IR::Return(*t);
                },
                IR::GotoIf(a, s) => if let Some(t) = copies.get(s) {
                    ir[idx] = IR::GotoIf(*a, *t);
                },
                IR::GotoIfNot(a, s) => if let Some(t) = copies.get(s) {
                    ir[idx] = IR::GotoIfNot(*a, *t);
                },
                // TODO: PHI
                IR::Phi(_, s1, ir1, s2, ir2) => {
                    intern(ir1, copies);
                    intern(ir2, copies);
                    if let Some(t) = copies.get(s1) {
                        // *t
                        *s1 = *t;
                    }
                    if let Some(t) = copies.get(s2) {
                        *s2 = *t;
                    }
                }
                IR::Define(b, s) => if let Some(t) = copies.get(s) {
                    ir[idx] = IR::Define(*b, *t);
                },
                //IR::Param(s) => if let Some(t) = copies.get(s) {
                //    ir[idx] = IR::Param(*t);
                //},
                IR::Call(_, s, args) => {
                    if let Some(t) = copies.get(s) {
                        *s = *t;
                    }

                    for arg in args {
                        if let Some(t) = copies.get(arg) {
                            *arg = *t;
                        }
                    }
                }
                IR::Fn(_, _, ir) => optimize_copies(ir),
                _ => (),
            }
            idx += 1;
        }
    }
    */
    let mut copies = HashMap::new();
    //intern(ir, &mut copies);
    let mut idx = 0;
    while idx < ir.len() {
        match &mut ir[idx] {
            IR::Copy(target, s) => if let Some(&t) = copies.get(s) {
                copies.insert(*target, t);
                ir.remove(idx);
                continue;
            } else {
                copies.insert(*target, *s);
                ir.remove(idx);
                continue;
            },
            IR::Move(target, s) => if let Some(t) = copies.get(s) {
                ir[idx] = IR::Move(*target, *t);
            },
            IR::Return(s) => if let Some(t) = copies.get(s) {
                ir[idx] = IR::Return(*t);
            },
            IR::GotoIf(a, s) => if let Some(t) = copies.get(s) {
                ir[idx] = IR::GotoIf(*a, *t);
            },
            IR::GotoIfNot(a, s) => if let Some(t) = copies.get(s) {
                ir[idx] = IR::GotoIfNot(*a, *t);
            },
            IR::Phi(_, cons, alt, _) => {
                if let Some(t) = copies.get(cons) {
                    *cons = *t;
                }
                if let Some(t) = copies.get(alt) {
                    *alt = *t;
                }
            }
            IR::Define(b, s) => if let Some(t) = copies.get(s) {
                ir[idx] = IR::Define(*b, *t);
            },
            IR::Call(_, s, args) => {
                if let Some(t) = copies.get(s) {
                    *s = *t;
                }

                for arg in args {
                    if let Some(t) = copies.get(arg) {
                        *arg = *t;
                    }
                }
            }
            IR::Fn(_, _, ir) => optimize_copies(ir),
            _ => (),
        }
        idx += 1;
    }

}

pub fn output_asm(ir: Vec<IR>) -> Vec<ASM> {
    let mut output = Output {
        var_reg: [None; 32],
        var_stack: Vec::new(),
        var_mapping: HashMap::new(),
        //var_location: HashMap::new(),
        used: HashMap::new(),
        live: HashMap::new(),
        //stack: Vec::new(),
        stack: 0,
        block_stack: Vec::new(),
    };
    output._output_asm(ir, Register(0))
}

#[derive(Clone)]
struct Output {
    var_reg: [Option<Symbol>; 32],
    var_stack: Vec<Symbol>,
    var_mapping: HashMap<Symbol, Register>,
    //var_location: HashMap<Symbol, M>,
    used: HashMap<Register, Symbol>,
    live: HashMap<Symbol, usize>,
    //stack: Vec<Symbol>,
    stack: usize,
    block_stack: Vec<usize>,
}

impl Output {
    fn _output_asm(&mut self, ir: Vec<IR>, target: Register) -> Vec<ASM> {
        self.register_allocation(&ir, target);

        let mut asm = Vec::new();
        for (idx, i) in ir.into_iter().enumerate() {
            match i {
                IR::Primitive(s, v) => {
                    let r = self.get_register(s, &mut asm, idx);
                    asm.push(ASM::LoadConst(r, v));
                },
                IR::Define(n, s2) => {
                    // TODO
                    let r = Register(17);
                    asm.push(ASM::LoadConst(r, Value::Symbol(n)));
                    let r2 = self.find_symbol(s2, &mut asm);
                    asm.push(ASM::Define(r, r2));
                }
                IR::Lookup(s, ident) => {
                    let r = self.get_register(s, &mut asm, idx);
                    asm.push(ASM::LoadConst(r, Value::Symbol(ident)));
                    asm.push(ASM::Lookup(r, r));
                }
                IR::Call(s, proc, args) => {
                    for (r, s) in &self.used {
                        if idx < *self.live.get(s).unwrap() {
                            self.var_stack.push(*s);
                            //self.var_location.insert(*s, M::S(self.stack));
                            self.stack += 1;
                            asm.push(ASM::Save(*r));
                        }
                    }
                    for (i, arg) in args.iter().enumerate() {
                        self.load_symbol(*arg, Register(i as u8 + 1), &mut asm);
                    }

                    let r = self.find_symbol(proc, &mut asm);
                    asm.push(ASM::Call(r));
                    if Register(0) != self.lookup_register(s) {
                        asm.push(ASM::Move(self.lookup_register(s), Register(0)));
                    }
                    //self.var_location.insert(s, M::R(self.lookup_register(s)));
                    self.var_reg = [None; 32];
                    self.var_reg[self.lookup_register(s).0 as usize] = Some(s);
                    self.used.clear();
                    self.used.insert(self.lookup_register(s), s);
                }
                IR::Fn(s, args, ir) => {
                    let mut output = Output {
                        var_reg: [None; 32],
                        var_stack: Vec::new(),
                        var_mapping: HashMap::new(),
                        //var_location: HashMap::new(),
                        used: HashMap::new(),
                        live: HashMap::new(),
                        //stack: Vec::new(),
                        stack: 0,
                        block_stack: Vec::new(),
                    };
                    for (i, arg) in args.iter().enumerate() {
                        output.var_mapping.insert(*arg, Register(i as u8 + 1));
                        //output.var_location.insert(*arg, M::R(Register(i as u8 + 1)));
                        output.var_reg[i+1] = Some(*arg);
                        output.used.insert(Register(i as u8 + 1), *arg);
                    }
                    let instructions = output._output_asm(ir, Register(0));
                    let r = self.get_register(s, &mut asm, idx);
                    asm.push(ASM::MakeClosure(r, Box::new(instructions)));
                }
                IR::Label(s) => asm.push(ASM::Label(s)),
                IR::Goto(l) => asm.push(ASM::Goto(GotoValue::Label(l))),
                IR::GotoIf(l, s) => asm.push(ASM::GotoIf(GotoValue::Label(l), self.lookup_register(s))),
                IR::GotoIfNot(l, s) => asm.push(ASM::GotoIfNot(GotoValue::Label(l), self.lookup_register(s))),
                IR::Return(s) => {
                    self.load_symbol(s, target, &mut asm);
                    /*
                    if target != self.lookup_register(s) {
                        asm.push(ASM::Move(target, self.lookup_register(s)));
                    }
                    */
                    asm.push(ASM::Return);
                }
                IR::Move(t, s) => {
                    self.load_symbol(s, *self.var_mapping.get(&t).unwrap(), &mut asm);
                }
                IR::Begin(_s, len) => self.block_stack.push(len),
                IR::End(s) => self.block_stack.pop(),
                IR::Phi(union, cons, alt, len) => {
                    self.block_stack.push(len);
                    // TODO
                    for (r, s) in &self.used {
                        if self.livep(*s, idx) {
                            self.var_stack.push(*s);
                            self.stack += 1;
                            asm.push(ASM::Save(*r));
                        }
                    }
                }
                /*
                IR::Phi(union, conss, cons, alts, alt) => {
                    // TODO: save variables that outlive this PHI, convert cons to asm followed by
                    // alt
                    for (r, s) in &self.used {
                        if idx < *self.live.get(s).unwrap() {
                            //self.var_location.insert(*s, M::S(self.stack));
                            self.var_stack.push(*s);
                            self.stack += 1;
                            asm.push(ASM::Save(*r));
                        }
                    }

                    let mut c = self.clone();
                    for i in cons {
                        c._output_asm_inner(idx, i, target, &mut asm);
                    }
                    let cons_pos = c.var_reg.iter().position(|x| *x == Some(conss)).unwrap();
                    //self.var_location.insert(conss, *c.var_location.get(&conss).unwrap());
                    let mut a = self.clone();
                    for i in alt {
                        a._output_asm_inner(idx, i, target, &mut asm);
                    }
                    let alt_pos = a.var_reg.iter().position(|x| *x == Some(alts)).unwrap();
                    //self.var_location.insert(alts, *a.var_location.get(&alts).unwrap());
                    //assert_eq!(self.var_location.get(&conss).unwrap(), self.var_location.get(&alts).unwrap());
                    assert_eq!(cons_pos, alt_pos);
                    //self.var_location.insert(union, *self.var_location.get(&conss).unwrap());
                    self.var_reg[alt_pos] = Some(union);
                }
                */
                //IR::Param(_) => (),
                // Only used for optimization
                IR::Copy(_, _) => unreachable!(),
            }
        }
        asm
    }

    // TODO
    fn livep(&self, s: Symbol, idx: usize) -> bool {
        let i = self.live.get(&s).unwrap();
        if idx >= i {
            return false;
        } else if let Some(block) = self.block_stack.last() {
            let phi = self.block_stack[self.block_stack.len() - 2];
            if i > block && i > phi
        } else {
            return true;
        }
        todo!()
    }

    /*
    fn _output_asm_inner(&mut self, idx: usize, i: IR, target: Register, asm: &mut Vec<ASM>) {
            match i {
                IR::Primitive(s, v) => {
                    let r = self.get_register(s, asm, idx);
                    asm.push(ASM::LoadConst(r, v));
                },
                IR::Define(n, s2) => {
                    // TODO
                    let r = Register(17);
                    asm.push(ASM::LoadConst(r, Value::Symbol(n)));
                    let r2 = self.find_symbol(s2, asm);
                    asm.push(ASM::Define(r, r2));
                }
                IR::Lookup(s, ident) => {
                    let r = self.get_register(s, asm, idx);
                    asm.push(ASM::LoadConst(r, Value::Symbol(ident)));
                    asm.push(ASM::Lookup(r, r));
                }
                IR::Call(s, proc, args) => {
                    for (r, s) in &self.used {
                        if idx < *self.live.get(s).unwrap() {
                            self.var_stack.push(*s);
                            //self.var_location.insert(*s, M::S(self.stack));
                            self.stack += 1;
                            asm.push(ASM::Save(*r));
                        }
                    }
                    for (i, arg) in args.iter().enumerate() {
                        self.load_symbol(*arg, Register(i as u8 + 1), asm);
                    }

                    let r = self.find_symbol(proc, asm);
                    asm.push(ASM::Call(r));
                    if Register(0) != self.lookup_register(s) {
                        asm.push(ASM::Move(self.lookup_register(s), Register(0)));
                    }
                    //self.var_location.insert(s, M::R(self.lookup_register(s)));
                    self.var_reg = [None; 32];
                    self.var_reg[self.lookup_register(s).0 as usize] = Some(s);
                    self.used.clear();
                    self.used.insert(self.lookup_register(s), s);
                }
                IR::Fn(s, args, ir) => {
                    let mut output = Output {
                        var_reg: [None; 32],
                        var_stack: Vec::new(),
                        var_mapping: HashMap::new(),
                        //var_location: HashMap::new(),
                        used: HashMap::new(),
                        live: HashMap::new(),
                        //stack: Vec::new(),
                        stack: 0,
                    };
                    for (i, arg) in args.iter().enumerate() {
                        output.var_mapping.insert(*arg, Register(i as u8 + 1));
                        //output.var_location.insert(*arg, M::R(Register(i as u8 + 1)));
                        output.var_reg[i+1] = Some(*arg);
                        output.used.insert(Register(i as u8 + 1), *arg);
                    }
                    let instructions = output._output_asm(ir, Register(0));
                    let r = self.get_register(s, asm, idx);
                    asm.push(ASM::MakeClosure(r, Box::new(instructions)));
                }
                IR::Label(s) => asm.push(ASM::Label(s)),
                IR::Goto(l) => asm.push(ASM::Goto(GotoValue::Label(l))),
                IR::GotoIf(l, s) => asm.push(ASM::GotoIf(GotoValue::Label(l), self.lookup_register(s))),
                IR::GotoIfNot(l, s) => asm.push(ASM::GotoIfNot(GotoValue::Label(l), self.lookup_register(s))),
                IR::Return(s) => {
                    self.load_symbol(s, target, asm);
                    /*
                    if target != self.lookup_register(s) {
                        asm.push(ASM::Move(target, self.lookup_register(s)));
                    }
                    */
                    asm.push(ASM::Return);
                }
                IR::Move(t, s) => {
                    self.load_symbol(s, *self.var_mapping.get(&t).unwrap(), asm);
                }
                // Not needed after register allocation
                IR::Phi(union, conss, cons, alts, alt) => {
                    // TODO: save variables that outlive this PHI, convert cons to asm followed by
                    // alt
                    for (r, s) in &self.used {
                        if idx < *self.live.get(s).unwrap() {
                            //self.var_location.insert(*s, M::S(self.stack));
                            self.var_stack.push(*s);
                            self.stack += 1;
                            asm.push(ASM::Save(*r));
                        }
                    }

                    let mut c = self.clone();
                    for i in cons {
                        c._output_asm_inner(idx, i, target, asm);
                    }
                    let cons_pos = c.var_reg.iter().position(|x| *x == Some(conss)).unwrap();
                    //self.var_location.insert(conss, *c.var_location.get(&conss).unwrap());
                    let mut a = self.clone();
                    for i in alt {
                        a._output_asm_inner(idx, i, target, asm);
                    }
                    let alt_pos = a.var_reg.iter().position(|x| *x == Some(alts)).unwrap();
                    //self.var_location.insert(alts, *a.var_location.get(&alts).unwrap());
                    //assert_eq!(self.var_location.get(&conss).unwrap(), self.var_location.get(&alts).unwrap());
                    assert_eq!(cons_pos, alt_pos);
                    //self.var_location.insert(union, *self.var_location.get(&conss).unwrap());
                    self.var_reg[alt_pos] = Some(union);
                }
                //IR::Param(_) => (),
                // Only used for optimization
                IR::Copy(_, _) => unreachable!(),
            }
    }
*/

    fn register_allocation(&mut self, ir: &[IR], target: Register) {
        // Iterate in reverse
        for (idx, i) in ir.iter().enumerate().rev() {
            match *i {
                IR::Call(s, proc, ref args) => {
                    self.live.entry(s).or_insert(idx);
                    self.live.entry(proc).or_insert(idx);
                    self.var_mapping.insert(proc, Register(0));
                    for (i, arg) in args.iter().enumerate() {
                        self.var_mapping.insert(*arg, Register(i as u8 + 1));
                        self.live.entry(*arg).or_insert(idx);
                    }
                }
                IR::Define(_, s) => if !self.live.contains_key(&s) {
                    self.live.insert(s, idx);
                }
                IR::Phi(s, cons, alt, _) => {
                    let r = *self.var_mapping.get(&s).unwrap();
                    self.var_mapping.insert(cons, r);
                    self.var_mapping.insert(alt, r);
                    if let Some(&i) = self.live.get(&s) {
                        self.live.insert(cons, i);
                        self.live.insert(alt, i);
                    } else {
                        self.live.insert(s, idx);
                        self.live.insert(cons, idx);
                        self.live.insert(alt, idx);
                    }
                }
                IR::Move(_, _) => (),
                IR::Begin(_, _) => (),
                IR::End(_) => (),
                IR::Return(s) => {
                    self.var_mapping.insert(s, target);
                    self.live.entry(s).or_insert(idx);
                }
                IR::GotoIf(_, s) => { self.live.entry(s).or_insert(idx); }
                IR::GotoIfNot(_, s) => { self.live.entry(s).or_insert(idx); }
                IR::Goto(_) | IR::Label(_) => (),
                IR::Fn(s, _, _) => if !self.var_mapping.contains_key(&s) {
                    panic!("Dead code?");
                },
                IR::Primitive(s, _) => if !self.var_mapping.contains_key(&s) {
                    panic!("Dead code?");
                },
                IR::Lookup(s, _) => if !self.live.contains_key(&s) {
                    panic!("Dead code?");
                },
                // Only used for optimization
                IR::Copy(_, _) => unreachable!(),
            }
        }
    }

    /*
    fn reg_alloc_inner(&mut self, i: &IR, idx: usize, target: Register) {
        match i {
            IR::Call(s, proc, ref args) => {
                self.live.entry(*s).or_insert(idx);
                self.live.entry(*proc).or_insert(idx);
                self.var_mapping.insert(*proc, Register(0));
                for (i, arg) in args.iter().enumerate() {
                    self.var_mapping.insert(*arg, Register(i as u8 + 1));
                    self.live.entry(*arg).or_insert(idx);
                }
            }
            IR::Define(_, s) => if !self.live.contains_key(&s) {
                self.live.insert(*s, idx);
            }
            IR::Phi(s1, conss, cons, alts, alt) => {
                let r = {*self.var_mapping.get(&s1).unwrap()};
                self.var_mapping.insert(*conss, r);
                self.var_mapping.insert(*alts, r);

                if let Some(&i) = self.live.get(&s1) {
                    self.live.insert(*conss, i);
                    self.live.insert(*alts, i);
                } else {
                    self.live.insert(*s1, idx);
                    self.live.insert(*conss, idx);
                    self.live.insert(*alts, idx);
                }
                for i in alt.iter().rev() {
                    self.reg_alloc_inner(i, idx, target);
                }
                for i in cons.iter().rev() {
                    self.reg_alloc_inner(i, idx, target);
                }
                //self.reg_alloc_inner(&alt, target);
                //self.reg_alloc_inner(&cons, target);
            }
            // Already allocated in Phi
            IR::Move(_, _) => (),
            IR::Return(s) => {
                self.var_mapping.insert(*s, target);
                self.live.entry(*s).or_insert(idx);
            }
            IR::GotoIf(_, s) => { self.live.entry(*s).or_insert(idx); }
            IR::GotoIfNot(_, s) => { self.live.entry(*s).or_insert(idx); }
            IR::Goto(_) | IR::Label(_) => (),
            IR::Fn(s, _, _) => if !self.var_mapping.contains_key(&s) {
                panic!("Dead code?");
            },
            IR::Primitive(s, _) => if !self.var_mapping.contains_key(&s) {
                panic!("Dead code?");
            },
            IR::Lookup(s, _) => if !self.live.contains_key(&s) {
                panic!("Dead code?");
            },
            // Only used for optimization
            IR::Copy(_, _) => unreachable!(),
        }
    }
    */

    fn get_register(&mut self, s: Symbol, asm: &mut Vec<ASM>, idx: usize) -> Register {
        let r = self.lookup_register(s);
        if let Some(s) = self.used.get(&r) {
            if idx <= *self.live.get(s).unwrap() {
                //self.var_location.insert(*s, M::S(self.stack));
                self.var_stack.push(*s);
                self.stack += 1;
                asm.push(ASM::Save(r));
            }
        }
        self.used.insert(r, s);
        //self.var_location.insert(s, M::R(r));
        self.var_reg[r.0 as usize] = Some(s);
        r
    }

    fn load_symbol(&mut self, s: Symbol, target: Register, asm: &mut Vec<ASM>) {
        if self.var_reg[target.0 as usize] != Some(s) {
            if let Some(r) = self.var_reg.iter().position(|x| *x == Some(s)) {
                asm.push(ASM::Move(target, Register(r as u8)));
                self.var_reg[target.0 as usize] = Some(s);
            } else if let Some(p) = self.var_stack.iter().rev().position(|x| *x == s) {
                // TODO
                asm.push(ASM::ReadStack(target, p+1));
                self.var_reg[target.0 as usize] = Some(s);
            } else {
                println!("{}", ::string_interner::get_value(s).unwrap());
                unreachable!();
            }
        }
        /*
        match self.var_location.get(&s) {
            Some(M::R(t)) if *t == target => (),
            None => unreachable!("stens"),
            Some(M::R(r)) => {
                asm.push(ASM::Move(target, *r));
                self.var_location.insert(s, M::R(target));
            },
            Some(M::S(l)) => {
                asm.push(ASM::ReadStack(target, self.stack-l));
                self.var_location.insert(s, M::R(target));
            },
        }
        */
    }

    fn find_symbol(&mut self, s: Symbol, asm: &mut Vec<ASM>) -> Register {
        if let Some(r) = self.var_reg.iter().position(|x| *x == Some(s)) {
            Register(r as u8)
        } else if let Some(p) = self.var_stack.iter().rev().position(|x| *x == s) {
            // TODO
            let target = Register(18);
            asm.push(ASM::ReadStack(target, p+1));
            target
        } else {
            println!("{}", ::string_interner::get_value(s).unwrap());
            println!("{}", self.live.get(&s).unwrap());
            unreachable!();
        }
        /*
        match self.var_location.get(&s).unwrap() {
            M::R(r) => *r,
            M::S(l) => {
                // TODO
                let target = Register(18);
                asm.push(ASM::ReadStack(target, self.stack-l));
                //self.var_location.insert(s, M::R(target));
                target
            }
        }
        */
    }

    // TODO: Option
    fn lookup_register(&self, s: Symbol) -> Register {
        if let Some(&r) = self.var_mapping.get(&s) {
            r
        } else {
            Register(17)
        }
    }

    // Ok => Register
    // Err => Stack
    /*
    fn lookup(&self, s: Symbol) -> Option<Result<Register, usize>> {
        for (i, r) in self.registers.iter().enumerate() {
            if Some(s) == r {
                return Some(Ok(Register(i)));
            }
        }

        for (i, r) in self.stack.iter().rev().enumerate() {
            if s == r {
                return Some(Err(i));
            }
        }

        None
    }
    */
}

/*
fn get_register(used: &HashSet<Register>) -> Option<Register> {
    for i in 0..16 {
        if !used.contains(&Register(i)) {
            return Some(Register(i));
        }
    }
    None
}
*/
