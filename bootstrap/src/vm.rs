#![allow(non_upper_case_globals, non_snake_case)]
use {Unit, Value};

// TODO: set values
pub mod Opcode {
    // General
    pub const Constant: u8 =    1;
    pub const Lookup: u8 =      2;
    pub const PushStack: u8 =   26;
    pub const PopStack: u8 =    27;
    pub const ReadStack: u8 =   3;
    pub const SetStack: u8 =    4;
    pub const Move: u8 =        5;
    pub const Set: u8 =         6;
    pub const Define: u8 =      28;
    pub const Call: u8 =        7;
    pub const Ret: u8 =         8;
    pub const Jump: u8 =        9;
    pub const JumpReg: u8 =     10;
    // Conditional
    pub const Beq: u8 =         11;
    pub const Blt: u8 =         12;
    pub const Bge: u8 =         13;
    // List
    pub const Cons: u8 =        14;
    pub const Car: u8 =         15;
    pub const Cdr: u8 =         16;
    pub const SetCar: u8 =      17;
    pub const SetCdr: u8 =      18;
    // Math
    //Neg,
    pub const Add: u8 =         19;
    pub const Sub: u8 =         20;
    pub const Mul: u8 =         21;
    pub const Div: u8 =         22;
    pub const Rem: u8 =         23;
    // Symbol
    pub const SymToStr: u8 =    24;
    pub const StrToSym: u8 =    25;
    // String
    // Map
    // Vec
}

pub struct VM {
    pub registers: [Value; 32],
    stack: Vec<Value>,
    pub bytecode: Vec<u8>,
    callstack: Vec<(usize, Value)>,
    lambda: Value,
    //constants: Vec<Value>,
    //kontinue: usize,
}

impl VM {
    pub fn new() -> Self {
        VM {
            registers: [Value(0); 32],
            stack: Vec::new(),
            bytecode: Vec::new(),
            callstack: Vec::new(),
            lambda: Value(0),
        }
    }

    pub fn reset(&mut self) {
        for i in 0..32 {
            self.registers[i] = Value(0);
        }
        self.stack.clear();
    }

    pub fn run(&mut self, lambda: Value) {
        self.lambda = lambda;
        let mut ip = 0;
        loop {
            match self.bytecode[ip] {
                Opcode::Constant => {
                    println!("constant");
                    let reg = self.bytecode[ip+1] as usize;
                    let mut value = self.bytecode[ip+2] as u64;
                    value = value << 8;
                    value = value | self.bytecode[ip+3] as u64;
                    value = value << 8;
                    value = value | self.bytecode[ip+4] as u64;
                    value = value << 8;
                    value = value | self.bytecode[ip+5] as u64;
                    value = value << 8;
                    value = value | self.bytecode[ip+6] as u64;
                    value = value << 8;
                    value = value | self.bytecode[ip+7] as u64;
                    value = value << 8;
                    value = value | self.bytecode[ip+8] as u64;
                    value = value << 8;
                    value = value | self.bytecode[ip+9] as u64;
                    self.registers[reg] = Value(value);
                    ip += 9;
                }
                Opcode::Lookup => {
                    println!("lookup");
                    let to = self.bytecode[ip+1] as usize;
                    let mut value = self.bytecode[ip+2] as u64;
                    value = value << 8;
                    value = value | self.bytecode[ip+3] as u64;
                    value = value << 8;
                    value = value | self.bytecode[ip+4] as u64;
                    value = value << 8;
                    value = value | self.bytecode[ip+5] as u64;
                    // TODO
                    let mut l = self.lambda.to_lambda();
                    let (_, v) = l.env.children.get(value).unwrap();
                    if let Unit::Value(v) = v {
                        self.registers[to] = v;
                    }
                    Box::into_raw(l);
                    ip += 5;
                }
                Opcode::PushStack => {
                    println!("push");
                    let reg = self.bytecode[ip+1] as usize;
                    self.stack.push(self.registers[reg]);
                    ip += 1;
                }
                Opcode::PopStack => {
                    println!("pop");
                    let reg = self.bytecode[ip+1] as usize;
                    // TODO: len check
                    self.registers[reg] = self.stack.pop().unwrap();
                    ip += 1;
                }
                Opcode::Move => {
                    println!("move");
                    let to = self.bytecode[ip+1] as usize;
                    let from = self.bytecode[ip+2] as usize;
                    self.registers[to] = self.registers[from];
                    ip += 2;
                }
                Opcode::Define => {
                    println!("define");
                    let from = self.bytecode[ip+1] as usize;
                    let mut value = self.bytecode[ip+2] as u64;
                    value = value << 8;
                    value = value | self.bytecode[ip+3] as u64;
                    value = value << 8;
                    value = value | self.bytecode[ip+4] as u64;
                    value = value << 8;
                    value = value | self.bytecode[ip+5] as u64;
                    let mut l = self.lambda.to_lambda();
                    l.env.children.insert(value, (false, Unit::Value(self.registers[from])));
                    Box::into_raw(l);
                    ip += 5;
                }
                Opcode::Call => {
                    println!("call");
                    let from = self.bytecode[ip+1] as usize;
                    ip += 2;
                    let lambda = self.registers[from];
                    self.callstack.push((ip, self.lambda));
                    ip = 0;
                    self.lambda = lambda;
                    continue;
                }
                Opcode::Ret => {
                    println!("ret");
                    if self.callstack.is_empty() {
                        return;
                    } else {
                        (ip, self.lambda) = self.callstack.pop().unwrap();
                        continue;
                    }
                }
                Opcode::Jump => {
                    println!("jump");
                    let mut jump = self.bytecode[ip+1] as isize;
                    jump = (jump << 8) | self.bytecode[ip+2] as isize;
                    ip = ((ip as isize) + jump) as usize;
                    //continue;
                }
                Opcode::JumpReg => {
                    let reg = self.bytecode[ip+1] as usize;
                    ip = ((ip as isize) + self.registers[reg].0 as isize) as usize;
                    continue;
                }
                Opcode::Beq => {
                    println!("beq");
                    let left = self.bytecode[ip+1] as usize;
                    let right = self.bytecode[ip+2] as usize;
                    ip += 4;
                    if self.registers[left] == self.registers[right] {
                        let mut jump = self.bytecode[ip-1] as isize;
                        jump = (jump << 8) | self.bytecode[ip] as isize;
                        ip = ((ip as isize) + jump) as usize;
                        //continue;
                    }
                }
                Opcode::Cons => {
                    println!("cons");
                    let to = self.bytecode[ip+1] as usize;
                    let car = self.bytecode[ip+2] as usize;
                    let cdr = self.bytecode[ip+3] as usize;
                    ip += 3;
                    let pair = Value::Pair(self.registers[car], self.registers[cdr]);
                    self.registers[to] = pair;
                }
                Opcode::Car => {
                    println!("car");
                    let to = self.bytecode[ip+1] as usize;
                    let from = self.bytecode[ip+2] as usize;
                    ip += 2;
                    self.registers[to] = self.registers[from].car();
                }
                Opcode::Cdr => {
                    println!("cdr");
                    let to = self.bytecode[ip+1] as usize;
                    let from = self.bytecode[ip+2] as usize;
                    ip += 2;
                    self.registers[to] = self.registers[from].cdr();
                }
                Opcode::Add => {
                    println!("add");
                    let to = self.bytecode[ip+1] as usize;
                    let left = self.bytecode[ip+2] as usize;
                    let right = self.bytecode[ip+3] as usize;
                    ip += 3;
                    let left = self.registers[left];
                    let right = self.registers[right];
                    if left.intp() && right.intp() {
                        self.registers[to] = Value::Integer(left.to_integer() + right.to_integer());
                    }
                }
                _ => unreachable!(),
            }
            ip += 1;
        }
    }
}

/*
impl VM {
    pub fn run(&mut self) -> Value {
        let mut ip = 0;
        loop {
            match self.bytecode[ip] {
                Opcode::Ret => return self.stack.pop().unwrap(),
                Opcode::Neg => {
                    let v = self.stack.pop().unwrap();
                    if v.intp() {
                        self.stack.push(Value::Integer(-v.to_integer()));
                    } else if v.bigintp() {
                        todo!();
                    } else if v.floatp() {
                        self.stack.push(Value::Float(-v.to_float()));
                    } else {
                        todo!();
                    }
                }
                Opcode::Add => {
                }
                Opcode::Sub => {
                }
                Opcode::Mul => {
                }
                Opcode::Div => {
                }
                Opcode::Rem => {
                }
            }
            ip += 1;
        }
    }
}
*/
