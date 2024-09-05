use value::Value;

#[repr(u32)]
enum Instruction {
    Eq          = 0,
    Add         = 1,
    Sub         = 2,
    Mul         = 3,
    Div         = 4,
    Rem         = 5,
    Equal       = 6,
    LT          = 7,
    GE          = 8,
    Cons        = 9,
    Car         = 10,
    Cdr         = 11,
    SetCar      = 12,
    SetCdr      = 13,
    MakeBVec    = 14,
    BVecPush    = 15,
    BVecPop     = 16,
    BVecRef     = 17,
    BVecSet     = 18,
    MakeVec     = 19,
    VecPush     = 20,
    VecPop      = 21,
    VecRef      = 22,
    VecSet      = 23,
    MakeMap     = 24,
    MapLookup   = 25,
    MapInsert   = 26,
    MapRemove   = 27,
    Define      = 28,
    Set         = 29,
    Call        = 30,
    Goto        = 31,
    Beq         = 32,
    Ret         = 33,
    LoadConst   = 34,
}

struct Vm {
    registers: [Value; 32],
    instructions: Vec<u32>,
    pc: usize,
}

impl Vm {
    pub fn run(&mut self) {
        let instruction = instructions[pc];
        let opcode = (instruction & 0b1111_1111) as Instruction;
        let rd = (instruction >> 8) & 0b11111;
        let rs1 = (instruction >> 13) & 0b1111;
        let rs2 = (instruction >> 18) & 0b1111;
        match opcode {
            Eq => registers[rd] = Value::Bool(registers[rs1] == registers[rs2]),
            Add | Sub | Mul | Div | Rem | Equal | LT | GE => {
                let l = self.registers[rs1];
                let r = self.registers[rs2];
                if l.intp() && r.intp() {
                    let l = l.to_int();
                    let r = r.to_int();
                    self.registers[rd] = match opcode {
                        Add => Value::Int(l + r),
                        Sub => Value::Int(l - r),
                        Mul => Value::Int(l * r),
                        Div => Value::Int(l / r),
                        Rem => Value::Int(l % r),
                        Equal => Value::Bool(l == r),
                        LT => Value::Bool(l < r),
                        GE => Value::Bool(l >= r),
                    };
                } else {
                    // error
                }
            }
            Cons => self.registers[rd] = Value::Pair(self.registers[rs1], self.registers[rs2]),
            Car => {
                if registers[rs].pairp() {
                    registers[rd] = registers[rs].car();
                } else {
                    // error
                }
            },
            Cdr => {
                if registers[rs].pairp() {
                    registers[rd] = registers[rs].cdr();
                } else {
                    // error
                }
            },
            SetCar => {
                // TODO
                if registers[rd].pairp() {
                    registers[rd].set_car(registers[rs1]);
                } else {
                    // error
                }
            },
            SetCdr => {
                // TODO
                if registers[rd].pairp() {
                    registers[rd].set_cdr(registers[rs1]);
                } else {
                    // error
                }
            },
            MakeBVec => registers[rd] = Value::bvec(registers[rs1].unwrap_int()),
            BVecPush => {
                if registers[rd].bvecp() && registers[rs1].intp() {
                    /*
                    let mut v = registers[rd].unwrap_bvec();
                    let b = registers[rs1].unwrap_int();
                    if b < 0 || b >= 256 {
                        // error
                    } else {
                        v.vec.push(b);
                    }
                    */
                } else {
                    // error
                }
            }
            BVecPop => {
                if registers[rs1].bvecp() {
                    /*
                    let mut v = registers[rs1].unwrap_bvec();
                    if v.vec.len() == 0 {
                        // TODO: error?
                    } else {
                        registers[rd] = Value::int(v.vec.pop());
                    }
                    */
                } else {
                    // error
                }
            },
            BVecRef => {
                if registers[rs1].bvecp() && registers[rs2].intp() {
                    /*
                    let mut v = registers[rs1].unwrap_bvec();
                    let i = registers[rs2].unwrap_int();
                    if i < 0 || i >= v.vec.len() {
                        // error
                    } else {
                        registers[rd] = Value::int(v.vec[i]);
                    }
                    */
                } else {
                    // error
                }
            },
            BVecSet => {
                if registers[rd].bvecp() && registers[rs1].intp() && registers[rs2].intp() {
                    /*
                    let mut v = registers[rd].unwrap_bvec();
                    let i = registers[rs1].unwrap_int();
                    let b = registers[rs2].unwrap_int();
                    if i < 0 || i >= v.vec.len() {
                        // error
                    } else if b < 0 || b >= 256 {
                        // error
                    } else {
                        v.vec[i] = b;
                    }
                    */
                } else {
                    // error
                }
            },
            MakeVec => registers[rd] = Value::Vec(registers[rs1].to_int()),
            VecPush => {
                if registers[rd].vecp() {
                    //registers[rd].unwrap_vec().vec.push(registers[rs1]);
                } else {
                    // error
                }
            },
            VecPop => {
                if registers[rs1].vecp() {
                    /*
                    let mut v = registers[rs1].unwrap_vec();
                    if v.vec.len() == 0 {
                        // TODO: error?
                    } else {
                        registers[rd] = v.vec.pop();
                    }
                    */
                } else {
                    // error
                }
            },
            VecRef => {
                if registers[rs1].vecp() && registers[rs2].intp() {
                    /*
                    let mut v = registers[rs1].unwrap_vec();
                    let i = registers[rs2].unwrap_int();
                    if i < 0 || i >= v.vec.len() {
                        // error
                    } else {
                        registers[rd] = v.vec[i];
                    }
                    */
                } else {
                    // error
                }
            },
            VecSet => {
                if registers[rd].vecp() && registers[rs1].intp() {
                    /*
                    let mut v = registers[rd].unwrap_vec();
                    let i = registers[rs1].unwrap_int();
                    if i < 0 || i >= v.vec.len() {
                        // error
                    } else {
                        v.vec[i] = registers[rs2];
                    }
                    */
                } else {
                    // error
                }
            },
            Define => {
                // TOOD
            },
            Set => {
                // TOOD
            },
        }
    }
}
