use Value;

#[repr(u8)]
enum Op {
    Neg,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Ret,
}

pub struct VM {
    stack: Vec<Value>,
    bytecode: Vec<Op>,
    constants: Vec<Value>,
}

impl VM {
    pub fn run(&mut self) -> Value {
        let mut ip = 0;
        loop {
            match self.bytecode[ip] {
                Op::Ret => return self.stack.pop().unwrap(),
                Op::Neg => {
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
                Op::Add => {
                }
                Op::Sub => {
                }
                Op::Mul => {
                }
                Op::Div => {
                }
                Op::Rem => {
                }
            }
            ip += 1;
        }
    }
}
