use tokenizer::{get_symbol, get_value};
use Value;

use std::mem;

pub fn add(args: Vec<Value>) -> Value {
    let mut v = 0;
    for a in args {
        if a.intp() {
            v += a.to_integer() as i64;
        } else if a.bigintp() {
            let b = a.to_bigint();
            v += b.int;
            let _ = Box::into_raw(b);
        } else {
            todo!();
        }
    }
    Value::Int(v)
}

pub fn lt(args: Vec<Value>) -> Value {
    if args.len() != 2 || !args[0].intp() || !args[1].intp() {
        todo!()
    } else {
        Value::Bool(args[0].to_integer() < args[1].to_integer())
    }
}

pub fn cons(args: Vec<Value>) -> Value {
    if args.len() != 2 {
        todo!()
    } else {
        Value::Pair(args[0], args[1])
    }
}

pub fn car(args: Vec<Value>) -> Value {
    if args.len() != 1 || !args[0].pairp() {
        todo!()
    } else {
        args[0].car()
    }
}

pub fn cdr(args: Vec<Value>) -> Value {
    if args.len() != 1 || !args[0].pairp() {
        todo!()
    } else {
        args[0].cdr()
    }
}

pub fn list(args: Vec<Value>) -> Value {
    let mut p = Value::Nil;
    for i in 0..args.len() {
        p = Value::Pair(args[args.len() - 1 - i], p);
    }
    p
}

pub fn read_file(args: Vec<Value>) -> Value {
    if args.len() != 1 || !args[0].bvecp() {
        todo!()
    } else {
        let s = args[0].to_bvec();
        let v = std::fs::read(String::from_utf8(s.vec.clone()).unwrap()).unwrap();
        let _ = Box::into_raw(s);
        Value::bvec_from_vec(v)
    }
}

pub fn makevec(args: Vec<Value>) -> Value {
    if !args.is_empty() {
        todo!()
    } else {
        Value::Vec(1)
    }
}

pub fn veclen(args: Vec<Value>) -> Value {
    if args.len() != 1 || !args[0].bvecp() {
        todo!()
    } else {
        let v = args[0].to_bvec();
        let x = Value::Integer(v.vec.len() as i32);
        let _ = Box::into_raw(v);
        x
    }
}

pub fn vecref(args: Vec<Value>) -> Value {
    if args.len() != 2 || !args[0].bvecp() || !args[1].intp() {
        todo!()
    } else {
        let i = args[1].to_integer();
        let v = args[0].to_bvec();
        if i < 0 || i as usize > v.vec.len() {
            todo!()
        }
        let x = Value::Integer(v.vec[i as usize] as i32);
        let _ = Box::into_raw(v);
        x
    }
}

pub fn vecpush(args: Vec<Value>) -> Value {
    if args.len() != 2 || (!args[0].bvecp() && !args[0].vecp()) {
        todo!()
    }
    if args[0].bvecp() {
        let i = args[1].to_integer();
        if !args[1].intp() || i < 0 || i > 255 {
            todo!()
        }
        let mut v = args[0].to_bvec();
        v.vec.push(i as u8);
        let _ = Box::into_raw(v);
    } else {
        let mut v = args[0].to_vec();
        v.vec.push(args[1]);
        let _ = Box::into_raw(v);
    }
    Value::Void
}

pub fn string_to_symbol(args: Vec<Value>) -> Value {
    if args.len() != 1 && !args[0].bvecp() {
        todo!()
    } else {
        // TODO: validate symbol
        let s = args[0].to_bvec();
        let sym = Value::Symbol(get_symbol(s.vec.clone()));
        let _ = Box::into_raw(s);
        sym
    }
}

pub fn symbol_to_string(args: Vec<Value>) -> Value {
    if args.len() != 1 && !args[0].symbolp() {
        todo!()
    } else {
        let sym = args[0].to_symbol();
        let mut s = get_value(sym).into_bytes();

        let v = Value::BVec(s.len() as i64);
        let mut bv = v.to_bvec();
        mem::swap(&mut s, &mut bv.vec);
        let _ = Box::into_raw(bv);
        v
    }
}

pub fn string_append(args: Vec<Value>) -> Value {
    let ret = Value::BVec(0);
    let mut sum = ret.to_bvec();
    for a in args {
        if !a.bvecp() {
            todo!()
        }
        let v = a.to_bvec();
        let mut vc = v.vec.clone();
        let _ = Box::into_raw(v);
        sum.vec.append(&mut vc);
    }
    let _ = Box::into_raw(sum);
    ret
}

pub fn eqp(args: Vec<Value>) -> Value {
    if args.len() < 2 {
        todo!()
    }
    for i in 1..args.len() {
        if args[i-1] != args[i] {
            return Value::False
        }
    }
    Value::True
}

pub fn itoa(args: Vec<Value>) -> Value {
    if args.len() != 1 || !args[0].intp() {
        todo!()
    }

    Value::bvec_from_vec(args[0].to_integer().to_string().into_bytes())
}

pub fn print(args: Vec<Value>) -> Value {
    for a in args {
        println!("{}", a);
    }
    Value::Void
}
