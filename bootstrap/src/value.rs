#![allow(non_upper_case_globals, non_snake_case)]
// TODO
#![allow(unused)]

use {symbols, Ast, Symbol, Token};
use self::heap_repr::*;

use std::fmt;

pub enum VType {
    Void = 0,
    Nil = 1,
    Bool = 2,
    Integer = 3,
    Float = 4,
    Symbol = 5,
    Lambda = 6,
    Pair = 7,
    BVec = 8,
    Vec = 9,
    HashMap = 11,
    BigInt = 12,
}

fn get_head() -> u64 {
    0
}

fn set_head(p: u64, ty: VType) {
}

impl From<u64> for VType {
    fn from(p: u64) -> VType {
        if p == VType::BVec as u64 {
            VType::BVec
        } else if p == VType::Vec as u64 {
            VType::Vec
        } else if p == VType::Lambda as u64 {
            VType::Lambda
        } else if p == VType::Pair as u64 {
            VType::Pair
        } else if p == VType::HashMap as u64 {
            VType::HashMap
        } else if p == VType::BigInt as u64 {
            VType::BigInt
        } else if p == VType::Void as u64 {
            VType::Void
        } else {
            unreachable!()
        }
    }
}

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Debug)]
pub struct Value(pub u64);

const NAN: u64 = 0x7FF0000000000000;
const TAG_MASK: u64 = 0b111 << 48;
const IMMEDIATE_MASK: u64 = 0b1111 << 44;

const IMMEDIATE_TAG: u64 = 0b000 << 48;
const VOID_TAG: u64 =   0b0001 << 44;
const NIL_TAG: u64 =    0b0010 << 44;
const BOOL_TAG: u64 =   0b0011 << 44;
const INT_TAG: u64 =    0b0100 << 44;
const SYMBOL_TAG: u64 = 0b0101 << 44;
const TRUE: u64 = 1;
const FALSE: u64 = 0;

const LAMBDA_TAG: u64 = 0b001 << 48;
const PAIR_TAG: u64 =   0b010 << 48;
const BVEC_TAG: u64 =    0b011 << 48;
const HASHMAP_TAG: u64 = 0b100 << 48;
const BIGINT_TAG: u64 = 0b101 << 48;
const VEC_TAG: u64 = 0b110 << 48;

macro_rules! is_imm {
    ($name:ident, $tag:ident) => {
        pub const fn $name(self) -> bool {
            ((self.0 & NAN) == NAN) && ((self.0 & TAG_MASK) == IMMEDIATE_TAG)
                && ((self.0 & IMMEDIATE_MASK) == $tag)
        }
    };
}

macro_rules! is_pointer {
    ($name:ident, $tag:ident) => {
        pub const fn $name(self) -> bool {
            ((self.0 & NAN) == NAN) && ((self.0 & TAG_MASK) == $tag)
        }
    };
}

macro_rules! to_pointer {
    ($name:ident, $t:ty) => {
        pub fn $name(self) -> Box<$t> {
            let pointer = self.to_pointer();
            unsafe { Box::from_raw(pointer as *mut $t) }
        }
    };
}

impl Value {
    pub fn ty(self) -> VType {
        if self.0 & NAN != NAN {
            VType::Float
        } else if self.0 & TAG_MASK == IMMEDIATE_TAG {
            match self.0 & IMMEDIATE_MASK {
                VOID_TAG => VType::Void,
                NIL_TAG => VType::Nil,
                BOOL_TAG => VType::Bool,
                INT_TAG => VType::Integer,
                SYMBOL_TAG => VType::Symbol,
                _ => unreachable!(),
            }
        } else {
            match self.0 & TAG_MASK {
                LAMBDA_TAG => VType::Lambda,
                PAIR_TAG => VType::Pair,
                BVEC_TAG => VType::BVec,
                HASHMAP_TAG => VType::HashMap,
                BIGINT_TAG => VType::BigInt,
                VEC_TAG => VType::Vec,
                _ => unreachable!(),
            }
        }
    }

    pub const Void: Self = Value(NAN | VOID_TAG);
    is_imm!(voidp, VOID_TAG);

    pub const Nil: Self = Value(NAN | NIL_TAG);
    is_imm!(nilp, NIL_TAG);

    pub const fn Bool(b: bool) -> Self {
        if b { Self::True } else { Self::False }
    }
    is_imm!(boolp, BOOL_TAG);

    pub const True: Self = Value(NAN | BOOL_TAG | TRUE);
    // TODO: make this const
    pub fn is_true(self) -> bool {
        self == Self::True
    }

    pub const False: Self = Value(NAN | BOOL_TAG | FALSE);
    // TODO: make this const
    pub fn is_false(self) -> bool {
        self == Self::False
    }

    // TODO
    pub fn truthy(self) -> bool {
        !self.is_false()
    }

    pub const fn Float(f: f64) -> Self {
        Value(f.to_bits())
    }

    pub fn to_float(self) -> f64 {
        f64::from_bits(self.0)
    }

    pub fn floatp(self) -> bool {
        self.0 & NAN != NAN
    }

    pub fn Int(i: i64) -> Self {
        if i > i32::MAX as i64 || i < i32::MIN as i64 {
            Self::BigInt(i)
        } else {
            Self::Integer(i as i32)
        }
    }

    pub const fn Integer(i: i32) -> Self {
        Value(NAN | INT_TAG | (i as u32 as u64))
    }
    is_imm!(intp, INT_TAG);

    pub const fn to_integer(self) -> i32 {
        self.0 as u32 as i32
    }

    pub fn Symbol(s: Symbol) -> Self {
        debug_assert!(s < (1 << 32));
        Value(NAN | SYMBOL_TAG | (s as u64))
    }
    is_imm!(symbolp, SYMBOL_TAG);

    pub fn to_symbol(self) -> Symbol {
        self.0 as u32 as usize
    }

    pub fn BigInt(int: i64) -> Self {
        let next = get_head();
        let bigint = Box::into_raw(Box::new(BigInt::new(next, int)));
        let p = bigint as u64;
        set_head(p, VType::BigInt);
        Value(NAN | BIGINT_TAG | (p & ((1 << 48) - 1)))
    }
    is_pointer!(bigintp, BIGINT_TAG);
    to_pointer!(to_bigint, BigInt);

    pub fn Pair(car: Self, cdr: Self) -> Self {
        let next = get_head();
        let pair = Box::into_raw(Box::new(Pair::new(next, car, cdr)));
        let p = pair as u64;
        set_head(p, VType::Pair);
        Value(NAN | PAIR_TAG | (p & ((1 << 48) - 1)))
    }
    is_pointer!(pairp, PAIR_TAG);
    to_pointer!(to_pair, Pair);

    pub fn car(self) -> Self {
        let p = self.to_pair();
        let c = p.car;
        Box::into_raw(p);
        c
    }

    pub fn cdr(self) -> Self {
        let p = self.to_pair();
        let c = p.cdr;
        Box::into_raw(p);
        c
    }

    pub fn set_car(self, v: Self) {
        let mut p = self.to_pair();
        p.car = v;
        Box::into_raw(p);
    }

    pub fn set_cdr(self, v: Self) {
        let mut p = self.to_pair();
        p.cdr = v;
        Box::into_raw(p);
    }

    //pub fn BVec(v: Vec<u8>) -> Self {
    pub fn BVec(cap: i64) -> Self {
        let next = get_head();
        let v = Vec::with_capacity(cap as usize);
        let vec = Box::into_raw(Box::new(BVec::new(next, v)));
        let p = vec as u64;
        set_head(p, VType::BVec);
        Value(NAN | BVEC_TAG | (p & ((1 << 48) - 1)))
    }
    is_pointer!(bvecp, BVEC_TAG);
    to_pointer!(to_bvec, BVec);

    pub fn bvec_from_vec(v: Vec<u8>) -> Self {
        let next = get_head();
        let vec = Box::into_raw(Box::new(BVec::new(next, v)));
        let p = vec as u64;
        set_head(p, VType::BVec);
        Value(NAN | BVEC_TAG | (p & ((1 << 48) - 1)))
    }

    //pub fn Vec(v: Vec<Self>) -> Self {
    pub fn Vec(cap: i64) -> Self {
        let next = get_head();
        let v = Vec::with_capacity(cap as usize);
        let vec = Box::into_raw(Box::new(SVec::new(next, v)));
        let p = vec as u64;
        set_head(p, VType::Vec);
        Value(NAN | VEC_TAG | (p & ((1 << 48) - 1)))
    }
    is_pointer!(vecp, VEC_TAG);
    to_pointer!(to_vec, SVec);

    pub fn to_pointer(self) -> u64 {
        Self::sign_extend(self.0, 47)
    }

    fn sign_extend(n: u64, at: u32) -> u64 {
        ((n.checked_shl(63 - at).unwrap() as i64) >> 63-at) as u64
    }

    pub fn Lambda(l: Ast) -> Self {
        let next = get_head();
        let l = match l {
            // TODO: macros, variadic
            Ast::Lambda { variadic, args, body } => Lambda::new(next, false, variadic, args, body),
            _ => unreachable!(),
        };
        let lambda = Box::into_raw(Box::new(l));
        let p = lambda as u64;
        set_head(p, VType::Lambda);
        Value(NAN | LAMBDA_TAG | (p & ((1 << 48) - 1)))
    }
    pub fn LambdaNative(f: fn(Vec<Value>) -> Value) -> Self {
        let next = get_head();
        let lambda = Box::into_raw(Box::new(Lambda::new_native(next, f)));
        let p = lambda as u64;
        set_head(p, VType::Lambda);
        Value(NAN | LAMBDA_TAG | (p & ((1 << 48) - 1)))
    }
    is_pointer!(lambdap, LAMBDA_TAG);
    to_pointer!(to_lambda, Lambda);

    pub fn to_tokens(self, tokens: &mut Vec<Token>) {
        if self.nilp() {
            todo!();
        } else if self.pairp() {
            tokens.push(Token::LParen);
            let mut p = self.to_pair();
            p.car.to_tokens(tokens);
            let mut c = p.cdr;
            while c.pairp() {
                Box::into_raw(p);
                p = c.to_pair();
                p.car.to_tokens(tokens);
                c = p.cdr;
            }
            Box::into_raw(p);
            if c.nilp() {
                tokens.push(Token::RParen);
            } else {
                todo!();
            }
        } else if self.intp() {
            tokens.push(Token::Integer(self.to_integer() as i64));
        } else if self.bigintp() {
            let b = self.to_bigint();
            tokens.push(Token::Integer(b.int));
            Box::into_raw(b);
        } else if self.symbolp() {
            tokens.push(Token::Symbol(self.to_symbol()));
        }
    }

    pub fn to_ast(self) -> Ast {
        match self.ty() {
            VType::Symbol => Ast::Ident(self.to_symbol()),
            VType::Bool | VType::Integer | VType::Float | VType::BigInt | VType::BVec => Ast::Constant(self),
            VType::Pair => {
                let c = self.car();
                if c.symbolp() {
                    match c.to_symbol() {
                        symbols::QUOTE => {
                            let v = self.cdr();
                            if v.nilp() || !v.cdr().nilp() {
                                todo!()
                            }
                            return Ast::Constant(self.cdr().car());
                        }
                        symbols::IF => {
                            // TODO
                            let alt = self.cdr().cdr().cdr();
                            let mut alternative = Ast::Constant(Value::Void);
                            if !alt.nilp() {
                                alternative = alt.car().to_ast();
                            }
                            return Ast::If {
                                predicate: Box::new(self.cdr().car().to_ast()),
                                consequent: Box::new(self.cdr().cdr().car().to_ast()),
                                alternative: Box::new(alternative),
                            };
                        }
                        symbols::SET => {
                            // TODO
                            return Ast::Set {
                                ident: self.cdr().car().to_symbol(),
                                expr: Box::new(self.cdr().cdr().car().to_ast()),
                            };
                        }
                        symbols::LAMBDA => {
                            // TODO
                            let mut variadic = false;
                            let mut args = self.cdr().car();
                            let mut sargs = Vec::new();
                            while args.pairp() {
                                sargs.push(args.car().to_symbol());
                                args = args.cdr();
                            }
                            if !args.nilp() {
                                variadic = true;
                                sargs.push(args.to_symbol());
                            }
                            let mut body = Vec::new();
                            let mut b = self.cdr().cdr();
                            while !b.nilp() {
                                body.push(b.car().to_ast());
                                b = b.cdr();
                            }
                            return Ast::Lambda {
                                variadic: variadic,
                                args: sargs,
                                body: body,
                            };
                        }
                        _ => {}
                    }
                }
                let proc = c.to_ast();
                let mut args = Vec::new();
                let mut pair = self.cdr();
                while !pair.nilp() {
                    args.push(pair.car().to_ast());
                    pair = pair.cdr();
                }
                Ast::Application(Box::new(proc), args)
            }
            _ => todo!(),
        }
    }
}
pub mod heap_repr {
    use {Ast, Symbol};
    use super::Value;

    use std::collections::HashMap;


    pub struct BigInt {
        pub(crate) gc: u64,
        pub int: i64,
    }

    impl BigInt {
        pub fn new(gc: u64, int: i64) -> Self {
            BigInt {
                gc: gc,
                int,
            }
        }
    }

    pub struct Pair {
        pub(crate) gc: u64,
        pub car: Value,
        pub cdr: Value,
    }

    impl Pair {
        pub fn new(gc: u64, car: Value, cdr: Value) -> Self {
            Pair {
                gc: gc,
                car,
                cdr,
            }
        }
    }

    pub struct BVec {
        pub(crate) gc: u64,
        pub vec: Vec<u8>,
    }

    impl BVec {
        pub fn new(gc: u64, v: Vec<u8>) -> Self {
            BVec {
                gc: gc,
                vec: v,
            }
        }
    }

    pub struct SVec {
        pub(crate) gc: u64,
        pub vec: Vec<Value>,
    }

    impl SVec {
        pub fn new(gc: u64, v: Vec<Value>) -> Self {
            SVec {
                gc: gc,
                vec: v,
            }
        }
    }

    pub struct SHashMap {
        pub(crate) gc: u64,
        pub map: HashMap<Value, Value>,
    }

    impl SHashMap {
        pub fn new(gc: u64, m: HashMap<Value, Value>) -> Self {
            SHashMap {
                gc: gc,
                map: m,
            }
        }
    }

    pub enum LambdaE {
        Native(fn(Vec<Value>) -> Value),
        Ast {
            macrop: bool,
            variadicp: bool,
            args: Vec<Symbol>,
            body: Vec<Ast>,
        }
    }

    pub struct Lambda {
        pub(crate) gc: u64,
        pub f: LambdaE,
    }

    impl Lambda {
        pub fn new(gc: u64, macrop: bool, variadicp: bool, args: Vec<Symbol>, body: Vec<Ast>) -> Self {
            Lambda {
                gc: gc,
                f: LambdaE::Ast {
                    macrop,
                    variadicp,
                    args,
                    body,
                },
            }
        }

        pub fn new_native(gc: u64, f: fn(Vec<Value>) -> Value) -> Self {
            Lambda {
                gc: gc,
                f: LambdaE::Native(f),
            }
        }

        pub fn nativep(&self) -> bool {
            match self.f {
                LambdaE::Native(_) => true,
                _ => false,
            }
        }
    }
}

/*
impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}
*/

    use std::io::Write;
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.floatp() {
            write!(f, "{}", self.to_float())
        } else if self.intp() {
            write!(f, "{}", self.to_integer())
        } else if self.symbolp() {
            let s = self.to_symbol();
            write!(f, "{}", super::get_value(s))
        } else if self.is_true() {
            write!(f, "#t")
        } else if self.is_false() {
            write!(f, "#f")
        } else if self.nilp() {
            write!(f, "()")
        } else if self.voidp() {
            Ok(())
        } else if self.lambdap() {
            write!(f, "#<procedure>")
        } else if self.pairp() {
            let p = Value::to_pair(*self);

            write!(f, "({}", p.car)?;
            let mut c = p.cdr;
            while c.pairp() {
                let p = Value::to_pair(c);
                write!(f, " {}", p.car)?;
                c = p.cdr;
                Box::into_raw(p);
            }
            let r = if c.nilp() {
                write!(f, ")")
            } else {
                write!(f, " . {})", c)
            };

            Box::into_raw(p);
            r
        } else if self.bvecp() {
            let vec = Value::to_bvec(*self);
            if let Ok(s) = String::from_utf8(vec.vec.clone()) {
                write!(f, "\"{}\"", s)?;
            } else {
                write!(f, "{:?}", vec.vec)?;
            }
            Box::into_raw(vec);
            Ok(())
                /*
        } else if self.is_string() {
            let s = Value::to_string(*self);
            let r = write!(f, "\"{}\"", s.str);
            Box::into_raw(s);
            r
                */
        } else if self.vecp() {
            let vec = Value::to_vec(*self);
            write!(f, "#(")?;
            for (i, v) in vec.vec.iter().enumerate() {
                if i+1 != vec.vec.len() {
                    write!(f, "{}, ", v)?;
                } else {
                    write!(f, "{}", v)?;
                }
            }
            Box::into_raw(vec);
            write!(f, ")")
        } else {
            write!(f, "debug: ")
            //write!(f, "debug: {:?}", self)
        }
    }
}
