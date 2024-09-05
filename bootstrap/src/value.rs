use Symbol;

#[repr(Transparent)]
#[derive(Copy, Clone, PartialEq, PartialOrd, Eq)]
struct Value(pub u64);

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
    pub const Void: Self = Value::new(NAN | VOID_TAG);
    is_imm!(voidp, VOID_TAG);

    pub const Nil: Self = Value::new(NAN | NIL_TAG);
    is_imm!(nilp, NIL_TAG);

    pub const fn Bool(b: bool) -> Self {
        if b { Self::True } else { Self::False }
    }
    is_imm!(boolp, BOOL_TAG);

    pub const True: Self = Value::new(NAN | BOOL_TAG | TRUE);
    // TODO: make this const
    pub fn is_true(self) -> bool {
        self == Self::True
    }

    pub const False: Self = Value::new(NAN | BOOL_TAG | FALSE);
    // TODO: make this const
    pub fn is_false(self) -> bool {
        self == Self::False
    }

    pub const fn Integer(i: i32) -> Self {
        Value::new(NAN | INT_TAG | (i as u32 as u64))
    }
    is_imm!(intp, INT_TAG);

    pub const fn to_integer(self) -> i32 {
        self.0 as u32 as i32
    }

    pub fn Symbol(s: Symbol) -> Self {
        debug_assert!(*s < (1 << 32));
        Value::new(NAN | SYMBOL_TAG | (*s as u64))
    }
    is_imm!(symbolp, SYMBOL_TAG);

    pub fn to_symbol(self) -> Symbol {
        Symbol::new(self.0 as u32 as usize)
    }

    pub fn BigInt(int: i64) -> Self {
        let next = get_head();
        let bigint = Box::into_raw(Box::new(BigInt::new(next, int)));
        let p = bigint as u64;
        set_head(p, VType::BigInt);
        Value::new(NAN | BIGINT_TAG | (p & ((1 << 48) - 1)))
    }

    pub fn Pair(car: Self, cdr: Self) -> Self {
        let next = get_head();
        let pair = Box::into_raw(Box::new(Pair::new(next, car, cdr)));
        let p = pair as u64;
        set_head(p, VType::Pair);
        Value::new(NAN | PAIR_TAG | (p & ((1 << 48) - 1)))
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
        Value::new(NAN | BVEC_TAG | (p & ((1 << 48) - 1)))
    }
    is_pointer!(bvecp, VEC_TAG);
    to_pointer!(to_bvec, BVec);

    //pub fn Vec(v: Vec<Self>) -> Self {
    pub fn Vec(cap: i64) -> Self {
        let next = get_head();
        let v = Vec::with_capacity(cap as usize);
        let vec = Box::into_raw(Box::new(SVec::new(next, v)));
        let p = vec as u64;
        set_head(p, VType::Vec);
        Value::new(NAN | VEC_TAG | (p & ((1 << 48) - 1)))
    }
    is_pointer!(is_vec, VEC_TAG);
    to_pointer!(to_vec, SVec);
}

pub mod heap_repr {
    use super::Value;

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
}
