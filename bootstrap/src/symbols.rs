use tokenizer::get_symbol;

pub const X31: usize = 32;
pub const ADD: usize = 33;
pub const SUB: usize = 34;
pub const SRA: usize = 40;
pub const MUL: usize = 43;
pub const DIV: usize = 44;
pub const REM: usize = 45;
pub const LAST_R: usize = 45;

pub const SUBI: usize = 47;
pub const SRAI: usize = 53;
pub const LAST_I: usize = 55;

pub const LAST_I2: usize = 62;

pub const LAST_S: usize = 66;

pub const LAST_B: usize = 72;

pub const JAL: usize = 73;
pub const JALR: usize = 74;
pub const LUI: usize = 75;
pub const AUIPC: usize = 76;
pub const ECALL: usize = 77;
pub const EBREAK: usize = 78;
pub const LA: usize = 79;

pub const LAST_INSTRUCTION: usize = 80;

pub const LEN: usize = 80;
pub const PLUS: usize = 81;
pub const NEG: usize = 82;
pub const IMPORT: usize = 83;
pub const MODULE: usize = 84;
pub const CARAT: usize = 85;
pub const STAR: usize = 86;
pub const INCLUDE: usize = 87;
pub const DEFINE: usize = 88;
pub const DEFVAR: usize = 89;
pub const DEFCON: usize = 90;
pub const LAMBDA: usize = 91;
pub const IF: usize = 92;
pub const SET: usize = 93;
pub const DEFMACRO: usize = 94;
pub const QUOTE: usize = 95;
pub const QUASIQUOTE: usize = 96;
pub const UNQUOTE: usize = 97;
pub const LIST: usize = 98;
pub const DOT: usize = 99;

pub fn init() {
    assert_eq!(get_symbol(b"x0".to_vec()), 1);
    get_symbol(b"x1".to_vec());
    get_symbol(b"x2".to_vec());
    get_symbol(b"x3".to_vec());
    get_symbol(b"x4".to_vec());
    get_symbol(b"x5".to_vec());
    get_symbol(b"x6".to_vec());
    get_symbol(b"x7".to_vec());
    get_symbol(b"x8".to_vec());
    get_symbol(b"x9".to_vec());
    get_symbol(b"x10".to_vec());
    get_symbol(b"x11".to_vec());
    get_symbol(b"x12".to_vec());
    get_symbol(b"x13".to_vec());
    get_symbol(b"x14".to_vec());
    get_symbol(b"x15".to_vec());
    get_symbol(b"x16".to_vec());
    get_symbol(b"x17".to_vec());
    get_symbol(b"x18".to_vec());
    get_symbol(b"x19".to_vec());
    get_symbol(b"x20".to_vec());
    get_symbol(b"x21".to_vec());
    get_symbol(b"x22".to_vec());
    get_symbol(b"x23".to_vec());
    get_symbol(b"x24".to_vec());
    get_symbol(b"x25".to_vec());
    get_symbol(b"x26".to_vec());
    get_symbol(b"x27".to_vec());
    get_symbol(b"x28".to_vec());
    get_symbol(b"x29".to_vec());
    get_symbol(b"x30".to_vec());
    assert_eq!(get_symbol(b"x31".to_vec()), X31);

    get_symbol(b"add".to_vec());
    get_symbol(b"sub".to_vec());
    get_symbol(b"xor".to_vec());
    get_symbol(b"or".to_vec());
    get_symbol(b"and".to_vec());
    get_symbol(b"sll".to_vec());
    get_symbol(b"srl".to_vec());
    get_symbol(b"sra".to_vec());
    get_symbol(b"slt".to_vec());
    get_symbol(b"sltu".to_vec());
    get_symbol(b"mul".to_vec());
    get_symbol(b"div".to_vec());
    assert_eq!(get_symbol(b"rem".to_vec()), LAST_R);

    get_symbol(b"addi".to_vec());
    get_symbol(b"subi".to_vec());
    get_symbol(b"xori".to_vec());
    get_symbol(b"ori".to_vec());
    get_symbol(b"andi".to_vec());
    get_symbol(b"slti".to_vec());
    get_symbol(b"sltiu".to_vec());
    get_symbol(b"srai".to_vec());
    get_symbol(b"slli".to_vec());
    assert_eq!(get_symbol(b"srli".to_vec()), LAST_I);

    get_symbol(b"lb".to_vec());
    get_symbol(b"lh".to_vec());
    get_symbol(b"lw".to_vec());
    get_symbol(b"ld".to_vec());
    get_symbol(b"lbu".to_vec());
    get_symbol(b"lhu".to_vec());
    assert_eq!(get_symbol(b"lwu".to_vec()), LAST_I2);

    get_symbol(b"sb".to_vec());
    get_symbol(b"sh".to_vec());
    get_symbol(b"sw".to_vec());
    assert_eq!(get_symbol(b"sd".to_vec()), LAST_S);

    get_symbol(b"beq".to_vec());
    get_symbol(b"bne".to_vec());
    get_symbol(b"blt".to_vec());
    get_symbol(b"bge".to_vec());
    get_symbol(b"bltu".to_vec());
    assert_eq!(get_symbol(b"bgeu".to_vec()), LAST_B);

    get_symbol(b"jal".to_vec());
    get_symbol(b"jalr".to_vec());
    get_symbol(b"lui".to_vec());
    get_symbol(b"auipc".to_vec());
    get_symbol(b"ecall".to_vec());
    get_symbol(b"ebreak".to_vec());
    get_symbol(b"la".to_vec());

    get_symbol(b"len".to_vec());
    get_symbol(b"+".to_vec());
    assert_eq!(get_symbol(b"-".to_vec()), 82);
    get_symbol(b"import".to_vec());
    get_symbol(b"module".to_vec());
    get_symbol(b"^".to_vec());
    get_symbol(b"*".to_vec());
    get_symbol(b"include!".to_vec());
    get_symbol(b"define".to_vec());
    get_symbol(b"defvar".to_vec());
    get_symbol(b"defcon".to_vec());
    get_symbol(b"lambda".to_vec());
    get_symbol(b"if".to_vec());
    get_symbol(b"set!".to_vec());
    get_symbol(b"defmacro".to_vec());
    get_symbol(b"quote".to_vec());
    get_symbol(b"quasiquote".to_vec());
    get_symbol(b"unquote".to_vec());
    get_symbol(b"list".to_vec());
    get_symbol(b".".to_vec());
}
