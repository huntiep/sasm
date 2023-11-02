#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Hash)]
pub struct Register(pub(crate) u8);

deref!(Register, u8);

impl Register {
    pub const X0: Self = Register(0);
    pub const X1: Self = Register(1);
    pub const X2: Self = Register(2);
    pub const X3: Self = Register(3);
    pub const X4: Self = Register(4);
    pub const X5: Self = Register(5);
    pub const X6: Self = Register(6);
    pub const X7: Self = Register(7);
    pub const X8: Self = Register(8);
    pub const X9: Self = Register(9);
    pub const X10: Self = Register(10);
    pub const X11: Self = Register(11);
    pub const X12: Self = Register(12);
    pub const X13: Self = Register(13);
    pub const X14: Self = Register(14);
    pub const X15: Self = Register(15);
    pub const X16: Self = Register(16);
    pub const X17: Self = Register(17);
    pub const X18: Self = Register(18);
    pub const X19: Self = Register(19);
    pub const X20: Self = Register(20);
    pub const X21: Self = Register(21);
    pub const X22: Self = Register(22);
    pub const X23: Self = Register(23);
    pub const X24: Self = Register(24);
    pub const X25: Self = Register(25);
    pub const X26: Self = Register(26);
    pub const X27: Self = Register(27);
    pub const X28: Self = Register(28);
    pub const X29: Self = Register(29);
    pub const X30: Self = Register(30);
    pub const X31: Self = Register(31);

    pub fn from_str(input: &str) -> Option<Self> {
        Some(match input {
            "x0" | "X0" => Register(0),
            "x1" | "X1" => Register(1),
            "x2" | "X2" => Register(2),
            "x3" | "X3" => Register(3),
            "x4" | "X4" => Register(4),
            "x5" | "X5" => Register(5),
            "x6" | "X6" => Register(6),
            "x7" | "X7" => Register(7),
            "x8" | "X8" => Register(8),
            "x9" | "X9" => Register(9),
            "x10" | "X10" => Register(10),
            "x11" | "X11" => Register(11),
            "x12" | "X12" => Register(12),
            "x13" | "X13" => Register(13),
            "x14" | "X14" => Register(14),
            "x15" | "X15" => Register(15),
            "x16" | "X16" => Register(16),
            "x17" | "X17" => Register(17),
            "x18" | "X18" => Register(18),
            "x19" | "X19" => Register(19),
            "x20" | "X20" => Register(20),
            "x21" | "X21" => Register(21),
            "x22" | "X22" => Register(22),
            "x23" | "X23" => Register(23),
            "x24" | "X24" => Register(24),
            "x25" | "X25" => Register(25),
            "x26" | "X26" => Register(26),
            "x27" | "X27" => Register(27),
            "x28" | "X28" => Register(28),
            "x29" | "X29" => Register(29),
            "x30" | "X30" => Register(30),
            "x31" | "X31" => Register(31),
            _ => return None,
        })
    }

    pub fn as_u32(self) -> u32 {
        self.0 as u32
    }
}
