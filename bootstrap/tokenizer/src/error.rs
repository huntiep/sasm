use std::fmt::{self, Display, Formatter};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenizeError {
    Char,
    EOF,
}

impl Display for TokenizeError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            TokenizeError::Char => write!(f, "Invalid character in char"),
            TokenizeError::EOF => write!(f, "Unexpected end of input"),
        }
    }
}
