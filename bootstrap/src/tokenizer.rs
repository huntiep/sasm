use std::collections::HashMap;
use std::sync::{LazyLock, Mutex};

pub static INTERNER: LazyLock<Mutex<HashMap<Vec<u8>, usize>>> = LazyLock::new(|| Mutex::new(HashMap::new()));

#[inline]
pub fn get_symbol(value: Vec<u8>) -> usize {
    let mut i = INTERNER.lock().unwrap();
    if i.contains_key(&value) {
        *i.get(&value).unwrap()
    } else {
        let s = i.len() + 1;
        i.insert(value, s);
        s
    }
}

#[inline]
pub fn get_value(s: usize) -> String {
    let i = INTERNER.lock().unwrap();
    for (k, v) in i.iter() {
        if *v == s {
            return String::from_utf8_lossy(k).to_string();
        }
    }
    panic!("INVARIANT BROKEN IN `get_value()`");
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    Pound,
    Symbol(usize),
    Integer(i64),
    Char(u8),
    String(usize, usize),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TokenInfo {
    pub line: usize,
    pub start: usize,
    pub end: usize,
}

pub struct Tokenizer {
    pub position: usize,
    pub input: Vec<u8>,
    pub token_position: usize,
    pub tokens: Vec<Token>,
    pub token_info: Vec<TokenInfo>,
    pub lines: Vec<(usize, usize)>,
    pub filename: String,
    pub err: bool,
}

impl Tokenizer {
    pub fn new(input: Vec<u8>, filename: String) -> Tokenizer {
        Tokenizer {
            position: 0,
            input: input,
            token_position: 0,
            tokens: Vec::new(),
            token_info: Vec::new(),
            lines: Vec::new(),
            filename: filename,
            err: false,
        }
    }

    pub fn next(&mut self) -> Option<Token> {
        assert!(self.token_position <= self.tokens.len());
        if self.token_position == self.tokens.len() {
            // shitty bc
            let t = self.tokenize()?;
            self.tokens.push(t);
        }
        self.token_position += 1;
        Some(self.tokens[self.token_position - 1])
    }

    pub fn peek(&mut self) -> Option<Token> {
        assert!(self.token_position <= self.tokens.len());
        if self.token_position == self.tokens.len() {
            // shitty bc
            let t = self.tokenize()?;
            self.tokens.push(t);
        }
        Some(self.tokens[self.token_position])
    }

    fn _next(&mut self) -> Option<u8> {
        if self.position < self.input.len() {
            self.position += 1;
            Some(self.input[self.position - 1])
        } else {
            None
        }
    }

    fn _peek(&self) -> Option<u8> {
        if self.position < self.input.len() {
            Some(self.input[self.position])
        } else {
            None
        }
    }

    fn tokenize(&mut self) -> Option<Token> {
        if let Some(c) = self._next() {
            match c {
                b'(' => {
                    self.token_info.push(TokenInfo { start: self.position-1, end: self.position, line: self.lines.len() });
                    Some(Token::LParen)
                }
                b')' => {
                    self.token_info.push(TokenInfo { start: self.position-1, end: self.position, line: self.lines.len() });
                    Some(Token::RParen)
                }
                b' ' | b'\t' | b'\r' => self.tokenize(),
                b'\n' => {
                    self.newline();
                    self.tokenize()
                }
                b';' => {
                    self.parse_comment();
                    self.tokenize()
                }
                b'"' => self.parse_string(),
                b'#' => self.parse_literal(),
                b'0' ..= b'9' | b'a' ..= b'f' | b'A' ..= b'F' | b'x' | b'o' | b'_' | b'+' | b'-' => self.parse_ambiguous(),
                _ => self.parse_identifier(self.position - 1, self.lines.len()),
            }
        } else {
            self.newline();
            None
        }
    }

    fn newline(&mut self) {
        let s = if let Some((_, e)) = self.lines.last() {
            *e
        } else {
            0
        };
        if s != self.position {
            self.lines.push((s, self.position));
        }
    }

    pub fn idx_in_line(&self, idx: usize) -> usize {
        for (s, e) in &self.lines {
            if *e > idx {
                return idx - *s;
            }
        }
        idx - self.lines.last().unwrap_or(&(0, 0)).1
    }

    fn print_err(&mut self, start: usize, line: usize, msg: &str) {
        let i = self.idx_in_line(start);
        let mut e = start;
        while e < self.input.len() && self.input[e] != b'\n' {
            e += 1;
        }
        if e < self.input.len() {
            e += 1;
        }
        let l = String::from_utf8_lossy(&self.input[start..e]);
        eprintln!("{} at {}:{}:{}.\n{}: {}", msg, self.filename, line+1, i+1, line+1, l);
        self.err = true;
    }

    fn parse_string(&mut self) -> Option<Token> {
        let start = self.position - 1;
        let line = self.lines.len();

        while let Some(c) = self._next() {
            if c == b'\\' {
                match self._next() {
                    Some(b'r') | Some(b'n') | Some(b't') | Some(b'0') | Some(b'\\') | Some(b'"') => (),
                    Some(c) => {
                        if c == b'\n' {
                            self.newline();
                        }
                        self.print_err(start, line, &format!("Invalid string escape code `\\{}`", c as char));
                    }
                    None => {
                        self.print_err(start, line, "Unclosed string beginning");
                        return None;
                    },
                }
            } else if c == b'\n' {
                self.newline();
            } else if c == b'"' {
                self.token_info.push(TokenInfo { start: start, end: self.position, line: line });
                return Some(Token::String(start+1, self.position - 1));
            }
        }
        self.print_err(start, line, "Unclosed string beginning");
        if line < self.lines.len() {
            self.position = self.lines[line].1;
            self.lines.truncate(line + 1);
        }
        self.token_info.push(TokenInfo { start: start-1, end: self.position, line: line });
        Some(Token::String(start, self.position - 1))
    }

    fn parse_literal(&mut self) -> Option<Token> {
        let start = self.position - 1;
        let line = self.lines.len();

        if let Some(c) = self._peek() {
            if c == b'|' {
                self.parse_block_comment(start, line);
                return self.tokenize();
            } else if c != b'\'' {
                self.token_info.push(TokenInfo { start: start, end: self.position, line: line });
                return Some(Token::Pound);
            }
        } else {
            self.print_err(start, line, "Expected array or character literal, got EOF");
            return None;
        }
        self._next();
        let ch = match self._next() {
            Some(b'\'') => {
                self.print_err(start, line, "Empty character literal");
                self.position -= 1;
                b'\0'
            }
            None => {
                self.print_err(start, line, "Unclosed char literal");
                return None;
            },
            Some(b'\n') => {
                self.newline();
                b'\n'
            }
            Some(b'\\') => match self._next() {
                None => {
                    self.print_err(start, line, "Unclosed char literal");
                    return None;
                }
                Some(b'\\') => b'\\',
                Some(b'\'') => b'\'',
                Some(b'r') => b'\r',
                Some(b'n') => b'\n',
                Some(b't') => b'\t',
                Some(b'0') => b'\0',
                Some(c) => {
                    if c == b'\n' {
                        self.newline();
                    }
                    self.print_err(start, line, &format!("Bad escape sequence `\\{}` in char literal", c as char));
                    b'\0'
                }
            }
            Some(c) => c,
        };

        if Some(b'\'') != self._next() {
            self.print_err(start, line, "Unclosed char literal");
        }
        self.token_info.push(TokenInfo { start: start, end: self.position, line: line });
        Some(Token::Char(ch))
    }

    fn parse_comment(&mut self) {
        while let Some(c) = self._next() {
            if c == b'\n' {
                self.newline();
                return;
            }
        }
    }

    fn parse_block_comment(&mut self, start: usize, line: usize) {
        let mut nesting = 0;
        while let Some(c) = self._next() {
            if c == b'|' && Some(b'#') == self._peek() {
                if nesting == 0 {
                    self._next();
                    return;
                } else {
                    nesting -= 1;
                }
            } else if c == b'#' && Some(b'|') == self._peek() {
                nesting += 1;
            }
        }
        self.print_err(start, line, "Unclosed block comment");
    }

    fn parse_identifier(&mut self, start: usize, line: usize) -> Option<Token> {
        while let Some(c) = self._peek() {
            match c {
                b' ' | b'\t' | b'\r' | b'\n' | b'(' | b')' | b'#' | b';' | b'"' => break,
                _ => self.position += 1,
            }
        }

        let s = get_symbol(self.input[start..self.position].to_vec());
        self.token_info.push(TokenInfo { start: start, end: self.position, line: line });
        Some(Token::Symbol(s))
    }

    fn parse_ambiguous(&mut self) -> Option<Token> {
        let start = self.position - 1;
        let line = self.lines.len();
        while let Some(c) = self._peek() {
            match c {
                // still ambiguous
                b'0' ..= b'9' | b'a' ..= b'f' | b'A' ..= b'F' | b'x' | b'o' | b'_' | b'+' | b'-' => (),
                b'(' | b')' | b'#' | b';' | b'"' | b' ' | b'\t' | b'\r' | b'\n' => break,
                _ => return self.parse_identifier(start, line),
            }
            self.position += 1;
        }
        self.distinguish_ambiguous(start, self.position, line)
    }

    fn distinguish_ambiguous(&mut self, start: usize, end: usize, line: usize) -> Option<Token> {
        self.token_info.push(TokenInfo { start: start, end: end, line: line });
        if let Some(i) = self.intp(start, end, line) {
            Some(Token::Integer(i))
        } else {
            let s = get_symbol(self.input[start..end].to_vec());
            Some(Token::Symbol(s))
        }
    }

    fn intp(&mut self, o_start: usize, end: usize, line: usize) -> Option<i64> {
        let mut start = o_start;
        let mut i: i64 = 0;
        let mut neg = false;
        if self.input[start] == b'+' || self.input[start] == b'-' {
            neg = self.input[start] == b'-';
            start += 1;
        }
        // Needed in case the input is just a sign
        if start == end || self.input[start] == b'_' {
            return None;
        }

        let mut decimal = true;
        let mut binary = false;
        let mut octal = false;
        let mut hex = false;
        if self.input[start] == b'0' && start + 1 != end {
            match self.input[start+1] {
                b'b' => {
                    binary = true;
                    decimal = false;
                    start += 2;
                }
                b'o' => {
                    octal = true;
                    decimal = false;
                    start += 2;
                }
                b'x' => {
                    hex = true;
                    decimal = false;
                    start += 2;
                }
                _ => ()
            }
        }

        while start < end && self.input[start] == b'_' {
            start += 1;
        }
        if start == end {
            return None;
        }

        let mut overflow = false;
        while start < end {
            match self.input[start] {
                b'_' => (),
                c @ b'0' | c @ b'1' if binary => {
                    i = if let Some(j) = i.checked_shl(1) {
                        j
                    } else {
                        overflow = true;
                        0
                    };
                    i |= (c - b'0') as i64;
                }
                c @ b'0' ..= b'7' if octal => {
                    i = if let Some(j) = i.checked_shl(3) {
                        j
                    } else {
                        overflow = true;
                        0
                    };
                    i |= (c - b'0') as i64;
                }
                c @ b'0' ..= b'9' if decimal => {
                    i = if let Some(j) = i.checked_mul(10) {
                        j
                    } else {
                        overflow = true;
                        0
                    };
                    i = if let Some(j) = i.checked_add((c - b'0') as i64) {
                        j
                    } else {
                        overflow = true;
                        0
                    };
                }
                c @ b'0' ..= b'9' if hex => {
                    i = if let Some(j) = i.checked_shl(4) {
                        j
                    } else {
                        overflow = true;
                        0
                    };
                    i |= (c - b'0') as i64;
                }
                c @ b'a' ..= b'f' if hex => {
                    i = if let Some(j) = i.checked_shl(4) {
                        j
                    } else {
                        overflow = true;
                        0
                    };
                    i |= (c - b'a' + 10) as i64;
                }
                c @ b'A' ..= b'F' if hex => {
                    i = if let Some(j) = i.checked_shl(4) {
                        j
                    } else {
                        overflow = true;
                        0
                    };
                    i |= (c - b'A' + 10) as i64;
                }
                _ => return None,
            }
            start += 1;
        }

        if overflow {
            let int = unsafe { std::str::from_utf8_unchecked(&self.input[o_start..end]) };
            self.print_err(o_start, line, &format!("Integer `{}` does not fit in i64", int));
            return Some(0);
        }

        if neg {
            i = -i;
        }
        Some(i)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn intp() {
        assert_eq!(Tokenizer::new(b"123".to_vec(), String::new()).next().unwrap(), Token::Integer(123));
        assert_eq!(Tokenizer::new(b"-123".to_vec(), String::new()).next().unwrap(), Token::Integer(-123));
        assert_eq!(Tokenizer::new(b"+123".to_vec(), String::new()).next().unwrap(), Token::Integer(123));
        assert_eq!(Tokenizer::new(b"0o123".to_vec(), String::new()).next().unwrap(), Token::Integer(0o123));
        assert_eq!(Tokenizer::new(b"-0o123".to_vec(), String::new()).next().unwrap(), Token::Integer(-0o123));
        assert_eq!(Tokenizer::new(b"+0o123".to_vec(), String::new()).next().unwrap(), Token::Integer(0o123));
        assert_eq!(Tokenizer::new(b"0x123".to_vec(), String::new()).next().unwrap(), Token::Integer(0x123));
        assert_eq!(Tokenizer::new(b"-0x123".to_vec(), String::new()).next().unwrap(), Token::Integer(-0x123));
        assert_eq!(Tokenizer::new(b"+0x123".to_vec(), String::new()).next().unwrap(), Token::Integer(0x123));
        assert_eq!(Tokenizer::new(b"0b101".to_vec(), String::new()).next().unwrap(), Token::Integer(0b101));
        assert_eq!(Tokenizer::new(b"-0b101".to_vec(), String::new()).next().unwrap(), Token::Integer(-0b101));
        assert_eq!(Tokenizer::new(b"+0b101".to_vec(), String::new()).next().unwrap(), Token::Integer(0b101));
        assert_eq!(Tokenizer::new(b"0xabc1".to_vec(), String::new()).next().unwrap(), Token::Integer(0xabc1));
        assert_eq!(Tokenizer::new(b"-0xabc1".to_vec(), String::new()).next().unwrap(), Token::Integer(-0xabc1));
        assert_eq!(Tokenizer::new(b"+0xabc1".to_vec(), String::new()).next().unwrap(), Token::Integer(0xabc1));

        assert!(!is_int(Tokenizer::new(b"_123".to_vec(), String::new()).next().unwrap()));
        assert!(!is_int(Tokenizer::new(b"+_123".to_vec(), String::new()).next().unwrap()));
        assert!(!is_int(Tokenizer::new(b"-_123".to_vec(), String::new()).next().unwrap()));
        assert_eq!(Tokenizer::new(b"1_2_3".to_vec(), String::new()).next().unwrap(), Token::Integer(123));
        assert_eq!(Tokenizer::new(b"0000___1_2_3__".to_vec(), String::new()).next().unwrap(), Token::Integer(123));
        assert_eq!(Tokenizer::new(b"0x00___1_2_3__".to_vec(), String::new()).next().unwrap(), Token::Integer(0x123));
        assert_eq!(Tokenizer::new(b"0x___1_2_3__".to_vec(), String::new()).next().unwrap(), Token::Integer(0x123));
        assert!(!is_int(Tokenizer::new(b"0x___".to_vec(), String::new()).next().unwrap()));
    }

    fn is_int(t: Token) -> bool {
        match t {
            Token::Integer(_) => true,
            _ => false,
        }
    }
}
