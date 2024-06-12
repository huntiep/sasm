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
                b'0' ..= b'9' | b'+' | b'-' => self.parse_ambiguous(),
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

    fn idx_in_line(&self, idx: usize) -> usize {
        for (s, e) in &self.lines {
            if *e > idx {
                return idx-*s;
            }
        }
        idx
    }

    fn parse_string(&mut self) -> Option<Token> {
        let start = self.position;
        let line = self.lines.len();

        while let Some(c) = self._next() {
            if c == b'\\' {
                match self._next() {
                    Some(b'r') | Some(b'n') | Some(b't') | Some(b'0') | Some(b'\\') | Some(b'"') => (),
                    Some(c) => {
                        let c = c as char;
                        let start = self.idx_in_line(self.position - 2);
                        let end = self.idx_in_line(self.position);
                        eprintln!("Invalid string escape code `\\{c}` on line {line} at index {start}:{end} in file `{}`.", self.filename);
                        self.err = true;
                    },
                    None => {
                        eprintln!("Unclosed string beginning on line {line} at index {} in file `{}`.", self.idx_in_line(start), self.filename);
                        self.err = true;
                        return None;
                    },
                }
            } else if c == b'\n' {
                self.newline();
            } else if c == b'"' {
                self.token_info.push(TokenInfo { start: start-1, end: self.position, line: line });
                return Some(Token::String(start, self.position - 1));
            }
        }
        eprintln!("Unclosed string beginning on line {line} at index {} in file `{}`.", self.idx_in_line(start), self.filename);
        self.err = true;
        if line < self.lines.len() {
            self.position = self.lines[line].1;
        }
        self.token_info.push(TokenInfo { start: start-1, end: self.position, line: line });
        Some(Token::String(start, self.position - 1))
    }

    fn parse_literal(&mut self) -> Option<Token> {
        let start = self.position - 1;
        let line = self.lines.len();

        if let Some(c) = self._peek() {
            if c != b'\'' {
                self.token_info.push(TokenInfo { start: start, end: self.position, line: line });
                return Some(Token::Pound);
            }
        } else {
            eprintln!("Unexpected EOF on line {line} at index {} in file `{}`: expected array or character literal.", self.idx_in_line(start), self.filename);
            self.err = true;
            return None;
        }
        self._next();
        let ch = match self._next() {
            Some(b'\'') => {
                eprintln!("Empty character literal beginning on line {line} at index {} in file `{}`.", self.idx_in_line(start), self.filename);
                self.err = true;
                b'\0'
            }
            None => {
                eprintln!("Unclosed char literal on line {line} at index {} in file `{}`.", self.idx_in_line(start), self.filename);
                self.err = true;
                return None;
            },
            Some(b'\n') => {
                self.newline();
                b'\n'
            }
            Some(b'\\') => match self._next() {
                None => {
                    eprintln!("Unclosed char literal on line {line} at index {} in file `{}`.", self.idx_in_line(start), self.filename);
                    self.err = true;
                    return None;
                }
                Some(b'\\') => b'\\',
                Some(b'\'') => b'\'',
                Some(b'r') => b'\r',
                Some(b'n') => b'\n',
                Some(b't') => b'\t',
                Some(b'0') => b'\0',
                Some(c) => {
                    let c = c as char;
                    eprintln!("Bad escape sequence `\\{c}` in char literal on line {line} at index {} in file `{}`.", self.idx_in_line(start), self.filename);
                    self.err = true;
                    b'\0'
                }
            }
            Some(c) => c,
        };

        if let Some(c) = self._next() {
            if c == b'\'' {
                self.token_info.push(TokenInfo { start: start, end: self.position, line: line });
                return Some(Token::Char(ch));
            } else {
                eprintln!("Unclosed char literal on line {line} at index {} in file `{}`.", self.idx_in_line(start), self.filename);
                self.err = true;
            }
        } else {
            eprintln!("Unclosed char literal on line {line} at index {} in file `{}`.", self.idx_in_line(start), self.filename);
            self.err = true;
        }
        None
    }

    fn parse_comment(&mut self) {
        while let Some(c) = self._next() {
            if c == b'\n' {
                self.newline();
                return;
            }
        }
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
                b'0' ..= b'9' | b'+' | b'-' => (),
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
        if start == end {
            return None;
        }

        let mut overflow = false;
        while start < end {
            match self.input[start] {
                c @ b'0' ..= b'9' => {
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
                _ => return None,
            }
            start += 1;
        }

        if overflow {
            let int = unsafe { std::str::from_utf8_unchecked(&self.input[o_start..end]) };
            eprintln!("Integer `{}` does not fit in i64 on line {line} at index {} in file `{}`.", int, self.idx_in_line(start), self.filename);
            self.err = true;
            return Some(0);
        }

        if neg {
            i = -i;
        }
        Some(i)
    }
}
