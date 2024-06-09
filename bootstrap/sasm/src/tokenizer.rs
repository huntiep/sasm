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
    line: usize,
    start: usize,
    end: usize,
}

pub fn tokenize(input: Vec<u8>, filename: String) -> Tokenizer {
    let mut t = Tokenizer {
        position: 0,
        input: input,
        tokens: Vec::new(),
        token_info: Vec::new(),
        lines: Vec::new(),
        filename: filename,
        err: false,
    };
    t.tokenize();
    t
}

pub struct Tokenizer {
    pub position: usize,
    pub input: Vec<u8>,
    pub tokens: Vec<Token>,
    pub token_info: Vec<TokenInfo>,
    pub lines: Vec<(usize, usize)>,
    pub filename: String,
    pub err: bool,
}

impl Tokenizer {
    fn next(&mut self) -> Option<u8> {
        if self.position < self.input.len() {
            self.position += 1;
            Some(self.input[self.position - 1])
        } else {
            None
        }
    }

    fn peek(&self) -> Option<u8> {
        if self.position < self.input.len() {
            Some(self.input[self.position])
        } else {
            None
        }
    }

    fn tokenize(&mut self) {
        while let Some(c) = self.next() {
            match c {
                b'(' => {
                    self.tokens.push(Token::LParen);
                    self.token_info.push(TokenInfo { start: self.position-1, end: self.position, line: self.lines.len() });
                }
                b')' => {
                    self.tokens.push(Token::RParen);
                    self.token_info.push(TokenInfo { start: self.position-1, end: self.position, line: self.lines.len() });
                }
                b' ' | b'\t' | b'\r' => (),
                b'\n' => self.newline(),
                b';' => self.parse_comment(),
                b'"' => self.parse_string(),
                b'#' => self.parse_literal(),
                b'0' ..= b'9' | b'+' | b'-' => self.parse_ambiguous(),
                _ => self.parse_identifier(self.position - 1, self.lines.len()),
            }
        }
        self.newline();
    }

    fn newline(&mut self) {
        let s = if let Some((_, e)) = self.lines.last() {
            *e
        } else {
            0
        };
        self.lines.push((s, self.position));
    }

    fn idx_in_line(&self, idx: usize) -> usize {
        for (s, e) in &self.lines {
            if *e > idx {
                return idx-*s;
            }
        }
        idx
    }

    fn parse_string(&mut self) {
        let start = self.position;
        let line = self.lines.len();

        while let Some(c) = self.next() {
            if c == b'\\' {
                match self.next() {
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
                        return;
                    },
                }
            } else if c == b'\n' {
                self.newline();
            } else if c == b'"' {
                self.tokens.push(Token::String(start, self.position-1));
                self.token_info.push(TokenInfo { start: start-1, end: self.position, line: line });
                return;
            }
        }
        eprintln!("Unclosed string beginning on line {line} at index {} in file `{}`.", self.idx_in_line(start), self.filename);
        self.err = true;
        if line < self.lines.len() {
            self.position = self.lines[line].1;
            self.tokens.push(Token::String(start, self.position-1));
            self.token_info.push(TokenInfo { start: start-1, end: self.position, line: line });
        }
    }

    fn parse_literal(&mut self) {
        let start = self.position - 1;
        let line = self.lines.len();

        if let Some(c) = self.peek() {
            if c != b'\'' {
                self.tokens.push(Token::Pound);
                self.token_info.push(TokenInfo { start: start, end: self.position, line: line });
                return;
            }
        } else {
            eprintln!("Unexpected EOF on line {line} at index {} in file `{}`: expected array or character literal.", self.idx_in_line(start), self.filename);
            self.err = true;
            return;
        }
        self.next();
        let ch = match self.next() {
            Some(b'\'') => {
                eprintln!("Empty character literal beginning on line {line} at index {} in file `{}`.", self.idx_in_line(start), self.filename);
                self.err = true;
                b'\0'
            }
            None => {
                eprintln!("Unclosed char literal on line {line} at index {} in file `{}`.", self.idx_in_line(start), self.filename);
                self.err = true;
                return;
            },
            Some(b'\n') => {
                self.newline();
                b'\n'
            }
            Some(b'\\') => match self.next() {
                None => {
                    eprintln!("Unclosed char literal on line {line} at index {} in file `{}`.", self.idx_in_line(start), self.filename);
                    self.err = true;
                    return;
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

        if let Some(c) = self.next() {
            if c == b'\'' {
                self.tokens.push(Token::Char(ch));
                self.token_info.push(TokenInfo { start: start, end: self.position, line: line });
            } else {
                eprintln!("Unclosed char literal on line {line} at index {} in file `{}`.", self.idx_in_line(start), self.filename);
                self.err = true;
            }
        } else {
            eprintln!("Unclosed char literal on line {line} at index {} in file `{}`.", self.idx_in_line(start), self.filename);
            self.err = true;
        }
    }

    fn parse_comment(&mut self) {
        while let Some(c) = self.next() {
            if c == b'\n' {
                self.newline();
                return;
            }
        }
    }

    fn parse_identifier(&mut self, start: usize, line: usize) {
        while let Some(c) = self.peek() {
            match c {
                b' ' | b'\t' | b'\r' | b'\n' | b'(' | b')' | b'#' | b';' | b'"' => break,
                _ => self.position += 1,
            }
        }

        let s = get_symbol(self.input[start..self.position].to_vec());
        self.tokens.push(Token::Symbol(s));
        self.token_info.push(TokenInfo { start: start, end: self.position, line: line });
    }

    fn parse_ambiguous(&mut self) {
        let start = self.position - 1;
        let line = self.lines.len();
        while let Some(c) = self.peek() {
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

    fn distinguish_ambiguous(&mut self, start: usize, end: usize, line: usize) {
        if let Some(i) = self.intp(start, end, line) {
            self.tokens.push(Token::Integer(i));
        } else {
            let s = get_symbol(self.input[start..end].to_vec());
            self.tokens.push(Token::Symbol(s));
        }
        self.token_info.push(TokenInfo { start: start, end: end, line: line });
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
