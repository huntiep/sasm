use super::{Index, TokenizeError, Token};
use super::Token as T;

use std::iter::Peekable;
use std::str::Chars;

type TokenizeResult = Result<(), TokenizeError>;

// TODO: consider implementing this as an iterator
// This ought to be more efficient, especially if we use a buffered reader as well, but I'm not
// sure how we can handle errors if we take this approach. We might have the Item be
// Result<Token, TokenizeError>.
pub struct Tokenizer<'a> {
    position: usize,
    raw_input: &'a str,
    input: Peekable<Chars<'a>>,
    tokens: Vec<Token>,
}

macro_rules! pt {
    ($ty:expr, $s:ident) => {
        $s.tokens.push($ty(Index::new($s.position, $s.position)))
    };
}

impl<'a> Tokenizer<'a> {
    pub fn tokenize(raw_input: &'a str) -> Result<Vec<Token>, TokenizeError> {
        let input = raw_input.chars().peekable();
        let mut tokenizer = Tokenizer {
            position: 0,
            raw_input: raw_input,
            input: input,
            tokens: Vec::new(),
        };
        tokenizer._tokenize()?;

        Ok(tokenizer.tokens)
    }

    fn next(&mut self) -> Option<char> {
        if let Some(c) = self.input.next() {
            // We don't just increment self.position here because Rust chars are UTF-8 which can be
            // 1-4 bytes. Slicing rust strings is by byte, so we must have accurate byte indexes in
            // order to retrieve the proper information.
            self.position += c.len_utf8();
            Some(c)
        } else {
            None
        }
    }

    fn peek(&mut self) -> Option<char> {
        match self.input.peek() {
            Some(c) => Some(*c),
            None => None,
        }
    }

    fn _tokenize(&mut self) -> TokenizeResult {
        while let Some(c) = self.next() {
            match c {
                '(' => pt!(T::LParen, self),
                ')' => pt!(T::RParen, self),
                '{' => pt!(T::LBrace, self),
                '}' => pt!(T::RBrace, self),
                '[' => pt!(T::LSBracket, self),
                ']' => pt!(T::RSBracket, self),
                '"' => self.parse_string()?,
                '|' => self.parse_identifier(self.position, true)?,
                ';' => self.parse_comment()?,
                '#' => match self.peek() {
                    Some('|') => self.parse_block_comment()?,
                    Some('\'') => self.parse_char()?,
                    _ => pt!(T::Pound, self),
                },
                c if c.is_whitespace() => {}
                '.' => self.parse_ambiguous()?,
                '0' ..= '9' | '+' | '-' | 'i' | 'n' | 'f' | 'N' | 'a' => self.parse_ambiguous()?,
                _ => {
                    let start = self.position;
                    if '\\' == c && self.next().is_none() {
                        return Err(TokenizeError::EOF);
                    }
                    self.parse_identifier(start, false)?;
                }
            }
        }
        Ok(())
    }

    fn parse_ambiguous(&mut self) -> TokenizeResult {
        let start = self.position;

        while let Some(c) = self.next() {
            match c {
                '0' ..= '9' | '+' | '-' | '/' | '.' | 'e' | 'i' | 'n' | 'f' | 'N' | 'a' => (),
                '(' | '{' | '[' => {
                    self.distinguish_ambiguous(start)?;
                    match c {
                        '(' => pt!(T::LParen, self),
                        '{' => pt!(T::LBrace, self),
                        '[' => pt!(T::LSBracket, self),
                        _ => unreachable!(),
                    }
                    return Ok(());
                }
                ')' | '}' | ']' => {
                    self.distinguish_ambiguous(start)?;
                    match c {
                        ')' => pt!(T::RParen, self),
                        '}' => pt!(T::RBrace, self),
                        ']' => pt!(T::RSBracket, self),
                        _ => unreachable!(),
                    }
                    return Ok(());
                }
                _ if c.is_whitespace() => break,
                '\\' => match self.next() {
                    Some(_) => return self.parse_identifier(start, false),
                    None => return Err(TokenizeError::EOF),
                },
                _ => return self.parse_identifier(start, c == '|'),
            }
        }
        self.distinguish_ambiguous(start)
    }

    fn distinguish_ambiguous(&mut self, start: usize) -> TokenizeResult {
        let end = if self.raw_input.len() == self.position {
            self.position
        } else {
            // TODO: might need char::len_utf8
            self.position - 1
        };

        let buf = &self.raw_input[start-1..end];
        let index = Index::new(start, end);

        if intp(buf) {
            self.tokens.push(T::Integer(index));
        } else if floatp(buf) {
            self.tokens.push(T::Float(index));
        } else {
            self.tokens.push(T::Symbol(index));
        }
        Ok(())
    }

    fn parse_identifier(&mut self, start: usize, mut in_bar: bool) -> TokenizeResult {
        while let Some(c) = self.next() {
            match c {
                '\\' => match self.next() {
                    Some(_) => (),
                    None => return Err(TokenizeError::EOF),
                },
                '|' => in_bar = !in_bar,
                c if is_delimiter(c) => if !in_bar {
                    self.tokens.push(T::Symbol(Index::new(start, self.position - c.len_utf8())));
                    return match c {
                        c if c.is_whitespace() => Ok(()),
                        '(' => Ok(pt!(T::LParen, self)),
                        ')' => Ok(pt!(T::RParen, self)),
                        '{' => Ok(pt!(T::LBrace, self)),
                        '}' => Ok(pt!(T::RBrace, self)),
                        '[' => Ok(pt!(T::LSBracket, self)),
                        ']' => Ok(pt!(T::RSBracket, self)),
                        '"' => self.parse_string(),
                        ';' => self.parse_comment(),
                        _ => panic!("Tokenizer error"),
                    };
                },
                _ => (),
            }
        }
        self.tokens.push(T::Symbol(Index::new(start, self.position)));
        Ok(())
    }

    pub fn parse_string(&mut self) -> TokenizeResult {
        let start = self.position;
        while let Some(c) = self.next() {
            match c {
                '\\' => if let Some(c) = self.next() {
                    match c {
                        'n' | 't' => (),
                        // TODO: handle other escapes
                        _ => (),
                    }
                } else {
                    return Err(TokenizeError::EOF);
                },
                '"' => {
                    self.tokens.push(T::String(Index::new(start, self.position)));
                    return Ok(());
                }
                _ => (),
            }
        }
        Err(TokenizeError::EOF)
    }

    pub fn parse_char(&mut self) -> TokenizeResult {
        let start = self.position;
        self.next();
        match self.next() {
            Some('\\') => match self.next() {
                Some('"') | Some('\\') | Some('\'') | Some('r') | Some('n') | Some('t') | Some('0') => (),
                Some(_) => return Err(TokenizeError::Char),
                None => return Err(TokenizeError::EOF),
            },
            Some('\'') => return Err(TokenizeError::Char),
            Some(_) => (),
            None => return Err(TokenizeError::EOF),
        }

        match self.next() {
            Some('\'') => {
                self.tokens.push(T::Char(Index::new(start, self.position)));
                Ok(())
            }
            Some(_) => Err(TokenizeError::Char),
            None => Err(TokenizeError::EOF),
        }
    }

    fn parse_block_comment(&mut self) -> TokenizeResult {
        // TODO: maybe -1 here
        let start = self.position;
        let mut nesting = 1;
        while let Some(c) = self.next() {
            match c {
                '|' => match self.next() {
                    Some('#') => {
                        nesting -= 1;
                        if nesting == 0 {
                            self.tokens.push(T::BlockComment(Index::new(start, self.position)));
                            return Ok(());
                        }
                    }
                    Some(_) => (),
                    None => return Err(TokenizeError::EOF),
                },
                '#' => match self.next() {
                    Some('|') => nesting += 1,
                    Some(_) => (),
                    None => return Err(TokenizeError::EOF),
                },
                _ => (),
            }
        }
        Err(TokenizeError::EOF)
    }

    fn parse_comment(&mut self) -> TokenizeResult {
        let start = self.position;
        while let Some(c) = self.next() {
            match c {
                '\\' => match self.next() {
                    Some(_) => (),
                    // TODO: maybe this should be an EOF error? probably not though
                    None => break,
                },
                '\n' => break,
                _ => (),
            }
        }
        self.tokens.push(T::Comment(Index::new(start, self.position)));
        Ok(())
    }
}

fn is_delimiter(c: char) -> bool {
    match c {
        '(' | '{' | '[' => true,
        ')' | '}' | ']' => true,
        c if c.is_whitespace() => true,
        '"' | ';' => true,
        _ => false,
    }
}

fn intp(input: &str) -> bool {
    let input = if input.as_bytes()[0] == b'+' || input.as_bytes()[0] == b'-' {
        &input[1..]
    } else {
        input
    };

    // Needed in case `input` is just a sign
    if input.len() == 0 {
        return false;
    }

    for ch in input.chars() {
        match ch {
            '0' ..= '9' => (),
            _ => return false,
        }
    }

    true
}

fn floatp(input: &str) -> bool {
    let input = if input.as_bytes()[0] == b'+' || input.as_bytes()[0] == b'-' {
        &input[1..]
    } else {
        input
    };

    // Needed in case `input` is just a sign
    if input.len() == 0 {
        return false;
    }

    if input == "NaN" || input == "inf" {
        return true;
    }

    let mut decimal = false;
    let mut exponent = false;
    let mut chars = input.chars().peekable();
    while let Some(ch) = chars.next() {
        match ch {
            '0' ..= '9' => (),
            'e' if !exponent => {
                exponent = true;
                if let Some(&ch) = chars.peek() {
                    // Skip over any sign characters for the exponent
                    if ch == '+' || ch == '-' {
                        chars.next();
                    }
                } else {
                    // Ran out of input with no exponent specified
                    return false;
                }
            }
            '.' if !decimal && !exponent => decimal = true,
            _ => return false,
        }
    }

    true
}
