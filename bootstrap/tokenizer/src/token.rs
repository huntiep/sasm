#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Token {
    /// (
    LParen(Index),
    /// )
    RParen(Index),
    /// {
    LBrace(Index),
    /// }
    RBrace(Index),
    /// [
    LSBracket(Index),
    /// ]
    RSBracket(Index),
    Comment(Index),
    BlockComment(Index),
    /// # used for creating literals
    Pound(Index),
    Char(Index),
    String(Index),
    Integer(Index),
    Float(Index),
    Symbol(Index),
}

impl Token {
    pub fn index(&self) -> Index {
        use self::Token::*;
        match self {
            LParen(i) => *i,
            RParen(i) => *i,
            LBrace(i) => *i,
            RBrace(i) => *i,
            LSBracket(i) => *i,
            RSBracket(i) => *i,
            Comment(i) => *i,
            BlockComment(i) => *i,
            Pound(i) => *i,
            Char(i) => *i,
            String(i) => *i,
            Integer(i) => *i,
            Float(i) => *i,
            Symbol(i) => *i,
        }
    }

    pub fn commentp(&self) -> bool {
        use self::Token::*;
        match self {
            Comment(_) | BlockComment(_) => true,
            _ => false,
        }
    }

    pub fn opener_match(self, closer: Self) -> bool {
        use self::Token::*;
        match (self, closer) {
            (LParen(_), RParen(_)) => true,
            (LBrace(_), RBrace(_)) => true,
            (LSBracket(_), RSBracket(_)) => true,
            _ => false,
        }
    }

    pub fn openerp(&self) -> bool {
        use self::Token::*;
        match self {
            LParen(_) | LBrace(_) | LSBracket(_) => true,
            _ => false,
        }
    }

    pub fn closerp(&self) -> bool {
        use self::Token::*;
        match self {
            RParen(_) | RBrace(_) | RSBracket(_) => true,
            _ => false,
        }
    }

    pub fn as_str<'a>(&self, input: &'a str) -> &'a str {
        let Index { start, end } = self.index();
        &input[start..end]
    }

    pub fn is_string(&self) -> bool {
        match self {
            Token::String(_) => true,
            _ => false,
        }
    }

    pub fn is_symbol(&self) -> bool {
        match self {
            Token::Symbol(_) => true,
            _ => false,
        }
    }

    // TODO: not sure this is the right place for this
    /*
    pub fn as_symbol(&self, input: &str, symbols: &mut DefaultStringInterner) -> Sym {
        let Index { start, end } = self.index();
        let symbol = &input[start..end];
        let mut s = symbol.chars();
        let mut buf = String::new();

        while let Some(c) = s.next() {
            match c {
                '\\' => match s.next() {
                    Some('n') => buf.push('\n'),
                    Some('t') => buf.push('\t'),
                    Some(c) => buf.push(c),
                    None => unreachable!(),
                },
                '|' => (),
                _ => buf.push(c),
            }
        }

        symbols.get_or_intern(buf)
    }
    */
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Index {
    start: usize,
    end: usize,
}

impl Index {
    pub fn new(start: usize, end: usize) -> Self {
        Index {
            start: start - 1,
            end,
        }
    }
}
