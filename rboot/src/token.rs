use std::ops::Range;

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub range: Range<u32>,
}

impl Token {
    pub fn new(kind: TokenKind, range: Range<u32>) -> Token {
        Token { kind, range }
    }

    pub fn to_str<'a>(&self, buffer: &'a str) -> &'a str {
        // SAFETY: tokens are always inside of the buffer, otherwise the lexer did not tokenize the
        // buffer correctly, which should not happen at all in case of our program
        unsafe {
            buffer
                .get(self.range.start as usize..self.range.end as usize)
                .unwrap_unchecked()
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind {
    Invalid,
    Identifier,
    SpecialIdentifier,
    StringLiteral,
    CharLiteral,
    Int,
    Float,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Comma,
    Period,
    DoublePeriod,
    TriplePeriod,
    Colon,
    DoubleColon,
    Semicolon,
    FatArrow,
    Plus,
    Minus,
    Star,
    Divide,
    Modulo,
    LessThan,
    GreaterThan,
    LessOrEql,
    GreaterOrEql,
    LeftShift,
    RightShift,
    Eql,
    NotEql,
    Boolwise(Boolwise),
    Bitwise(Bitwise),
    Assign(Assign),
    Keyword(Keyword),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Boolwise {
    Not,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Bitwise {
    Not,
    And,
    Or,
    Xor,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Assign {
    None,
    Colon,
    Plus,
    Minus,
    Star,
    Divide,
    Modulo,
    Bitwise(Bitwise),
    LeftShift,
    RightShift,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Keyword {
    Const,
    Defer,
    Struct,
    Enum,
    Fn,
    Switch,
    If,
    Then,
    Else,
    While,
    Break,
    Continue,
    Asm,
    As,
    Return,
}

impl Keyword {
    pub fn from_str(input: &str) -> Option<Keyword> {
        match input {
            "const" => Some(Keyword::Const),
            "defer" => Some(Keyword::Defer),
            "struct" => Some(Keyword::Struct),
            "enum" => Some(Keyword::Enum),
            "fn" => Some(Keyword::Fn),
            "switch" => Some(Keyword::Switch),
            "if" => Some(Keyword::If),
            "then" => Some(Keyword::Then),
            "else" => Some(Keyword::Else),
            "while" => Some(Keyword::While),
            "break" => Some(Keyword::Break),
            "continue" => Some(Keyword::Continue),
            "asm" => Some(Keyword::Asm),
            "as" => Some(Keyword::As),
            "return" => Some(Keyword::Return),

            _ => None,
        }
    }
}
