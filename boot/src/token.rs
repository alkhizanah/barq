use std::fmt;

use crate::bcu::BcuFile;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub range: ByteRange,
}

impl Token {
    pub fn new(kind: TokenKind, range: ByteRange) -> Token {
        Token { kind, range }
    }
}

pub type ByteOffset = u32;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ByteRange {
    pub start: ByteOffset,
    pub end: ByteOffset,
}

impl ByteRange {
    pub fn new(start: ByteOffset, end: ByteOffset) -> ByteRange {
        ByteRange { start, end }
    }

    pub fn get<'a>(&self, buffer: &'a str) -> &'a str {
        &buffer[self.start as usize..self.end as usize]
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct TokenLoc {
    pub file_path: String,
    pub line: u32,
    pub column: u32,
}

impl fmt::Display for TokenLoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file_path, self.line, self.column)
    }
}

impl TokenLoc {
    pub fn find(start: ByteOffset, file: &'_ BcuFile) -> TokenLoc {
        let mut loc = TokenLoc {
            file_path: file.path.clone(),
            line: 1,
            column: 1,
        };

        for (i, c) in file.buffer.char_indices() {
            if i == start as usize {
                break;
            }

            if c == '\n' {
                loc.line += 1;
                loc.column = 1;
            } else {
                loc.column += 1;
            }
        }

        loc
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Eof,
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
    Semicolon,
    FatArrow,
    Boolwise(Boolwise),
    Operator(Operator),
    Comparison(Comparison),
    Assign(Option<Operator>),
    Keyword(Keyword),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Boolwise {
    Not,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Bitwise(Bitwise),
    LeftShift,
    RightShift,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Comparison {
    LessThan,
    GreaterThan,
    LessOrEql,
    GreaterOrEql,
    Eql,
    NotEql,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Bitwise {
    Not,
    And,
    Or,
    Xor,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | TokenKind::Eof => write!(f, "end of file")?,
            | TokenKind::Invalid => write!(f, "invalid token")?,
            | TokenKind::Identifier => write!(f, "an identifier")?,
            | TokenKind::SpecialIdentifier => write!(f, "a special identifier")?,
            | TokenKind::StringLiteral => write!(f, "a string literal")?,
            | TokenKind::CharLiteral => write!(f, "a character literal")?,
            | TokenKind::Int => write!(f, "an integer")?,
            | TokenKind::Float => write!(f, "a float")?,
            | TokenKind::OpenParen => write!(f, "a '('")?,
            | TokenKind::CloseParen => write!(f, "a ')'")?,
            | TokenKind::OpenBrace => write!(f, "a '{{'")?,
            | TokenKind::CloseBrace => write!(f, "a '}}'")?,
            | TokenKind::OpenBracket => write!(f, "a '['")?,
            | TokenKind::CloseBracket => write!(f, "a ']'")?,
            | TokenKind::Comma => write!(f, "a ','")?,
            | TokenKind::Period => write!(f, "a '.'")?,
            | TokenKind::DoublePeriod => write!(f, "a '..'")?,
            | TokenKind::TriplePeriod => write!(f, "a '...'")?,
            | TokenKind::Colon => write!(f, "a ':'")?,
            | TokenKind::Semicolon => write!(f, "a ';'")?,
            | TokenKind::FatArrow => write!(f, "a '=>'")?,

            | TokenKind::Boolwise(boolwise) => match boolwise {
                | Boolwise::Not => write!(f, "a '!'")?,
            },

            | TokenKind::Operator(operator) => match operator {
                | Operator::Plus => write!(f, "a '+'")?,
                | Operator::Minus => write!(f, "a '-'")?,
                | Operator::Multiply => write!(f, "a '*'")?,
                | Operator::Divide => write!(f, "a '/'")?,
                | Operator::Modulo => write!(f, "a '%'")?,
                | Operator::LeftShift => write!(f, "a '<<'")?,
                | Operator::RightShift => write!(f, "a '>>'")?,
                | Operator::Bitwise(bitwise) => match bitwise {
                    | Bitwise::Not => write!(f, "a '~'")?,
                    | Bitwise::And => write!(f, "a '&'")?,
                    | Bitwise::Or => write!(f, "a '|'")?,
                    | Bitwise::Xor => write!(f, "a '^'")?,
                },
            },

            | TokenKind::Comparison(comparison) => match comparison {
                | Comparison::LessThan => write!(f, "a '<'")?,
                | Comparison::GreaterThan => write!(f, "a '>'")?,
                | Comparison::LessOrEql => write!(f, "a '<='")?,
                | Comparison::GreaterOrEql => write!(f, "a '>='")?,
                | Comparison::Eql => write!(f, "a '=='")?,
                | Comparison::NotEql => write!(f, "a '!='")?,
            },

            | TokenKind::Assign(operator) => {
                let Some(operator) = operator else {
                    return write!(f, "a '='");
                };

                match operator {
                    | Operator::Plus => write!(f, "a '+='")?,
                    | Operator::Minus => write!(f, "a '-='")?,
                    | Operator::Multiply => write!(f, "a '*='")?,
                    | Operator::Divide => write!(f, "a '/='")?,
                    | Operator::Modulo => write!(f, "a '%='")?,
                    | Operator::LeftShift => write!(f, "a '<<='")?,
                    | Operator::RightShift => write!(f, "a '>>='")?,
                    | Operator::Bitwise(bitwise) => match bitwise {
                        | Bitwise::Not => unreachable!(),
                        | Bitwise::And => write!(f, "a '&='")?,
                        | Bitwise::Or => write!(f, "a '|='")?,
                        | Bitwise::Xor => write!(f, "a '^='")?,
                    },
                }
            }

            | TokenKind::Keyword(keyword) => match keyword {
                | Keyword::Const => write!(f, "a 'const' keyword'")?,
                | Keyword::Defer => write!(f, "a 'defer' keyword'")?,
                | Keyword::Struct => write!(f, "a 'struct' keyword'")?,
                | Keyword::Enum => write!(f, "an 'enum' keyword'")?,
                | Keyword::Fn => write!(f, "a 'fn' keyword")?,
                | Keyword::Switch => write!(f, "a 'switch' keyword")?,
                | Keyword::If => write!(f, "an 'if' keyword")?,
                | Keyword::Then => write!(f, "a 'then' keyword")?,
                | Keyword::Else => write!(f, "an 'else' keyword")?,
                | Keyword::While => write!(f, "a 'while' keyword")?,
                | Keyword::Break => write!(f, "a 'break' keyword")?,
                | Keyword::Continue => write!(f, "a 'continue' keyword")?,
                | Keyword::Asm => write!(f, "an 'asm' keyword")?,
                | Keyword::As => write!(f, "an 'as' keyword")?,
                | Keyword::Return => write!(f, "a 'return' keyword")?,
            },
        }

        Ok(())
    }
}
