use crate::token::*;

struct Cursor<'a> {
    buffer: &'a str,
    index: u32,
}

impl Iterator for Cursor<'_> {
    type Item = char;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let buffer_index = self.index as usize;

        self.index += 1;

        if buffer_index >= self.buffer.len() {
            None
        } else {
            Some(self.buffer.as_bytes()[buffer_index] as char)
        }
    }
}

impl Cursor<'_> {
    #[inline]
    pub fn peek(&mut self) -> Option<char> {
        let buffer_index = self.index as usize;

        if buffer_index >= self.buffer.len() {
            None
        } else {
            Some(self.buffer.as_bytes()[buffer_index] as char)
        }
    }

    pub fn next_if(&mut self, func: impl FnOnce(char) -> bool) -> Option<char> {
        if let Some(ch) = self.peek() {
            if func(ch) {
                self.index += 1;

                return Some(ch);
            }
        }

        None
    }

    pub fn next_if_eq(&mut self, expected: char) -> Option<char> {
        self.next_if(|next| next == expected)
    }
}

pub struct Lexer<'a> {
    cursor: Cursor<'a>,
}

impl Lexer<'_> {
    pub fn new(buffer: &'_ str) -> Lexer<'_> {
        Lexer {
            cursor: Cursor { buffer, index: 0 },
        }
    }

    #[inline]
    pub fn peek(&mut self) -> Option<Token> {
        let prev_index = self.cursor.index;
        let token = self.next();
        self.cursor.index = prev_index;
        token
    }

    pub fn next_if(&mut self, func: impl FnOnce(Token) -> bool) -> Option<Token> {
        if let Some(token) = self.peek() {
            if func(token) {
                return self.next();
            }
        }

        None
    }

    pub fn next_if_eq(&mut self, expected: TokenKind) -> Option<Token> {
        self.next_if(|next| next.kind == expected)
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while self.cursor.next_if(|x| x.is_whitespace()).is_some() {}

        let start = self.cursor.index;

        let ch = self.cursor.next()?;

        let kind = match ch {
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            '{' => TokenKind::OpenBrace,
            '}' => TokenKind::CloseBrace,
            '[' => TokenKind::OpenBracket,
            ']' => TokenKind::CloseBracket,
            '~' => TokenKind::Bitwise(Bitwise::Not),
            ',' => TokenKind::Comma,
            ';' => TokenKind::Semicolon,

            '.' => {
                if self.cursor.next_if_eq('.').is_some() {
                    if self.cursor.next_if_eq('.').is_some() {
                        TokenKind::TriplePeriod
                    } else {
                        TokenKind::DoublePeriod
                    }
                } else {
                    TokenKind::Period
                }
            }

            '=' => {
                if self.cursor.next_if_eq('=').is_some() {
                    TokenKind::Assign(Assign::None)
                } else {
                    TokenKind::Eql
                }
            }

            '!' => {
                if self.cursor.next_if_eq('=').is_some() {
                    TokenKind::NotEql
                } else {
                    TokenKind::Boolwise(Boolwise::Not)
                }
            }

            '<' => {
                if self.cursor.next_if_eq('=').is_some() {
                    TokenKind::LessOrEql
                } else if self.cursor.next_if_eq('<').is_some() {
                    if self.cursor.next_if_eq('=').is_some() {
                        TokenKind::Assign(Assign::LeftShift)
                    } else {
                        TokenKind::LeftShift
                    }
                } else {
                    TokenKind::LessThan
                }
            }

            '>' => {
                if self.cursor.next_if_eq('=').is_some() {
                    TokenKind::GreaterOrEql
                } else if self.cursor.next_if_eq('>').is_some() {
                    if self.cursor.next_if_eq('=').is_some() {
                        TokenKind::Assign(Assign::RightShift)
                    } else {
                        TokenKind::RightShift
                    }
                } else {
                    TokenKind::GreaterThan
                }
            }

            '+' => {
                if self.cursor.next_if_eq('=').is_some() {
                    TokenKind::Assign(Assign::Plus)
                } else {
                    TokenKind::Plus
                }
            }

            '-' => {
                if self.cursor.next_if_eq('=').is_some() {
                    TokenKind::Assign(Assign::Minus)
                } else {
                    TokenKind::Minus
                }
            }

            '*' => {
                if self.cursor.next_if_eq('=').is_some() {
                    TokenKind::Assign(Assign::Star)
                } else {
                    TokenKind::Star
                }
            }

            '/' => {
                if self.cursor.next_if_eq('=').is_some() {
                    TokenKind::Assign(Assign::Divide)
                } else {
                    TokenKind::Divide
                }
            }

            '%' => {
                if self.cursor.next_if_eq('=').is_some() {
                    TokenKind::Assign(Assign::Modulo)
                } else {
                    TokenKind::Modulo
                }
            }

            '&' => {
                if self.cursor.next_if_eq('=').is_some() {
                    TokenKind::Assign(Assign::Bitwise(Bitwise::And))
                } else {
                    TokenKind::Bitwise(Bitwise::And)
                }
            }

            '|' => {
                if self.cursor.next_if_eq('=').is_some() {
                    TokenKind::Assign(Assign::Bitwise(Bitwise::Or))
                } else {
                    TokenKind::Bitwise(Bitwise::Or)
                }
            }

            '^' => {
                if self.cursor.next_if_eq('=').is_some() {
                    TokenKind::Assign(Assign::Bitwise(Bitwise::Xor))
                } else {
                    TokenKind::Bitwise(Bitwise::Xor)
                }
            }

            ':' => {
                if self.cursor.next_if_eq(':').is_some() {
                    TokenKind::DoubleColon
                } else {
                    TokenKind::Colon
                }
            }

            'a'..='z' | 'A'..='Z' | '_' => {
                while self
                    .cursor
                    .next_if(|x| matches!(x, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'))
                    .is_some()
                {}

                match &self.cursor.buffer[start as usize..self.cursor.index as usize] {
                    "const" => TokenKind::Keyword(Keyword::Const),
                    "defer" => TokenKind::Keyword(Keyword::Defer),
                    "struct" => TokenKind::Keyword(Keyword::Struct),
                    "enum" => TokenKind::Keyword(Keyword::Enum),
                    "fn" => TokenKind::Keyword(Keyword::Fn),
                    "switch" => TokenKind::Keyword(Keyword::Switch),
                    "if" => TokenKind::Keyword(Keyword::If),
                    "then" => TokenKind::Keyword(Keyword::Then),
                    "else" => TokenKind::Keyword(Keyword::Else),
                    "while" => TokenKind::Keyword(Keyword::While),
                    "break" => TokenKind::Keyword(Keyword::Break),
                    "continue" => TokenKind::Keyword(Keyword::Continue),
                    "asm" => TokenKind::Keyword(Keyword::Asm),
                    "as" => TokenKind::Keyword(Keyword::As),
                    "return" => TokenKind::Keyword(Keyword::Return),

                    _ => TokenKind::Identifier,
                }
            }

            '@' => {
                while self
                    .cursor
                    .next_if(|x| matches!(x, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'))
                    .is_some()
                {}

                if self.cursor.index > start {
                    TokenKind::SpecialIdentifier
                } else {
                    TokenKind::Invalid
                }
            }

            '"' | '\'' => {
                while let Some(nch) = self.cursor.next_if(|x| x != ch) {
                    if nch == '\\' && self.cursor.peek() == Some(ch) {
                        self.cursor.index += 1;
                    }
                }

                if self.cursor.next() != Some(ch) {
                    TokenKind::Invalid
                } else {
                    return Some(Token::new(
                        if ch == '"' {
                            TokenKind::StringLiteral
                        } else {
                            TokenKind::CharLiteral
                        },
                        (start + 1)..(self.cursor.index - 1),
                    ));
                }
            }

            '0'..='9' => {
                let mut kind = TokenKind::Int;

                while let Some(ch) = self
                    .cursor
                    .next_if(|x| matches!(x, 'a'..='z' | 'A'..='Z' | '_' | '.' | '0'..='9'))
                {
                    if ch == '.' {
                        if self.cursor.peek() == Some('.') {
                            self.cursor.index -= 1;

                            break;
                        } else {
                            kind = TokenKind::Float;
                        }
                    }
                }

                kind
            }

            _ => TokenKind::Invalid,
        };

        Some(Token::new(kind, start..self.cursor.index))
    }
}

#[cfg(test)]
mod test {
    use super::{Lexer, TokenKind};

    #[test]
    fn tokenize_identifiers() {
        let mut lexer = Lexer::new("Hello HelloWorld @THIS_IS_SPECIAL HELLO_WORLD HELl0_WORLD");

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Identifier);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Identifier);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::SpecialIdentifier);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Identifier);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Identifier);

        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn tokenize_numbers() {
        let mut lexer = Lexer::new("0x10 2.9 6..9");

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Int);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Float);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Int);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::DoublePeriod);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Int);

        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn tokenize_strings_and_characters() {
        let buffer = r#""Hello World" 'H' "Hello" '\''"#;

        let mut lexer = Lexer::new(buffer);

        assert_eq!(lexer.peek().unwrap().to_str(buffer), "Hello World");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::StringLiteral);

        assert_eq!(lexer.peek().unwrap().to_str(buffer), "H");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::CharLiteral);

        assert_eq!(lexer.peek().unwrap().to_str(buffer), "Hello");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::StringLiteral);

        // You might wonder why it is \' and not ' directly, because this is not parsing, the
        // escaping is not handled yet, we are here just testing that it should not stop if the '
        // is escaped using \
        assert_eq!(lexer.peek().unwrap().to_str(buffer), "\\'");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::CharLiteral);

        assert_eq!(lexer.next(), None);
    }
}
