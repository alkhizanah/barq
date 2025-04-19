use crate::token::*;

pub struct Cursor<'a> {
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
    prev_token: Option<Token>,
    peek_token: Option<Token>,
}

impl Lexer<'_> {
    pub const fn new(buffer: &'_ str) -> Lexer<'_> {
        Lexer {
            cursor: Cursor { buffer, index: 0 },
            prev_token: None,
            peek_token: None,
        }
    }

    #[inline]
    pub fn prev(&self) -> Option<Token> {
        self.prev_token
    }

    #[inline]
    pub fn back_to(&mut self, index: TokenIdx) {
        self.prev_token = None;
        self.peek_token = None;

        self.cursor.index = index;
    }

    #[inline]
    pub fn peek(&mut self) -> Token {
        if let Some(token) = self.peek_token {
            token
        } else {
            let token = self.next_token();
            self.peek_token = Some(token);
            token
        }
    }

    #[allow(clippy::should_implement_trait)]
    #[inline]
    pub fn next(&mut self) -> Token {
        let token = self.peek_token.take().unwrap_or_else(|| self.next_token());
        self.prev_token = Some(token);
        token
    }

    #[inline]
    pub fn next_if(&mut self, func: impl FnOnce(Token) -> bool) -> Option<Token> {
        if func(self.peek()) {
            return Some(self.next());
        }

        None
    }

    #[inline]
    pub fn next_if_eq(&mut self, expected: TokenKind) -> Option<Token> {
        self.next_if(|next| next.kind == expected)
    }

    fn next_token(&mut self) -> Token {
        let kind;
        let mut start;
        let mut modified_end = None;

        // We try again if we skipped a comment, otherwise we just break out of the loop
        loop {
            while self.cursor.next_if(|x| x.is_whitespace()).is_some() {}

            start = self.cursor.index;

            let Some(ch) = self.cursor.next() else {
                return Token::new(TokenKind::Eof, TokenRange::new(start, start));
            };

            kind = match ch {
                | '(' => TokenKind::OpenParen,
                | ')' => TokenKind::CloseParen,
                | '{' => TokenKind::OpenBrace,
                | '}' => TokenKind::CloseBrace,
                | '[' => TokenKind::OpenBracket,
                | ']' => TokenKind::CloseBracket,
                | ':' => TokenKind::Colon,
                | ',' => TokenKind::Comma,
                | ';' => TokenKind::Semicolon,

                | '.' => {
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

                | '=' => {
                    if self.cursor.next_if_eq('=').is_some() {
                        TokenKind::Comparison(Comparison::Eql)
                    } else if self.cursor.next_if_eq('>').is_some() {
                        TokenKind::FatArrow
                    } else {
                        TokenKind::Assign(None)
                    }
                }

                | '!' => {
                    if self.cursor.next_if_eq('=').is_some() {
                        TokenKind::Comparison(Comparison::NotEql)
                    } else {
                        TokenKind::Boolwise(Boolwise::Not)
                    }
                }

                | '<' => {
                    if self.cursor.next_if_eq('=').is_some() {
                        TokenKind::Comparison(Comparison::LessOrEql)
                    } else if self.cursor.next_if_eq('<').is_some() {
                        if self.cursor.next_if_eq('=').is_some() {
                            TokenKind::Assign(Some(Operator::LeftShift))
                        } else {
                            TokenKind::Operator(Operator::LeftShift)
                        }
                    } else {
                        TokenKind::Comparison(Comparison::LessThan)
                    }
                }

                | '>' => {
                    if self.cursor.next_if_eq('=').is_some() {
                        TokenKind::Comparison(Comparison::GreaterOrEql)
                    } else if self.cursor.next_if_eq('>').is_some() {
                        if self.cursor.next_if_eq('=').is_some() {
                            TokenKind::Assign(Some(Operator::RightShift))
                        } else {
                            TokenKind::Operator(Operator::RightShift)
                        }
                    } else {
                        TokenKind::Comparison(Comparison::GreaterThan)
                    }
                }

                | '+' => {
                    if self.cursor.next_if_eq('=').is_some() {
                        TokenKind::Assign(Some(Operator::Plus))
                    } else {
                        TokenKind::Operator(Operator::Plus)
                    }
                }

                | '-' => {
                    if self.cursor.next_if_eq('=').is_some() {
                        TokenKind::Assign(Some(Operator::Minus))
                    } else {
                        TokenKind::Operator(Operator::Minus)
                    }
                }

                | '*' => {
                    if self.cursor.next_if_eq('=').is_some() {
                        TokenKind::Assign(Some(Operator::Multiply))
                    } else {
                        TokenKind::Operator(Operator::Multiply)
                    }
                }

                | '/' => {
                    if self.cursor.next_if_eq('=').is_some() {
                        TokenKind::Assign(Some(Operator::Divide))
                    } else if self.cursor.next_if_eq('/').is_some() {
                        while self.cursor.next_if(|x| x != '\n').is_some() {}

                        self.cursor.next();

                        continue;
                    } else {
                        TokenKind::Operator(Operator::Divide)
                    }
                }

                | '%' => {
                    if self.cursor.next_if_eq('=').is_some() {
                        TokenKind::Assign(Some(Operator::Modulo))
                    } else {
                        TokenKind::Operator(Operator::Modulo)
                    }
                }

                | '~' => TokenKind::Operator(Operator::Bitwise(Bitwise::Not)),

                | '&' => {
                    if self.cursor.next_if_eq('=').is_some() {
                        TokenKind::Assign(Some(Operator::Bitwise(Bitwise::And)))
                    } else {
                        TokenKind::Operator(Operator::Bitwise(Bitwise::And))
                    }
                }

                | '|' => {
                    if self.cursor.next_if_eq('=').is_some() {
                        TokenKind::Assign(Some(Operator::Bitwise(Bitwise::Or)))
                    } else {
                        TokenKind::Operator(Operator::Bitwise(Bitwise::Or))
                    }
                }

                | '^' => {
                    if self.cursor.next_if_eq('=').is_some() {
                        TokenKind::Assign(Some(Operator::Bitwise(Bitwise::Xor)))
                    } else {
                        TokenKind::Operator(Operator::Bitwise(Bitwise::Xor))
                    }
                }

                | 'a'..='z' | 'A'..='Z' | '_' => {
                    while self
                        .cursor
                        .next_if(|x| matches!(x, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'))
                        .is_some()
                    {}

                    match &self.cursor.buffer[start as usize..self.cursor.index as usize] {
                        | "const" => TokenKind::Keyword(Keyword::Const),
                        | "defer" => TokenKind::Keyword(Keyword::Defer),
                        | "struct" => TokenKind::Keyword(Keyword::Struct),
                        | "enum" => TokenKind::Keyword(Keyword::Enum),
                        | "fn" => TokenKind::Keyword(Keyword::Fn),
                        | "switch" => TokenKind::Keyword(Keyword::Switch),
                        | "if" => TokenKind::Keyword(Keyword::If),
                        | "then" => TokenKind::Keyword(Keyword::Then),
                        | "else" => TokenKind::Keyword(Keyword::Else),
                        | "while" => TokenKind::Keyword(Keyword::While),
                        | "break" => TokenKind::Keyword(Keyword::Break),
                        | "continue" => TokenKind::Keyword(Keyword::Continue),
                        | "asm" => TokenKind::Keyword(Keyword::Asm),
                        | "as" => TokenKind::Keyword(Keyword::As),
                        | "return" => TokenKind::Keyword(Keyword::Return),

                        | _ => TokenKind::Identifier,
                    }
                }

                | '@' => {
                    while self
                        .cursor
                        .next_if(|x| matches!(x, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'))
                        .is_some()
                    {}

                    if self.cursor.index > start {
                        start += 1;

                        TokenKind::SpecialIdentifier
                    } else {
                        TokenKind::Invalid
                    }
                }

                | '"' | '\'' => {
                    let mut unescaping = false;

                    while let Some(nch) = self.cursor.next_if(|x| unescaping || x != ch) {
                        if unescaping {
                            unescaping = false;
                        } else if nch == '\\' {
                            unescaping = true;
                        }
                    }

                    if self.cursor.next() != Some(ch) {
                        TokenKind::Invalid
                    } else {
                        start += 1;
                        modified_end = Some(self.cursor.index - 1);

                        if ch == '"' {
                            TokenKind::StringLiteral
                        } else {
                            TokenKind::CharLiteral
                        }
                    }
                }

                | '0'..='9' => {
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

                | _ => TokenKind::Invalid,
            };

            break;
        }

        let range = TokenRange::new(start, modified_end.unwrap_or(self.cursor.index));

        Token::new(kind, range)
    }
}

#[cfg(test)]
mod test {
    use super::{Lexer, TokenKind};

    #[test]
    fn tokenize_identifiers() {
        let mut lexer = Lexer::new("Hello HelloWorld @THIS_IS_SPECIAL HELLO_WORLD HELl0_WORLD");

        assert_eq!(lexer.next().kind, TokenKind::Identifier);
        assert_eq!(lexer.next().kind, TokenKind::Identifier);
        assert_eq!(lexer.next().kind, TokenKind::SpecialIdentifier);
        assert_eq!(lexer.next().kind, TokenKind::Identifier);
        assert_eq!(lexer.next().kind, TokenKind::Identifier);
        assert_eq!(lexer.next().kind, TokenKind::Eof);
    }

    #[test]
    fn tokenize_numbers() {
        let mut lexer = Lexer::new("0x10 2.9 6..9");

        assert_eq!(lexer.next().kind, TokenKind::Int);
        assert_eq!(lexer.next().kind, TokenKind::Float);
        assert_eq!(lexer.next().kind, TokenKind::Int);
        assert_eq!(lexer.next().kind, TokenKind::DoublePeriod);
        assert_eq!(lexer.next().kind, TokenKind::Int);
        assert_eq!(lexer.next().kind, TokenKind::Eof);
    }

    #[test]
    fn tokenize_strings_and_characters() {
        let buffer = r#""Hello World" 'H' "Hello" '\''"#;

        let mut lexer = Lexer::new(buffer);

        assert_eq!(lexer.peek().range.get(buffer), "Hello World");
        assert_eq!(lexer.next().kind, TokenKind::StringLiteral);

        assert_eq!(lexer.peek().range.get(buffer), "H");
        assert_eq!(lexer.next().kind, TokenKind::CharLiteral);

        assert_eq!(lexer.peek().range.get(buffer), "Hello");
        assert_eq!(lexer.next().kind, TokenKind::StringLiteral);

        assert_eq!(lexer.peek().range.get(buffer), "\\'");
        assert_eq!(lexer.next().kind, TokenKind::CharLiteral);

        assert_eq!(lexer.next().kind, TokenKind::Eof);
    }
}
