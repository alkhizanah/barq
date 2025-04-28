//! Parser
//!
//! A data transformer that converts `Token`s (in token.rs) to `Ast` (in ast.rs)

use std::{cmp::Ordering, fmt};

use thin_vec::{ThinVec, thin_vec};

use crate::{ast::*, bcu::*, lexer::Lexer, token::*};

pub enum ParserError {
    UnexpectedValue(TokenLoc, &'static str, String),
    UnexpectedToken(TokenLoc, Vec<TokenKind>),
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | ParserError::UnexpectedValue(loc, reason, provided) => {
                write!(f, "{}: {}: '{}'", loc, reason, provided)?
            }

            | ParserError::UnexpectedToken(loc, expected_kinds) => {
                write!(f, "{}: ", loc)?;

                if expected_kinds.is_empty() {
                    write!(f, "unexpected token")?;
                } else if expected_kinds.len() == 1 {
                    write!(f, "expected {}", expected_kinds[0])?;
                } else {
                    write!(f, "expected either {}", expected_kinds[0])?;

                    for expected_kind in &expected_kinds[1..] {
                        write!(f, " or {}", expected_kind)?;
                    }
                }
            }
        }

        Ok(())
    }
}

pub type ParserResult<T> = Result<T, ParserError>;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum ParserPrecedence {
    Lowest,
    Assign,
    BitOr,
    BitXor,
    BitAnd,
    Comparison,
    Shift,
    Sum,
    Product,
    Cast,
    Prefix,
    Field,
    Element,
    Call,
}

impl From<TokenKind> for ParserPrecedence {
    fn from(kind: TokenKind) -> Self {
        match kind {
            | TokenKind::Assign(_) => ParserPrecedence::Assign,
            | TokenKind::Comparison(_) => ParserPrecedence::Comparison,

            | TokenKind::Operator(Operator::Bitwise(bitwise)) => match bitwise {
                | Bitwise::Or => ParserPrecedence::BitOr,
                | Bitwise::Xor => ParserPrecedence::BitXor,
                | Bitwise::And => ParserPrecedence::BitAnd,

                | _ => ParserPrecedence::Lowest,
            },

            | TokenKind::Operator(Operator::LeftShift | Operator::RightShift) => ParserPrecedence::Shift,
            | TokenKind::Operator(Operator::Plus | Operator::Minus) => ParserPrecedence::Sum,

            | TokenKind::Operator(Operator::Multiply | Operator::Divide | Operator::Modulo) => {
                ParserPrecedence::Product
            }

            | TokenKind::Keyword(Keyword::As) => ParserPrecedence::Cast,
            | TokenKind::Period => ParserPrecedence::Field,
            | TokenKind::OpenBracket => ParserPrecedence::Element,
            | TokenKind::OpenParen => ParserPrecedence::Call,

            | _ => ParserPrecedence::Lowest,
        }
    }
}

pub struct Parser<'a> {
    bcu: &'a mut Bcu,
    file: &'a BcuFile,
    lexer: Lexer<'a>,
    nodes: Vec<Node>,
    strings: String,
}

impl Parser<'_> {
    pub fn new<'a>(bcu: &'a mut Bcu, file: &'a BcuFile) -> Parser<'a> {
        Parser {
            bcu,
            file,
            lexer: Lexer::new(file.buffer.as_str()),
            nodes: Vec::new(),
            strings: String::new(),
        }
    }

    fn unexpected_token(&mut self, expected_kinds: Vec<TokenKind>) -> ParserError {
        ParserError::UnexpectedToken(
            TokenLoc::find(self.lexer.peek().range.start, self.file),
            expected_kinds,
        )
    }

    pub fn parse(mut self) -> ParserResult<Ast> {
        self.parse_struct_inner(0, TokenKind::Eof).map(|module| {
            self.strings.shrink_to_fit();
            self.nodes.shrink_to_fit();

            Ast {
                module,
                strings: self.strings,
                nodes: self.nodes,
            }
        })
    }

    fn expect(&mut self, expect_kind: TokenKind) -> ParserResult<Token> {
        if let Some(token) = self.lexer.next_if_eq(expect_kind) {
            Ok(token)
        } else {
            Err(self.unexpected_token(vec![expect_kind]))
        }
    }

    fn expect_either(&mut self, expect_kinds: &[TokenKind]) -> ParserResult<Token> {
        if let Some(token) = self.lexer.next_if(|x| expect_kinds.contains(&x.kind)) {
            Ok(token)
        } else {
            Err(self.unexpected_token(expect_kinds.to_vec()))
        }
    }

    fn expect_semicolon(&mut self) -> ParserResult<()> {
        if self.lexer.next_if_eq(TokenKind::Semicolon).is_none()
            && self.lexer.prev().map(|x| x.kind) != Some(TokenKind::CloseBrace)
        {
            Err(self.unexpected_token(vec![TokenKind::Semicolon]))
        } else {
            Ok(())
        }
    }

    fn push_node(&mut self, node: Node) -> NodeIdx {
        let node_idx = NodeIdx(self.nodes.len() as u32);

        self.nodes.push(node);

        node_idx
    }

    fn parse_binding(&mut self, name: ByteRange) -> ParserResult<(TokenKind, Binding)> {
        let ty;
        let kind;
        let value;

        if matches!(self.lexer.peek().kind, TokenKind::Colon | TokenKind::Assign(None)) {
            ty = None;

            kind = self.lexer.next().kind;

            value = self.parse_expr(ParserPrecedence::Lowest)?;
        } else {
            ty = Some(self.parse_node_a(ParserPrecedence::Lowest)?);

            kind = self
                .expect_either(&[TokenKind::Colon, TokenKind::Assign(None)])?
                .kind;

            value = self.parse_expr(ParserPrecedence::Lowest)?;
        }

        Ok((kind, Binding { name, ty, value }))
    }

    fn parse_global_assembly(&mut self) -> ParserResult<()> {
        self.lexer.next();

        self.expect(TokenKind::OpenBrace)?;

        while let Some(token) = self.lexer.next_if_eq(TokenKind::StringLiteral) {
            let line = token.range.get(self.file.buffer.as_str());

            self.bcu.global_assembly.push_str(line);
            self.bcu.global_assembly.push('\n');
        }

        self.expect(TokenKind::CloseBrace)?;

        Ok(())
    }

    fn parse_stmts(&mut self) -> ParserResult<ThinVec<NodeIdx>> {
        let mut body = ThinVec::new();

        self.expect(TokenKind::OpenBrace)?;

        while self.lexer.next_if_eq(TokenKind::CloseBrace).is_none() {
            body.push(self.parse_stmt()?);

            self.expect_semicolon()?;
        }

        body.shrink_to_fit();

        Ok(body)
    }

    fn parse_stmt(&mut self) -> ParserResult<NodeIdx> {
        let node = match self.lexer.peek().kind {
            | TokenKind::Identifier => {
                let range = self.lexer.next().range;

                if self.lexer.peek().kind == TokenKind::Colon {
                    self.parse_local_binding(range)
                } else {
                    self.lexer.back_to(range.start);

                    return self.parse_expr(ParserPrecedence::Lowest);
                }
            }

            | TokenKind::Keyword(Keyword::While) => self.parse_while_loop(),
            | TokenKind::Keyword(Keyword::Break) => Ok(Node::Break(self.lexer.next().range.start)),
            | TokenKind::Keyword(Keyword::Continue) => Ok(Node::Continue(self.lexer.next().range.start)),

            | TokenKind::Keyword(Keyword::Defer) => Ok(Node::Defer(self.parse_stmts()?)),

            | TokenKind::OpenBrace => Ok(Node::Block(self.parse_stmts()?)),

            | TokenKind::Keyword(Keyword::Return) => self.parse_return(),

            | _ => return self.parse_expr(ParserPrecedence::Lowest),
        }?;

        Ok(self.push_node(node))
    }

    fn parse_while_loop(&mut self) -> ParserResult<Node> {
        let start = self.lexer.next().range.start;

        let condition = self.parse_expr(ParserPrecedence::Lowest)?;

        let body = if self.lexer.peek().kind == TokenKind::OpenBrace {
            self.parse_stmt()?
        } else {
            self.expect(TokenKind::Keyword(Keyword::Then))?;

            self.parse_expr(ParserPrecedence::Lowest)?
        };

        Ok(Node::WhileLoop(WhileLoop {
            condition,
            body,
            start,
        }))
    }

    fn parse_local_binding(&mut self, name: ByteRange) -> ParserResult<Node> {
        self.lexer.next();

        match self.parse_binding(name)? {
            | (TokenKind::Colon, binding) => Ok(Node::Constant(binding)),
            | (TokenKind::Assign(None), binding) => Ok(Node::Variable(binding)),
            | _ => unreachable!(),
        }
    }

    fn parse_return(&mut self) -> ParserResult<Node> {
        let start = self.lexer.next().range.start;

        let value = if self.lexer.peek().kind == TokenKind::Semicolon {
            None
        } else {
            Some(self.parse_expr(ParserPrecedence::Lowest)?)
        };

        Ok(Node::Return(Return { value, start }))
    }

    fn parse_expr(&mut self, precedence: ParserPrecedence) -> ParserResult<NodeIdx> {
        self.parse_node_while(|token| {
            token.kind != TokenKind::Semicolon && ParserPrecedence::from(token.kind) > precedence
        })
    }

    fn parse_node_a(&mut self, precedence: ParserPrecedence) -> ParserResult<NodeIdx> {
        self.parse_node_while(|token| {
            !matches!(token.kind, TokenKind::Semicolon | TokenKind::Assign(None))
                && ParserPrecedence::from(token.kind) > precedence
        })
    }

    fn parse_node_while(&mut self, mut f: impl FnMut(Token) -> bool) -> ParserResult<NodeIdx> {
        let mut lhs = self.parse_unary_expr()?;

        while f(self.lexer.peek()) {
            lhs = self.parse_binary_expr(lhs)?;
        }

        Ok(lhs)
    }

    fn parse_unary_expr(&mut self) -> ParserResult<NodeIdx> {
        let node = match self.lexer.peek().kind {
            | TokenKind::Identifier => self.parse_idxentifier(),
            | TokenKind::SpecialIdentifier => self.parse_special_idxentifier(),
            | TokenKind::StringLiteral => self.parse_string(),
            | TokenKind::CharLiteral => self.parse_char(),
            | TokenKind::Int => self.parse_int(),
            | TokenKind::Float => self.parse_float(),

            | TokenKind::Keyword(Keyword::Fn) => self.parse_function(),

            | TokenKind::Operator(Operator::Multiply) => self.parse_pointer_ty(),
            | TokenKind::OpenBracket => self.parse_array_ty(),
            | TokenKind::Keyword(Keyword::Struct) => self.parse_struct_ty(),
            | TokenKind::Keyword(Keyword::Enum) => self.parse_enum_ty(),

            | TokenKind::Keyword(Keyword::Asm) => self.parse_inline_assembly(),

            | TokenKind::Keyword(Keyword::If) => self.parse_conditional(),

            | TokenKind::Keyword(Keyword::Switch) => self.parse_switch(),

            | TokenKind::Boolwise(Boolwise::Not) => self.parse_unary_operation(UnaryOperator::BoolNot),

            | TokenKind::Operator(Operator::Bitwise(Bitwise::Not)) => {
                self.parse_unary_operation(UnaryOperator::BitNot)
            }

            | TokenKind::Operator(Operator::Minus) => self.parse_unary_operation(UnaryOperator::Negate),

            | TokenKind::Operator(Operator::Bitwise(Bitwise::And)) => {
                self.parse_unary_operation(UnaryOperator::Reference)
            }

            | TokenKind::OpenParen => return self.parse_parentheses(),

            | _ => Err(self.unexpected_token(Vec::new())),
        }?;

        Ok(self.push_node(node))
    }

    fn parse_idxentifier(&mut self) -> ParserResult<Node> {
        Ok(Node::Identifier(self.lexer.next().range))
    }

    fn parse_special_idxentifier(&mut self) -> ParserResult<Node> {
        let token = self.lexer.next();
        let token_value = token.range.get(self.file.buffer.as_str());

        match token_value {
            | "import" => {
                self.expect(TokenKind::OpenParen)?;

                let file_path = self.parse_expr(ParserPrecedence::Lowest)?;

                self.expect(TokenKind::CloseParen)?;

                Ok(Node::BuiltinCall(BuiltinCall {
                    kind: BuiltinKind::Import,
                    arguments: thin_vec![file_path],
                    start: token.range.start,
                }))
            }

            | "uninitialized" => {
                self.expect(TokenKind::OpenParen)?;

                let value_ty = self.parse_expr(ParserPrecedence::Lowest)?;

                self.expect(TokenKind::CloseParen)?;

                Ok(Node::BuiltinCall(BuiltinCall {
                    kind: BuiltinKind::Uninitialized,
                    arguments: thin_vec![value_ty],
                    start: token.range.start,
                }))
            }

            | "has_field" => {
                self.expect(TokenKind::OpenParen)?;

                let container_ty = self.parse_expr(ParserPrecedence::Lowest)?;

                self.expect(TokenKind::Comma)?;

                let field_name = self.parse_expr(ParserPrecedence::Lowest)?;

                self.expect(TokenKind::CloseParen)?;

                Ok(Node::BuiltinCall(BuiltinCall {
                    kind: BuiltinKind::HasField,
                    arguments: thin_vec![container_ty, field_name],
                    start: token.range.start,
                }))
            }

            | _ => Err(ParserError::UnexpectedValue(
                TokenLoc::find(token.range.start, self.file),
                "unknown special identifier",
                token_value.to_string(),
            )),
        }
    }

    fn unescape(input: &str, output: &mut String) -> Result<(), char> {
        output.reserve(input.len());

        let mut unescaping = false;

        for ch in input.chars() {
            match unescaping {
                | true => {
                    unescaping = false;

                    output.push(match ch {
                        | 'n' => '\n',
                        | 't' => '\t',
                        | 'r' => '\r',
                        | 'b' => '\x08',
                        | 'v' => '\x0b',
                        | 'f' => '\x14',
                        | 'e' => '\x1b',

                        | '\\' | '"' | '\'' => ch,

                        | _ => return Err(ch),
                    });
                }

                | false => match ch {
                    | '\\' => unescaping = true,

                    | _ => output.push(ch),
                },
            }
        }

        Ok(())
    }

    fn parse_string(&mut self) -> ParserResult<Node> {
        let token = self.lexer.next();
        let token_value = token.range.get(self.file.buffer.as_str());

        let mut range = ByteRange::new(self.strings.len() as u32, 0);

        if let Err(esc) = Parser::unescape(token_value, &mut self.strings) {
            return Err(ParserError::UnexpectedValue(
                TokenLoc::find(token.range.start, self.file),
                "invalid escape code",
                esc.to_string(),
            ));
        }

        range.end = self.strings.len() as u32;

        Ok(Node::String(range))
    }

    fn parse_char(&mut self) -> ParserResult<Node> {
        let token = self.lexer.next();
        let token_value = token.range.get(self.file.buffer.as_str());

        let mut content = String::new();

        if let Err(esc) = Parser::unescape(token_value, &mut content) {
            return Err(ParserError::UnexpectedValue(
                TokenLoc::find(token.range.start, self.file),
                "invalid escape code",
                esc.to_string(),
            ));
        }

        let count = content.chars().count();

        match count.cmp(&1) {
            | Ordering::Less => Err(ParserError::UnexpectedValue(
                TokenLoc::find(token.range.start, self.file),
                "invalid character literal (amount of characters is less than 1)",
                content,
            )),

            | Ordering::Greater => Err(ParserError::UnexpectedValue(
                TokenLoc::find(token.range.start, self.file),
                "invalid character literal (amount of characters is more than 1)",
                content,
            )),

            | Ordering::Equal => {
                let ch = unsafe { content.chars().next().unwrap_unchecked() };

                Ok(Node::Int(ch as u64))
            }
        }
    }

    fn parse_int(&mut self) -> ParserResult<Node> {
        let token = self.lexer.next();

        let mut src = token.range.get(self.file.buffer.as_str());

        let radix = if src.len() <= 2 {
            10
        } else {
            match &src[0..2] {
                | "0b" => {
                    src = &src[2..];

                    2
                }

                | "0o" => {
                    src = &src[2..];

                    8
                }

                | "0x" => {
                    src = &src[2..];

                    16
                }

                | _ => 10,
            }
        };

        let Ok(val) = u64::from_str_radix(src, radix) else {
            return Err(ParserError::UnexpectedValue(
                TokenLoc::find(token.range.start, self.file),
                "invalid int",
                src.to_string(),
            ));
        };

        Ok(Node::Int(val))
    }

    fn parse_float(&mut self) -> ParserResult<Node> {
        let token = self.lexer.next();

        let src = token.range.get(self.file.buffer.as_str());

        let Ok(val) = src.parse::<f64>() else {
            return Err(ParserError::UnexpectedValue(
                TokenLoc::find(token.range.start, self.file),
                "invalid float",
                src.to_string(),
            ));
        };

        Ok(Node::Float(val))
    }

    fn parse_function(&mut self) -> ParserResult<Node> {
        let fn_keyword = self.lexer.next();

        let mut fn_ty = FunctionTy {
            parameters: ThinVec::new(),
            is_var_args: false,
            return_ty: None,
            calling_convention: CallingConvention::Auto,
            start: fn_keyword.range.start,
        };

        self.expect(TokenKind::OpenParen)?;

        while self.lexer.next_if_eq(TokenKind::CloseParen).is_none() {
            let param_name = self.expect(TokenKind::Identifier)?.range;

            self.expect(TokenKind::Colon)?;

            let param_ty = self.parse_expr(ParserPrecedence::Lowest)?;

            fn_ty.parameters.push((param_name, param_ty));

            if self
                .expect_either(&[TokenKind::Comma, TokenKind::CloseParen])?
                .kind
                == TokenKind::CloseParen
            {
                break;
            }

            if self.lexer.next_if_eq(TokenKind::TriplePeriod).is_some() {
                fn_ty.is_var_args = true;

                self.expect(TokenKind::CloseParen)?;

                break;
            }
        }

        fn_ty.parameters.shrink_to_fit();

        if !matches!(
            self.lexer.peek().kind,
            TokenKind::OpenBrace | TokenKind::SpecialIdentifier
        ) {
            fn_ty.return_ty = Some(self.parse_expr(ParserPrecedence::Lowest)?);
        }

        let mut foreign = None;
        let mut explicit_callconv = false;

        while let Some(token) = self.lexer.next_if_eq(TokenKind::SpecialIdentifier) {
            let token_value = token.range.get(self.file.buffer.as_str());

            match token_value {
                | "foreign" => {
                    if (self.lexer.next_if_eq(TokenKind::OpenParen)).is_some() {
                        foreign = Some(self.expect(TokenKind::StringLiteral)?.range);

                        if !explicit_callconv {
                            fn_ty.calling_convention = CallingConvention::C;
                        }

                        self.expect(TokenKind::CloseParen)?;
                    } else {
                        foreign = Some(ByteRange::new(0, 0));
                    }
                }

                | "callconv" => {
                    self.expect(TokenKind::OpenParen)?;

                    let token = self.expect(TokenKind::Identifier)?;
                    let token_value = token.range.get(self.file.buffer.as_str());

                    fn_ty.calling_convention = match token_value {
                        | "auto" => CallingConvention::Auto,
                        | "c" => CallingConvention::C,
                        | "inline" => CallingConvention::Inline,
                        | "naked" => CallingConvention::Naked,

                        | _ => {
                            return Err(ParserError::UnexpectedValue(
                                TokenLoc::find(token.range.start, self.file),
                                "unknown calling convention",
                                token_value.to_string(),
                            ));
                        }
                    };

                    explicit_callconv = true;

                    self.expect(TokenKind::CloseParen)?;
                }

                | _ => {
                    return Err(ParserError::UnexpectedValue(
                        TokenLoc::find(token.range.start, self.file),
                        "unknown special identifier",
                        token_value.to_string(),
                    ));
                }
            }
        }

        let body = if self.lexer.peek().kind == TokenKind::OpenBrace {
            self.parse_stmts()?
        } else {
            ThinVec::new()
        };

        if !body.is_empty() || foreign.is_some() {
            Ok(Node::Function(Function {
                signature: fn_ty,
                foreign,
                body,
            }))
        } else {
            Ok(Node::FunctionTy(fn_ty))
        }
    }

    fn parse_pointer_ty(&mut self) -> ParserResult<Node> {
        let start = self.lexer.next().range.start;

        let is_const = self
            .lexer
            .next_if_eq(TokenKind::Keyword(Keyword::Const))
            .is_some();

        let child_ty = self.parse_node_a(ParserPrecedence::Lowest)?;

        Ok(Node::PointerTy(PointerTy {
            size: PointerSize::One,
            is_const,
            child_ty,
            start,
        }))
    }

    fn parse_array_ty(&mut self) -> ParserResult<Node> {
        let start = self.lexer.next().range.start;

        let size = match self
            .lexer
            .next_if(|x| {
                matches!(
                    x.kind,
                    TokenKind::CloseBracket | TokenKind::Operator(Operator::Multiply)
                )
            })
            .map(|x| x.kind)
        {
            | Some(TokenKind::CloseBracket) => PointerSize::Slice,

            | Some(TokenKind::Operator(Operator::Multiply)) => {
                self.expect(TokenKind::CloseBracket)?;

                PointerSize::Many
            }

            | None => {
                let len = self.parse_expr(ParserPrecedence::Lowest)?;

                self.expect(TokenKind::CloseBracket)?;

                let child_ty = self.parse_node_a(ParserPrecedence::Lowest)?;

                return Ok(Node::ArrayTy(ArrayTy { len, child_ty, start }));
            }

            | _ => unreachable!(),
        };

        let is_const = self
            .lexer
            .next_if_eq(TokenKind::Keyword(Keyword::Const))
            .is_some();

        let child_ty = self.parse_node_a(ParserPrecedence::Lowest)?;

        Ok(Node::PointerTy(PointerTy {
            size,
            is_const,
            child_ty,
            start,
        }))
    }

    fn parse_struct_ty(&mut self) -> ParserResult<Node> {
        let start = self.lexer.next().range.start;

        self.expect(TokenKind::OpenBrace)?;

        self.parse_struct_inner(start, TokenKind::CloseBrace)
            .map(Node::StructTy)
    }

    fn parse_struct_inner(&mut self, start: ByteOffset, end_token_kind: TokenKind) -> ParserResult<StructTy> {
        let mut fields = ThinVec::new();
        let mut constants = ThinVec::new();
        let mut variables = ThinVec::new();

        while self.lexer.next_if_eq(end_token_kind).is_none() {
            while self.lexer.peek().kind == TokenKind::Keyword(Keyword::Asm) {
                self.parse_global_assembly()?;
            }

            if self.lexer.next_if_eq(end_token_kind).is_some() {
                break;
            }

            let name = self.expect(TokenKind::Identifier)?.range;

            self.expect(TokenKind::Colon)?;

            let ty;
            let kind;
            let value;

            if matches!(self.lexer.peek().kind, TokenKind::Colon | TokenKind::Assign(None)) {
                ty = None;

                kind = self.lexer.next().kind;

                value = self.parse_expr(ParserPrecedence::Lowest)?;

                self.expect_semicolon()?;
            } else {
                ty = Some(self.parse_node_a(ParserPrecedence::Lowest)?);

                if matches!(self.lexer.peek().kind, TokenKind::Colon | TokenKind::Assign(None)) {
                    kind = self.lexer.next().kind;

                    value = self.parse_expr(ParserPrecedence::Lowest)?;

                    self.expect_semicolon()?;
                } else {
                    if self.expect_either(&[TokenKind::Comma, end_token_kind])?.kind == end_token_kind {
                        break;
                    }

                    kind = TokenKind::Comma;

                    value = NodeIdx(0);
                }
            }

            let binding = Binding { name, ty, value };

            match kind {
                | TokenKind::Colon => constants.push(binding),
                | TokenKind::Assign(None) => variables.push(binding),
                | TokenKind::Comma => fields.push((binding.name, binding.ty.unwrap())),

                | _ => unreachable!(),
            }
        }

        fields.shrink_to_fit();
        constants.shrink_to_fit();
        variables.shrink_to_fit();

        Ok(StructTy {
            fields,
            constants,
            variables,
            start,
        })
    }

    fn parse_enum_ty(&mut self) -> ParserResult<Node> {
        let start = self.lexer.next().range.start;

        let backing_ty = if self.lexer.peek().kind == TokenKind::OpenBrace {
            None
        } else {
            Some(self.parse_expr(ParserPrecedence::Lowest)?)
        };

        let mut fields = ThinVec::new();
        let mut constants = ThinVec::new();
        let mut variables = ThinVec::new();

        self.expect(TokenKind::OpenBrace)?;

        while self.lexer.next_if_eq(TokenKind::CloseBrace).is_none() {
            let name = self.expect(TokenKind::Identifier)?.range;

            if self.lexer.next_if_eq(TokenKind::Colon).is_some() {
                let (kind, binding) = self.parse_binding(name)?;

                match kind {
                    | TokenKind::Colon => constants.push(binding),
                    | TokenKind::Assign(None) => variables.push(binding),

                    | _ => unreachable!(),
                }

                self.expect_semicolon()?;
            } else {
                let value = if self.lexer.next_if_eq(TokenKind::Assign(None)).is_none() {
                    None
                } else {
                    Some(self.parse_expr(ParserPrecedence::Lowest)?)
                };

                fields.push((name, value));

                if self
                    .expect_either(&[TokenKind::Comma, TokenKind::CloseBrace])?
                    .kind
                    == TokenKind::CloseBrace
                {
                    break;
                }
            }
        }

        fields.shrink_to_fit();
        constants.shrink_to_fit();
        variables.shrink_to_fit();

        Ok(Node::EnumTy(EnumTy {
            backing_ty,
            fields,
            constants,
            variables,
            start,
        }))
    }

    fn parse_inline_assembly(&mut self) -> ParserResult<Node> {
        let start = self.lexer.next().range.start;

        self.expect(TokenKind::OpenBrace)?;

        let mut content = ByteRange::new(self.strings.len() as u32, 0);

        let parsed_string = self.lexer.peek().kind == TokenKind::StringLiteral;

        while let Some(token) = self.lexer.next_if_eq(TokenKind::StringLiteral) {
            let token_value = token.range.get(self.file.buffer.as_str());

            if let Err(esc) = Parser::unescape(token_value, &mut self.strings) {
                return Err(ParserError::UnexpectedValue(
                    TokenLoc::find(token.range.start, self.file),
                    "invalid escape code",
                    esc.to_string(),
                ));
            }

            self.strings.push('\n');
        }

        content.end = self.strings.len() as u32;

        let mut input_constraints = ThinVec::new();
        let mut output_constraint = None;
        let mut clobbers = ThinVec::new();

        if parsed_string {
            if self.lexer.next_if_eq(TokenKind::Colon).is_some()
                && !matches!(self.lexer.peek().kind, TokenKind::Colon | TokenKind::CloseBrace)
            {
                output_constraint = Some(self.parse_constraint()?);
            }

            if self.lexer.next_if_eq(TokenKind::Colon).is_some() {
                while !matches!(self.lexer.peek().kind, TokenKind::Colon | TokenKind::CloseBrace) {
                    input_constraints.push(self.parse_constraint()?);

                    if self.lexer.next_if_eq(TokenKind::Comma).is_none()
                        && !matches!(self.lexer.peek().kind, TokenKind::Colon | TokenKind::CloseBrace)
                    {
                        return Err(self.unexpected_token(vec![
                            TokenKind::Comma,
                            TokenKind::Colon,
                            TokenKind::OpenBrace,
                        ]));
                    }
                }
            }

            if self.lexer.next_if_eq(TokenKind::Colon).is_some() {
                while self.lexer.peek().kind != TokenKind::CloseBrace {
                    clobbers.push(self.expect(TokenKind::StringLiteral)?.range);

                    if self.lexer.next_if_eq(TokenKind::Comma).is_none()
                        && self.lexer.peek().kind != TokenKind::CloseBrace
                    {
                        return Err(self.unexpected_token(vec![TokenKind::Comma, TokenKind::OpenBrace]));
                    }
                }
            }
        }

        self.expect(TokenKind::CloseBrace)?;

        input_constraints.shrink_to_fit();
        clobbers.shrink_to_fit();

        Ok(Node::InlineAssembly(InlineAssembly {
            content,
            input_constraints,
            output_constraint,
            clobbers,
            start,
        }))
    }

    fn parse_constraint(&mut self) -> ParserResult<Constraint> {
        let register = self.expect(TokenKind::StringLiteral)?.range;

        self.expect(TokenKind::OpenParen)?;

        let value = self.parse_expr(ParserPrecedence::Lowest)?;

        self.expect(TokenKind::CloseParen)?;

        Ok(Constraint { register, value })
    }

    fn parse_conditional(&mut self) -> ParserResult<Node> {
        let start = self.lexer.next().range.start;

        let condition = self.parse_expr(ParserPrecedence::Lowest)?;

        let then_case = if self.lexer.peek().kind == TokenKind::OpenBrace {
            self.parse_stmt()?
        } else {
            self.expect(TokenKind::Keyword(Keyword::Then))?;

            self.parse_expr(ParserPrecedence::Lowest)?
        };

        let else_case = if self.lexer.next_if_eq(TokenKind::Keyword(Keyword::Else)).is_some() {
            Some(if self.lexer.peek().kind == TokenKind::OpenBrace {
                self.parse_stmt()?
            } else {
                self.parse_expr(ParserPrecedence::Lowest)?
            })
        } else {
            None
        };

        Ok(Node::Conditional(Conditional {
            condition,
            then_case,
            else_case,
            start,
        }))
    }

    fn parse_switch(&mut self) -> ParserResult<Node> {
        let start = self.lexer.next().range.start;

        let value = self.parse_expr(ParserPrecedence::Lowest)?;

        let mut cases = ThinVec::new();

        let mut else_case = None;

        self.expect(TokenKind::OpenBrace)?;

        while self.lexer.next_if_eq(TokenKind::CloseBrace).is_none() {
            if let Some(else_token) = self.lexer.next_if_eq(TokenKind::Keyword(Keyword::Else)) {
                if else_case.is_some() {
                    return Err(ParserError::UnexpectedValue(
                        TokenLoc::find(else_token.range.start, self.file),
                        "duplicate switch case",
                        "else case".to_string(),
                    ));
                }

                self.expect(TokenKind::FatArrow)?;

                else_case = Some(if self.lexer.peek().kind == TokenKind::OpenBrace {
                    self.parse_stmt()?
                } else {
                    self.parse_expr(ParserPrecedence::Lowest)?
                });
            } else {
                let mut case_values = thin_vec![self.parse_expr(ParserPrecedence::Lowest)?];

                while self.lexer.next_if_eq(TokenKind::Comma).is_some()
                    && self.lexer.peek().kind != TokenKind::FatArrow
                {
                    case_values.push(self.parse_expr(ParserPrecedence::Lowest)?);
                }

                self.expect(TokenKind::FatArrow)?;

                let case_body = if self.lexer.peek().kind == TokenKind::OpenBrace {
                    self.parse_stmt()?
                } else {
                    self.parse_expr(ParserPrecedence::Lowest)?
                };

                cases.push((case_values, case_body));
            }

            if self
                .expect_either(&[TokenKind::Comma, TokenKind::CloseBrace])?
                .kind
                == TokenKind::CloseBrace
            {
                break;
            }
        }

        cases.shrink_to_fit();

        Ok(Node::Switch(Switch {
            value,
            cases,
            else_case,
            start,
        }))
    }

    fn parse_parentheses(&mut self) -> ParserResult<NodeIdx> {
        self.lexer.next();

        let value = self.parse_expr(ParserPrecedence::Lowest)?;

        self.expect(TokenKind::CloseParen)?;

        Ok(value)
    }

    fn parse_unary_operation(&mut self, operator: UnaryOperator) -> ParserResult<Node> {
        let start = self.lexer.next().range.start;

        let rhs = self.parse_expr(ParserPrecedence::Prefix)?;

        let operation = UnaryOperation { rhs, start };

        Ok(match operator {
            | UnaryOperator::BoolNot => Node::BoolNot(operation),
            | UnaryOperator::BitNot => Node::BitNot(operation),
            | UnaryOperator::Negate => Node::Negate(operation),
            | UnaryOperator::Reference => Node::Reference(operation),
        })
    }

    fn parse_binary_expr(&mut self, lhs: NodeIdx) -> ParserResult<NodeIdx> {
        let node = match self.lexer.peek().kind {
            | TokenKind::Operator(operator) => match operator {
                | Operator::Plus => self.parse_binary_operation(lhs, BinaryOperator::Plus),
                | Operator::Minus => self.parse_binary_operation(lhs, BinaryOperator::Minus),
                | Operator::Multiply => self.parse_binary_operation(lhs, BinaryOperator::Multiply),
                | Operator::Divide => self.parse_binary_operation(lhs, BinaryOperator::Divide),
                | Operator::Modulo => self.parse_binary_operation(lhs, BinaryOperator::Modulo),
                | Operator::LeftShift => self.parse_binary_operation(lhs, BinaryOperator::LeftShift),

                | Operator::RightShift => self.parse_binary_operation(lhs, BinaryOperator::RightShift),

                | Operator::Bitwise(bitwise) => match bitwise {
                    | Bitwise::And => self.parse_binary_operation(lhs, BinaryOperator::BitAnd),
                    | Bitwise::Or => self.parse_binary_operation(lhs, BinaryOperator::BitOr),
                    | Bitwise::Xor => self.parse_binary_operation(lhs, BinaryOperator::BitXor),

                    | _ => Err(self.unexpected_token(Vec::new())),
                },
            },

            | TokenKind::Comparison(comparison) => match comparison {
                | Comparison::Eql => self.parse_binary_operation(lhs, BinaryOperator::Eql),
                | Comparison::NotEql => self.parse_binary_operation(lhs, BinaryOperator::NotEql),
                | Comparison::LessThan => self.parse_binary_operation(lhs, BinaryOperator::LessThan),
                | Comparison::LessOrEql => self.parse_binary_operation(lhs, BinaryOperator::LessOrEql),
                | Comparison::GreaterThan => self.parse_binary_operation(lhs, BinaryOperator::GreaterThan),
                | Comparison::GreaterOrEql => self.parse_binary_operation(lhs, BinaryOperator::GreaterOrEql),
            },

            | TokenKind::Assign(operator) => self.parse_assign(lhs, operator),

            | TokenKind::OpenParen => self.parse_call(lhs),

            | TokenKind::Keyword(Keyword::As) => self.parse_cast(lhs),

            | TokenKind::OpenBracket => self.parse_element_access(lhs),

            | TokenKind::Period => self.parse_field_access(lhs),

            | _ => Err(self.unexpected_token(Vec::new())),
        }?;

        Ok(self.push_node(node))
    }

    fn parse_binary_operation(&mut self, lhs: NodeIdx, operator: BinaryOperator) -> ParserResult<Node> {
        let operator_token = self.lexer.next();

        let start = operator_token.range.start;

        let rhs = self.parse_expr(ParserPrecedence::from(operator_token.kind))?;

        let operation = BinaryOperation { lhs, rhs, start };

        Ok(match operator {
            | BinaryOperator::Plus => Node::Add(operation),
            | BinaryOperator::Minus => Node::Subtract(operation),
            | BinaryOperator::Multiply => Node::Multiply(operation),
            | BinaryOperator::Divide => Node::Divide(operation),
            | BinaryOperator::Modulo => Node::Modulo(operation),
            | BinaryOperator::LeftShift => Node::LeftShift(operation),
            | BinaryOperator::RightShift => Node::RightShift(operation),
            | BinaryOperator::BitAnd => Node::BitAnd(operation),
            | BinaryOperator::BitOr => Node::BitOr(operation),
            | BinaryOperator::BitXor => Node::BitXor(operation),
            | BinaryOperator::Eql => Node::Eql(operation),
            | BinaryOperator::NotEql => Node::NotEql(operation),
            | BinaryOperator::LessThan => Node::LessThan(operation),
            | BinaryOperator::LessOrEql => Node::LessOrEql(operation),
            | BinaryOperator::GreaterThan => Node::GreaterThan(operation),
            | BinaryOperator::GreaterOrEql => Node::GreaterOrEql(operation),
        })
    }

    fn parse_assign(&mut self, target: NodeIdx, operator: Option<Operator>) -> ParserResult<Node> {
        let operator = operator.map(BinaryOperator::from);

        let start = self.lexer.next().range.start;

        let value = self.parse_expr(ParserPrecedence::Lowest)?;

        Ok(Node::Assign(Assign {
            target,
            operator,
            value,
            start,
        }))
    }

    fn parse_call(&mut self, callable: NodeIdx) -> ParserResult<Node> {
        let mut arguments = ThinVec::new();

        let start = self.lexer.next().range.start;

        while self.lexer.next_if_eq(TokenKind::CloseParen).is_none() {
            arguments.push(self.parse_expr(ParserPrecedence::Lowest)?);

            if self
                .expect_either(&[TokenKind::Comma, TokenKind::CloseParen])?
                .kind
                == TokenKind::CloseParen
            {
                break;
            }
        }

        arguments.shrink_to_fit();

        Ok(Node::Call(Call {
            callable,
            arguments,
            start,
        }))
    }

    fn parse_cast(&mut self, value: NodeIdx) -> ParserResult<Node> {
        let start = self.lexer.next().range.start;

        let ty = self.parse_expr(ParserPrecedence::Cast)?;

        Ok(Node::Cast(BinaryOperation {
            lhs: value,
            rhs: ty,
            start,
        }))
    }

    fn parse_element_access(&mut self, target: NodeIdx) -> ParserResult<Node> {
        let start = self.lexer.next().range.start;

        let index = self.parse_expr(ParserPrecedence::Assign)?;

        if self
            .expect_either(&[TokenKind::DoublePeriod, TokenKind::CloseBracket])?
            .kind
            == TokenKind::DoublePeriod
        {
            let another_index = self.parse_expr(ParserPrecedence::Cast)?;

            self.expect(TokenKind::CloseBracket)?;

            Ok(Node::Slice(Slice {
                target,
                range_start: index,
                range_end: another_index,
                start,
            }))
        } else {
            Ok(Node::ElementAccess(BinaryOperation {
                lhs: target,
                rhs: index,
                start,
            }))
        }
    }

    fn parse_field_access(&mut self, target: NodeIdx) -> ParserResult<Node> {
        self.lexer.next();

        if let Some(start) = self
            .lexer
            .next_if_eq(TokenKind::Operator(Operator::Multiply))
            .map(|x| x.range.start)
        {
            Ok(Node::Dereference(UnaryOperation { rhs: target, start }))
        } else if let Some(start) = self.lexer.next_if_eq(TokenKind::OpenBrace).map(|x| x.range.start) {
            let ty = target;

            if self.lexer.peek().kind == TokenKind::Period {
                let mut fields = ThinVec::new();

                while self.lexer.next_if_eq(TokenKind::CloseBrace).is_none() {
                    self.expect(TokenKind::Period)?;

                    let name = self.expect(TokenKind::Identifier)?.range;

                    self.expect(TokenKind::Assign(None))?;

                    let value = self.parse_expr(ParserPrecedence::Lowest)?;

                    fields.push((name, value));

                    if self
                        .expect_either(&[TokenKind::Comma, TokenKind::CloseBrace])?
                        .kind
                        == TokenKind::CloseBrace
                    {
                        break;
                    }
                }

                fields.shrink_to_fit();

                Ok(Node::Struct(Struct { ty, fields, start }))
            } else {
                let mut values = ThinVec::new();

                while self.lexer.next_if_eq(TokenKind::CloseBrace).is_none() {
                    values.push(self.parse_expr(ParserPrecedence::Lowest)?);

                    if self
                        .expect_either(&[TokenKind::Comma, TokenKind::CloseBrace])?
                        .kind
                        == TokenKind::CloseBrace
                    {
                        break;
                    }
                }

                values.shrink_to_fit();

                Ok(Node::Array(Array { ty, values, start }))
            }
        } else {
            let field = self.expect(TokenKind::Identifier)?.range;

            Ok(Node::FieldAccess(FieldAccess { target, field }))
        }
    }
}
