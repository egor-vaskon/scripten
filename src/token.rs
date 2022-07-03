use std::collections::btree_map::IntoKeys;
use std::collections::HashMap;
use std::iter::Enumerate;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct IdentifierRegistry {
    registry: HashMap<String, u32>
}

impl<'a> IdentifierRegistry {
    pub fn new() -> IdentifierRegistry {
        IdentifierRegistry {
            registry: HashMap::new()
        }
    }

    #[cfg(test)]
    pub fn with_idents(idents: impl IntoIterator<Item = &'a str>) -> IdentifierRegistry {
        let idents: HashMap<String, u32> = idents
            .into_iter()
            .map(str::to_owned)
            .enumerate()
            .map(|(usize, ident)| (ident, usize as u32 + 1))
            .collect();

        IdentifierRegistry {
            registry: idents
        }
    }

    fn add(&mut self, identifier: &str) -> u32 {
        if !self.registry.contains_key(identifier) {
            let id = self.registry.len() as u32 + 1; //id 0 is reserved
            self.registry.insert(identifier.to_owned(), id);
            id
        } else {
            self.registry[identifier]
        }
    }
}

pub trait CharExt {
    fn is_delimiter(&self) -> bool;
}

impl CharExt for char {
    fn is_delimiter(&self) -> bool {
        return match self {
            '(' | ')' | '[' | ']' | '{' | '}' => true,
            _ => false
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Keyword {
    If,
    Xor,
    Let
}

impl Keyword {
    pub fn from_str(token: &str) -> Option<Keyword> {
        match token {
            "if" => Some(Keyword::If),
            "xor" => Some(Keyword::Xor),
            "let" => Some(Keyword::Let),
            _ => None
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Identifier {
    id: u32
}

impl Identifier {
    pub fn new(registry: &mut IdentifierRegistry, identifier: &str) -> Identifier {
        Identifier { id: registry.add(identifier) }
    }

    #[cfg(test)]
    pub fn from_id(id: u32) -> Identifier {
        Identifier { id }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Punctuation {
    Add,
    Sub,
    Mul,
    Div,
    Assign
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StrLiteral {
    str: String
}

impl StrLiteral {
    pub fn new(str: String) -> StrLiteral {
        StrLiteral { str }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct IntLiteral {
    value: i32
}

impl IntLiteral {
    pub fn new(value: i32) -> IntLiteral {
        IntLiteral {
            value
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Literal {
    Str(StrLiteral),
    Int(IntLiteral)
}

impl From<IntLiteral> for Literal {
    fn from(value: IntLiteral) -> Self {
        Literal::Int(value)
    }
}

impl From<StrLiteral> for Literal {
    fn from(value: StrLiteral) -> Self {
        Literal::Str(value)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum DelimiterKind {
    Parenthesis,
    SquareBracket,
    CurlyBracket
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum DelimiterPosition {
    Opening,
    Closing
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Delimiter {
    kind: DelimiterKind,
    position: DelimiterPosition
}

impl Delimiter {
    pub const fn new(kind: DelimiterKind,
                     position: DelimiterPosition) -> Delimiter {
        Delimiter {
            kind,
            position
        }
    }

    pub const fn opening(kind: DelimiterKind) -> Delimiter {
        Delimiter::new(kind, DelimiterPosition::Opening)
    }

    pub const fn closing(kind: DelimiterKind) -> Delimiter {
        Delimiter::new(kind, DelimiterPosition::Closing)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TokenKind {
    Keyword(Keyword),
    Identifier(Identifier),
    Punctuation(Punctuation),
    Literal(Literal),
    Delimiter(Delimiter)
}

impl From<Keyword> for TokenKind {
    fn from(value: Keyword) -> Self {
        TokenKind::Keyword(value)
    }
}

impl From<Identifier> for TokenKind {
    fn from(value: Identifier) -> Self {
        TokenKind::Identifier(value)
    }
}

impl From<Punctuation> for TokenKind {
    fn from(value: Punctuation) -> Self {
        TokenKind::Punctuation(value)
    }
}

impl From<Literal> for TokenKind {
    fn from(value: Literal) -> Self {
        TokenKind::Literal(value)
    }
}

impl From<Delimiter> for TokenKind {
    fn from(value: Delimiter) -> Self {
        TokenKind::Delimiter(value)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Token {
    kind: TokenKind
}

impl Token {
    pub fn new(kind: TokenKind) -> Token {
        Token {
            kind
        }
    }

    pub fn integer(value: i32) -> Token {
        TokenKind::Literal(IntLiteral::new(value).into()).into()
    }
}

impl From<TokenKind> for Token {
    fn from(value: TokenKind) -> Self {
        Token::new(value)
    }
}

impl From<Keyword> for Token {
    fn from(value: Keyword) -> Self {
        TokenKind::from(value).into()
    }
}

impl From<Identifier> for Token {
    fn from(value: Identifier) -> Self {
        TokenKind::from(value).into()
    }
}

impl From<Delimiter> for Token {
    fn from(value: Delimiter) -> Self {
        TokenKind::from(value).into()
    }
}

impl From<Punctuation> for Token {
    fn from(value: Punctuation) -> Self {
        TokenKind::from(value).into()
    }
}