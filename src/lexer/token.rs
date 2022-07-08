use std::convert::Infallible;
use std::iter::Peekable;
use std::num::ParseIntError;
use std::str::FromStr;
use crate::diagnostics::Span;
use crate::lexer::identifiers::IdentId;
use crate::lexer::token::OperatorKind::{AddAssign, BitAndAssign};
use crate::lexer::token::TokenKind::Literal;
use crate::source::Source;

/// Integer literal representation (e.g. `123`)
pub struct LitInt(i32);

impl FromStr for LitInt {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let value: i32 = s.parse()?;
        Ok(LitInt::new(value))
    }
}

impl LitInt {
    /// Creates an instance of [`LitInt`] given the
    /// integer it represents
    ///
    /// # Arguments
    ///
    /// * `value` - Integer the created token represents
    fn new(value: i32) -> LitInt {
        LitInt(value)
    }
}

/// String literal representation (e.g. `"Hello world!"`)
pub struct LitStr(String);

impl LitStr {
    /// Creates an instance of [`LitStr`] given the
    /// string it represents
    ///
    /// # Arguments
    ///
    /// * `value` - String the created token represents
    fn new(value: String) -> LitStr {
        LitStr(value)
    }
}

impl FromStr for LitStr {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(LitStr::new(s.to_owned()))
    }
}

/// Literal token such as integer or string.
pub enum LiteralKind {
    /// Integer token (`123`)
    Int(LitInt),

    /// Quoted string token (`"Hi"`)
    Str(LitStr),
}

impl From<LitInt> for LiteralKind {
    fn from(lit: LitInt) -> Self {
        LiteralKind::Int(lit)
    }
}

impl From<LitStr> for LiteralKind {
    fn from(lit: LitStr) -> Self {
        LiteralKind::Str(lit)
    }
}

/// Separator token that breaks down expressions (e.g. `(` - parenthesis).
pub enum SeparatorKind {
    /// `(`
    OpenParen,

    /// `)`
    CloseParen,

    /// `{`
    OpenBrace,

    /// `}`
    CloseBrace,

    /// `[`
    OpenBracket,

    /// `]`
    CloseBracket,
}

impl SeparatorKind {
    /// Creates separator from a character if possible
    ///
    /// Returns a separator corresponding to the given character if present, otherwise
    /// returns `None`.
    ///
    /// # Arguments
    ///
    /// * `ch` The character to create a separator from
    pub fn from_ch(ch: char) -> Option<SeparatorKind> {
        match ch {
            '(' => Some(SeparatorKind::OpenParen),
            ')' => Some(SeparatorKind::CloseParen),

            '{' => Some(SeparatorKind::OpenBrace),
            '}' => Some(SeparatorKind::CloseBrace),

            '[' => Some(SeparatorKind::OpenBracket),
            ']' => Some(SeparatorKind::CloseBracket),

            _ => None
        }
    }

    /// Returns whether given character is a separator
    pub const fn is_separator(ch: char) -> bool {
        match ch {
            '(' | ')' | '[' | ']' | '{' | '}' => true,
            _ => false
        }
    }
}

/// Operator token that is part of an expression
pub enum OperatorKind {
    /// `+`
    Add,

    /// `+=`
    AddAssign,

    /// `++`
    Increment,

    /// `-`
    Sub,

    /// `-=`
    SubAssign,

    /// `--`
    Decrement,

    /// `*`
    Mul,

    /// `*=`
    MulAssign,

    /// `/`
    Div,

    /// `/=`
    DivAssign,

    /// `%`
    Mod,

    /// `%=`
    ModAssign,

    /// `=`
    Assign,

    /// `!`
    Not,

    /// `&&`
    And,

    /// `||`
    Or,

    /// `==`
    Eq,

    /// `!=`
    Neq,

    /// `~`
    BitNot,

    /// `&`
    BitAnd,

    /// `&=`
    BitAndAssign,

    /// `|`
    BitOr,

    /// `|=`
    BitOrAssign,

    /// `^`
    BitXor,

    /// `^=`
    BitXorAssign,

    /// `<<`
    BitShl,

    /// `<<=`
    BitShlAssign,

    /// `>>`
    BitShr,

    /// `>>=`
    BitShrAssign,

    /// `<`
    Less,

    /// `>`
    Greater
}

impl OperatorKind {
    /// Constructs an operator from characters.
    ///
    /// You must ensure that [`OperatorKind::starts_with`] returns true
    /// with `first`.
    ///
    /// After the function returns, the `chars` iterator is set right
    /// after the final character of operator.
    ///
    /// # Arguments
    ///
    /// * `first` - The first character
    /// * `chars` - An iterator over remaining characters whose `next` method
    /// should return the second character
    pub const fn from<'a>(first: char, mut chars: impl Source<'a>) -> OperatorKind {
        macro_rules! try_second {
            ($iter:expr, $second:expr, {$(($ch:expr, $val:expr)),+}) => {
                match $second {
                    $(
                        $ch => {
                            $iter.next();
                            return $val
                        },
                    )*

                    _ => {}
                }
            }
        }

        return match first {
            '+' => {
                if let Some(second) = chars.next() {
                    try_second!(chars, second, {
                        ('=', OperatorKind::AddAssign),
                        ('+', OperatorKind::Increment)
                    });
                }

                OperatorKind::Add
            },

            '-' => {
                if let Some(second) = chars.next() {
                    try_second!(chars, second, {
                        ('=', OperatorKind::SubAssign),
                        ('-', OperatorKind::Decrement)
                    });
                }

                OperatorKind::Sub
            },

            '*' => {
                if let Some(second) = chars.next() {
                    try_second!(chars, second, {
                        ('=', OperatorKind::MulAssign)
                    });
                }

                OperatorKind::Mul
            },

            '/' => {
                if let Some(second) = chars.next() {
                    try_second!(chars, second, {
                        ('=', OperatorKind::DivAssign)
                    });
                }

                OperatorKind::Div
            },

            '%' => {
                if let Some(second) = chars.next() {
                    try_second!(chars, second, {
                        ('=', OperatorKind::ModAssign)
                    });
                }

                OperatorKind::Mod
            },

            '=' => {
                if let Some(second) = chars.next() {
                    try_second!(chars, second, {
                        ('=', OperatorKind::Eq)
                    });
                }

                OperatorKind::Assign
            },

            '!' => {
                if let Some(second) = chars.next() {
                    try_second!(chars, second, {
                        ('=', OperatorKind::Neq)
                    });
                }

                OperatorKind::Not
            },

            '&' => {
                if let Some(second) = chars.next() {
                    try_second!(chars, second, {
                       ('&', OperatorKind::And),
                        ('=', BitAndAssign)
                    });
                }

                OperatorKind::BitAnd
            },

            '|' => {
                if let Some(second) = chars.next() {
                    try_second!(chars, second, {
                       ('|', OperatorKind::Or),
                        ('=', OperatorKind::BitOrAssign)
                    });
                }

                OperatorKind::BitOr
            },

            '^' => {
                if let Some(second) = chars.next() {
                    try_second!(chars, second, {
                        ('=', OperatorKind::BitXorAssign)
                    });
                }

                OperatorKind::BitXor
            },

            '~' => OperatorKind::BitNot,

            '<' => {
                if let Some(second) = chars.next() {
                    if second == '<' {
                        if let Some(third) = chars.next() {
                            if third == '=' {
                                chars.next();
                                return OperatorKind::BitShlAssign
                            }
                        }

                        return OperatorKind::BitShl
                    }
                }

                OperatorKind::Less
            },

            '>' => {
                if let Some(second) = chars.next() {
                    if second == '>' {
                        if let Some(third) = chars.next() {
                            if third == '=' {
                                chars.next();
                                return OperatorKind::BitShrAssign
                            }
                        }

                        return OperatorKind::BitShr
                    }
                }

                OperatorKind::Greater
            },

            _ => panic!("expected operator, but found {}", first)
        }
    }

    /// Returns true, if there's an operator that start with
    /// given character, otherwise false.
    pub const fn starts_with(ch: char) -> bool {
        match ch {
            '+' | '-' | '*' | '/' | '%' | '=' | '!' | '&' | '|' | '~' | '^' | '>' | '<' => true,
            _ => false
        }
    }
}

/// Keyword token
///
/// Basically, keyword is an identifier
/// with some special meaning for the language.
pub enum KeywordKind {
    /// `let`
    Let,

    /// `const`
    Const,

    /// `if`
    If,

    /// `elif`
    Elif,

    /// `else`
    Else,

    /// `while`
    While,

    /// `for`
    For,

    /// `do`
    Do,

    /// `break`
    Break,

    /// `continue`
    Continue,

    /// `def`
    Def,

    /// `return`
    Return,

    /// `in`
    In,
}

/// Enum representing all token kinds used
/// in Scripten.
///
/// Unlike [`Token`], this enum doesn't hold a [`Span`],
/// so it should be used only with [`Token`].
pub enum TokenKind {
    /// Keyword
    Keyword(KeywordKind),

    /// Identifier
    Ident(IdentId),

    /// Literal
    Literal(LiteralKind),

    /// Separator
    Separator(SeparatorKind),

    /// Operator
    Operator(OperatorKind),

    /// `,`
    Comma,

    /// `.`
    Dot,

    /// `;`
    Semicolon
}

impl From<KeywordKind> for TokenKind {
    fn from(kw: KeywordKind) -> Self {
        TokenKind::Keyword(kw)
    }
}

impl From<IdentId> for TokenKind {
    fn from(ident: IdentId) -> Self {
        TokenKind::Ident(ident)
    }
}

impl From<LiteralKind> for TokenKind {
    fn from(lit: LiteralKind) -> Self {
        TokenKind::Literal(lit)
    }
}

impl From<SeparatorKind> for TokenKind {
    fn from(separator: SeparatorKind) -> Self {
        TokenKind::Separator(separator)
    }
}

impl From<OperatorKind> for TokenKind {
    fn from(operator: OperatorKind) -> Self {
        TokenKind::Operator(operator)
    }
}

/// Token that represents a lexeme.
///
/// Lexemes are created by lexer, and then processed by parser. Each token also
/// contains information about region of the source file
/// it came from (in the form of [`Span`]).
pub struct Token {
    kind: TokenKind,
    span: Span
}

impl Token {
    /// Creates a new token from the given [`TokenKind`] and [`Span`]
    ///
    /// # Arguments
    ///
    /// * `kind` - Type of the token with associated data (represented by [`TokenKind`] enum)
    /// * `span` - Location the token comes from ([`Span`] of this token)
    pub fn new(kind: TokenKind, span: Span) -> Token {
        Token {
            kind,
            span
        }
    }
}

/// Test helpers for [`Token`]
#[cfg(test)]
impl Token {
    /// Convenience method to create an integer literal token
    ///
    /// * `value` - The integer this token represents
    /// * `span` - Where the token came from
    pub fn int_lit(value: i32, span: Span) -> Token {
        Token::new(LiteralKind::from(LitInt::new(value)).into(), span)
    }

    /// Convenience method to create a string literal token
    ///
    /// * `value` - The string this token represents
    /// * `span` - Indicates the region of the source file where the token came from
    pub fn str_lit(value: String, span: Span) -> Token {
        Token::new(LiteralKind::from(LitStr::new(value)).into(), span)
    }

    /// Convenience method to create a separator token
    ///
    /// # Arguments
    ///
    /// * `separator` - the separator this token represents
    /// * `span` - Indicates the region of the source file where the token came from
    pub fn separator(separator: SeparatorKind, span: Span) -> Token {
        Token::new(separator.into(), span)
    }

    /// Convenience method to create an operator token
    ///
    /// # Arguments
    ///
    /// * `operator` - The operator this token represents
    /// * `span` - Indicates the region of the source file where the token came from
    pub fn operator(operator: OperatorKind, span: Span) -> Token {
        Token::new(operator.into(), span)
    }

    /// Convenience method to create a keyword token
    ///
    /// # Arguments
    ///
    /// * `keyword` - The keyword this token represents
    /// * `span` - Indicates the region of the source file where the token came from
    pub fn keyword(keyword: KeywordKind, span: Span) -> Token {
        Token::new(keyword.into(), span)
    }
}