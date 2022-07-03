use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::iter::{Peekable, Skip};
use std::ops::Range;
use std::num::ParseIntError;
use std::str::Chars;
use crate::token::{CharExt, Delimiter, DelimiterKind, Identifier, IdentifierRegistry, Keyword, Literal, Punctuation, Token, TokenKind};

#[cfg(test)]
mod test {
    use super::*;

    type AnyResult = std::result::Result<(), Box<dyn std::error::Error>>;

    fn get_tokens(source: &str) -> Result<(Vec<Token>, LexerContext)> {
        let mut context = LexerContext::default();
        let mut source = StrSource::new(source);
        let tokens: Result<Vec<Token>> = source.tokens(&mut context).collect();
        tokens.map(|tokens| (tokens, context))
    }

    macro_rules! lexer_test {
        ($source:expr, $tokens:expr $(, $context:expr)?) => {
            {
                let (tokens, context) = get_tokens($source)?;

                assert_eq!(tokens, $tokens);
                $(assert_eq!(context, $context);)*
            }
        }
    }

    #[test]
    fn test_blank() -> AnyResult {
        lexer_test!("", vec!());
        lexer_test!("  ", vec!());

        Ok(())
    }

    #[test]
    fn test_integers() -> AnyResult {
        let expected = vec!(
            Token::integer(123),
            Token::integer(567),
            Token::integer(1),
            Token::integer(5));

        lexer_test!(" 123  567  1 5", expected);
        lexer_test!(" 123  567  1 5  ", expected);

        Ok(())
    }

    #[test]
    fn test_keywords() -> AnyResult {
        let expected = vec!(
            Token::integer(1),
            Keyword::Xor.into(),
            Token::integer(2)
        );

        lexer_test!(" 1 xor 2  ", expected);
        lexer_test!("    if", vec!(Keyword::If.into()));

        Ok(())
    }

    #[test]
    fn test_idents() -> AnyResult {
        let tokens = vec!(
            Keyword::If.into(),
            Identifier::from_id(1).into(),
            Keyword::Xor.into(),
            Token::integer(2)
        );

        let idents = IdentifierRegistry::with_idents(vec!(
            "a12"
        ));

        lexer_test!(" if a12 xor 2", tokens, LexerContext::with_idents(idents));

        Ok(())
    }

    #[test]
    fn test_delimiters() -> AnyResult {
        let tokens = vec! (
            Keyword::If.into(),
            Delimiter::opening(DelimiterKind::Parenthesis).into(),
            Identifier::from_id(1).into(),
            Delimiter::opening(DelimiterKind::SquareBracket).into(),
            Identifier::from_id(2).into(),
            Delimiter::closing(DelimiterKind::SquareBracket).into(),
            Keyword::Xor.into(),
            Token::integer(1),
            Delimiter::closing(DelimiterKind::Parenthesis).into()
        );

        let idents = IdentifierRegistry::with_idents(vec!(
            "arr",
            "i"
        ));

        lexer_test!(" if (arr[i] xor 1) ", tokens, LexerContext::with_idents(idents));

        Ok(())
    }

    #[test]
    fn test_punctuation() -> AnyResult {
        let tokens = vec! (
            Keyword::Let.into(),
            Identifier::from_id(1).into(),
            Punctuation::Assign.into(),
            Token::integer(1),
            Punctuation::Mul.into(),
            Identifier::from_id(2).into(),
            Punctuation::Add.into(),
            Token::integer(5678),
            Punctuation::Add.into(),
            Identifier::from_id(3).into(),
            Delimiter::opening(DelimiterKind::SquareBracket).into(),
            Token::integer(2),
            Punctuation::Mul.into(),
            Identifier::from_id(4).into(),
            Punctuation::Sub.into(),
            Identifier::from_id(2).into(),
            Delimiter::closing(DelimiterKind::SquareBracket).into(),
        );

        let idents = IdentifierRegistry::with_idents(vec!(
            "sum_of_numbers",
            "b",
            "arr",
            "i",
        ));

        lexer_test!(
            " let sum_of_numbers = 1*b + 5678+arr[2*i-b] ",
            tokens,
            LexerContext::with_idents(idents));

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    UnexpectedCharacter(char),
    UnexpectedToken(Range<usize>),
    ParseIntError(ParseIntError),
    Eof
}

impl From<ParseIntError> for ErrorKind {
    fn from(value: ParseIntError) -> Self {
        ErrorKind::ParseIntError(value)
    }
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::UnexpectedCharacter(ref ch) => {
                f.write_fmt(format_args!("unexpected character: {}", ch))
            }
            ErrorKind::UnexpectedToken(tkn) => {
                f.write_fmt(format_args!("unexpected token in [{}..{})", tkn.start, tkn.end))
            }
            ErrorKind::Eof => f.write_str("unexpected EOF"),
            ErrorKind::ParseIntError(err) => {
                f.write_fmt(format_args!("{}", err))
            }
        }
    }
}

impl Error for ErrorKind {}

pub type Result<T> = std::result::Result<T, ErrorKind>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LexerContext {
    idents: IdentifierRegistry
}

impl<'a> LexerContext {
    pub fn with_idents(registry: IdentifierRegistry) -> LexerContext {
        LexerContext {
            idents: registry
        }
    }

    pub fn idents_mut(&'a mut self) -> &'a mut IdentifierRegistry {
        &mut self.idents
    }
}

impl Default for LexerContext {
    fn default() -> Self {
        LexerContext {
            idents: IdentifierRegistry::new()
        }
    }
}

pub trait Source<'a> {
    fn peek(&mut self) -> Option<char>;
    fn get(&mut self) -> Option<char>;
    fn pos(&self) -> usize;
    fn slice(&self, range: Range<usize>) -> &'a str;

    fn next_pos(&self) -> usize {
        self.pos()+1
    }

    fn tokens(&'a mut self, context: &'a mut LexerContext) -> Tokens<'a, Self>
        where Self: Sized {
        Tokens::new(self, context)
    }
}

pub struct Tokens<'a, T: Source<'a>> {
    lexer: Lexer<'a, T>
}

impl<'a, T: Source<'a>> Tokens<'a, T> {
    fn new(source: &'a mut T, context: &'a mut LexerContext) -> Tokens<'a, T> {
        Tokens {
            lexer: Lexer::new(source, context)
        }
    }
}

impl<'a, T: Source<'a>> Iterator for Tokens<'a, T> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lexer.next_token() {
            Ok(token) => Some(Ok(token)),
            Err(err) => {
                if let ErrorKind::Eof = err {
                    return None
                }

                return Some(Err(err))
            }
        }
    }
}

struct StrSource<'a> {
    content: &'a str,
    iter: Peekable<Skip<Chars<'a>>>,
    pos: usize,
    next_pos: usize
}

impl<'a> StrSource<'a> {
    pub fn new(source: &'a str) -> StrSource<'a> {
        let first_ch = source.chars().next();
        let next_pos = if let Some(ch) = first_ch {
              ch.len_utf8()
        } else {
            0
        };

        StrSource {
            content: source,
            iter: source.chars().skip(1).peekable(),
            pos: 0,
            next_pos
        }
    }
}

impl<'a> Source<'a> for StrSource<'a> {
    fn peek(&mut self) -> Option<char> {
        self.iter.peek().map(|ch| *ch)
    }

    fn get(&mut self) -> Option<char> {
        if let Some(ch) = self.iter.next() {
            self.pos = self.next_pos;
            self.next_pos += ch.len_utf8();
            return Some(ch)
        }

        None
    }

    fn pos(&self) -> usize {
        self.pos
    }

    fn slice(&self, range: Range<usize>) -> &'a str {
        &self.content[range]
    }
}

struct Lexer<'a, T: Source<'a>> {
    source: &'a mut T,
    context: &'a mut LexerContext
}

impl<'a, T: Source<'a>> Lexer<'a, T> {
    pub fn new(source: &'a mut T,
               context: &'a mut LexerContext) -> Lexer<'a, T> {
        Lexer {
            source,
            context
        }
    }

    pub fn next_token(&mut self) -> Result<Token> {
        while Some(true) == self.source.peek().map(|x| x.is_whitespace()) {
            self.source.get().expect(&ErrorKind::Eof.to_string());
        }

        if let Some(ch) = self.source.peek() {
            debug_assert_eq!(self.source.get(), Some(ch));

            return if ch.is_ascii_digit() {
                self.number()
            } else if ch.is_alphabetic() {
                self.ident_or_keyword()
            } else if ch.is_delimiter()  {
                self.delimiter(ch)
            } else {
                self.punctuation(ch)
            }
        }

        Err(ErrorKind::Eof)
    }

    fn number(&mut self) -> Result<Token> {
        let begin = self.source.pos();

        while let Some(ch) = self.source.peek() {
            if !ch.is_ascii_digit() {
                break
            } else {
                self.source.get().unwrap();
            }
        }

        let value = self.source.slice(begin..self.source.next_pos());
        let value: i32 = value.parse()?;

        return Ok(Token::integer(value))
    }

    fn ident_or_keyword(&mut self) -> Result<Token> {
        let begin = self.source.pos();

        while let Some(ch) = self.source.peek() {
            if !ch.is_alphanumeric() && ch != '_' {
                break
            } else {
                self.source.get().unwrap();
            }
        }

        let value = self.source.slice(begin..self.source.next_pos());

        match Keyword::from_str(value) {
            Some(keyword) => Ok(keyword.into()),
            None => Ok(Identifier::new(self.context.idents_mut(), value).into())
        }
    }

    fn delimiter(&mut self, ch: char) -> Result<Token> {
        match ch {
            '(' => Ok(Delimiter::opening(DelimiterKind::Parenthesis).into()),
            ')' => Ok(Delimiter::closing(DelimiterKind::Parenthesis).into()),

            '[' => Ok(Delimiter::opening(DelimiterKind::SquareBracket).into()),
            ']' => Ok(Delimiter::closing(DelimiterKind::SquareBracket).into()),

            '{' => Ok(Delimiter::opening(DelimiterKind::CurlyBracket).into()),
            '}' => Ok(Delimiter::closing(DelimiterKind::CurlyBracket).into()),

            _ => Err(ErrorKind::UnexpectedCharacter(ch)),
        }
    }

    fn punctuation(&mut self, ch: char) -> Result<Token> {
        match ch {
            '+' => Ok(Punctuation::Add.into()),
            '-' => Ok(Punctuation::Sub.into()),
            '*' => Ok(Punctuation::Mul.into()),
            '/' => Ok(Punctuation::Div.into()),
            '=' => Ok(Punctuation::Assign.into()),

            _ => Err(ErrorKind::UnexpectedCharacter(ch))
        }
    }
}