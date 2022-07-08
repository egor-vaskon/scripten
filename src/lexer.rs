pub mod context;
pub mod identifiers;
pub mod token;

use std::fmt::{Debug, Display, Formatter};
use std::iter::{Peekable, Skip};
use std::ops::Range;
use std::num::ParseIntError;
use std::str::Chars;
use crate::diagnostics::Span;
use crate::lexer::context::LexerContext;
use crate::lexer::token::{LiteralKind, LitInt, LitStr, OperatorKind, SeparatorKind, Token, TokenKind};
use crate::source::Source;

#[cfg(test)]
mod test {
    use crate::lexer::token::{KeywordKind, Token};
    use crate::source::StrSource;
    use super::*;

    type AnyResult = std::result::Result<(), Box<dyn std::error::Error>>;

    /*fn get_tokens(source: &str) -> Result<(Vec<Token>, LexerContext)> {
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
    }*/

    /*#[test]
    fn test_blank() -> AnyResult {
        lexer_test!("", tokens!());
        lexer_test!("  ", tokens!());

        Ok(())
    }

    #[test]
    fn test_integers() -> AnyResult {
        let expected = tokens! {
            int 123;
            int 567;
            int 1;
            int 5;
        };

        lexer_test!(" 123  567  1 5", expected);
        lexer_test!(" 123  567  1 5  ", expected);

        Ok(())
    }

    #[test]
    fn test_keywords() -> AnyResult {
        let expected = tokens! {
            int 1;
            tkn KeywordKind::Xor;
            int 2;
        };

        lexer_test!(" 1 xor 2  ", expected);

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
            Punctuation::Asterisk.into(),
            Identifier::from_id(2).into(),
            Punctuation::Add.into(),
            Token::integer(5678),
            Punctuation::Add.into(),
            Identifier::from_id(3).into(),
            Delimiter::opening(DelimiterKind::SquareBracket).into(),
            Token::integer(2),
            Punctuation::Asterisk.into(),
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
    }*/
}

#[derive(Debug, Clone)]
pub enum ParseTokenError {
    ParseIntError(ParseIntError)
}

impl Display for ParseTokenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseTokenError::ParseIntError(err) =>
                f.write_fmt(format_args!("{}", err))
        }
    }
}

impl std::error::Error for ParseTokenError {}

impl From<ParseIntError> for ParseTokenError {
    fn from(value: ParseIntError) -> Self {
        ParseTokenError::ParseIntError(value)
    }
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    UnexpectedCharacter(String),
    ParseTokenError(ParseTokenError),
    Eof
}

impl From<ParseTokenError> for ErrorKind {
    fn from(value: ParseTokenError) -> Self {
        ErrorKind::ParseTokenError(value)
    }
}

impl From<ParseIntError> for ErrorKind {
    fn from(value: ParseIntError) -> Self {
        ParseTokenError::from(value).into()
    }
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    span: Span
}

impl Error {
    fn new(kind: ErrorKind, span: Span) -> Error {
        Error {
            kind, span
        }
    }

    fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    fn span(&self) -> Span {
        self.span
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ErrorKind::UnexpectedCharacter(ref err) =>
                f.write_str(err),
            ErrorKind::Eof => f.write_str("unexpected EOF"),
            ErrorKind::ParseTokenError(err) =>
                f.write_fmt(format_args!("cannot parse token in [{}..{}) ({})",
                                         self.span.start(),
                                         self.span.end(),
                                         err))
        }
    }
}

impl std::error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;

/// Trait that describes how to create
/// token from the stream of characters
///
/// This trait is the core part of lexer.
trait NextToken {
    /// Returns whether this token could start with the
    /// given character
    ///
    /// The default implementation always returns true.
    ///
    /// # Arguments
    /// * `ch` - First character of the token
    fn check(ch: char) -> bool {
        return true;
    }

    /// Returns the next token or error
    ///
    /// Implementations may assume that [`NextToken::check`] returns
    /// true for the first character of input and [`Source::pos`] (later - `cursor position`)
    /// for `source` returns the position of the first character (cursor points to `first`).
    /// After the method returns, the cursor should be set right after the
    /// last character belonging to the token. In case of an error, the cursor position should left
    /// unchanged since the method was called.
    ///
    /// # Arguments
    /// * `first` - The first character of `source`
    /// * `source` - An abstract source of characters. The cursor is set to the first character
    fn next_token<'a, S>(first: char, source: S) -> Result<Self>
        where
            S: Source<'a>,
            Self: Sized;
}

/// Helper macro to combine uses of [`NextToken`] trait
macro_rules! try_next_token {
    ($type:ty, $first:expr, $source:expr) => {
        if <$type as NextToken>::check($first) {
            let result = <$type as NextToken>::next_token($first, $source)?;
            return Ok(result.into());
        }
    }
}

impl NextToken for LitInt {
    fn check(ch: char) -> bool {
        return ch.is_ascii_digit()
    }

    fn next_token<'a, S>(first: char, mut source: S) -> Result<Self>
        where
            S: Source<'a>,
            Self: Sized
    {
        let begin = source.pos();
        while let Some(ch) = source.peek() {
            source.next().unwrap(); // Move cursor forward

            if !ch.is_ascii_digit() {
                break;
            }
        }

        let span = Span::new(begin, source.pos());
        let raw_value = source.slice(span);
        return match raw_value.parse::<LitInt>() {
            Ok(val) => Ok(val),
            Err(err) =>
                Err(Error::new(ParseTokenError::from(err).into(), span))
        }
    }
}

impl NextToken for LitStr {
    fn check(ch: char) -> bool {
        return ch == '"'
    }

    fn next_token<'a, S>(first: char,mut source: S) -> Result<Self>
        where
            S: Source<'a>,
            Self: Sized
    {
        source.next().unwrap(); // Skip quote

        let start = source.pos();

        while let Some(ch) = source.peek() {
            source.next().unwrap(); // Move cursor forward

            if ch == '"' {
                break;
            }
        }

        let span = Span::new(start, source.pos());
        let raw_value = source.slice(span);

        source.next().unwrap(); // Skip quote

        // Unwrap will never panic, because the error type is Infallible
        return Ok(raw_value.parse::<LitStr>().unwrap())
    }
}

impl NextToken for LiteralKind {
    fn check(ch: char) -> bool {
        LitInt::check(ch) || LitStr::check(ch)
    }

    fn next_token<'a, S>(first: char, source: S) -> Result<Self>
        where
            S: Source<'a>,
            Self: Sized
    {
        try_next_token!(LitInt, first, source);
        Ok(LitStr::next_token(first, source).unwrap().into())
    }
}

impl NextToken for SeparatorKind {
    fn check(ch: char) -> bool {
        SeparatorKind::is_separator(ch)
    }

    fn next_token<'a, S>(first: char, mut source: S) -> Result<Self>
        where
            S: Source<'a>,
            Self: Sized
    {
        source.next().unwrap();
        Ok(SeparatorKind::from_ch(first).unwrap())
    }
}

impl NextToken for OperatorKind {
    fn check(ch: char) -> bool {
        OperatorKind::starts_with(ch)
    }

    fn next_token<'a, S>(first: char, mut source: S) -> Result<Self>
        where
            S: Source<'a>,
            Self: Sized
    {
        Ok(OperatorKind::from(first, source))
    }
}

