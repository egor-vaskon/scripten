use crate::lexer::identifiers::{IdentId, Identifiers};

/// Public state of the lexer
///
/// Holds the necessary information
/// to perform further code analysis that
/// isn't stored in tokens.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LexerContext {
    /// Stores all identifiers used in program
    idents: Identifiers
}

impl<'a> LexerContext {
    /// Test helper to build instance of `LexerContext`
    /// with specified identifiers
    ///
    /// # Arguments
    /// * `idents` - Identifiers to be stored by the created instance
    #[cfg(test)]
    pub const fn with_idents(idents: Identifiers) -> LexerContext {
        LexerContext {
            idents
        }
    }

    /// Returns reference to the `Identifiers`
    /// stored within current instance
    pub const fn idents(&'a self) -> &'a Identifiers {
        &self.idents
    }

    /// Returns mutable reference to the `Identifiers`
    /// stored within current instance
    pub fn idents_mut(&'a mut self) -> &'a mut Identifiers {
        &mut self.idents
    }
}

impl Default for LexerContext {
    fn default() -> Self {
        LexerContext {
            idents: Identifiers::new()
        }
    }
}