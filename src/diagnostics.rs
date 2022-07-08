use std::ops::Range;

/// Represents some region of input source file.
///
/// Stores information of where the syntax element
/// came from. In order to retrieve more detailed
/// information about the origin of code fragment,
/// refer to the [`Source`] trait methods.
///
/// [`Source`]: crate::source::Source
#[derive(Debug, Copy, Clone)]
pub struct Span {
    start: u32,
    end: u32
}

impl Span {
    /// Crates a new Span representing the specified region.
    ///
    /// # Arguments
    ///
    /// * `start` - starting position of the source region (inclusive)
    /// * `end` - end of the source region (exclusive)
    pub const fn new(start: u32, end: u32) -> Span {
        Span {
            start,
            end
        }
    }

    /// Returns starting position of the source
    /// region denoted by this Span. (inclusive)
    ///
    /// # Example
    ///
    /// ```
    /// use scripten::diagnostics::Span;
    ///
    /// let span = Span::new(0, 1);
    /// assert_eq!(span.start(), 0);
    /// ```
    pub const fn start(&self) -> u32 {
        self.start
    }

    /// Returns the end of source region
    /// denoted by this Span. (exclusive)
    ///
    /// # Example
    ///
    /// ```
    /// use scripten::diagnostics::Span;
    ///
    /// let span = Span::new(0, 1);
    /// assert_eq!(span.end(), 1);
    /// ```
    pub const fn end(&self) -> u32 {
        self.end
    }

    /// Returns length of the region represented
    /// by this [`Span`].
    pub const fn len(&self) -> u32 {
        self.end-self.start
    }

    /// Returns span representing an empty region
    pub const fn empty() -> Span {
        Span::new(0, 0)
    }

    /// Returns span that represents any possible region
    pub const fn any() -> Span {
        Span::new(1, 0)
    }

    /// Returns span representing one character
    /// with specified position.
    ///
    /// # Arguments
    /// * `pos` - The position of character this span represents
    pub const fn single(pos: u32) -> Span {
        Span::new(pos, pos+1)
    }
}