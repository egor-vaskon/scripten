use std::collections::BTreeSet;
use std::iter::{Peekable, Skip};
use std::ops::{Range, Add, Sub, Mul};
use std::str;
use std::str::Chars;
use crate::diagnostics::Span;

pub trait Source<'a> {
    fn peek(&mut self) -> Option<char>;
    fn next(&mut self) -> Option<char>;
    fn pos(&self) -> u32;
    fn slice<T: Into<Span>>(&self, span: T) -> &'a str;

    fn next_pos(&self) -> u32 {
        self.pos() + 1
    }
}

pub struct StrSource<'a> {
    content: &'a str,
    iter: Peekable<Skip<Chars<'a>>>,
    pos: u32,
    next_pos: u32
}

impl<'a> StrSource<'a> {
    pub fn new(source: &'a str) -> StrSource<'a> {
        let first_ch = source.chars().next();
        let next_pos = if let Some(ch) = first_ch {
            ch.len_utf8() as u32
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

    fn next(&mut self) -> Option<char> {
        if let Some(ch) = self.iter.next() {
            self.pos = self.next_pos;
            self.next_pos += ch.len_utf8() as u32;
            return Some(ch)
        }

        None
    }

    fn pos(&self) -> u32 {
        self.pos
    }

    fn slice<T: Into<Span>>(&self, span: T) -> &'a str {
        let span = span.into();
        &self.content[(span.start() as usize)..(span.end() as usize)]
    }
}