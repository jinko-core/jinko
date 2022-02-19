//! The location module contains helpful types regarding the location of jinko
//! source code. The information is created during the parsing phase, and
//! kept in various Instruction types.
//! Such types usually keep *two* instances of a [`Location`] struct: A start one,
//! and an end one, which indicates the range used to implement said instruction
//! by the programmer. Since such a construct is needed in multiple places, a
//! wrapper type containing a start and end [`Location`] exist: [`SpanTuple`].

use nom_locate::LocatedSpan;

use std::cmp::max;
use std::fmt::Display;
use std::fs;
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};

use crate::log;

#[derive(Debug, PartialEq, Clone)]
enum Column {
    EndOfLine,
    Precise(NonZeroUsize),
}

/// Base type for the location of jinko instructions, generated during parsing
/// phase.
#[derive(Debug, PartialEq, Clone)]
pub struct Location {
    line: NonZeroUsize,
    column: Column,
}

impl Location {
    pub fn new(line: usize, column: usize) -> Location {
        Location {
            // If we ever create a location with zero as a line or column, panic
            line: NonZeroUsize::new(line).unwrap(),
            column: Column::Precise(NonZeroUsize::new(column).unwrap()),
        }
    }

    pub fn whole_line(line: usize) -> Location {
        Location {
            // If we ever create a location with zero as a line, panic
            line: NonZeroUsize::new(line).unwrap(),
            column: Column::EndOfLine,
        }
    }

    pub fn line(&self) -> usize {
        self.line.get()
    }

    pub fn column(&self) -> usize {
        match self.column {
            Column::Precise(nz) => nz.get(),
            // When accessing a location which represents a whole line, we can
            // simply say that it starts at the first character: Thus, return 1
            Column::EndOfLine => 1usize,
        }
    }
}

impl<T: nom::AsBytes, X> From<LocatedSpan<T, X>> for Location {
    fn from(span: LocatedSpan<T, X>) -> Location {
        Location::new(span.location_line() as usize, span.get_column())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SpanTuple {
    path: Option<PathBuf>,
    start: Location,
    end: Location,
}

impl SpanTuple {
    pub fn new<T: Into<PathBuf>>(path: Option<T>, start: Location, end: Location) -> SpanTuple {
        SpanTuple {
            path: path.map(|p| p.into()),
            start,
            end,
        }
    }

    pub fn start(&self) -> &Location {
        &self.start
    }

    pub fn end(&self) -> &Location {
        &self.end
    }

    pub fn path(&self) -> &Option<PathBuf> {
        &self.path
    }

    /// Amount of lines to use when creating before and after context for a
    /// [`SpanTuple`]
    const CONTEXT_LINES: usize = 3;

    /// Create a span context from a given range of text. This returns two new
    /// [`SpanTuple`]s, which wrap around the original one in order to add context.
    /// The before context is created by substracting the line number, if possible,
    /// and the after context by adding to the line number.
    /// You can then display both of these [`SpanTuple`]s as you would normally.
    /// Since the before context depends on whether or not enough lines are
    /// available before `self`, it will sometimes not be possible to create
    /// one. An after context is always given, as checking whether or not there
    /// are enough lines in the input is a very costly operation and unecessary:
    /// if there aren't enough lines, they will all be skipped during the emission
    /// and nothing will be printed.
    pub fn generate_context(&self) -> (Option<SpanTuple>, SpanTuple) {
        let before_ctx = if self.start().line() > SpanTuple::CONTEXT_LINES + 1 {
            let before_start =
                Location::whole_line(max(self.start().line() - SpanTuple::CONTEXT_LINES, 1));
            let before_end = Location::whole_line(self.start().line() - 1);
            Some(SpanTuple::new(self.path.clone(), before_start, before_end))
        } else {
            None
        };

        let after_start = Location::whole_line(self.end().line() + 1);
        let after_end = Location::whole_line(self.end().line() + SpanTuple::CONTEXT_LINES);
        let after_ctx = SpanTuple::new(self.path.clone(), after_start, after_end);

        (before_ctx, after_ctx)
    }

    /// Write the content in the range included by the [`SpanTuple`]. This
    /// methods splits the input in lines and starts printing from the
    /// `start.line()` line at the `start.column()` character until the
    /// `end.line()` and `end.column()`. The "algorithm" is repeated in this
    /// struct's [`to_string`] method, which is only for testing
    /// purposes and contains extra allocations.
    pub fn emit<T1: Display, T2: Display>(&self, separator: T1, repetitor: T2) {
        // FIXME: As the API and behavior is still young, use self.to_string()
        // for now. This is quite slow, but since emitting an error is already
        // slow, we'll allow it. This NEEDS to be removed at some point once
        // we are sure than the algorithm for emitting spans is stable
        eprintln!("{}", self.to_string(&separator, &repetitor))
    }

    fn format_line<T: Display>(&self, separator: &T, line_number: usize, line: &str) -> String {
        format!(
            "{:5} {} {}",
            self.start.line() + line_number,
            separator,
            line
        )
    }

    fn from_path<T1: Display, T2: Display>(
        &self,
        separator: &T1,
        repetitor: &T2,
        path: &Path,
    ) -> String {
        let mut result = String::new();

        // If the file has been removed between parsing and emitting errors...
        // We're in trouble
        let input = fs::read_to_string(path).unwrap();

        if self.start.line() > self.end.line() {
            return result;
        }

        if self.start.line() == self.end.line() && self.start.column() > self.end.column() {
            return result;
        }

        for (i, line) in input.lines().skip(self.start.line() - 1).enumerate() {
            let start_col = match self.start.column {
                Column::EndOfLine => 1,
                Column::Precise(nz) => nz.get(),
            };
            let end_col = match self.end.column {
                Column::EndOfLine => line.len(),
                Column::Precise(nz) => nz.get(),
            };

            if self.start.line() == self.end.line() {
                result.push_str(&self.format_line(separator, i, line));
                result.push('\n');

                let mut underline = String::new();
                if start_col != 1 {
                    for _ in 0..start_col - 1 {
                        underline.push(' ');
                    }
                }
                for _ in start_col..end_col {
                    underline = format!("{}{}", underline, repetitor);
                }

                result.push_str(&self.format_line(&' ', i, &underline));
                break;
            }
            // Four possible cases: First line, for which we need to skip
            // start.column characters
            else if i == 0 {
                result.push_str(&self.format_line(separator, i, &line[(start_col - 1)..]));
            }
            // Last line, for which we only push up to end.column characters
            else if self.start.line() + i == self.end.line() {
                result.push_str(&self.format_line(separator, i, &line[..end_col]));
                break;
            } else if self.start.line() == self.end.line() {
                result.push_str(&self.format_line(separator, i, &line[start_col..end_col]));
            }
            // Any other line, which gets pushed entirely into the string
            else {
                result.push_str(&self.format_line(separator, i, line));
            }

            result.push('\n');
        }

        result
    }

    fn to_string<T1: Display, T2: Display>(&self, separator: &T1, repetitor: &T2) -> String {
        log!("span: {:?}", &self);
        match &self.path {
            Some(path) => self.from_path(separator, repetitor, path),
            None => String::new(), // FIXME: Do we want to return an empty string if there is no path?
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn span_same_line() {
        let s = Location::new(1, 3);
        let e = Location::new(1, 6);
        let span = SpanTuple::new(
            Some(PathBuf::from("tests/fixtures/span_test/code.jk")),
            s,
            e,
        );

        assert_eq!(
            span.to_string(&'>', &'-'),
            r#"    1 > type Nothing;
    1     ---"#
        );
    }

    #[test]
    fn multi_line_span() {
        let s = Location::new(1, 1);
        let e = Location::new(9, 1);
        let span = SpanTuple::new(
            Some(PathBuf::from("tests/fixtures/span_test/code.jk")),
            s,
            e,
        );

        assert_eq!(
            span.to_string(&'>', &'_'),
            r#"    1 > type Nothing;
    2 > type Maybe[T](T | Nothing);
    3 > 
    4 > func map[T, U](m: Maybe[T], fn: func(T) -> U) -> Maybe[U] {
    5 >     switch m {
    6 >         n: Nothing => Nothing,
    7 >         t: T => fn(t)
    8 >     }
    9 > }"#
        );
    }

    #[test]
    fn span_reversed_prints_nothing() {
        let s = Location::new(9, 1);
        let e = Location::new(1, 1);
        let span = SpanTuple::new(
            Some(PathBuf::from("tests/fixtures/span_test/code.jk")),
            s,
            e,
        );

        assert!(span.to_string(&' ', &' ').is_empty());
    }

    #[test]
    #[should_panic]
    fn zero_line() {
        Location::new(0, 15);
    }

    #[test]
    #[should_panic]
    fn zero_col() {
        Location::new(10, 0);
    }

    #[test]
    #[should_panic]
    fn zero_line_col() {
        Location::new(0, 0);
    }
}
