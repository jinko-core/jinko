//! The location module contains helpful types regarding the location of jinko
//! source code. The information is created during the parsing phase, and
//! kept in various Instruction types.
//! Such types usually keep *two* instances of a [`Location`] struct: A start one,
//! and an end one, which indicates the range used to implement said instruction
//! by the programmer. Since such a construct is needed in multiple places, a
//! wrapper type containing a start and end [`Location`] exist: [`SpanTuple`].

use nom_locate::LocatedSpan;

use std::fmt::Display;
use std::num::NonZeroUsize;

/// Base type for the location of jinko instructions, generated during parsing
/// phase.
#[derive(Debug, PartialEq, Clone)]
pub struct Location {
    line: NonZeroUsize,
    column: NonZeroUsize,
}

impl Location {
    pub fn new(line: usize, column: usize) -> Location {
        Location {
            // If we ever create a location with zero as a line or column, panic
            line: NonZeroUsize::new(line).unwrap(),
            column: NonZeroUsize::new(column).unwrap(),
        }
    }

    pub fn line(&self) -> usize {
        self.line.get()
    }

    pub fn column(&self) -> usize {
        self.column.get()
    }
}

impl<T: nom::AsBytes> From<LocatedSpan<T>> for Location {
    fn from(span: LocatedSpan<T>) -> Location {
        Location::new(span.location_line() as usize, span.get_column())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SpanTuple {
    start: Location,
    end: Location,
}

impl SpanTuple {
    pub fn new(start: Location, end: Location) -> SpanTuple {
        SpanTuple { start, end }
    }

    pub fn start(&self) -> &Location {
        &self.start
    }

    pub fn end(&self) -> &Location {
        &self.end
    }

    /// Write the content in the range included by the [`SpanTuple`]. This
    /// methods splits the input in lines and starts printing from the
    /// `start.line()` line at the `start.column()` character until the
    /// `end.line()` and `end.column()`. The "algorithm" is repeated in this
    /// struct's [`to_string`] method, which is only for testing
    /// purposes and contains extra allocations.
    pub fn emit<T: Display>(&self, separator: T, input: &str) {
        // FIXME: As the API and behavior is still young, use self.to_string()
        // for now. This is quite slow, but since emitting an error is already
        // slow, we'll allow it. This NEEDS to be removed at some point once
        // we are sure than the algorithm for emitting spans is stable
        eprintln!("{}", self.to_string(&separator, input))
    }

    fn format_line<T: Display>(&self, separator: &T, line_number: usize, line: &str) -> String {
        format!(
            "{:5} {} {}",
            self.start.line() + line_number,
            separator,
            line
        )
    }

    fn to_string<T: Display>(&self, separator: &T, input: &str) -> String {
        let mut result = String::new();

        if self.start.line() > self.end.line() {
            return result;
        }

        if self.start.line() == self.end.line() && self.start.column() > self.end.column() {
            return result;
        }

        for (i, line) in input.lines().skip(self.start.line() - 1).enumerate() {
            if self.start.line() == self.end.line() {
                result.push_str(&self.format_line(
                    separator,
                    i,
                    &line[self.start.column() - 1..self.end.column()],
                ));
                break;
            }
            // Four possible cases: First line, for which we need to skip
            // start.column characters
            else if i == 0 {
                result.push_str(&self.format_line(
                    separator,
                    i,
                    &line[(self.start.column() - 1)..],
                ));
            }
            // Last line, for which we only push up to end.column characters
            else if self.start.line() + i == self.end.line() {
                result.push_str(&self.format_line(separator, i, &line[..self.end.column() - 1]));
                break;
            } else if self.start.line() == self.end.line() {
                result.push_str(&self.format_line(
                    separator,
                    i,
                    &line[self.start.column()..self.end.column() + 1],
                ));
            }
            // Any other line, which gets pushed entirely into the string
            else {
                result.push_str(&self.format_line(separator, i, line));
            }

            result.push('\n');
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const CODE: &str = r#"type Nothing;
type Maybe[T](T | Nothing);

func map[T, U](m: Maybe[T], fn: func(T) -> U) -> Maybe[U] {
    switch m {
        n: Nothing => Nothing,
        t: T => fn(t)
    }
}

func into[T, E](m: Maybe[T], err_fn: fn() -> E) -> Result[T, E] {
    switch m {
        n: Nothing => Err(err_fn),
        t: T => Ok(t),
    }
}

func is_nothing[T](m: Maybe[T]) -> bool {
    switch m {
        n: Nothing => true,
        _ => false
    }
}

func is_some[T](m: Maybe[T]) -> bool {
    m.is_nothing().not()
}
        "#;

    #[test]
    fn span_same_line() {
        let s = Location::new(1, 3);
        let e = Location::new(1, 6);
        let span = SpanTuple::new(s, e);

        assert_eq!(span.to_string(&'>', CODE), "    1 > pe N");
    }

    #[test]
    fn multi_line_span() {
        let s = Location::new(1, 1);
        let e = Location::new(9, 2);
        let span = SpanTuple::new(s, e);

        assert_eq!(
            span.to_string(&'>', CODE),
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
        let span = SpanTuple::new(s, e);

        assert!(span.to_string(&' ', CODE).is_empty());
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
