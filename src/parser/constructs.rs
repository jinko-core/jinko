//! A `Construct` is a complex set of tokens. For example, `fn()` is an identifier, a
//! left parenthesis and a right parenthesis. Together, they constitute a function call.
//! In the same vein, `x = 12;` is 4 tokens used to represent variable assignment.
//! Therefore, constructs use tokens while the parser only uses constructs. This is an
//! abstraction for all possible ways to parse a line in broccoli.
//!
//! Each of the functions in that module contain the grammar they represent above their
//! name. The syntax used for the grammar is loosely based on regular expressions and
//! globbing. One can use * to indicate 0 or more, ? to indicate 1 or more, etc etc.
//! Optional parameters are included between brackets. For example,
//!
//! `[mut] <identifier> = <const> | <function_call> | <block> | <identifier>`
//!
//! is the grammar for a variable assignment.

use nom::{branch::alt, IResult};

use super::tokens::Token;

pub struct Construct;

impl Construct {
    /// Constants are raw values in the source code. For example, `"string"`, `12` and
    /// `0.5`.
    ///
    /// `'<any_char>' | "<any_char>*" | <num>? | <num>?.<num>?`
    pub fn constant(input: &str) -> IResult<&str, &str> {
        // FIXME: Return primitive maybe ?
        todo!()
    }

    /// Parses an identifier. An identifier can have alphanumeric characters. It cannot
    /// only be constituted of numbers. An identifier is NOT a `const`.
    ///
    /// `<alphanumeric>?`
    pub fn identifier(input: &str) -> IResult<&str, &str> {
        todo!()
    }

    /// When a function is called in the source code.
    ///
    /// ```
    /// fn(); // Function call
    /// fn() // Call the function `fn` and use the return result as an expression
    /// x = fn(); // Assign the result of the function call to the variable x
    /// ```
    ///
    /// `<identifier>(<arg_list>)`
    pub fn function_call(input: &str) -> IResult<&str, &str> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
