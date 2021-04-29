//! String interpolation is performed during execution of a `JkString`.
//! The format used is the following: {expression}. This will replace {expression} with
//! the current value of the `expression`'s execution in the interpreter at that time.
//! In order to use the '{' or '}' character themselves, use '{{' and '}}'.

use crate::parser::constructs::Construct;
use crate::{InstrKind, Instruction, Interpreter, JkErrKind, JkError};

use nom::bytes::complete::{is_not, take_while};
use nom::character::complete::char;
use nom::combinator::opt;
use nom::multi::many0;
use nom::sequence::delimited;
use nom::IResult;

const L_DELIM: char = '{';
const R_DELIM: char = '}';

pub struct JkStringFmt;

impl JkStringFmt {
    /// Consume input until a certain character is met. This is useful for interpolation,
    /// as consume_until("Some non interpolated text {some_var}", '{') will return
    /// "Some non interpolated text " and leave "{some_var}", which can easily be
    /// separated into a parsable expression
    fn consume_until(input: &str, limit: char) -> IResult<&str, Option<&str>> {
        match opt(take_while(|c| c != limit))(input) {
            Ok((i, Some(data))) => match data.len() {
                0 => Ok((i, None)),
                _ => Ok((i, Some(data))),
            },
            Ok(v) => Ok(v),
            Err(e) => Err(e),
        }
    }

    /// Parse a "pre expression" in a string to interpolate. The pre expressions are
    /// highlighted in the following string, spaces included
    ///
    /// ```
    /// "Hey my name is {name} and I am {age} years old"
    ///  ^^^^^^^^^^^^^^^      ^^^^^^^^^^     ^^^^^^^^^^
    /// ```
    fn pre_expr(input: &str) -> IResult<&str, Option<&str>> {
        JkStringFmt::consume_until(input, L_DELIM)
    }

    /// Parse an expression in a string to interpolate. The expressions are
    /// highlighted in the following string, delimiters included. The expressions go
    /// through the same parsing rules as regular jinko expressions. They must be valid
    /// code.
    ///
    /// ```
    /// "Hey my name is {name} and I am {age} years old"
    ///                 ^^^^^^          ^^^^^
    /// ```
    fn expr(input: &str) -> IResult<&str, Option<Box<dyn Instruction>>> {
        let (input, expr) = match opt(delimited(char(L_DELIM), is_not("{}"), char(R_DELIM)))(input)?
        {
            (i, Some("")) | (i, None) => return Ok((i, None)), // Early return, no need to parse an empty input
            (i, Some(e)) => (i, e),
        };

        let (_, expr) = Construct::instruction(expr)?;

        Ok((input, Some(expr)))
    }

    /// Everything in the string is either a `pre_expr` or an `expr`
    /// The string "Hello {name}" has a pre_expr "Hello" and an expression "name".
    /// The string "{name}, how are you?" has an empty pre_expr, an expression "name", a
    /// pre_expr ", how are you?" and an empty expr
    fn parser(input: &str) -> IResult<&str, (Option<&str>, Option<Box<dyn Instruction>>)> {
        use nom::error::{ErrorKind, ParseError};
        use nom::Err;

        let (input, pre_expr) = JkStringFmt::pre_expr(input)?;
        let (input, expr) = JkStringFmt::expr(input)?;

        match (pre_expr, &expr) {
            // Stop multi-parsing when we couldn't parse anything anymore
            (None, None) => Err(Err::Error(ParseError::from_error_kind(
                input,
                ErrorKind::OneOf,
            ))),
            _ => Ok((input, (pre_expr, expr))),
        }
    }

    /// Execute a parsed expression in the current context. This function returns the
    /// expression's execution's result as a String
    fn interpolate_expr(
        expr: Box<dyn Instruction>,
        s: &str,
        interpreter: &mut Interpreter,
    ) -> Result<String, JkError> {
        // We must check if the expression is a statement or not. If it is
        // an expression, then it is invalid in an Interpolation context
        match expr.kind() {
            InstrKind::Statement => Err(JkError::new(
                JkErrKind::Interpreter,
                format!("invalid argument to string interpolation: {}", expr.print()),
                None,
                String::from(s),
            )), // FIXME: Fix loc and input
            InstrKind::Expression(_) => {
                match expr.execute(interpreter)? {
                    // FIXME: Call some jinko code, in order to enable custom types,
                    // instead of `to_string()` in Rust
                    InstrKind::Expression(Some(res)) => Ok(res.to_string()),
                    // We just checked that we couldn't execute
                    // anything other than an expression. This pattern should never
                    // be encountered
                    _ => unreachable!(),
                }
            }
        }
    }

    /// Interpolates the string passed as parameter
    pub fn interpolate(s: &str, interpreter: &mut Interpreter) -> Result<String, JkError> {
        // We know the final string will be roughly the same size as `s`. We can pre-allocate
        // to save some performance
        let mut result = String::with_capacity(s.len());

        let (_, expressions) = many0(JkStringFmt::parser)(s)?;

        for (pre_expr, expr) in expressions {
            result.push_str(pre_expr.unwrap_or(""));
            match expr {
                Some(expr) => {
                    result.push_str(&JkStringFmt::interpolate_expr(expr, s, interpreter)?)
                }
                None => {}
            }
        }

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_interpolation() {
        let s = "nothing to change here";
        let mut interpreter = Interpreter::new();

        assert_eq!(JkStringFmt::interpolate(s, &mut interpreter).unwrap(), s);
    }

    /// Creates two variables, a and b, in an interpreter
    fn setup(i: &mut Interpreter) {
        let e = Construct::instruction("a = 1").unwrap().1;
        e.execute(i).unwrap();

        let e = Construct::instruction("b = 2").unwrap().1;
        e.execute(i).unwrap();
    }

    #[test]
    fn empty() {
        let mut interpreter = Interpreter::new();
        setup(&mut interpreter);

        let s = "";
        assert_eq!(JkStringFmt::interpolate(s, &mut interpreter).unwrap(), "");
    }

    #[test]
    fn only_expr() {
        let mut interpreter = Interpreter::new();
        setup(&mut interpreter);

        let s = "{a}";
        assert_eq!(JkStringFmt::interpolate(s, &mut interpreter).unwrap(), "1");
    }

    #[test]
    fn pre_expr_plus_expr() {
        let mut interpreter = Interpreter::new();
        setup(&mut interpreter);

        let s = "Hey {a}";
        assert_eq!(
            JkStringFmt::interpolate(s, &mut interpreter).unwrap(),
            "Hey 1"
        );
    }

    #[test]
    fn expr_plus_pre_expr() {
        let mut interpreter = Interpreter::new();
        setup(&mut interpreter);

        let s = "{a} Hey";
        assert_eq!(
            JkStringFmt::interpolate(s, &mut interpreter).unwrap(),
            "1 Hey"
        );
    }

    #[test]
    fn e_plus_pe_plus_e() {
        let mut interpreter = Interpreter::new();
        setup(&mut interpreter);

        let s = "{a} Hey {b}";
        assert_eq!(
            JkStringFmt::interpolate(s, &mut interpreter).unwrap(),
            "1 Hey 2"
        );
    }

    #[test]
    fn pe_plus_e_plus_pe() {
        let mut interpreter = Interpreter::new();
        setup(&mut interpreter);

        let s = "Hey {b} Hey";
        assert_eq!(
            JkStringFmt::interpolate(s, &mut interpreter).unwrap(),
            "Hey 2 Hey"
        );
    }

    #[test]
    fn stmt_as_interp() {
        let mut interpreter = Interpreter::new();
        setup(&mut interpreter);
        let e = Construct::instruction("mut x = 1").unwrap().1;
        e.execute(&mut interpreter).unwrap();

        let s = "Hey {x = 12}";
        assert!(JkStringFmt::interpolate(s, &mut interpreter).is_err());
    }
}
