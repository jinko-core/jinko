//! String interpolation is performed during execution of a `JkString`.
//! The format used is the following: {expression}. This will replace {expression} with
//! the current value of the `expression`'s execution in the interpreter at that time.
//! In order to use the '{' or '}' character themselves, use '{{' and '}}'.

use crate::{Instruction, Interpreter, JkError, InstrKind};
use crate::parser::constructs::Construct;

use nom::bytes::complete::take_while;
use nom::IResult;
use nom::multi::many0;

const L_DELIM: char = '{';
const R_DELIM: char = '}';

pub struct JkStringFmt;

impl JkStringFmt {
    fn consume_until(input: &str, limit: char) -> IResult<&str, &str> {
        take_while(|c| c != limit)(input)
    }

    fn pre_expr(input: &str) -> IResult<&str, &str> {
        JkStringFmt::consume_until(input, L_DELIM)
    }

    fn expr(input: &str) -> IResult<&str, Box<dyn Instruction>> {
        let (input, expr) = JkStringFmt::consume_until(input, R_DELIM)?;

        let (_, expr) = Construct::instruction(expr)?;

        Ok((input, expr))
    }

    /// Everything in the string is either a `pre_expr` or an `expr`
    /// The string "Hello {name}" has a pre_expr "Hello" and an expression "name".
    /// The string "{name}, how are you?" has an empty pre_expr, an expression "name", a
    /// pre_expr ", how are you?" and an empty expr
    fn parser(input: &str) -> IResult<&str, (&str, Box<dyn Instruction>)> {
        let (input, pre_expr) = JkStringFmt::pre_expr(input)?;
        let (input, expr) = JkStringFmt::expr(input)?;

        Ok((input, (pre_expr, expr)))
    }

    /// Interpolates the string passed as parameter
    pub fn interpolate(s: &str, interpreter: &mut Interpreter) -> Result<String, JkError> {
        // We know the final string will be roughly the same size as `s`. We can pre-allocate
        // to save some performance
        let mut result = String::with_capacity(s.len());

        let (_, expressions) = many0(JkStringFmt::parser)(s)?;

        for (pre_expr, expr) in expressions {
            result.push_str(pre_expr);
            let res = expr.execute(interpreter)?;
            let expr_str = match res {
                InstrKind::Expression(Some(res)) => res.to_string(),
                InstrKind::Statement | InstrKind::Expression(None) => String::from(""), // Should never happen?
            };

            result.push_str(expr_str.as_str());
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
}
