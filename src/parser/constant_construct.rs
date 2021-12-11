//! The ConstantConstruct module deals with parsing constant values in jinko source code.
//! This is limited to booleans, integers, floats, characters and strings. Their
//! only purpose is to wrap the lexing/parsing of tokens and box the result, for it
//! to be used by high level Construct functions. It is very similar to BoxConstruct in
//! that sense.

use super::constructs::expr;
use crate::instruction::{FunctionCall, Instruction, MethodCall};
use crate::parser::{ParseResult, Token};
use crate::{ErrKind, Error, JkBool, JkChar, JkFloat, JkInt, JkString};

use nom::sequence::terminated;
use nom::Err::Error as NomError;

pub struct ConstantConstruct;

#[doc(hidden)]
impl ConstantConstruct {
    pub(crate) fn char_constant(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
        let (input, char_value) = Token::char_constant(input)?;

        Ok((input, Box::new(JkChar::from(char_value))))
    }

    pub(crate) fn string_constant(mut input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
        let mut ast: Box<dyn Instruction> = Box::new(JkString::from(""));
        let special: &[_] = &['"', '{'];
        while !input.is_empty() {
            if let Ok((input, _)) = Token::double_quote(input) {
                return Ok((input, ast));
            }
            if let Ok((new_input, _)) = Token::left_curly_bracket(input) {
                let (new_input, expr) = terminated(expr, Token::right_curly_bracket)(new_input)?;
                ast = ConstantConstruct::format_expr(ast, expr);
                input = new_input;
            } else if let Some(index) = input.find(special) {
                ast = Box::new(JkString::from(&input[..index]));
                input = &input[index..];
            } else {
                break;
            }
        }
        Err(NomError(
            Error::new(ErrKind::Parsing).with_msg(String::from("Undelimited string")),
        ))
    }

    fn format_expr(
        string: Box<dyn Instruction>,
        expr: Box<dyn Instruction>,
    ) -> Box<dyn Instruction> {
        Box::new(MethodCall::new(
            string,
            FunctionCall::new(String::from("concat"), vec![], vec![expr]),
        ))
    }

    pub(crate) fn float_constant(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
        let (input, float_value) = Token::float_constant(input)?;

        Ok((input, Box::new(JkFloat::from(float_value))))
    }

    pub(crate) fn int_constant(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
        let (input, int_value) = Token::int_constant(input)?;

        Ok((input, Box::new(JkInt::from(int_value))))
    }

    pub(crate) fn bool_constant(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
        let (input, bool_value) = Token::bool_constant(input)?;

        Ok((input, Box::new(JkBool::from(bool_value))))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_string() {
        let input = "hello\"";

        let (input, expr) = ConstantConstruct::string_constant(input).unwrap();
        assert_eq!(input, "");
        let string = expr.downcast_ref::<JkString>().unwrap();
        assert_eq!(string.0, "hello");
    }

    #[test]
    fn formatted_once() {
        let input = "hello {world}\"";

        let (input, expr) = ConstantConstruct::string_constant(input).unwrap();
        assert_eq!(input, "");
        assert!(expr.downcast_ref::<MethodCall>().is_some());
    }

    #[test]
    fn formatted_not_delimited() {
        let input = "hello {world}";

        assert!(ConstantConstruct::string_constant(input).is_err());
    }

    #[test]
    fn basic_not_delimited() {
        let input = "Rust Transmute Task Force";

        assert!(ConstantConstruct::string_constant(input).is_err());
    }
}
