//! The ConstantConstruct module deals with parsing constant values in jinko source code.
//! This is limited to booleans, integers, floats, characters and strings. Their
//! only purpose is to wrap the lexing/parsing of tokens and box the result, for it
//! to be used by high level Construct functions. It is very similar to BoxConstruct in
//! that sense.

use super::constructs::expr;
use crate::instruction::{FunctionCall, Instruction, MethodCall};
use crate::parser::{ParseInput, ParseResult, Token};
use crate::{ErrKind, Error, JkBool, JkChar, JkFloat, JkInt, JkString};

use nom::bytes::complete::take;
use nom::sequence::{preceded, terminated};
use nom::Err::Error as NomError;
use nom::Slice;

pub struct ConstantConstruct;

#[doc(hidden)]
impl ConstantConstruct {
    pub(crate) fn char_constant(
        input: ParseInput,
    ) -> ParseResult<ParseInput, Box<dyn Instruction>> {
        let (input, char_value) = Token::char_constant(input)?;

        Ok((input, Box::new(JkChar::from(char_value))))
    }

    pub(crate) fn string_constant(
        input: ParseInput,
    ) -> ParseResult<ParseInput, Box<dyn Instruction>> {
        let (input, inner) = preceded(Token::double_quote, ConstantConstruct::inner_string)(input)?;
        let string = inner.unwrap_or_else(|| Box::new(JkString::from("")));

        Ok((input, string))
    }

    /// inner_string = '"'
    ///              | special string
    fn inner_string(input: ParseInput) -> ParseResult<ParseInput, Option<Box<dyn Instruction>>> {
        if let Ok((input, _)) = Token::double_quote(input) {
            Ok((input, None))
        } else {
            let (input, special) = ConstantConstruct::special(input)?;
            let (input, next) = ConstantConstruct::inner_string(input)?;

            Ok((input, Some(ConstantConstruct::concat(special, next))))
        }
    }

    /// | '{' expr '}'
    /// | '\' CHAR
    /// | CHAR (* anything except "{\ *)
    fn special(input: ParseInput) -> ParseResult<ParseInput, Box<dyn Instruction>> {
        let special: &[_] = &['"', '{', '\\'];

        if let Ok((input, _)) = Token::left_curly_bracket(input) {
            terminated(expr, Token::right_curly_bracket)(input)
        } else if let Ok((input, _)) = Token::backslash(input) {
            if input.is_empty() {
                return ConstantConstruct::special(input);
            }
            let (input, special) = take(1usize)(input)?;
            let escaped = match *special.fragment() {
                "\"" => "\"",
                "{" => "{",
                "}" => "}",
                "n" => "\n",
                "r" => "\r",
                "t" => "\t",
                _ => {
                    return Err(NomError(
                        Error::new(ErrKind::Parsing)
                            .with_msg(String::from("Unknown character escape")),
                    ))
                }
            };

            Ok((input, Box::new(JkString::from(escaped))))
        } else if let Some(index) = input.find(special) {
            Ok((
                input.slice(index..),
                Box::new(JkString::from(&input[..index])),
            ))
        } else {
            Err(NomError(
                Error::new(ErrKind::Parsing).with_msg(String::from("Undelimited string")),
            ))
        }
    }

    fn concat(
        left: Box<dyn Instruction>,
        right: Option<Box<dyn Instruction>>,
    ) -> Box<dyn Instruction> {
        match right {
            None => left,
            Some(right) => Box::new(MethodCall::new(
                left,
                FunctionCall::new(String::from("concat"), vec![], vec![right]),
            )),
        }
    }

    pub(crate) fn float_constant(
        input: ParseInput,
    ) -> ParseResult<ParseInput, Box<dyn Instruction>> {
        let (input, float_value) = Token::float_constant(input)?;

        Ok((input, Box::new(JkFloat::from(float_value))))
    }

    pub(crate) fn int_constant(input: ParseInput) -> ParseResult<ParseInput, Box<dyn Instruction>> {
        let (input, int_value) = Token::int_constant(input)?;

        Ok((input, Box::new(JkInt::from(int_value))))
    }

    pub(crate) fn bool_constant(
        input: ParseInput,
    ) -> ParseResult<ParseInput, Box<dyn Instruction>> {
        let (input, bool_value) = Token::bool_constant(input)?;

        Ok((input, Box::new(JkBool::from(bool_value))))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span;

    #[test]
    fn empty_string() {
        let input = span!("\"\"");

        let (input, expr) = ConstantConstruct::string_constant(input).unwrap();
        assert_eq!(*input.fragment(), "");
        let string = expr.downcast_ref::<JkString>().unwrap();
        assert_eq!(string.0, "");
    }

    #[test]
    fn basic_string() {
        let input = span!("\"hello\"");

        let (input, expr) = ConstantConstruct::string_constant(input).unwrap();
        assert_eq!(*input.fragment(), "");
        let string = expr.downcast_ref::<JkString>().unwrap();
        assert_eq!(string.0, "hello");
    }

    #[test]
    fn formatted_once() {
        let input = span!("\"hello {world}\"");

        let (input, expr) = ConstantConstruct::string_constant(input).unwrap();
        assert_eq!(*input.fragment(), "");
        assert!(expr.downcast_ref::<MethodCall>().is_some());
    }

    #[test]
    fn formatted_middle() {
        let input = span!("\"hello {world} !\"");

        let (input, expr) = ConstantConstruct::string_constant(input).unwrap();
        assert_eq!(*input.fragment(), "");
        assert!(expr.downcast_ref::<MethodCall>().is_some());
    }

    #[test]
    fn formatted_start() {
        let input = span!("\"{10 + 9} is 21\"");

        let (input, expr) = ConstantConstruct::string_constant(input).unwrap();
        assert_eq!(*input.fragment(), "");
        assert!(expr.downcast_ref::<MethodCall>().is_some());
    }

    #[test]
    fn escape_start() {
        let input = span!("\"\\neat\"");

        let (input, expr) = ConstantConstruct::string_constant(input).unwrap();
        assert_eq!(*input.fragment(), "");
        assert!(expr.downcast_ref::<MethodCall>().is_some());
    }

    #[test]
    fn escape_end() {
        let input = span!("\"hello\\n\"");

        let (input, expr) = ConstantConstruct::string_constant(input).unwrap();
        assert_eq!(*input.fragment(), "");
        assert!(expr.downcast_ref::<MethodCall>().is_some());
    }

    #[test]
    fn escape_formatting() {
        let input = span!("\"hello \\{world\\}\"");

        let (input, expr) = ConstantConstruct::string_constant(input).unwrap();
        assert_eq!(*input.fragment(), "");
        assert!(expr.downcast_ref::<MethodCall>().is_some());
    }

    #[test]
    fn formatted_not_delimited() {
        let input = span!("\"hello {world}");

        assert!(ConstantConstruct::string_constant(input).is_err());
    }

    #[test]
    fn basic_not_delimited() {
        let input = span!("\"Rust Transmute Task Force");

        assert!(ConstantConstruct::string_constant(input).is_err());
    }

    #[test]
    fn escape_unexpected_end_of_string() {
        let input = span!("\"Rust Transmute Task Force\\");

        assert!(ConstantConstruct::string_constant(input).is_err());
    }

    #[test]
    fn invalid_escape() {
        let input = span!("\"Rust Transmute \\a Task Force\"");

        assert!(ConstantConstruct::string_constant(input).is_err());
    }
}
