//! The ConstantConstruct module deals with parsing constant values in jinko source code.
//! This is limited to booleans, integers, floats, characters and strings. Their
//! only purpose is to wrap the lexing/parsing of tokens and box the result, for it
//! to be used by high level Construct functions. It is very similar to BoxConstruct in
//! that sense.

use crate::instruction::Instruction;
use crate::parser::{ParseResult, Token};
use crate::{JkBool, JkChar, JkFloat, JkInt, JkString};

pub struct ConstantConstruct;

#[doc(hidden)]
impl ConstantConstruct {
    pub(crate) fn char_constant(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
        let (input, char_value) = Token::char_constant(input)?;

        Ok((input, Box::new(JkChar::from(char_value))))
    }

    pub(crate) fn string_constant(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
        let (input, string_value) = Token::string_constant(input)?;

        Ok((input, Box::new(JkString::from(string_value))))
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
