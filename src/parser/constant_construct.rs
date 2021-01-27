//! The ConstantConstruct module helps when parsing constant values in jinko source code

use crate::instruction::Instruction;
use crate::parser::Token;
use crate::{JkBool, JkChar, JkFloat, JkInt, JkString};

use nom::IResult;

pub struct ConstantConstruct;

impl ConstantConstruct {
    pub(crate) fn c_char_constant(input: &str) -> IResult<&str, Box<dyn Instruction>> {
        let (input, char_value) = Token::char_constant(input)?;

        Ok((input, Box::new(JkChar::from(char_value))))
    }

    pub(crate) fn c_string_constant(input: &str) -> IResult<&str, Box<dyn Instruction>> {
        let (input, string_value) = Token::string_constant(input)?;

        Ok((input, Box::new(JkString::from(string_value))))
    }

    pub(crate) fn c_float_constant(input: &str) -> IResult<&str, Box<dyn Instruction>> {
        let (input, float_value) = Token::float_constant(input)?;

        Ok((input, Box::new(JkFloat::from(float_value))))
    }

    pub(crate) fn c_int_constant(input: &str) -> IResult<&str, Box<dyn Instruction>> {
        let (input, int_value) = Token::int_constant(input)?;

        Ok((input, Box::new(JkInt::from(int_value))))
    }

    pub(crate) fn c_bool_constant(input: &str) -> IResult<&str, Box<dyn Instruction>> {
        let (input, bool_value) = Token::bool_constant(input)?;

        Ok((input, Box::new(JkBool::from(bool_value))))
    }
}
