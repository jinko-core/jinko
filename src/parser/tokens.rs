//! Token parser functions are used to define and recognize the particular tokens of
//! the language, so that { a + b } gets recognized into LEFT_BRACKET ID ADD ID RIGHT_BRACKET
//! and so on. This module consists of a lot of uninteresting helper/wrapper functions

use nom::{
    bytes::complete::is_not, bytes::complete::tag, bytes::complete::take_while1,
    character::complete::anychar, character::complete::char, character::is_digit, combinator::opt,
    error::ErrorKind, error::ParseError, sequence::delimited, IResult,
};

pub struct Token;

impl Token {
    /// Function used to recognize a specific character such as '[' or '>'. A function
    /// calling this is specifically trying to recognize the given character
    fn specific_char(input: &str, character: char) -> IResult<&str, char> {
        char(character)(input)
    }

    /// Function used to recognize a specific string token such as "func" or "ext"
    /// When a function calls specific_token(_, "token"), that means it's trying to
    /// recognize specifically the word "token".
    fn specific_token<'tok>(input: &'tok str, token: &'tok str) -> IResult<&'tok str, &'tok str> {
        tag(token)(input)
    }

    pub fn single_quote(input: &str) -> IResult<&str, char> {
        char('\'')(input)
    }

    pub fn double_quote(input: &str) -> IResult<&str, char> {
        char('"')(input)
    }

    pub fn add(input: &str) -> IResult<&str, char> {
        Token::specific_char(input, '+')
    }

    pub fn left_curly_bracket(input: &str) -> IResult<&str, char> {
        Token::specific_char(input, '{')
    }

    pub fn right_curly_bracket(input: &str) -> IResult<&str, char> {
        Token::specific_char(input, '}')
    }

    pub fn left_bracket(input: &str) -> IResult<&str, char> {
        Token::specific_char(input, '[')
    }

    pub fn right_bracket(input: &str) -> IResult<&str, char> {
        Token::specific_char(input, ']')
    }

    pub fn func_tok(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "func")
    }

    pub fn ext_tok(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "ext")
    }

    pub fn test_tok(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "test")
    }

    pub fn mock_tok(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "mock")
    }

    pub fn loop_tok(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "loop")
    }

    pub fn while_tok(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "while")
    }

    pub fn for_tok(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "for")
    }

    fn non_neg_num(input: &str) -> IResult<&str, &str> {
        take_while1(|c| is_digit(c as u8))(input)
    }

    pub fn float_constant(input: &str) -> IResult<&str, f64> {
        let (input, negative_sign) = opt(char('-'))(input)?;
        let (input, whole) = Token::int_constant(input)?;
        let (input, _) = char('.')(input)?;
        let (input, decimal) = Token::non_neg_num(input)?;

        match format!("{}.{}", whole, decimal).parse::<f64>() {
            Ok(value) => match negative_sign {
                Some(_) => Ok((input, -value)),
                None => Ok((input, value)),
            },
            // FIXME: Return better error with err message
            Err(_) => Err(nom::Err::Failure((
                "Invalid floating point number",
                ErrorKind::OneOf,
            ))),
        }
    }

    pub fn int_constant(input: &str) -> IResult<&str, i64> {
        let (input, negative_sign) = opt(char('-'))(input)?;
        let (input, num) = Token::non_neg_num(input)?;

        match num.parse::<i64>() {
            Ok(value) => match negative_sign {
                Some(_) => Ok((input, -value)),
                None => Ok((input, value)),
            },
            // FIXME: Return better error with err message
            Err(_) => Err(nom::Err::Failure(("Invalid integer", ErrorKind::OneOf))),
        }
    }

    /// Parse a single character constant and return the character inside the quotes
    pub fn char_constant(input: &str) -> IResult<&str, char> {
        let (input, _) = Token::single_quote(input)?;
        let (input, character) = anychar(input)?;
        let (input, _) = Token::single_quote(input)?;

        // FIXME: Handle escaping as well

        Ok((input, character))
    }

    /// Parse a string constant and return the characters between the double quotes
    pub fn string_constant(input: &str) -> IResult<&str, &str> {
        // FIXME: This does not allow for string escaping yet
        delimited(Token::double_quote, is_not("\""), Token::double_quote)(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_char_constant_valid() {
        assert_eq!(Token::char_constant("'a'"), Ok(("", 'a')));
        assert_eq!(Token::char_constant("'9'"), Ok(("", '9')));

        // FIXME: Add escaping
    }

    #[test]
    fn t_char_constant_invalid() {
        // Multiple characters
        match Token::char_constant("'abc'") {
            Ok(_) => assert!(false, "Too many characters in constant"),
            Err(_) => assert!(true),
        };
    }

    #[test]
    fn t_string_constant() {
        // Simple string
        assert_eq!(Token::string_constant("\"a str\""), Ok(("", "a str")));
        assert_eq!(Token::string_constant("\"999 89 9\""), Ok(("", "999 89 9")));
        assert_eq!(Token::string_constant("\"4.01f\""), Ok(("", "4.01f")));

        // FIXME: Fix string escaping
    }

    #[test]
    fn t_string_constant_unclosed_quote() {
        // Simple string
        match Token::string_constant("\"a str") {
            Ok(_) => assert!(false, "Unclosed quote delimiter"),
            Err(_) => assert!(true),
        }
    }

    #[test]
    fn t_int_constant_valid() {
        assert_eq!(Token::int_constant("12"), Ok(("", 12)));
        assert_eq!(Token::int_constant("-45"), Ok(("", -45)));
    }

    #[test]
    fn t_int_constant_invalid() {
        match Token::int_constant("ff2") {
            Ok(_) => assert!(false, "Characters in integer"),
            Err(_) => assert!(true),
        }
    }

    #[test]
    fn t_float_constant_valid() {
        assert_eq!(Token::float_constant("12.2"), Ok(("", 12.2f64)));
        assert_eq!(Token::float_constant("-45.06"), Ok(("", -45.06f64)));
    }

    #[test]
    fn t_float_constant_invalid() {
        match Token::float_constant("ff2") {
            Ok(_) => assert!(false, "Characters in float"),
            Err(_) => assert!(true),
        }

        match Token::float_constant("12") {
            Ok(_) => assert!(false, "It's an integer"),
            Err(_) => assert!(true),
        }
    }
}
