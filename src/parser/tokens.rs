//! Token parser functions are used to define and recognize the particular tokens of
//! the language, so that { a + b } gets recognized into LEFT_BRACKET ID ADD ID RIGHT_BRACKET
//! and so on. This module consists of a lot of uninteresting helper/wrapper functions

use nom::{
    bytes::complete::is_not, bytes::complete::tag, character::complete::anychar,
    character::complete::char, combinator::opt, sequence::delimited, IResult,
    bytes::complete::take_while, character::is_digit, error::ParseError, error::ErrorKind,
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

    pub fn func(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "func")
    }

    pub fn ext(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "ext")
    }

    pub fn test(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "test")
    }

    pub fn mock(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "mock")
    }

    pub fn r#loop(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "loop")
    }

    pub fn r#while(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "while")
    }

    pub fn r#for(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "for")
    }

    pub fn float_constant(input: &str) -> IResult<&str, f64> {
        todo!()
    }

    pub fn int_constant(input: &str) -> IResult<&str, i64> {
        let (input, negative_sign) = opt(char('-'))(input)?;
        let (input, num) = take_while(|c| is_digit(c as u8))(input)?;

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
}
