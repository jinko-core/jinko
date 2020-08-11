//! Token parser functions are used to define and recognize the particular tokens of
//! the language, so that { a + b } gets recognized into LEFT_BRACKET ID ADD ID RIGHT_BRACKET
//! and so on. This module consists of a lot of uninteresting helper/wrapper functions

use nom::{bytes::complete::tag, character::complete::char, IResult};

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

    pub fn float_constant(input: &str) -> IResult<&str, &str> {
        todo!()
    }

    pub fn int_constant(input: &str) -> IResult<&str, &str> {
        todo!()
    }

    /// Parse a single character constant and return the character inside the quotes
    pub fn char_constant(input: &str) -> IResult<&str, char> {
        todo!()
    }

    /// Parse a string constant and return the characters between the double quotes
    pub fn string_constant(input: &str) -> IResult<&str, &str> {
        // FIXME: This does not allow for string escaping yet
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_char_constant_valid() {
        assert_eq!(Token::char_constant("'a'"), Ok(("", 'a')));
        assert_eq!(Token::char_constant("'9'"), Ok(("", '9')));
    }

    #[test]
    fn t_char_constant_invalid() {
        // Multiple characters
        match Token::char_constant("'abc'") {
            Ok(tuple) => assert!(false, "Too many characters in constant"),
            Err(_) => assert!(true),
        };
    }

    #[test]
    fn t_string_constant() {
        // Simple string
        assert_eq!(Token::string_constant("\"a str\""), Ok(("", "a_str")));

        // FIXME: Fix string escaping
    }
}
