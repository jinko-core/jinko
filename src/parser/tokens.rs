//! Token parser functions are used to define and recognize the particular tokens of
//! the language, so that { a + b } gets recognized into LEFT_BRACKET ID ADD ID RIGHT_BRACKET
//! and so on. This module consists of a lot of uninteresting helper/wrapper functions

use nom::{
    branch::alt, bytes::complete::is_not, bytes::complete::tag, bytes::complete::take_until,
    bytes::complete::take_while, bytes::complete::take_while1, character::complete::anychar,
    character::complete::char, character::is_alphabetic, character::is_alphanumeric,
    character::is_digit, combinator::opt, combinator::peek, error::ErrorKind, sequence::delimited,
    IResult,
};

/// Reserved Keywords by jinko
const RESERVED_KEYWORDS: [&str; 10] = [
    "func", "test", "mock", "ext", "for", "while", "loop", "mut", "true", "false",
];

const OPERATORS: [&str; 6] = ["+", "-", "*", "/", "(", ")"];

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
        let (input, tag) = tag(token)(input)?;
        match input.len() {
            0 => Ok((input, tag)),
            _ => {
                peek(alt((
                    char('\t'),
                    char('\r'),
                    char('\n'),
                    char(' '),
                    char('{'),
                    char(';'),
                )))(input)?;
                Ok((input, tag))
            }
        }
    }

    pub fn single_quote(input: &str) -> IResult<&str, char> {
        char('\'')(input)
    }

    pub fn double_quote(input: &str) -> IResult<&str, char> {
        char('"')(input)
    }

    pub fn equal(input: &str) -> IResult<&str, char> {
        Token::specific_char(input, '=')
    }

    pub fn comma(input: &str) -> IResult<&str, char> {
        Token::specific_char(input, ',')
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

    pub fn colon(input: &str) -> IResult<&str, char> {
        Token::specific_char(input, ':')
    }

    pub fn semicolon(input: &str) -> IResult<&str, char> {
        Token::specific_char(input, ';')
    }

    pub fn at_sign(input: &str) -> IResult<&str, char> {
        Token::specific_char(input, '@')
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

    pub fn in_tok(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "in")
    }

    pub fn mut_tok(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "mut")
    }

    pub fn if_tok(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "if")
    }

    pub fn else_tok(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "else")
    }

    pub fn audit_tok(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "audit ")
    }

    pub fn add(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "+")
    }

    pub fn sub(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "-")
    }

    pub fn mul(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "*")
    }

    pub fn div(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "/")
    }

    pub fn left_parenthesis(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "(")
    }

    pub fn right_parenthesis(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, ")")
    }

    pub fn left_shift(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "<<")
    }

    fn non_digit_nor_alpha(input: &str) -> IResult<&str, char> {
        match input.len() {
            0 => Ok((input, '\0')),
            _ => peek(alt((
                char(';'),
                char(' '),
                char('\t'),
                char('\r'),
                char('\n'),
            )))(input),
        }
    }

    pub fn true_tok(input: &str) -> IResult<&str, &str> {
        let (input, t) = Token::specific_token(input, "true")?;

        Ok((input, t))
    }

    pub fn false_tok(input: &str) -> IResult<&str, &str> {
        let (input, f) = Token::specific_token(input, "false")?;

        Ok((input, f))
    }

    pub fn arrow(input: &str) -> IResult<&str, &str> {
        Token::specific_token(input, "->")
    }

    pub fn comment_multi_start(input: &str) -> IResult<&str, &str> {
        tag("/*")(input)
    }

    pub fn comment_multi_end(input: &str) -> IResult<&str, &str> {
        tag("*/")(input)
    }

    pub fn comment_single(input: &str) -> IResult<&str, &str> {
        tag("//")(input)
    }

    pub fn identifier(input: &str) -> IResult<&str, &str> {
        let (input, id) = take_while1(|c| is_alphanumeric(c as u8) || c == '_')(input)?;

        match RESERVED_KEYWORDS.contains(&id) {
            true => {
                return Err(nom::Err::Error((
                    "Identifer cannot be keyword",
                    ErrorKind::OneOf,
                )));
            }
            _ => {}
        }

        // FIXME: Ugly
        // At least one alphabetical character is required
        for c in id.chars() {
            if is_alphabetic(c as u8) {
                return Ok((input, id));
            }
        }

        Err(nom::Err::Error(("Invalid identifier", ErrorKind::Eof)))
    }

    fn non_neg_num(input: &str) -> IResult<&str, &str> {
        take_while1(|c| is_digit(c as u8))(input)
    }

    pub fn bool_constant(input: &str) -> IResult<&str, bool> {
        let (input, b) = alt((Token::true_tok, Token::false_tok))(input)?;

        // We can unwrap since we recognized valid tokens already
        Ok((input, b.parse::<bool>().unwrap()))
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
            Err(_) => Err(nom::Err::Error((
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
            Err(_) => Err(nom::Err::Error(("Invalid integer", ErrorKind::OneOf))),
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

    fn is_whitespace(c: char) -> bool {
        c == ' ' || c == '\t' || c == '\n'
    }

    // FIXME: Documentation
    pub fn is_operator(c: char) -> bool {
        // We can unwrap since all operators are at least one character wide
        OPERATORS
            .iter()
            .map(|op| op.chars().next().unwrap())
            .collect::<Vec<char>>()
            .contains(&c)
    }

    /// Consumes 1 or more whitespaces in an input. A whitespace is a space or a tab
    pub fn consume_whitespaces(input: &str) -> IResult<&str, &str> {
        take_while1(|c| Token::is_whitespace(c))(input)
    }

    /// Consumes 0 or more whitespaces in an input. A whitespace is a space or a tab
    pub fn maybe_consume_whitespaces(input: &str) -> IResult<&str, &str> {
        take_while(|c| Token::is_whitespace(c))(input)
    }

    fn maybe_consume_multi_comment(input: &str) -> IResult<&str, &str> {
        let (input, _) = Token::comment_multi_start(input)?;
        let (input, _) = take_until("*/")(input)?;
        Token::comment_multi_end(input)
    }

    fn maybe_consume_single_comment(input: &str) -> IResult<&str, &str> {
        let (input, _) = Token::comment_single(input)?;
        let (input, _) = take_while(|c| c != '\n' && c != '\0')(input)?;
        match opt(char('\n'))(input) {
            Ok((i, Some(_))) | Ok((i, None)) => Ok((i, "")),
            Err(e) => Err(e),
        }
    }

    /// Consumes all kinds of comments: Multi-line or single-line
    pub fn maybe_consume_comment(input: &str) -> IResult<&str, &str> {
        alt((
            Token::maybe_consume_single_comment,
            Token::maybe_consume_multi_comment,
        ))(input)
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

    #[test]
    fn t_consume_whitespace() {
        assert_eq!(Token::consume_whitespaces("   input"), Ok(("input", "   ")));
        assert_eq!(
            Token::consume_whitespaces(" \t input"),
            Ok(("input", " \t "))
        );
    }

    #[test]
    fn t_consume_whitespace_invalid() {
        match Token::consume_whitespaces("something") {
            Ok(_) => assert!(false, "At least one whitespace required"),
            Err(_) => assert!(true),
        }
    }

    #[test]
    fn t_id() {
        assert_eq!(Token::identifier("x"), Ok(("", "x")));
        assert_eq!(Token::identifier("x_"), Ok(("", "x_")));
        assert_eq!(Token::identifier("x_99"), Ok(("", "x_99")));
        assert_eq!(Token::identifier("99x"), Ok(("", "99x")));
        assert_eq!(Token::identifier("n99 x"), Ok((" x", "n99")));
        assert_eq!(Token::identifier("func_ x"), Ok((" x", "func_")));
    }

    #[test]
    fn t_id_invalid() {
        match Token::identifier("99") {
            Ok(_) => assert!(false, "At least one alphabetical required"),
            Err(_) => assert!(true),
        }
        match Token::identifier("__99_") {
            Ok(_) => assert!(false, "At least one alphabetical required"),
            Err(_) => assert!(true),
        }
        match Token::identifier("func") {
            Ok(_) => assert!(false, "ID can't be a reserved keyword"),
            Err(_) => assert!(true),
        }
    }

    #[test]
    fn t_bool_valid() {
        assert_eq!(Token::bool_constant("true"), Ok(("", true)));
        assert_eq!(Token::bool_constant("false"), Ok(("", false)));
        assert_eq!(Token::bool_constant("true a"), Ok((" a", true)));
        assert_eq!(Token::bool_constant("true; false"), Ok(("; false", true)));
    }

    #[test]
    fn t_bool_invalid() {
        match Token::bool_constant("tru") {
            Ok(_) => assert!(false, "Not a valid boolean, too short"),
            Err(_) => assert!(true),
        }
        match Token::bool_constant("tru a") {
            Ok(_) => assert!(false, "Not a valid boolean, too short + character"),
            Err(_) => assert!(true),
        }
        match Token::bool_constant("trueast") {
            Ok(_) => assert!(false, "Not a valid boolean, too long"),
            Err(_) => assert!(true),
        }
    }

    #[test]
    fn t_multi_comment_valid() {
        match Token::maybe_consume_comment("/* */") {
            Ok(_) => assert!(true),
            Err(_) => assert!(false, "Valid to have one space"),
        };
        match Token::maybe_consume_comment("/**/") {
            Ok(_) => assert!(true),
            Err(_) => assert!(false, "Valid to have zero space"),
        };
        match Token::maybe_consume_comment("/*            */") {
            Ok(_) => assert!(true),
            Err(_) => assert!(false, "Valid to have tons of spaces"),
        };
        match Token::maybe_consume_comment("/* a bbbb a something   */") {
            Ok(_) => assert!(true),
            Err(_) => assert!(false, "Valid to have tons of text and stuff"),
        };
    }

    #[test]
    fn t_single_comment_valid() {
        match Token::maybe_consume_comment("//") {
            Ok(_) => assert!(true),
            Err(_) => assert!(false, "Valid to have nothing after the slashes"),
        };
        match Token::maybe_consume_comment("//                   ") {
            Ok(_) => assert!(true),
            Err(_) => assert!(false, "Valid to have lots of spaces"),
        };
        match Token::maybe_consume_comment("//          \nhey") {
            Ok((left, _)) => assert_eq!(left, "hey"),
            Err(_) => assert!(false, "Don't consume stuff after the newline"),
        };
        match Token::maybe_consume_comment("//// ") {
            Ok(_) => assert!(true),
            Err(_) => assert!(false, "Valid to have multiple slashes"),
        };
        match Token::maybe_consume_comment("// a bbbb a something  /* hey */") {
            Ok(_) => assert!(true),
            Err(_) => assert!(
                false,
                "Valid to have tons of text and stuff, even other comment"
            ),
        };
    }

    #[test]
    fn t_multi_comment_invalid() {
        match Token::maybe_consume_comment("/*") {
            Ok(_) => assert!(false, "Unclosed start delimiter"),
            Err(_) => assert!(true),
        };
    }

    #[test]
    fn t_keyword_next_to_curly() {
        assert_eq!(Token::loop_tok("loop{}"), Ok(("{}", "loop")));
    }
}
