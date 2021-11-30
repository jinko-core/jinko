//! Token parser functions are used to define and recognize the particular tokens of
//! the language, so that { a + b } gets recognized into LEFT_BRACKET ID ADD ID RIGHT_BRACKET
//! and so on. This module consists of a lot of uninteresting helper/wrapper functions

use nom::{
    branch::alt, bytes::complete::is_not, bytes::complete::tag, bytes::complete::take_until,
    bytes::complete::take_while, bytes::complete::take_while1, character::complete::anychar,
    character::complete::char, character::is_alphanumeric, character::is_digit, combinator::not,
    combinator::opt, combinator::peek, multi::many0, sequence::delimited, sequence::pair,
};

use crate::{parser::ParseResult, ErrKind, Error};
use nom::Err::Error as NomError;

/// Reserved Keywords by jinko
const RESERVED_KEYWORDS: [&str; 14] = [
    "func", "test", "mock", "type", "ext", "for", "while", "loop", "mut", "true", "false", "incl",
    "as", "return",
];

pub struct Token;

impl Token {
    /// Function used to recognize a specific character such as '[' or '>'. A function
    /// calling this is specifically trying to recognize the given character
    fn specific_char(input: &str, character: char) -> ParseResult<&str, char> {
        let c = char::<&str, Error>(character)(input)?;

        Ok(c)
    }

    /// Match a simple token. No rules apply to the characters following it, unlike
    /// specific_token
    fn token<'input>(
        input: &'input str,
        token: &'input str,
    ) -> ParseResult<&'input str, &'input str> {
        let tok = tag::<&str, &str, Error>(token)(input)?;

        Ok(tok)
    }

    /// Function used to recognize a specific string token such as "func" or "ext"
    /// When a function calls specific_token(_, "token"), that means it's trying to
    /// recognize specifically the word "token".
    fn specific_token<'tok>(
        input: &'tok str,
        token: &'tok str,
    ) -> ParseResult<&'tok str, &'tok str> {
        let (input, tag) = tag::<&str, &str, Error>(token)(input)?;

        if let Some(next_char) = input.chars().next() {
            if next_char.is_alphanumeric() || next_char == '_' {
                return Err(NomError(Error::new(ErrKind::Parsing).with_msg(
                    String::from("Unexpected alphanum character after symbol"),
                )));
            }
        }
        Ok((input, tag))
    }

    pub fn single_quote(input: &str) -> ParseResult<&str, char> {
        Token::specific_char(input, '\'')
    }

    pub fn double_quote(input: &str) -> ParseResult<&str, char> {
        Token::specific_char(input, '"')
    }

    pub fn equal(input: &str) -> ParseResult<&str, char> {
        let (input, token) = Token::specific_char(input, '=')?;
        if !input.is_empty() {
            peek(not(char('=')))(input)?;
        }

        Ok((input, token))
    }

    pub fn comma(input: &str) -> ParseResult<&str, char> {
        Token::specific_char(input, ',')
    }

    pub fn pipe(input: &str) -> ParseResult<&str, char> {
        Token::specific_char(input, '|')
    }

    pub fn left_curly_bracket(input: &str) -> ParseResult<&str, char> {
        Token::specific_char(input, '{')
    }

    pub fn right_curly_bracket(input: &str) -> ParseResult<&str, char> {
        Token::specific_char(input, '}')
    }

    pub fn left_bracket(input: &str) -> ParseResult<&str, char> {
        Token::specific_char(input, '[')
    }

    pub fn right_bracket(input: &str) -> ParseResult<&str, char> {
        Token::specific_char(input, ']')
    }

    pub fn colon(input: &str) -> ParseResult<&str, char> {
        Token::specific_char(input, ':')
    }

    pub fn semicolon(input: &str) -> ParseResult<&str, char> {
        Token::specific_char(input, ';')
    }

    pub fn at_sign(input: &str) -> ParseResult<&str, char> {
        Token::specific_char(input, '@')
    }

    pub fn func_tok(input: &str) -> ParseResult<&str, &str> {
        Token::specific_token(input, "func")
    }

    pub fn ext_tok(input: &str) -> ParseResult<&str, &str> {
        Token::specific_token(input, "ext")
    }

    pub fn test_tok(input: &str) -> ParseResult<&str, &str> {
        Token::specific_token(input, "test")
    }

    pub fn mock_tok(input: &str) -> ParseResult<&str, &str> {
        Token::specific_token(input, "mock")
    }

    pub fn loop_tok(input: &str) -> ParseResult<&str, &str> {
        Token::specific_token(input, "loop")
    }

    pub fn while_tok(input: &str) -> ParseResult<&str, &str> {
        Token::specific_token(input, "while")
    }

    pub fn for_tok(input: &str) -> ParseResult<&str, &str> {
        Token::specific_token(input, "for")
    }

    pub fn in_tok(input: &str) -> ParseResult<&str, &str> {
        Token::specific_token(input, "in")
    }

    pub fn mut_tok(input: &str) -> ParseResult<&str, &str> {
        Token::specific_token(input, "mut")
    }

    pub fn if_tok(input: &str) -> ParseResult<&str, &str> {
        Token::specific_token(input, "if")
    }

    pub fn else_tok(input: &str) -> ParseResult<&str, &str> {
        Token::specific_token(input, "else")
    }

    pub fn return_tok(input: &str) -> ParseResult<&str, &str> {
        Token::specific_token(input, "return")
    }

    pub fn _type_tok(input: &str) -> ParseResult<&str, &str> {
        Token::specific_token(input, "type")
    }

    pub fn incl_tok(input: &str) -> ParseResult<&str, &str> {
        Token::specific_token(input, "incl")
    }

    // Parse the `as` token. Rename it so clippy does not complain
    pub fn az_tok(input: &str) -> ParseResult<&str, &str> {
        Token::specific_token(input, "as")
    }

    pub fn add(input: &str) -> ParseResult<&str, &str> {
        Token::token(input, "+")
    }

    pub fn sub(input: &str) -> ParseResult<&str, &str> {
        Token::token(input, "-")
    }

    pub fn mul(input: &str) -> ParseResult<&str, &str> {
        Token::token(input, "*")
    }

    pub fn div(input: &str) -> ParseResult<&str, &str> {
        Token::token(input, "/")
    }

    pub fn left_parenthesis(input: &str) -> ParseResult<&str, &str> {
        Token::token(input, "(")
    }

    pub fn right_parenthesis(input: &str) -> ParseResult<&str, &str> {
        Token::token(input, ")")
    }

    pub fn lt(input: &str) -> ParseResult<&str, &str> {
        Token::token(input, "<")
    }

    pub fn gt(input: &str) -> ParseResult<&str, &str> {
        Token::token(input, ">")
    }

    pub fn lt_eq(input: &str) -> ParseResult<&str, &str> {
        Token::token(input, "<=")
    }

    pub fn gt_eq(input: &str) -> ParseResult<&str, &str> {
        Token::token(input, ">=")
    }

    pub fn equals(input: &str) -> ParseResult<&str, &str> {
        Token::token(input, "==")
    }

    pub fn not_equals(input: &str) -> ParseResult<&str, &str> {
        Token::token(input, "!=")
    }

    pub fn _left_shift(input: &str) -> ParseResult<&str, &str> {
        Token::token(input, "<<")
    }

    pub fn true_tok(input: &str) -> ParseResult<&str, &str> {
        let (input, t) = Token::specific_token(input, "true")?;

        Ok((input, t))
    }

    pub fn false_tok(input: &str) -> ParseResult<&str, &str> {
        let (input, f) = Token::specific_token(input, "false")?;

        Ok((input, f))
    }

    pub fn arrow(input: &str) -> ParseResult<&str, &str> {
        Token::specific_token(input, "->")
    }

    pub fn comment_multi_start(input: &str) -> ParseResult<&str, &str> {
        let comment = tag("/*")(input)?;

        Ok(comment)
    }

    pub fn comment_multi_end(input: &str) -> ParseResult<&str, &str> {
        let comment_end = tag("*/")(input)?;

        Ok(comment_end)
    }

    pub fn comment_single(input: &str) -> ParseResult<&str, &str> {
        let comment = tag("//")(input)?;

        Ok(comment)
    }

    pub fn comment_shebang(input: &str) -> ParseResult<&str, &str> {
        let comment = tag("#")(input)?;

        Ok(comment)
    }

    pub fn dot(input: &str) -> ParseResult<&str, &str> {
        Token::token(input, ".")
    }

    pub fn inner_identifer(input: &str) -> ParseResult<&str, &str> {
        let (input, id) = take_while1(|c| is_alphanumeric(c as u8) || c == '_')(input)?;

        if RESERVED_KEYWORDS.contains(&id) {
            return Err(NomError(
                Error::new(ErrKind::Parsing).with_msg(String::from("identifier cannot be keyword")),
            ));
        }

        // At least one alphabetical character is required
        if id.chars().any(|c| c.is_alphabetic()) {
            return Ok((input, id));
        }

        Err(NomError(
            Error::new(ErrKind::Parsing).with_msg(String::from("invalid identifier")),
        ))
    }

    pub fn namespace_separator(input: &str) -> ParseResult<&str, &str> {
        Token::token(input, "::")
    }

    pub fn identifier(input: &str) -> ParseResult<&str, String> {
        let (input, first_id) = Token::inner_identifer(input)?;

        let (input, namespaced) =
            many0(pair(Token::namespace_separator, Token::inner_identifer))(input)?;

        let mut identifier = String::from(first_id);
        namespaced.into_iter().for_each(|(sep, nspace)| {
            identifier.push_str(sep);
            identifier.push_str(nspace)
        });

        // If a namespace_separator remains, then it means we're in a situation where
        // the identifier is of the following form:
        //
        // `<id>::<id>::<id>...<id>::`
        //
        // which is not a valid identifier
        if Token::namespace_separator(input).is_ok() {
            return Err(NomError(Error::new(ErrKind::Parsing).with_msg(
                String::from("cannot finish identifier on namespace separator `::`"),
            )));
        }

        Ok((input, identifier))
    }

    fn non_neg_num(input: &str) -> ParseResult<&str, &str> {
        let num = take_while1(|c| is_digit(c as u8))(input)?;

        Ok(num)
    }

    pub fn bool_constant(input: &str) -> ParseResult<&str, bool> {
        let (input, b) = alt((Token::true_tok, Token::false_tok))(input)?;

        // We can unwrap since we recognized valid tokens already
        Ok((input, b.parse::<bool>().unwrap()))
    }

    pub fn float_constant(input: &str) -> ParseResult<&str, f64> {
        let (input, negative_sign) = opt(char('-'))(input)?;
        let (input, whole) = Token::int_constant(input)?;
        let (input, _) = char('.')(input)?;
        let (input, decimal) = Token::non_neg_num(input)?;

        match format!("{}.{}", whole, decimal).parse::<f64>() {
            Ok(value) => match negative_sign {
                Some(_) => Ok((input, -value)),
                None => Ok((input, value)),
            },
            Err(_) => Err(NomError(Error::new(ErrKind::Parsing).with_msg(format!(
                "invalid floating point number: {}.{}",
                whole, decimal
            )))),
        }
    }

    pub fn int_constant(input: &str) -> ParseResult<&str, i64> {
        let (input, negative_sign) = opt(char('-'))(input)?;
        let (input, num) = Token::non_neg_num(input)?;

        match num.parse::<i64>() {
            Ok(value) => match negative_sign {
                Some(_) => Ok((input, -value)),
                None => Ok((input, value)),
            },
            Err(_) => Err(NomError(
                Error::new(ErrKind::Parsing).with_msg(format!("invalid integer: {}", num)),
            )),
        }
    }

    /// Parse a single character constant and return the character inside the quotes
    pub fn char_constant(input: &str) -> ParseResult<&str, char> {
        let (input, _) = Token::single_quote(input)?;
        let (input, character) = anychar(input)?;
        let (input, _) = Token::single_quote(input)?;

        // FIXME: Handle escaping as well

        Ok((input, character))
    }

    /// Parse a string constant and return the characters between the double quotes
    pub fn string_constant(input: &str) -> ParseResult<&str, &str> {
        // FIXME: This does not allow for string escaping yet
        let string = delimited(Token::double_quote, is_not("\""), Token::double_quote)(input)?;

        Ok(string)
    }

    #[inline(always)]
    pub fn consume_multi_comment(input: &str) -> ParseResult<&str, &str> {
        let (input, _) = Token::comment_multi_start(input)?;
        let (input, content) = take_until("*/")(input)?;
        let (input, _) = Token::comment_multi_end(input)?;

        Ok((input, content))
    }

    #[inline(always)]
    pub fn consume_single_comment(input: &str) -> ParseResult<&str, &str> {
        let (input, _) = Token::comment_single(input)?;
        let comment = take_while(|c| c != '\n' && c != '\0')(input)?;

        Ok(comment)
    }

    #[inline(always)]
    pub fn consume_shebang_comment(input: &str) -> ParseResult<&str, &str> {
        let (input, _) = Token::comment_shebang(input)?;
        let comment = take_while(|c| c != '\n' && c != '\0')(input)?;

        Ok(comment)
    }

    /// Consumes all kinds of comments: Multi-line or single-line
    pub fn consume_comment(input: &str) -> ParseResult<&str, &str> {
        alt((
            Token::consume_shebang_comment,
            Token::consume_single_comment,
            Token::consume_multi_comment,
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
        assert!(Token::char_constant("'abc'").is_err());
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
        assert!(Token::string_constant("\"a str").is_err());
    }

    #[test]
    fn t_int_constant_valid() {
        assert_eq!(Token::int_constant("12"), Ok(("", 12)));
        assert_eq!(Token::int_constant("-45"), Ok(("", -45)));
    }

    #[test]
    fn t_int_constant_invalid() {
        assert!(Token::int_constant("ff2").is_err());
    }

    #[test]
    fn t_float_constant_valid() {
        assert_eq!(Token::float_constant("12.2"), Ok(("", 12.2f64)));
        assert_eq!(Token::float_constant("-45.06"), Ok(("", -45.06f64)));
    }

    #[test]
    fn t_float_constant_invalid() {
        assert!(Token::float_constant("ff2").is_err());

        assert!(Token::float_constant("12").is_err());
    }

    #[test]
    fn t_id() {
        assert_eq!(Token::identifier("x"), Ok(("", "x".to_string())));
        assert_eq!(Token::identifier("x_"), Ok(("", "x_".to_string())));
        assert_eq!(Token::identifier("x_99"), Ok(("", "x_99".to_string())));
        assert_eq!(Token::identifier("99x"), Ok(("", "99x".to_string())));
        assert_eq!(Token::identifier("n99 x"), Ok((" x", "n99".to_string())));
        assert_eq!(
            Token::identifier("func_ x"),
            Ok((" x", "func_".to_string()))
        );
    }

    #[test]
    fn t_id_invalid() {
        assert!(Token::identifier("99").is_err());
        assert!(Token::identifier("__99_").is_err());
        assert!(Token::identifier("func").is_err());
    }

    #[test]
    fn t_bool_valid() {
        assert_eq!(Token::bool_constant("true"), Ok(("", true)));
        assert_eq!(Token::bool_constant("false"), Ok(("", false)));
        assert_eq!(Token::bool_constant("true a"), Ok((" a", true)));
        assert_eq!(Token::bool_constant("true; false"), Ok(("; false", true)));
        assert_eq!(Token::bool_constant("true*false"), Ok(("*false", true)));
    }

    #[test]
    fn t_bool_invalid() {
        assert!(Token::bool_constant("tru").is_err());
        assert!(Token::bool_constant("tru a").is_err());
        assert!(Token::bool_constant("trueast").is_err());
    }

    #[test]
    fn t_multi_comment_valid() {
        assert!(Token::consume_comment("/* */").is_ok());
        assert!(Token::consume_comment("/**/").is_ok());
        assert!(Token::consume_comment("/*            */").is_ok());
        assert!(Token::consume_comment("/* a bbbb a something   */").is_ok());
    }

    #[test]
    fn t_single_comment_valid() {
        assert!(Token::consume_comment("//").is_ok());
        assert!(Token::consume_comment("//                   ").is_ok());
        assert!(Token::consume_comment("//          \nhey").is_ok());
        assert!(Token::consume_comment("//// ").is_ok());
        assert!(Token::consume_comment("// a bbbb a something  /* hey */").is_ok());
    }

    #[test]
    fn t_multi_comment_invalid() {
        assert!(Token::consume_multi_comment("/*").is_err());
    }

    #[test]
    fn t_keyword_next_to_curly() {
        assert_eq!(Token::loop_tok("loop{}"), Ok(("{}", "loop")));
    }

    #[test]
    fn t_dot_token() {
        assert_eq!(Token::dot("."), Ok(("", ".")));
    }

    #[test]
    fn t_identifier_no_namespace() {
        assert_eq!(Token::identifier("id"), Ok(("", String::from("id"))));
    }

    #[test]
    fn t_identifier_plus_one_namespace() {
        assert_eq!(
            Token::identifier("nspace::id"),
            Ok(("", String::from("nspace::id")))
        );
    }

    #[test]
    fn t_identifier_plus_many_namespace() {
        assert_eq!(
            Token::identifier("nspace::id::sub"),
            Ok(("", String::from("nspace::id::sub")))
        );
    }

    #[test]
    fn t_identifier_invalid_just_sep() {
        assert!(Token::identifier("::").is_err());
    }

    #[test]
    fn t_identifier_invalid_nspace_no_id() {
        assert!(Token::identifier("nspace::").is_err());
    }

    #[test]
    fn t_identifier_invalid_nspace_no_id_multi() {
        assert!(Token::identifier("nspace::id::sub::").is_err());
    }
}
