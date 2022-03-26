//! Token parser functions are used to define and recognize the particular tokens of
//! the language, so that { a + b } gets recognized into LEFT_BRACKET ID ADD ID RIGHT_BRACKET
//! and so on. This module consists of a lot of uninteresting helper/wrapper functions

use nom::Err::Error as NomError;
use nom::{
    branch::alt, bytes::complete::tag, bytes::complete::take_until, bytes::complete::take_while,
    bytes::complete::take_while1, character::complete::anychar, character::complete::char,
    character::is_alphanumeric, character::is_digit, combinator::not, combinator::opt,
    combinator::peek, multi::many0, sequence::delimited, sequence::pair,
};

use crate::error::{ErrKind, Error};
use crate::parser::{ParseInput, ParseResult};

/// Reserved Keywords by jinko
const RESERVED_KEYWORDS: [&str; 14] = [
    "func", "test", "mock", "type", "ext", "for", "while", "loop", "mut", "true", "false", "incl",
    "as", "return",
];

pub struct Token;

impl Token {
    /// Function used to recognize a specific character such as '[' or '>'. A function
    /// calling this is specifically trying to recognize the given character
    fn specific_char(input: ParseInput, character: char) -> ParseResult<ParseInput, char> {
        let c = char::<ParseInput, Error>(character)(input)?;

        Ok(c)
    }

    /// Match a simple token. No rules apply to the characters following it, unlike
    /// specific_token
    fn token<'input>(
        input: ParseInput<'input>,
        token: &'input str,
    ) -> ParseResult<ParseInput<'input>, ParseInput<'input>> {
        let tok = tag(token)(input)?;

        Ok(tok)
    }

    /// Function used to recognize a specific string token such as "func" or "ext"
    /// When a function calls specific_token(_, "token"), that means it's trying to
    /// recognize specifically the word "token".
    fn specific_token<'tok>(
        input: ParseInput<'tok>,
        token: &'tok str,
    ) -> ParseResult<ParseInput<'tok>, ParseInput<'tok>> {
        let (input, tag) = tag(token)(input)?;

        if let Some(next_char) = input.chars().next() {
            if next_char.is_alphanumeric() || next_char == '_' {
                return Err(NomError(Error::new(ErrKind::Parsing).with_msg(
                    String::from("Unexpected alphanum character after symbol"),
                )));
            }
        }
        Ok((input, tag))
    }

    pub fn single_quote(input: ParseInput) -> ParseResult<ParseInput, char> {
        Token::specific_char(input, '\'')
    }

    pub fn double_quote(input: ParseInput) -> ParseResult<ParseInput, char> {
        Token::specific_char(input, '"')
    }

    pub fn equal(input: ParseInput) -> ParseResult<ParseInput, char> {
        let (input, token) = Token::specific_char(input, '=')?;
        if !input.is_empty() {
            peek(not(char('=')))(input)?;
        }

        Ok((input, token))
    }

    pub fn comma(input: ParseInput) -> ParseResult<ParseInput, char> {
        Token::specific_char(input, ',')
    }

    pub fn pipe(input: ParseInput) -> ParseResult<ParseInput, char> {
        Token::specific_char(input, '|')
    }

    pub fn left_curly_bracket(input: ParseInput) -> ParseResult<ParseInput, char> {
        Token::specific_char(input, '{')
    }

    pub fn right_curly_bracket(input: ParseInput) -> ParseResult<ParseInput, char> {
        Token::specific_char(input, '}')
    }

    pub fn left_bracket(input: ParseInput) -> ParseResult<ParseInput, char> {
        Token::specific_char(input, '[')
    }

    pub fn right_bracket(input: ParseInput) -> ParseResult<ParseInput, char> {
        Token::specific_char(input, ']')
    }

    pub fn colon(input: ParseInput) -> ParseResult<ParseInput, char> {
        Token::specific_char(input, ':')
    }

    pub fn semicolon(input: ParseInput) -> ParseResult<ParseInput, char> {
        Token::specific_char(input, ';')
    }

    pub fn at_sign(input: ParseInput) -> ParseResult<ParseInput, char> {
        Token::specific_char(input, '@')
    }

    pub fn func_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::specific_token(input, "func")
    }

    pub fn ext_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::specific_token(input, "ext")
    }

    pub fn test_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::specific_token(input, "test")
    }

    pub fn mock_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::specific_token(input, "mock")
    }

    pub fn loop_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::specific_token(input, "loop")
    }

    pub fn while_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::specific_token(input, "while")
    }

    pub fn for_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::specific_token(input, "for")
    }

    pub fn in_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::specific_token(input, "in")
    }

    pub fn mut_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::specific_token(input, "mut")
    }

    pub fn if_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::specific_token(input, "if")
    }

    pub fn else_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::specific_token(input, "else")
    }

    pub fn return_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::specific_token(input, "return")
    }

    pub fn type_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::specific_token(input, "type")
    }

    pub fn incl_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::specific_token(input, "incl")
    }

    // Parse the `as` token. Rename it so clippy does not complain
    pub fn az_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::specific_token(input, "as")
    }

    pub fn backslash(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::token(input, "\\")
    }

    pub fn add(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::token(input, "+")
    }

    pub fn sub(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::token(input, "-")
    }

    pub fn mul(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::token(input, "*")
    }

    pub fn div(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::token(input, "/")
    }

    pub fn left_parenthesis(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::token(input, "(")
    }

    pub fn right_parenthesis(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::token(input, ")")
    }

    pub fn lt(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::token(input, "<")
    }

    pub fn gt(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::token(input, ">")
    }

    pub fn lt_eq(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::token(input, "<=")
    }

    pub fn gt_eq(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::token(input, ">=")
    }

    pub fn equals(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::token(input, "==")
    }

    pub fn not_equals(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::token(input, "!=")
    }

    pub fn _left_shift(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::token(input, "<<")
    }

    pub fn true_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        let (input, t) = Token::specific_token(input, "true")?;

        Ok((input, t))
    }

    pub fn false_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        let (input, f) = Token::specific_token(input, "false")?;

        Ok((input, f))
    }

    pub fn arrow(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::specific_token(input, "->")
    }

    pub fn comment_multi_start(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        let comment = tag("/*")(input)?;

        Ok(comment)
    }

    pub fn comment_multi_end(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        let comment_end = tag("*/")(input)?;

        Ok(comment_end)
    }

    pub fn comment_single(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        let comment = tag("//")(input)?;

        Ok(comment)
    }

    pub fn comment_shebang(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        let comment = tag("#")(input)?;

        Ok(comment)
    }

    pub fn dot(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::token(input, ".")
    }

    pub fn inner_identifer(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
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

    pub fn namespace_separator(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        Token::token(input, "::")
    }

    pub fn identifier(input: ParseInput) -> ParseResult<ParseInput, String> {
        let (input, first_id) = Token::inner_identifer(input)?;

        let (input, namespaced) =
            many0(pair(Token::namespace_separator, Token::inner_identifer))(input)?;

        let mut identifier = first_id.fragment().to_string();
        namespaced.into_iter().for_each(|(sep, nspace)| {
            identifier.push_str(sep.fragment());
            identifier.push_str(nspace.fragment())
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

    fn non_neg_num(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        let num = take_while1(|c| is_digit(c as u8))(input)?;

        Ok(num)
    }

    pub fn bool_constant(input: ParseInput) -> ParseResult<ParseInput, bool> {
        let (input, b) = alt((Token::true_tok, Token::false_tok))(input)?;

        // We can unwrap since we recognized valid tokens already
        Ok((input, b.parse::<bool>().unwrap()))
    }

    pub fn float_constant(input: ParseInput) -> ParseResult<ParseInput, f64> {
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

    pub fn int_constant(input: ParseInput) -> ParseResult<ParseInput, i64> {
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
    pub fn char_constant(input: ParseInput) -> ParseResult<ParseInput, char> {
        let (input, _) = Token::single_quote(input)?;
        let (input, character) = anychar(input)?;
        let (input, _) = Token::single_quote(input)?;

        // FIXME: Handle escaping as well

        Ok((input, character))
    }

    /// Parse a string constant and return the characters between the double quotes
    pub fn string_constant(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        // FIXME: This does not allow for string escaping yet
        let string = delimited(Token::double_quote, take_until("\""), Token::double_quote)(input)?;

        Ok(string)
    }

    #[inline(always)]
    pub fn consume_multi_comment(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        let (input, _) = Token::comment_multi_start(input)?;
        let (input, content) = take_until("*/")(input)?;
        let (input, _) = Token::comment_multi_end(input)?;

        Ok((input, content))
    }

    #[inline(always)]
    pub fn consume_single_comment(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        let (input, _) = Token::comment_single(input)?;
        let comment = take_while(|c| c != '\n' && c != '\0')(input)?;

        Ok(comment)
    }

    #[inline(always)]
    pub fn consume_shebang_comment(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
        let (input, _) = Token::comment_shebang(input)?;
        let comment = take_while(|c| c != '\n' && c != '\0')(input)?;

        Ok(comment)
    }

    /// Consumes all kinds of comments: Multi-line or single-line
    pub fn consume_comment(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
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
    use crate::span;

    macro_rules! frag_first {
        ($span:expr) => {
            $span.map(|(s0, s1)| (*s0.fragment(), s1))
        };
    }

    macro_rules! frag_tuple {
        ($span:expr) => {
            $span.map(|(s0, s1)| (*s0.fragment(), *s1.fragment()))
        };
    }

    #[test]
    fn t_char_constant_valid() {
        assert_eq!(
            frag_first!(Token::char_constant(span!("'a'"))),
            Ok(("", 'a'))
        );
        assert_eq!(
            frag_first!(Token::char_constant(span!("'9'"))),
            Ok(("", '9'))
        );

        // FIXME: Add escaping
    }

    #[test]
    fn t_char_constant_invalid() {
        // Multiple characters
        assert!(Token::char_constant(span!("'abc'")).is_err());
    }

    #[test]
    fn t_string_constant() {
        // Simple string
        assert_eq!(
            frag_tuple!(Token::string_constant(span!("\"a str\""))),
            Ok(("", "a str"))
        );
        assert_eq!(
            frag_tuple!(Token::string_constant(span!("\"999 89 9\""))),
            Ok(("", "999 89 9"))
        );
        assert_eq!(
            frag_tuple!(Token::string_constant(span!("\"4.01f\""))),
            Ok(("", "4.01f"))
        );
        assert_eq!(
            frag_tuple!(Token::string_constant(span!("\"\""))),
            Ok(("", ""))
        );

        // FIXME: Fix string escaping
    }

    #[test]
    fn t_string_constant_unclosed_quote() {
        // Simple string
        assert!(Token::string_constant(span!("\"a str")).is_err());
    }

    #[test]
    fn t_int_constant_valid() {
        assert_eq!(frag_first!(Token::int_constant(span!("12"))), Ok(("", 12)));
        assert_eq!(
            frag_first!(Token::int_constant(span!("-45"))),
            Ok(("", -45))
        );
    }

    #[test]
    fn t_int_constant_invalid() {
        assert!(Token::int_constant(span!("ff2")).is_err());
    }

    #[test]
    fn t_float_constant_valid() {
        assert_eq!(
            frag_first!(Token::float_constant(span!("12.2"))),
            Ok(("", 12.2f64))
        );
        assert_eq!(
            frag_first!(Token::float_constant(span!("-45.06"))),
            Ok(("", -45.06f64))
        );
    }

    #[test]
    fn t_float_constant_invalid() {
        assert!(Token::float_constant(span!("ff2")).is_err());

        assert!(Token::float_constant(span!("12")).is_err());
    }

    #[test]
    fn t_id() {
        assert_eq!(
            frag_first!(Token::identifier(span!("x"))),
            Ok(("", "x".to_string()))
        );
        assert_eq!(
            frag_first!(Token::identifier(span!("x_"))),
            Ok(("", "x_".to_string()))
        );
        assert_eq!(
            frag_first!(Token::identifier(span!("x_99"))),
            Ok(("", "x_99".to_string()))
        );
        assert_eq!(
            frag_first!(Token::identifier(span!("99x"))),
            Ok(("", "99x".to_string()))
        );
        assert_eq!(
            frag_first!(Token::identifier(span!("n99 x"))),
            Ok((" x", "n99".to_string()))
        );
        assert_eq!(
            frag_first!(Token::identifier(span!("func_ x"))),
            Ok((" x", "func_".to_string()))
        );
    }

    #[test]
    fn t_id_invalid() {
        assert!(Token::identifier(span!("99")).is_err());
        assert!(Token::identifier(span!("__99_")).is_err());
        assert!(Token::identifier(span!("func")).is_err());
    }

    #[test]
    fn t_bool_valid() {
        assert_eq!(
            frag_first!(Token::bool_constant(span!("true"))),
            Ok(("", true))
        );
        assert_eq!(
            frag_first!(Token::bool_constant(span!("false"))),
            Ok(("", false))
        );
        assert_eq!(
            frag_first!(Token::bool_constant(span!("true a"))),
            Ok((" a", true))
        );
        assert_eq!(
            frag_first!(Token::bool_constant(span!("true; false"))),
            Ok(("; false", true))
        );
        assert_eq!(
            frag_first!(Token::bool_constant(span!("true*false"))),
            Ok(("*false", true))
        );
    }

    #[test]
    fn t_bool_invalid() {
        assert!(Token::bool_constant(span!("tru")).is_err());
        assert!(Token::bool_constant(span!("tru a")).is_err());
        assert!(Token::bool_constant(span!("trueast")).is_err());
    }

    #[test]
    fn t_multi_comment_valid() {
        assert!(Token::consume_comment(span!("/* */")).is_ok());
        assert!(Token::consume_comment(span!("/**/")).is_ok());
        assert!(Token::consume_comment(span!("/*            */")).is_ok());
        assert!(Token::consume_comment(span!("/* a bbbb a something   */")).is_ok());
    }

    #[test]
    fn t_single_comment_valid() {
        assert!(Token::consume_comment(span!("//")).is_ok());
        assert!(Token::consume_comment(span!("//                   ")).is_ok());
        assert!(Token::consume_comment(span!("//          \nhey")).is_ok());
        assert!(Token::consume_comment(span!("//// ")).is_ok());
        assert!(Token::consume_comment(span!("// a bbbb a something  /* hey */")).is_ok());
    }

    #[test]
    fn t_multi_comment_invalid() {
        assert!(Token::consume_multi_comment(span!("/*")).is_err());
    }

    #[test]
    fn t_keyword_next_to_curly() {
        assert_eq!(
            frag_tuple!(Token::loop_tok(span!("loop{}"))),
            Ok(("{}", "loop"))
        );
    }

    #[test]
    fn t_dot_token() {
        assert_eq!(frag_tuple!(Token::dot(span!("."))), Ok(("", ".")));
    }

    #[test]
    fn t_identifier_no_namespace() {
        assert_eq!(
            frag_first!(Token::identifier(span!("id"))),
            Ok(("", String::from("id")))
        );
    }

    #[test]
    fn t_identifier_plus_one_namespace() {
        assert_eq!(
            frag_first!(Token::identifier(span!("nspace::id"))),
            Ok(("", String::from("nspace::id")))
        );
    }

    #[test]
    fn t_identifier_plus_many_namespace() {
        assert_eq!(
            frag_first!(Token::identifier(span!("nspace::id::sub"))),
            Ok(("", String::from("nspace::id::sub")))
        );
    }

    #[test]
    fn t_identifier_invalid_just_sep() {
        assert!(Token::identifier(span!("::")).is_err());
    }

    #[test]
    fn t_identifier_invalid_nspace_no_id() {
        assert!(Token::identifier(span!("nspace::")).is_err());
    }

    #[test]
    fn t_identifier_invalid_nspace_no_id_multi() {
        assert!(Token::identifier(span!("nspace::id::sub::")).is_err());
    }
}
