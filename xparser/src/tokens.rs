//! Token parser functions are used to define and recognize the particular tokens of
//! the language, so that { a + b } gets recognized into LEFT_BRACKET ID ADD ID RIGHT_BRACKET
//! and so on. This module consists of a lot of uninteresting helper/wrapper functions

use ast::{FunctionKind, Operator};
use nom::Err::Error as NomError;
use nom::{
    branch::alt, bytes::complete::tag, bytes::complete::take_until, bytes::complete::take_while,
    bytes::complete::take_while1, character::complete::anychar, character::complete::char,
    character::is_alphanumeric, character::is_digit, combinator::not, combinator::opt,
    combinator::peek, multi::many0, sequence::pair,
};

use super::Error;
use super::{ParseInput, ParseResult};

/// Reserved Keywords by jinko
const RESERVED_KEYWORDS: &[&str] = &[
    "func", "test", "mock", "type", "ext", "for", "while", "loop", "mut", "true", "false", "incl",
    "as", "return", "where",
];

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
            // FIXME: Reuse this
            // return Err(NomError(Error::new(ErrKind::Parsing).with_msg(
            //     String::from("Unexpected alphanum character after symbol"),
            // )));
            return Err(NomError(Error));
        }
    }
    Ok((input, tag))
}

pub fn single_quote(input: ParseInput) -> ParseResult<ParseInput, char> {
    specific_char(input, '\'')
}

pub fn double_quote(input: ParseInput) -> ParseResult<ParseInput, char> {
    specific_char(input, '"')
}

pub fn equal(input: ParseInput) -> ParseResult<ParseInput, char> {
    let (input, token) = specific_char(input, '=')?;
    if !input.is_empty() {
        peek(not(char('=')))(input)?;
    }

    Ok((input, token))
}

pub fn comma(input: ParseInput) -> ParseResult<ParseInput, char> {
    specific_char(input, ',')
}

pub fn pipe(input: ParseInput) -> ParseResult<ParseInput, char> {
    specific_char(input, '|')
}

pub fn left_curly_bracket(input: ParseInput) -> ParseResult<ParseInput, char> {
    specific_char(input, '{')
}

pub fn right_curly_bracket(input: ParseInput) -> ParseResult<ParseInput, char> {
    specific_char(input, '}')
}

pub fn left_bracket(input: ParseInput) -> ParseResult<ParseInput, char> {
    specific_char(input, '[')
}

pub fn right_bracket(input: ParseInput) -> ParseResult<ParseInput, char> {
    specific_char(input, ']')
}

pub fn colon(input: ParseInput) -> ParseResult<ParseInput, char> {
    specific_char(input, ':')
}

pub fn semicolon(input: ParseInput) -> ParseResult<ParseInput, char> {
    specific_char(input, ';')
}

pub fn at_sign(input: ParseInput) -> ParseResult<ParseInput, char> {
    specific_char(input, '@')
}

pub fn func_tok(input: ParseInput) -> ParseResult<ParseInput, FunctionKind> {
    specific_token(input, "func").map(|(input, _)| (input, FunctionKind::Func))
}

pub fn ext_tok(input: ParseInput) -> ParseResult<ParseInput, FunctionKind> {
    specific_token(input, "ext").map(|(input, _)| (input, FunctionKind::Extern))
}

pub fn test_tok(input: ParseInput) -> ParseResult<ParseInput, FunctionKind> {
    specific_token(input, "test").map(|(input, _)| (input, FunctionKind::Test))
}

pub fn mock_tok(input: ParseInput) -> ParseResult<ParseInput, FunctionKind> {
    specific_token(input, "mock").map(|(input, _)| (input, FunctionKind::Mock))
}

pub fn loop_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    specific_token(input, "loop")
}

pub fn while_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    specific_token(input, "while")
}

pub fn for_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    specific_token(input, "for")
}

pub fn in_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    specific_token(input, "in")
}

pub fn mut_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    specific_token(input, "mut")
}

pub fn if_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    specific_token(input, "if")
}

pub fn else_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    specific_token(input, "else")
}

pub fn return_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    specific_token(input, "return")
}

pub fn type_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    specific_token(input, "type")
}

pub fn incl_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    specific_token(input, "incl")
}

// Parse the `as` token. Rename it so clippy does not complain
pub fn az_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    specific_token(input, "as")
}

pub fn where_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    specific_token(input, "where")
}

pub fn backslash(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    token(input, "\\")
}

pub fn add(input: ParseInput) -> ParseResult<ParseInput, Operator> {
    token(input, "+").map(|(input, _)| (input, Operator::Add))
}

pub fn sub(input: ParseInput) -> ParseResult<ParseInput, Operator> {
    token(input, "-").map(|(input, _)| (input, Operator::Sub))
}

pub fn mul(input: ParseInput) -> ParseResult<ParseInput, Operator> {
    token(input, "*").map(|(input, _)| (input, Operator::Mul))
}

pub fn div(input: ParseInput) -> ParseResult<ParseInput, Operator> {
    token(input, "/").map(|(input, _)| (input, Operator::Div))
}

pub fn left_parenthesis(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    token(input, "(")
}

pub fn right_parenthesis(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    token(input, ")")
}

pub fn lt(input: ParseInput) -> ParseResult<ParseInput, Operator> {
    token(input, "<").map(|(input, _)| (input, Operator::Lt))
}

pub fn gt(input: ParseInput) -> ParseResult<ParseInput, Operator> {
    token(input, ">").map(|(input, _)| (input, Operator::Gt))
}

pub fn lt_eq(input: ParseInput) -> ParseResult<ParseInput, Operator> {
    token(input, "<=").map(|(input, _)| (input, Operator::LtEq))
}

pub fn gt_eq(input: ParseInput) -> ParseResult<ParseInput, Operator> {
    token(input, ">=").map(|(input, _)| (input, Operator::GtEq))
}

pub fn equals(input: ParseInput) -> ParseResult<ParseInput, Operator> {
    token(input, "==").map(|(input, _)| (input, Operator::Equals))
}

pub fn not_equals(input: ParseInput) -> ParseResult<ParseInput, Operator> {
    token(input, "!=").map(|(input, _)| (input, Operator::NotEquals))
}

// TODO: Add left and right shift
// pub fn _left_shift(input: ParseInput) -> ParseResult<ParseInput, Operator> {
//     token(input, "<<").map(|(input, _)| (input, Operator::LeftShift))
// }

pub fn true_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    let (input, t) = specific_token(input, "true")?;

    Ok((input, t))
}

pub fn false_tok(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    let (input, f) = specific_token(input, "false")?;

    Ok((input, f))
}

pub fn arrow(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    specific_token(input, "->")
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
    token(input, ".")
}

pub fn inner_identifer(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    let (input, id) = take_while1(|c| is_alphanumeric(c as u8) || c == '_')(input)?;

    if RESERVED_KEYWORDS.contains(&id) {
        return Err(NomError(
            // FIXME: Reuse this
            // Error::new(ErrKind::Parsing).with_msg(String::from("identifier cannot be keyword")),
            Error,
        ));
    }

    // At least one alphabetical character is required
    if id.chars().any(|c| c.is_alphabetic()) {
        return Ok((input, id));
    }

    Err(NomError(
        // FIXME: Reuse this
        // Error::new(ErrKind::Parsing).with_msg(String::from("invalid identifier")),
        Error,
    ))
}

pub fn namespace_separator(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    token(input, "::")
}

pub fn identifier(input: ParseInput) -> ParseResult<ParseInput, String> {
    let (input, first_id) = inner_identifer(input)?;

    let (input, namespaced) = many0(pair(namespace_separator, inner_identifer))(input)?;

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
    if namespace_separator(input).is_ok() {
        return Err(NomError(
            // FIXME: Reuse this
            // Error::new(ErrKind::Parsing).with_msg(
            // String::from("cannot finish identifier on namespace separator `::`"))
            Error,
        ));
    }

    Ok((input, identifier))
}

fn non_neg_num(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    let num = take_while1(|c| is_digit(c as u8))(input)?;

    Ok(num)
}

pub fn bool_constant(input: ParseInput) -> ParseResult<ParseInput, bool> {
    let (input, b) = alt((true_tok, false_tok))(input)?;

    // We can unwrap since we recognized valid tokens already
    Ok((input, b.parse::<bool>().unwrap()))
}

pub fn float_constant(input: ParseInput) -> ParseResult<ParseInput, f64> {
    let (input, negative_sign) = opt(char('-'))(input)?;
    let (input, whole) = int_constant(input)?;
    let (input, _) = char('.')(input)?;
    let (input, decimal) = non_neg_num(input)?;

    match format!("{whole}.{decimal}").parse::<f64>() {
        Ok(value) => match negative_sign {
            Some(_) => Ok((input, -value)),
            None => Ok((input, value)),
        },
        Err(_) => Err(NomError(
            // FIXME: Reuse this
            //     Error::new(ErrKind::Parsing).with_msg(format!(
            //     "invalid floating point number: {}.{}",
            //     whole, decimal
            // ))
            Error,
        )),
    }
}

pub fn int_constant(input: ParseInput) -> ParseResult<ParseInput, i64> {
    let (input, negative_sign) = opt(char('-'))(input)?;
    let (input, num) = non_neg_num(input)?;

    match num.parse::<i64>() {
        Ok(value) => match negative_sign {
            Some(_) => Ok((input, -value)),
            None => Ok((input, value)),
        },
        Err(_) => Err(NomError(
            // FIXME: Reuse this
            // Error::new(ErrKind::Parsing).with_msg(format!("invalid integer: {}", num)),
            Error,
        )),
    }
}

/// Parse a single character constant and return the character inside the quotes
pub fn char_constant(input: ParseInput) -> ParseResult<ParseInput, char> {
    let (input, _) = single_quote(input)?;
    let (input, character) = anychar(input)?;
    let (input, _) = single_quote(input)?;

    // FIXME: Handle escaping as well

    Ok((input, character))
}

#[inline(always)]
pub fn consume_multi_comment(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    let (input, _) = comment_multi_start(input)?;
    let (input, content) = take_until("*/")(input)?;
    let (input, _) = comment_multi_end(input)?;

    Ok((input, content))
}

#[inline(always)]
pub fn consume_single_comment(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    let (input, _) = comment_single(input)?;
    let comment = take_while(|c| c != '\n' && c != '\0')(input)?;

    Ok(comment)
}

#[inline(always)]
pub fn consume_shebang_comment(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    let (input, _) = comment_shebang(input)?;
    let comment = take_while(|c| c != '\n' && c != '\0')(input)?;

    Ok(comment)
}

/// Consumes all kinds of comments: Multi-line or single-line
pub fn consume_comment(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    alt((
        consume_shebang_comment,
        consume_single_comment,
        consume_multi_comment,
    ))(input)
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
        assert_eq!(frag_first!(char_constant(span!("'a'"))), Ok(("", 'a')));
        assert_eq!(frag_first!(char_constant(span!("'9'"))), Ok(("", '9')));

        // FIXME: Add escaping
    }

    #[test]
    fn t_char_constant_invalid() {
        // Multiple characters
        assert!(char_constant(span!("'abc'")).is_err());
    }

    // FIXME: Should these be moved to `constructs`?
    // #[test]
    // fn t_string_constant() {
    //     // Simple string
    //     assert_eq!(
    //         frag_tuple!(string_constant(span!("\"a str\""))),
    //         Ok(("", "a str"))
    //     );
    //     assert_eq!(
    //         frag_tuple!(string_constant(span!("\"999 89 9\""))),
    //         Ok(("", "999 89 9"))
    //     );
    //     assert_eq!(
    //         frag_tuple!(string_constant(span!("\"4.01f\""))),
    //         Ok(("", "4.01f"))
    //     );
    //     assert_eq!(frag_tuple!(string_constant(span!("\"\""))), Ok(("", "")));

    //     // FIXME: Fix string escaping
    // }

    // #[test]
    // fn t_string_constant_unclosed_quote() {
    //     // Simple string
    //     assert!(string_constant(span!("\"a str")).is_err());
    // }

    #[test]
    fn t_int_constant_valid() {
        assert_eq!(frag_first!(int_constant(span!("12"))), Ok(("", 12)));
        assert_eq!(frag_first!(int_constant(span!("-45"))), Ok(("", -45)));
    }

    #[test]
    fn t_int_constant_invalid() {
        assert!(int_constant(span!("ff2")).is_err());
    }

    #[test]
    fn t_float_constant_valid() {
        assert_eq!(
            frag_first!(float_constant(span!("12.2"))),
            Ok(("", 12.2f64))
        );
        assert_eq!(
            frag_first!(float_constant(span!("-45.06"))),
            Ok(("", -45.06f64))
        );
    }

    #[test]
    fn t_float_constant_invalid() {
        assert!(float_constant(span!("ff2")).is_err());

        assert!(float_constant(span!("12")).is_err());
    }

    #[test]
    fn t_id() {
        assert_eq!(
            frag_first!(identifier(span!("x"))),
            Ok(("", "x".to_string()))
        );
        assert_eq!(
            frag_first!(identifier(span!("x_"))),
            Ok(("", "x_".to_string()))
        );
        assert_eq!(
            frag_first!(identifier(span!("x_99"))),
            Ok(("", "x_99".to_string()))
        );
        assert_eq!(
            frag_first!(identifier(span!("99x"))),
            Ok(("", "99x".to_string()))
        );
        assert_eq!(
            frag_first!(identifier(span!("n99 x"))),
            Ok((" x", "n99".to_string()))
        );
        assert_eq!(
            frag_first!(identifier(span!("func_ x"))),
            Ok((" x", "func_".to_string()))
        );
    }

    #[test]
    fn t_id_invalid() {
        assert!(identifier(span!("99")).is_err());
        assert!(identifier(span!("__99_")).is_err());
        assert!(identifier(span!("func")).is_err());
    }

    #[test]
    fn t_bool_valid() {
        assert_eq!(frag_first!(bool_constant(span!("true"))), Ok(("", true)));
        assert_eq!(frag_first!(bool_constant(span!("false"))), Ok(("", false)));
        assert_eq!(
            frag_first!(bool_constant(span!("true a"))),
            Ok((" a", true))
        );
        assert_eq!(
            frag_first!(bool_constant(span!("true; false"))),
            Ok(("; false", true))
        );
        assert_eq!(
            frag_first!(bool_constant(span!("true*false"))),
            Ok(("*false", true))
        );
    }

    #[test]
    fn t_bool_invalid() {
        assert!(bool_constant(span!("tru")).is_err());
        assert!(bool_constant(span!("tru a")).is_err());
        assert!(bool_constant(span!("trueast")).is_err());
    }

    #[test]
    fn t_multi_comment_valid() {
        assert!(consume_comment(span!("/* */")).is_ok());
        assert!(consume_comment(span!("/**/")).is_ok());
        assert!(consume_comment(span!("/*            */")).is_ok());
        assert!(consume_comment(span!("/* a bbbb a something   */")).is_ok());
    }

    #[test]
    fn t_single_comment_valid() {
        assert!(consume_comment(span!("//")).is_ok());
        assert!(consume_comment(span!("//                   ")).is_ok());
        assert!(consume_comment(span!("//          \nhey")).is_ok());
        assert!(consume_comment(span!("//// ")).is_ok());
        assert!(consume_comment(span!("// a bbbb a something  /* hey */")).is_ok());
    }

    #[test]
    fn t_multi_comment_invalid() {
        assert!(consume_multi_comment(span!("/*")).is_err());
    }

    #[test]
    fn t_keyword_next_to_curly() {
        assert_eq!(frag_tuple!(loop_tok(span!("loop{}"))), Ok(("{}", "loop")));
    }

    #[test]
    fn t_dot_token() {
        assert_eq!(frag_tuple!(dot(span!("."))), Ok(("", ".")));
    }

    #[test]
    fn t_identifier_no_namespace() {
        assert_eq!(
            frag_first!(identifier(span!("id"))),
            Ok(("", String::from("id")))
        );
    }

    #[test]
    fn t_identifier_plus_one_namespace() {
        assert_eq!(
            frag_first!(identifier(span!("nspace::id"))),
            Ok(("", String::from("nspace::id")))
        );
    }

    #[test]
    fn t_identifier_plus_many_namespace() {
        assert_eq!(
            frag_first!(identifier(span!("nspace::id::sub"))),
            Ok(("", String::from("nspace::id::sub")))
        );
    }

    #[test]
    fn t_identifier_invalid_just_sep() {
        assert!(identifier(span!("::")).is_err());
    }

    #[test]
    fn t_identifier_invalid_nspace_no_id() {
        assert!(identifier(span!("nspace::")).is_err());
    }

    #[test]
    fn t_identifier_invalid_nspace_no_id_multi() {
        assert!(identifier(span!("nspace::id::sub::")).is_err());
    }
}
