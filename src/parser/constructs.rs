//! A `Construct` is a complex set of tokens. For example, `fn()` is an identifier, a
//! left parenthesis and a right parenthesis. Together, they constitute a function call.
//! In the same vein, `x = 12;` is 4 tokens used to represent variable assignment.  Therefore, constructs use tokens while the parser only uses constructs. This is an
//! abstraction for all possible ways to parse a line in broccoli.
//!
//! Each of the functions in that module contain the grammar they represent above their
//! name. The syntax used for the grammar is loosely based on regular expressions and
//! globbing. One can use * to indicate 0 or more, ? to indicate 1 or more, etc etc.
//! Optional parameters are included between brackets. For example,
//!
//! `[mut] <identifier> = <const> | <function_call> | <block> | <identifier>`
//!
//! is the grammar for a variable assignment.

use nom::{branch::alt, combinator::opt, multi::many0, IResult};

use crate::instruction::{FunctionCall, VarAssign};
use crate::value::constant::{ConstKind, Constant};

use super::tokens::Token;

pub struct Construct;

impl Construct {
    /// Constants are raw values in the source code. For example, `"string"`, `12` and
    /// `0.5`.
    ///
    /// `'<any_char>' | "<any_char>*" | <num>? | <num>?.<num>?`
    pub fn constant(input: &str) -> IResult<&str, Constant> {
        let (input, char_value) = opt(Token::char_constant)(input)?;
        let (input, str_value) = opt(Token::string_constant)(input)?;
        let (input, float_value) = opt(Token::float_constant)(input)?;
        let (input, int_value) = opt(Token::int_constant)(input)?;

        match (char_value, str_value, int_value, float_value) {
            (Some(c), None, None, None) => Ok((input, Constant::new(ConstKind::Char).with_cv(c))),
            (None, Some(s), None, None) => {
                Ok((input, Constant::new(ConstKind::Str).with_sv(s.to_owned())))
            }
            (None, None, Some(i), None) => Ok((input, Constant::new(ConstKind::Int).with_iv(i))),
            (None, None, None, Some(f)) => Ok((input, Constant::new(ConstKind::Float).with_fv(f))),
            _ => Err(nom::Err::Failure((
                "Not a valid constant",
                nom::error::ErrorKind::OneOf,
            ))),
        }
    }

    /// Parse a function call with no arguments
    ///
    /// `<identifier> ( )`
    fn function_call_no_args(input: &str) -> IResult<&str, FunctionCall> {
        let (input, fn_id) = Token::identifier(input)?;
        let (input, _) = Token::left_parenthesis(input)?;
        let (input, _) = Token::right_parenthesis(input)?;

        Ok((input, FunctionCall::new(fn_id.to_owned())))
    }

    // FIXME: Allow something else than constants
    /// Parse an argument given to a function. Consumes the whitespaces before and after
    /// the argument
    fn arg(input: &str) -> IResult<&str, Constant> {
        let (input, _) = Token::maybe_consume_whitespaces(input)?;

        // FIXME: Allow something else than constants, as above
        let (input, constant) = Construct::constant(input)?;

        let (input, _) = Token::maybe_consume_whitespaces(input)?;

        Ok((input, constant))
    }
    fn arg_and_comma(input: &str) -> IResult<&str, Constant> {
        let (input, constant) = Construct::arg(input)?;
        let (input, _) = Token::comma(input)?;

        Ok((input, constant))
    }

    /// Parse a function call with arguments
    fn function_call_args(input: &str) -> IResult<&str, FunctionCall> {
        let (input, fn_id) = Token::identifier(input)?;
        let (input, _) = Token::left_parenthesis(input)?;

        let mut fn_call = FunctionCall::new(fn_id.to_owned());

        // Get 1 or more arguments with a comma to the function call
        let (input, mut arg_vec) = many0(Construct::arg_and_comma)(input)?;

        // Parse the last argument, which does not have a comma. There needs to be
        // at least one argument, which can be this one
        let (input, last_arg) = Construct::arg(input)?;

        arg_vec.drain(0..).for_each(|arg| fn_call.add_arg(arg));
        fn_call.add_arg(last_arg);

        Ok((input, fn_call))
    }

    /// When a function is called in the source code.
    ///
    /// ```
    /// fn(); // Function call
    /// fn() // Call the function `fn` and use the return result as an expression
    /// x = fn(); // Assign the result of the function call to the variable x
    /// ```
    ///
    /// `<arg_list> := [(<constant> | <variable> | <expression>)*]
    /// `<identifier> ( <arg_list> )`
    pub fn function_call(input: &str) -> IResult<&str, FunctionCall> {
        alt((
            Construct::function_call_no_args,
            Construct::function_call_args,
        ))(input)
    }

    /// When a variable is assigned a value. Ideally, a variable cannot be assigned the
    /// `void` type.
    ///
    /// ```
    /// x = 12; // Store 12 into the variable `x`
    /// x = 456; // Forbidden, `x` is immutable
    /// mut n = 12; // Store 12 into `n`, a mutable variable
    /// n = 1586; // Allowed
    /// ```
    ///
    /// A variable assignment is a Statement. It cannot be used as an Expression
    ///
    /// ```
    /// {
    ///     x = 12; // Block returns void
    /// }
    /// {
    ///     x = 12 // Forbidden
    /// }
    /// {
    ///     x = call();
    ///     x // Okay
    /// } // But it's easier to just...
    /// {
    ///     call()
    /// }
    /// ```
    ///
    /// `[mut] <identifier> = ( <constant> | <function_call> ) ;`
    pub fn var_assignment(input: &'static str) -> IResult<&str, VarAssign> {
        // FIXME: Maybe use alt ?
        let (input, mut_opt) = opt(Token::mut_tok)(input)?;
        let (input, _) = Token::maybe_consume_whitespaces(input)?;

        let (input, id) = Token::identifier(input)?;
        let (input, _) = opt(Token::consume_whitespaces)(input)?;
        let (input, _) = Token::equal(input)?;
        let (input, _) = opt(Token::consume_whitespaces)(input)?;
        let (input, constant) = Construct::constant(input)?;
        let (input, _) = Token::semicolon(input)?;

        match mut_opt {
            Some(_) => Ok((input, VarAssign::new(true, id.to_owned(), constant))),
            None => Ok((input, VarAssign::new(false, id.to_owned(), constant))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_constant_valid() {
        assert_eq!(Construct::constant("12").unwrap().1.kind(), ConstKind::Int);
        assert_eq!(
            Construct::constant("12.2").unwrap().1.kind(),
            ConstKind::Float
        );
        assert_eq!(
            Construct::constant("'a'").unwrap().1.kind(),
            ConstKind::Char
        );
        assert_eq!(
            Construct::constant("\"a\"").unwrap().1.kind(),
            ConstKind::Str
        );
    }

    #[test]
    fn t_var_assign_valid() {
        assert_eq!(
            Construct::var_assignment("x = 12;").unwrap().1.mutable(),
            false
        );
        assert_eq!(
            Construct::var_assignment("x = 12;").unwrap().1.symbol(),
            "x"
        );

        assert_eq!(
            Construct::var_assignment("mut x_99 = 129;")
                .unwrap()
                .1
                .mutable(),
            true
        );
        assert_eq!(
            Construct::var_assignment("mut x_99 = 129;")
                .unwrap()
                .1
                .symbol(),
            "x_99"
        );

        assert_eq!(
            Construct::var_assignment("mut_x_99 = 129;")
                .unwrap()
                .1
                .mutable(),
            false
        );
        assert_eq!(
            Construct::var_assignment("mut_x_99 = 129;")
                .unwrap()
                .1
                .symbol(),
            "mut_x_99"
        );

        assert_eq!(
            Construct::var_assignment("mut mut_x_99 = 129;")
                .unwrap()
                .1
                .mutable(),
            true
        );
        assert_eq!(
            Construct::var_assignment("mut mut_x_99 = 129;")
                .unwrap()
                .1
                .symbol(),
            "mut_x_99"
        );

        match Construct::var_assignment("mut x=12;") {
            Ok(_) => assert!(true),
            Err(_) => assert!(false, "Equal stuck to id is allowed"),
        }
        match Construct::var_assignment("mut x= 12;") {
            Ok(_) => assert!(true),
            Err(_) => assert!(false, "Equal stuck to id is allowed"),
        }
        match Construct::var_assignment("mut x =12;") {
            Ok(_) => assert!(true),
            Err(_) => assert!(false, "Equal stuck to value is allowed"),
        }
    }

    #[test]
    fn t_var_assign_invalid() {
        match Construct::var_assignment("mutable x = 12") {
            Ok(_) => assert!(false, "Mutable isn't mut"),
            Err(_) => assert!(true),
        }
        match Construct::var_assignment("mut x = 12") {
            Ok(_) => assert!(false, "No semicolon"),
            Err(_) => assert!(true),
        }
        match Construct::var_assignment("mut_x = 12") {
            Ok(_) => assert!(false, "No semicolon"),
            Err(_) => assert!(true),
        }
    }

    #[test]
    fn t_function_call_no_args_valid() {
        assert_eq!(Construct::function_call("fn()").unwrap().1.name(), "fn");
        assert_eq!(Construct::function_call("fn()").unwrap().1.args().len(), 0);
    }

    #[test]
    fn t_function_call_valid() {
        assert_eq!(Construct::function_call("fn(2)").unwrap().1.name(), "fn");
        assert_eq!(Construct::function_call("fn(2)").unwrap().1.args().len(), 1);

        assert_eq!(
            Construct::function_call("fn(1, 2, 3)").unwrap().1.name(),
            "fn"
        );
        assert_eq!(
            Construct::function_call("fn(1, 2, 3)")
                .unwrap()
                .1
                .args()
                .len(),
            3
        );

        assert_eq!(
            Construct::function_call("fn(1   , 2,3)").unwrap().1.name(),
            "fn"
        );
        assert_eq!(
            Construct::function_call("fn(1   , 2,3)")
                .unwrap()
                .1
                .args()
                .len(),
            3
        );

        // FIXME: Add constants and expressions
    }

    #[test]
    fn t_function_call_invalid() {
        match Construct::function_call("fn(") {
            Ok(_) => assert!(false, "Unterminated parenthesis"),
            Err(_) => assert!(true),
        }
        match Construct::function_call("fn))") {
            Ok(_) => assert!(false, "Wrong parenthesis"),
            Err(_) => assert!(true),
        }
        match Construct::function_call("fn((") {
            Ok(_) => assert!(false, "Wrong parenthesis again"),
            Err(_) => assert!(true),
        }
        match Construct::function_call("fn((") {
            Ok(_) => assert!(false, "Wrong parenthesis again"),
            Err(_) => assert!(true),
        }

        match Construct::function_call("fn((") {
            Ok(_) => assert!(false, "Wrong parenthesis again"),
            Err(_) => assert!(true),
        }
    }
}
