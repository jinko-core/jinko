//! String interpolation is performed during execution of a `JkString`.
//! The format used is the following: {expression}. This will replace {expression} with
//! the current value of the `expression`'s execution in the ctx at that time.
//! In order to use the '{' or '}' character themselves, use '{{' and '}}'.

use crate::generics::format_function_name;
use crate::instruction::{FunctionCall, Var};
use crate::parser::{constructs, ParseResult};
use crate::{
    log, CheckedType, Context, ErrKind, Error, FromObjectInstance, InstrKind, Instruction,
    JkString, ObjectInstance,
};

use nom::bytes::complete::{is_not, take_while};
use nom::character::complete::char;
use nom::combinator::opt;
use nom::multi::many0;
use nom::sequence::delimited;

const L_DELIM: char = '{';
const R_DELIM: char = '}';

pub struct JkStringFmt;

impl JkStringFmt {
    /// Consume input until a certain character is met. This is useful for interpolation,
    /// as consume_until("Some non interpolated text {some_var}", '{') will return
    /// "Some non interpolated text " and leave "{some_var}", which can easily be
    /// separated into a parsable expression
    fn consume_until(input: &str, limit: char) -> ParseResult<&str, Option<&str>> {
        match opt(take_while(|c| c != limit))(input) {
            Ok((i, Some(data))) => match data.len() {
                0 => Ok((i, None)),
                _ => Ok((i, Some(data))),
            },
            Ok(v) => Ok(v),
            Err(e) => Err(e),
        }
    }

    /// Parse a "pre expression" in a string to interpolate. The pre expressions are
    /// highlighted in the following string, spaces included
    ///
    /// ```ignore
    /// "Hey my name is {name} and I am {age} years old"
    ///  ^^^^^^^^^^^^^^^      ^^^^^^^^^^     ^^^^^^^^^^
    /// ```
    fn pre_expr(input: &str) -> ParseResult<&str, Option<&str>> {
        JkStringFmt::consume_until(input, L_DELIM)
    }

    /// Parse an expression in a string to interpolate. The expressions are
    /// highlighted in the following string, delimiters included. The expressions go
    /// through the same parsing rules as regular jinko expressions. They must be valid
    /// code.
    ///
    /// ```ignore
    /// "Hey my name is {name} and I am {age} years old"
    ///                 ^^^^^^          ^^^^^
    /// ```
    fn expr(input: &str) -> ParseResult<&str, Option<Box<dyn Instruction>>> {
        let (input, expr) = match opt(delimited(char(L_DELIM), is_not("{}"), char(R_DELIM)))(input)?
        {
            (i, Some("")) | (i, None) => return Ok((i, None)), // Early return, no need to parse an empty input
            (i, Some(e)) => (i, e),
        };

        let (_, expr) = constructs::expr(expr)?;

        Ok((input, Some(expr)))
    }

    /// Everything in the string is either a `pre_expr` or an `expr`
    /// The string "Hello {name}" has a pre_expr "Hello" and an expression "name".
    /// The string "{name}, how are you?" has an empty pre_expr, an expression "name", a
    /// pre_expr ", how are you?" and an empty expr
    fn parser(input: &str) -> ParseResult<&str, (Option<&str>, Option<Box<dyn Instruction>>)> {
        use nom::error::{ErrorKind, ParseError};
        use nom::Err;

        let (input, pre_expr) = JkStringFmt::pre_expr(input)?;
        let (input, expr) = JkStringFmt::expr(input)?;

        match (pre_expr, &expr) {
            // Stop multi-parsing when we couldn't parse anything anymore
            (None, None) => Err(Err::Error(ParseError::from_error_kind(
                input,
                ErrorKind::OneOf,
            ))),
            _ => Ok((input, (pre_expr, expr))),
        }
    }

    fn format_instance(inst: ObjectInstance, ctx: &mut Context) -> Result<String, Error> {
        // TODO: This is a *very* PoC and simple way of calling generic functions and
        // should be refactored and bettered later on. It works here because the idea
        // behind `fmt` functions is very simple: We can only ever call them with one
        // instance and thus one generic type
        let type_id = match inst.ty() {
            CheckedType::Resolved(ty_id) => ty_id.clone(),
            _ => unreachable!("Formatting operation which has not been typechecked or executed"),
        };

        log!("interpolating on type: {}", &type_id.id());

        ctx.scope_enter();
        let mut tmp_var = Var::new(String::from("+instance"));
        tmp_var.set_mutable(true);

        let mut tmp_var_expanded = tmp_var.clone();
        tmp_var_expanded.set_instance(inst);
        ctx.add_variable(tmp_var_expanded)?;

        let fmt_name = format_function_name("fmt", vec![type_id.clone()]);
        log!("generic fmt name: {}", &fmt_name);

        let fmt_call = FunctionCall::new(fmt_name, vec![], vec![Box::new(tmp_var)]);

        let result = match fmt_call.execute(ctx) {
            None => Err(Error::new(ErrKind::Context).with_msg(
                    format!("invalid call to formatting function: is the `fmt()` function implemented for the type `{}`?",
                    CheckedType::Resolved(type_id)))),
            Some(instance) => Ok(JkString::from_instance(&instance).rust_value())
        };

        ctx.scope_enter();

        result
    }

    /// Execute a parsed expression in the current context. This function returns the
    /// expression's execution's result as a String
    fn interpolate_expr(
        expr: Box<dyn Instruction>,
        _s: &str,
        ctx: &mut Context,
    ) -> Result<String, Error> {
        // We must check if the expression is a statement or not. If it is
        // an expression, then it is invalid in an Interpolation context
        match expr.kind() {
            InstrKind::Statement => Err(Error::new(ErrKind::Context).with_msg(format!(
                "invalid argument to string interpolation: {}",
                expr.print()
            ))), // FIXME: Fix loc and input
            InstrKind::Expression(_) => {
                match expr.execute(ctx) {
                    // FIXME: Call some jinko code, in order to enable custom types,
                    // instead of `to_string()` in Rust
                    Some(res) => JkStringFmt::format_instance(res, ctx),
                    // We just checked that we couldn't execute
                    // anything other than an expression. This pattern should never
                    // be encountered
                    _ => unreachable!(),
                }
            }
        }
    }

    /// Interpolates the string passed as parameter
    pub fn interpolate(s: &str, ctx: &mut Context) -> Result<String, Error> {
        // We know the final string will be roughly the same size as `s`. We can pre-allocate
        // to save some performance
        let mut result = String::with_capacity(s.len());

        let (_, expressions) = many0(JkStringFmt::parser)(s)?;

        for (pre_expr, expr) in expressions {
            result.push_str(pre_expr.unwrap_or(""));
            match expr {
                Some(expr) => result.push_str(&JkStringFmt::interpolate_expr(expr, s, ctx)?),
                None => {}
            }
        }

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    // FIXME: All of these tests should be fixed before merging

    use super::*;
    use crate::jinko;

    #[test]
    fn no_interpolation() {
        let s = "nothing to change here";
        let mut ctx = Context::new();

        assert_eq!(JkStringFmt::interpolate(s, &mut ctx).unwrap(), s);
    }

    /// Creates two variables, a and b, in an ctx
    fn setup() -> Context {
        jinko! {
            a = 1;
            b = 2;
        }
    }

    #[test]
    fn empty() {
        let mut ctx = setup();

        let s = "";
        assert_eq!(JkStringFmt::interpolate(s, &mut ctx).unwrap(), "");
    }

    #[test]
    fn only_expr() {
        let mut ctx = setup();

        let s = "{a}";
        assert_eq!(JkStringFmt::interpolate(s, &mut ctx).unwrap(), "1");
    }

    #[test]
    fn pre_expr_plus_expr() {
        let mut ctx = setup();

        let s = "Hey {a}";
        assert_eq!(JkStringFmt::interpolate(s, &mut ctx).unwrap(), "Hey 1");
    }

    #[test]
    fn expr_plus_pre_expr() {
        let mut ctx = setup();

        let s = "{a} Hey";
        assert_eq!(JkStringFmt::interpolate(s, &mut ctx).unwrap(), "1 Hey");
    }

    #[test]
    fn e_plus_pe_plus_e() {
        let mut ctx = setup();

        let s = "{a} Hey {b}";
        assert_eq!(JkStringFmt::interpolate(s, &mut ctx).unwrap(), "1 Hey 2");
    }

    #[test]
    fn pe_plus_e_plus_pe() {
        let mut ctx = setup();

        let s = "Hey {b} Hey";
        assert_eq!(JkStringFmt::interpolate(s, &mut ctx).unwrap(), "Hey 2 Hey");
    }

    #[test]
    fn stmt_as_interp() {
        let mut ctx = setup();

        let e = constructs::expr("mut x = 1").unwrap().1;
        e.execute(&mut ctx);

        let s = "Hey {x = 12}";
        assert!(JkStringFmt::interpolate(s, &mut ctx).is_err());
    }
}
