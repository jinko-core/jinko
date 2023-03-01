use ast::{Ast, Node, Value, Visitor};
use error::{ErrKind, Error};
use location::{Source, SpanTuple};
use xparser::{ParseInput, ParseResult};

use nom::bytes::complete::take_until;
use nom::character::complete::char;
use nom_locate::{position, LocatedSpan};

struct Ctx;

fn parse_expr(to_parse: ParseInput) -> ParseResult<ParseInput, Ast> {
    let (input, _) = char('{')(to_parse)?;
    let (input, expr) = xparser::expr(input)?;
    let (input, _) = char('}')(input)?;

    Ok((input, expr))
}

fn parse_str(to_parse: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    // how to handle reaching the end of the string?
    take_until("{")(to_parse)
}

fn parse_inner(to_parse: &str) -> ParseResult<ParseInput, Ast> {
    todo!()
}

/// This function parses the following grammar:  `double_quote ( .* ( '{' expr '}' )* )* double_quote`
fn parse_format_string(to_parse: &str) -> Result<Vec<Ast>, Error> {
    let input = LocatedSpan::new_extra(to_parse, Source::Input(to_parse));
    todo!()
}

impl Visitor for Ctx {
    fn visit_constant(&mut self, location: SpanTuple, value: Value) -> Result<Ast, Error> {
        let s = match value {
            Value::Str(s) => s,
            _ => todo!(),
        };

        Error::new(ErrKind::Hint)
            .with_loc(Some(location.clone()))
            .with_msg(format!("saw a string: {s}!"))
            .emit_debug();

        Ok(Ast {
            location,
            node: Node::Constant(Value::Str(s)),
        })
    }
}

pub trait Interpolator: Sized {
    fn interpolate(self) -> Result<Self, Error>;
}

impl Interpolator for Ast {
    fn interpolate(self) -> Result<Self, Error> {
        Ctx.visit(self)
    }
}

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::{Node::*, Operator, Value::*};

    macro_rules! loc {
        () => {
            location::SpanTuple::with_source(
                location::SourceOwned::Empty,
                location::Location::new(1, 0),
                location::Location::new(1, 0),
            )
        };
    }

    macro_rules! ast {
        ($n:expr) => {
            Ast {
                location: loc!(),
                node: $n,
            }
        };
    }

    #[test]
    fn parse_one() {
        let s = "hello";

        let expected = ast! {
            Constant(Str(String::from("hello")))
        };

        assert_eq!(parse_format_string(s).unwrap()[0].node, expected.node)
    }

    #[test]
    fn parse_one_expr() {
        let s = "{15}";

        let expected = ast! {
            Constant(Integer(15))
        };

        assert_eq!(parse_format_string(s).unwrap()[0].node, expected.node)
    }

    #[test]
    fn parse_one_expr_one_string() {
        let s = "hello {15 + 4}";

        let expected_s = ast! {
            Constant(Str(String::from("hello ")))
        };
        let expected_expr = ast! {
            BinaryOp(
                Operator::Add,
                Box::new(ast! {
                    Constant(Integer(15))
                }),
                Box::new(ast! {
                    Constant(Integer(4))
                }),
            )
        };

        assert_eq!(parse_format_string(s).unwrap()[0].node, expected_s.node);
        assert_eq!(parse_format_string(s).unwrap()[1].node, expected_expr.node);
    }
}
