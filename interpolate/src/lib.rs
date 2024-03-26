use ast::{Ast, Node, Value, Visitor};
use error::{ErrKind, Error};
use location::{Location, Source, SpanTuple};
use nom::multi::many0;
use xparser::{Error as ParseError, ParseInput, ParseResult};

use nom::character::complete::char;
use nom::{bytes::complete::take_until, combinator::opt};
use nom_locate::{position, LocatedSpan};

struct Ctx;

fn parse_expr(input: ParseInput) -> ParseResult<ParseInput, Ast> {
    let (input, _) = char('{')(input)?;
    let (input, expr) = xparser::expr(input)?;
    let (input, _) = char('}')(input)?;

    Ok((input, expr))
}

fn parse_str(input: ParseInput) -> ParseResult<ParseInput, ParseInput> {
    // how to handle reaching the end of the string?
    take_until("{")(input)
}

pub(crate) fn pos_to_loc(
    input: ParseInput,
    start: impl Into<Location>,
    end: impl Into<Location>,
) -> SpanTuple {
    SpanTuple::with_source_ref(input.extra, start.into(), end.into())
}

fn parse_inner(input: ParseInput) -> ParseResult<ParseInput, Ast> {
    let (input, start) = position(input)?;
    let (input, s) = opt(parse_str)(input)?;
    let (input, end) = position(input)?;
    if let Some(s) = s {
        return Ok((
            input,
            Ast {
                location: pos_to_loc(input, start, end),
                node: Node::Constant(Value::Str(s.fragment().to_string())),
            },
        ));
    }

    let (input, s) = opt(parse_expr)(input)?;
    let (input, end) = position(input)?;
    if let Some(expr) = s {
        return Ok((input, expr));
    }

    let loc = pos_to_loc(input, start, end);

    Error::new(ErrKind::Parsing)
        .with_msg(format!("unexpected character in format string: {input}"))
        .with_loc(Some(loc))
        .emit();

    Err(nom::Err::Error(ParseError))
}

/// This function parses the following grammar:  `double_quote ( .* ( '{' expr '}' )* )* double_quote`
fn parse_format_string(to_parse: &str) -> Result<Vec<Ast>, Error> {
    let input = LocatedSpan::new_extra(to_parse, Source::Input(to_parse));

    let res = many0(parse_inner)(input);

    if let Ok((_, exprs)) = res {
        Ok(exprs)
    } else {
        Err(Error::new(ErrKind::Parsing))
    }
}

impl Visitor for Ctx {
    fn visit_constant(&mut self, location: SpanTuple, value: Value) -> Result<Ast, Error> {
        let s = match value {
            Value::Str(s) => s,
            any_const => {
                return Ok(Ast {
                    location,
                    node: Node::Constant(any_const),
                })
            }
        };

        Error::new(ErrKind::Hint)
            .with_loc(Some(location.clone()))
            .with_msg(format!("saw a string: {s}!"))
            .emit_debug();

        let exprs = parse_format_string(&s)?;
        dbg!(exprs);

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
