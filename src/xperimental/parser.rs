mod constructs;
mod tokens;

use crate::error::Error;
use crate::location::Source;

use ast::Ast;

use ast::Node;
use nom_locate::position;
use nom_locate::LocatedSpan;

pub type ParseInput<'i> = LocatedSpan<&'i str, Source<'i>>;
pub type ParseResult<T, I> = nom::IResult<T, I, Error>;

/// Parses the entire user input into a vector of instructions in the context
pub fn parse(input: &str, source: Source) -> Result<Ast, Error> {
    let input = LocatedSpan::new_extra(input, source);

    let (input, start) = position::<ParseInput, Error>(input)?;
    let (input, stmts) = constructs::many_exprs(input)?;
    let (input, end) = position::<ParseInput, Error>(input)?;

    // TODO: How does this work with the last statement being an expression?
    Ok(Ast {
        location: constructs::pos_to_loc(input, start, end),
        node: Node::Block {
            stmts,
            last_is_expr: false,
        }, // FIXME: Invalid
    })
}
