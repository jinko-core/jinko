//! The jinko parser transforms user inputs into a map of instructions. A special
//! entry is created for the "main" function of the program. Including modules adds
//! instructions to that main entry.

use crate::context::Context;
use crate::error::Error;
use crate::location::Source;

mod constant_construct;
pub mod constructs;
mod tokens;

pub use constant_construct::ConstantConstruct;
use nom_locate::LocatedSpan;
pub use tokens::Token;

pub type ParseInput<'i> = LocatedSpan<&'i str, Source<'i>>;
pub type ParseResult<T, I> = nom::IResult<T, I, Error>;

/// Parses the entire user input into a vector of instructions in the context
pub fn parse(ctx: &mut Context, input: &str, source: Source) -> Result<(), Error> {
    // FIXME: Keep input in context here
    ctx.set_code(input.to_string());
    let entry_block = ctx.entry_point.block_mut().unwrap();
    let input = LocatedSpan::new_extra(input, source);

    let (_, instructions) = constructs::many_expr(input)?;

    entry_block.add_instructions(instructions);

    Ok(())
}

#[cfg(test)]
#[macro_export]
macro_rules! span {
    ($s:literal) => {
        nom_locate::LocatedSpan::new_extra($s, $crate::location::Source::Input($s))
    };
}
