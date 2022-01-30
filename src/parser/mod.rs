//! The jinko parser transforms user inputs into a map of instructions. A special
//! entry is created for the "main" function of the program. Including modules adds
//! instructions to that main entry.

use crate::{log, Context, Error};

mod constant_construct;
pub mod constructs;
mod tokens;

pub use constant_construct::ConstantConstruct;
use nom_locate::LocatedSpan;
pub use tokens::Token;

pub type ParseResult<T, I> = nom::IResult<T, I, Error>;

/// Parses the entire user input and returns a hashmap corresponding to the user
/// program
pub fn parse(ctx: &mut Context, input: &str) -> Result<(), Error> {
    // FIXME: Keep input in context here
    log!("parsing file: {:?}", ctx.path());
    ctx.set_code(input.to_string());
    let entry_block = ctx.entry_point.block_mut().unwrap();
    let input = LocatedSpan::new(input);

    let (_, instructions) = constructs::many_expr(input)?;

    entry_block.add_instructions(instructions);

    Ok(())
}

#[cfg(test)]
#[macro_export]
macro_rules! span {
    ($s:literal) => {
        nom_locate::LocatedSpan::new($s)
    };
}
