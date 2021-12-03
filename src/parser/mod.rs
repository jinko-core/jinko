//! The jinko parser transforms user inputs into a map of instructions. A special
//! entry is created for the "main" function of the program. Including modules adds
//! instructions to that main entry.

use crate::{log, Context, Error};

mod constant_construct;
pub mod constructs;
mod tokens;

pub use constant_construct::ConstantConstruct;
pub use tokens::Token;

pub type ParseResult<T, I> = nom::IResult<T, I, Error>;

/// Parses the entire user input and returns a hashmap corresponding to the user
/// program
pub fn parse(ctx: &mut Context, input: &str) -> Result<(), Error> {
    log!("parsing file: {:?}", ctx.path());
    let entry_block = ctx.entry_point.block_mut().unwrap();

    let (_, instructions) = constructs::many_expr(input)?;

    entry_block.add_instructions(instructions);

    Ok(())
}
