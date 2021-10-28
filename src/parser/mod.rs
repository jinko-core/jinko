//! The jinko parser transforms user inputs into a map of instructions. A special
//! entry is created for the "main" function of the program. Including modules adds
//! instructions to that main entry.

use crate::{Context, Error, InstrKind};

mod constant_construct;
pub mod constructs;
mod tokens;

pub use constant_construct::ConstantConstruct;
pub use tokens::Token;

pub type ParseResult<T, I> = nom::IResult<T, I, Error>;

pub struct Parser;

#[macro_export]
macro_rules! jinko_ex {
    ($($t:tt) *) => {
        $crate::Parser::parse(stringify!( $( $t ) * )).unwrap().execute().unwrap()
    }
}

impl Parser {
    /// Parses the entire user input and returns a hashmap corresponding to the user
    /// program
    pub fn parse(input: &str) -> Result<Context, Error> {
        let mut ctx = Context::new();

        let entry_block = ctx.entry_point.block_mut().unwrap();

        let (_, instructions) = constructs::many_expr(input)?;

        entry_block.add_instructions(instructions);

        // We must create the block "manually", by checking if the last parsed operation
        // is an expression or not. If it is an expression, then use it as the return
        // value. If not, simply execute it.
        if let Some(last) = entry_block.pop_instruction() {
            match last.kind() {
                InstrKind::Expression(_) => entry_block.set_last(Some(last)),
                InstrKind::Statement => entry_block.add_instruction(last),
            }
        }

        Ok(ctx)
    }
}
