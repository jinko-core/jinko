//! The jinko parser transforms user inputs into a map of instructions. A special
//! entry is created for the "main" function of the program. Including modules adds
//! instructions to that main entry.

mod box_construct;
mod constant_construct;
mod constructs;
mod jinko_insts;
mod shunting_yard;
mod tokens;

use crate::{error::JinkoError, interpreter::Interpreter};

pub use constructs::Construct;

pub struct Parser;

impl Parser {
    /// Parses the entire user input and returns a hashmap corresponding to the user
    /// program
    pub fn parse(input: &str) -> Result<Interpreter, JinkoError> {
        let mut interpreter = Interpreter::new();
        let (_, (instructions, last)) = Construct::stmts_and_maybe_last(input)?;

        let entry_block = interpreter.entry_point.block_mut().unwrap();

        entry_block.set_instructions(instructions);
        entry_block.set_last(last);

        Ok(interpreter)
    }
}
