//! The jinko parser transforms user inputs into a map of instructions. A special
//! entry is created for the "main" function of the program. Including modules adds
//! instructions to that main entry.

use crate::{InstrKind, Interpreter, JkError};

use nom::multi::many0;

mod box_construct;
mod constant_construct;
pub mod constructs;
mod shunting_yard;
mod tokens;

pub use box_construct::BoxConstruct;
pub use constant_construct::ConstantConstruct;
pub use constructs::Construct;
pub use shunting_yard::ShuntingYard;
pub use tokens::Token;

pub struct Parser;

impl Parser {
    /// Parses the entire user input and returns a hashmap corresponding to the user
    /// program
    pub fn parse(input: &str) -> Result<Interpreter, JkError> {
        let mut interpreter = Interpreter::new();

        let entry_block = interpreter.entry_point.block_mut().unwrap();

        let (_, instructions) = many0(Construct::instruction_maybe_semicolon)(input)?;

        entry_block.set_instructions(instructions);

        // We must create the block "manually", by checking if the last parsed operation
        // is an expression or not. If it is an expression, then use it as the return
        // value. If not, simply execute it.
        match entry_block.pop_instruction() {
            Some(last) => match last.kind() {
                InstrKind::Expression(_) => entry_block.set_last(Some(last)),
                InstrKind::Statement => entry_block.add_instruction(last),
            },
            None => {}
        }

        Ok(interpreter)
    }
}
