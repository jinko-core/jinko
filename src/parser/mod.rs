//! The jinko parser transforms user inputs into a map of instructions. A special
//! entry is created for the "main" function of the program. Including modules adds
//! instructions to that main entry.

mod box_construct;
mod constructs;
mod tokens;

use crate::interpreter::Interpreter;

pub use constructs::Construct;

pub struct Parser;

impl Parser {
    /// Parses the entire user input and returns a hashmap corresponding to the user
    /// program
    pub fn parse(input: &str) -> Result<Interpreter, String> {
        let interpreter = Interpreter::new();

        Ok(interpreter)
    }
}
