//! The REPL module implements an interactive mode for the broccoli interpreter. You can
//! use it as is, or run a file and then enter the interactive mode.

mod prompt;
use prompt::Prompt;

use linefeed::{Interface, ReadResult};

use crate::error::JinkoError;
use crate::instruction::Instruction;
use crate::interpreter::Interpreter;
use crate::parser::Construct;

/// Empty struct for the Repl methods
pub struct Repl;

impl Repl {
    /// Parse a new input, adding it to an existing interpreter
    fn parse_instruction(input: &str) -> Result<Box<dyn Instruction>, JinkoError> {
        match Construct::expression(input) {
            Ok((_, value)) => Ok(value),
            Err(e) => Err(JinkoError::from(e)),
        }
    }

    /// Launch the REPL
    pub fn launch_repl() -> Result<(), JinkoError> {
        let line_reader = Interface::new("jinko")?;
        let mut interpreter = Interpreter::new();

        // FIXME: Add actual prompt
        line_reader.set_prompt(&Prompt::get(&interpreter))?;

        while let ReadResult::Input(input) = line_reader.read_line()? {
            let inst = match Repl::parse_instruction(&input) {
                Ok(i) => i,
                Err(e) => {
                    println!("{}", e.to_string());
                    continue;
                }
            };

            match inst.execute(&mut interpreter) {
                Ok(()) => {}
                Err(e) => println!("{}", e.to_string()),
            };

            line_reader.set_prompt(&Prompt::get(&interpreter))?;
        }

        Ok(())
    }
}
