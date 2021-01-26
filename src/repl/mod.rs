//! The REPL module implements an interactive mode for the jinko interpreter. You can
//! use it as is, or run a file and then enter the interactive mode.

mod prompt;
use prompt::Prompt;

use linefeed::{Interface, ReadResult};

use crate::args::Args;
use crate::error::JinkoError;
use crate::instruction::{InstrKind, Instruction};
use crate::interpreter::Interpreter;
use crate::parser::Construct;

/// Empty struct for the Repl methods
pub struct Repl;

impl Repl {
    /// Parse a new input, adding it to an existing interpreter
    fn parse_instruction(input: &str) -> Result<Option<Box<dyn Instruction>>, JinkoError> {
        match input.is_empty() {
            true => Ok(None),
            false => match Construct::expression(input) {
                Ok((_, value)) => Ok(Some(value)),
                Err(e) => Err(JinkoError::from(e)),
            },
        }
    }

    /// Launch the REPL
    pub fn launch_repl(args: &Args) -> Result<(), JinkoError> {
        let line_reader = Interface::new("jinko")?;
        let mut interpreter = Interpreter::new();
        interpreter.debug_mode = args.debug;

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

            let inst = match inst {
                Some(i) => i,
                None => continue,
            };

            match inst.execute(&mut interpreter) {
                // FIXME: Handle statements and expressions differently
                Ok(InstrKind::Expression(None)) | Ok(InstrKind::Statement) => {}
                Ok(InstrKind::Expression(Some(result))) => println!("{}", result),
                Err(e) => println!("{}", e.to_string()),
            };

            line_reader.set_prompt(&Prompt::get(&interpreter))?;
        }

        Ok(())
    }
}
