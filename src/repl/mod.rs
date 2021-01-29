//! The REPL module implements an interactive mode for the jinko interpreter. You can
//! use it as is, or run a file and then enter the interactive mode.

mod prompt;
use prompt::Prompt;

use linefeed::{Interface, ReadResult};

use crate::args::Args;
use crate::{
    parser::Construct, FromObjectInstance, ObjectInstance, InstrKind, Instruction, Interpreter, JkConstant,
    JkError,
};

/// Empty struct for the Repl methods
pub struct Repl;

// FIXME:
// - Is Display really how we want to go about it?
// - Cleanup the code
impl std::fmt::Display for ObjectInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self.ty() {
                Some(ty) => match ty.as_ref() {
                    "int" => JkConstant::<i64>::from_instance(self).print(),
                    "float" => JkConstant::<f64>::from_instance(self).print(),
                    "char" => JkConstant::<char>::from_instance(self).print(),
                    "string" => JkConstant::<String>::from_instance(self).print(),
                    "bool" => JkConstant::<bool>::from_instance(self).print(),
                    _ => format!("{:?}", self),
                },
                None => format!(""),
            }
        )
    }
}

impl Repl {
    /// Parse a new input, adding it to an existing interpreter
    fn parse_instruction(input: &str) -> Result<Option<Box<dyn Instruction>>, JkError> {
        match input.is_empty() {
            true => Ok(None),
            false => match Construct::instruction(input) {
                Ok((_, value)) => Ok(Some(value)),
                Err(e) => Err(JkError::from(e)),
            },
        }
    }

    /// Launch the REPL
    pub fn launch_repl(args: &Args) -> Result<(), JkError> {
        let line_reader = Interface::new("jinko")?;

        let mut interpreter = Interpreter::new();
        interpreter.set_debug(args.debug());

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
                Ok(InstrKind::Expression(None)) | Ok(InstrKind::Statement) => {}
                Ok(InstrKind::Expression(Some(result))) => println!("{}", result),
                Err(e) => println!("{}", e.to_string()),
            };

            line_reader.set_prompt(&Prompt::get(&interpreter))?;
        }

        Ok(())
    }
}
