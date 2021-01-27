//! `JinkoInst`s are special directives given to the interpreter. There is only a limited
//! amount of them, and they are mostly useful for debugging or testing. They aren't
//! really an `Instruction`, and therefore their implementation lives in the parser
//! module. They are executed at "compile" time, when running through the code first.

use crate::error::{JkErrKind, JkError};
use crate::instruction::{InstrKind, Instruction};
use crate::interpreter::Interpreter;

/// The potential interpreter instructions
#[derive(Clone, Debug, PartialEq)]
pub enum JinkoInst {
    Dump,
    Quit,
}

impl JinkoInst {
    /// Construct a `JinkoInst` from a given keyword
    pub fn from_str(keyword: &str) -> Result<Self, JkError> {
        match keyword {
            "dump" => Ok(JinkoInst::Dump),
            "quit" => Ok(JinkoInst::Quit),
            // FIXME: Fix location
            _ => Err(JkError::new(
                JkErrKind::Parsing,
                format!("unknown interpreter directive @{}", keyword),
                None,
                keyword.to_owned(),
            )),
        }
    }
}

impl Instruction for JinkoInst {
    fn kind(&self) -> InstrKind {
        InstrKind::Statement
    }

    fn print(&self) -> String {
        match self {
            JinkoInst::Dump => "@dump",
            JinkoInst::Quit => "@quit",
        }
        .to_string()
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JkError> {
        interpreter.debug("JINKO_INST", &self.print());

        match self {
            JinkoInst::Dump => println!("{}", interpreter.print()),
            JinkoInst::Quit => std::process::exit(0),
        };

        // JinkInsts cannot return anything. They simply act directly from the interpreter,
        // on the interpreter.
        Ok(InstrKind::Statement)
    }
}
