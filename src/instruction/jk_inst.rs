//! `JkInst`s are special directives given to the interpreter. There is only a limited
//! amount of them, and they are mostly useful for debugging or testing. They aren't
//! really an `Instruction`, and therefore their implementation lives in the parser
//! module. They are executed at "compile" time, when running through the code first.

use crate::instruction::{InstrKind, Instruction};
use crate::{Interpreter, JkErrKind, JkError, Rename};

/// The potential interpreter instructions
#[derive(Clone, Debug, PartialEq)]
pub enum JkInst {
    Dump,
    Quit,
}

impl JkInst {
    /// Construct a `JkInst` from a given keyword
    pub fn from_str(keyword: &str) -> Result<Self, JkError> {
        match keyword {
            "dump" => Ok(JkInst::Dump),
            "quit" => Ok(JkInst::Quit),
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

impl Instruction for JkInst {
    fn kind(&self) -> InstrKind {
        InstrKind::Statement
    }

    fn print(&self) -> String {
        match self {
            JkInst::Dump => "@dump",
            JkInst::Quit => "@quit",
        }
        .to_string()
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JkError> {
        interpreter.debug("JINKO_INST", &self.print());

        match self {
            JkInst::Dump => println!("{}", interpreter.print()),
            JkInst::Quit => std::process::exit(0),
        };

        // JinkInsts cannot return anything. They simply act directly from the interpreter,
        // on the interpreter.
        Ok(InstrKind::Statement)
    }
}

impl Rename for JkInst {
    fn prefix(&mut self, _: &str) {}
}
