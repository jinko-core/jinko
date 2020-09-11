//! `JinkoInst`s are special directives given to the interpreter. There is only a limited
//! amount of them, and they are mostly useful for debugging or testing. They aren't
//! really an `Instruction`, and therefore their implementation lives in the parser
//! module. They are executed at "compile" time, when running through the code first.

use crate::error::JinkoError;
use crate::interpreter::Interpreter;
use crate::instruction::{InstrKind, Instruction};

/// The potential interpreter instructions
#[derive(Debug)]
pub enum JinkoInst {
    Dump,
}

impl JinkoInst {
    /// Traps the interpretation when encountering an unknown interpreter instruction.
    /// This function is just a shorthand wrapper around `unreachable!`
    fn unreachable(&self) -> ! {
        unreachable!("Unknown JinkoInst {:#?}. This is a bug", self)
    }
}

impl Instruction for JinkoInst {
    fn kind(&self) -> InstrKind {
        InstrKind::Statement
    }

    fn print(&self) -> String {
        match self {
            JinkoInst::Dump => "@dump",
            _ => self.unreachable(),
        }.to_string()
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<(), JinkoError> {
        match self {
            JinkoInst::Dump => println!("{}", interpreter.print()),
            _ => self.unreachable()
        };

        Ok(())
    }
}
