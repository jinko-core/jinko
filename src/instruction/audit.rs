//! Audit blocks are more permissive than normal blocks. They allow ignoring a return
//! value, for example.

use crate::error::JinkoError;
use crate::interpreter::Interpreter;

use super::{Block, InstrKind, Instruction};

#[derive(Clone)]
pub struct Audit {
    block: Block,
}

impl Audit {
    /// Create a new assign block
    pub fn new(block: Block) -> Audit {
        Audit { block }
    }
}

impl Instruction for Audit {
    fn kind(&self) -> InstrKind {
        self.block.kind()
    }

    fn print(&self) -> String {
        format!("audit {}", self.block.print())
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JinkoError> {
        interpreter.audit_enter();
        interpreter.debug_step("AUDIT ENTER");

        let r = self.block.execute(interpreter);

        interpreter.audit_exit();
        interpreter.debug_step("AUDIT EXIT");

        r
    }
}
