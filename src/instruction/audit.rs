//! Audit blocks are more permissive than normal blocks. They allow ignoring a return
//! value, for example.

use crate::{instruction::Block, InstrKind, Instruction, Interpreter, JkError, Rename};

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

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JkError> {
        interpreter.audit_enter();
        interpreter.debug_step("AUDIT ENTER");

        let r = self.block.execute(interpreter);

        interpreter.audit_exit();
        interpreter.debug_step("AUDIT EXIT");

        r
    }
}

impl Rename for Audit {
    fn prefix(&mut self, prefix: &str) {
        self.block.prefix(prefix)
    }
}
