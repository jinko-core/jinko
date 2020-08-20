//! `IfElse`s are used to represent an if/else statement in the source code. They have
//! a condition, a body and an optional else body.

use super::instruction::{Instruction, InstrKind, Block};

struct IfElse {
    condition: Box<dyn Instruction>,
    if_body: Block,
    else_body: Option<Block>,
}

impl Instruction for IfElse {
    fn kind(&self) -> InstrKind {
        /// We don't check the kind of the else_body, since the typechecker will have
        /// approved that the if_body and else_body return the same thing
        self.block.kind()
    }
}
