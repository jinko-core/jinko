//! The Loop instruction is used for repeating instructions. They can be of three
//! different kinds, `for`, `while` or `loop`.

use crate::{error::BroccoliError, interpreter::Interpreter};

use super::{Block, InstrKind, Instruction, Var};

/// What kind of loop the loop block represents: Either a for Loop, with a variable and
/// a range expression, a while loop with just an upper bound, or a loop with no bound
/// at all
pub enum LoopKind {
    For(Var, Box<dyn Instruction>),
    While(Box<dyn Instruction>),
    Loop,
}

/// The Loop block struct. Contains the block to execute, as well as the kind of loop
/// it represents.
pub struct Loop {
    kind: LoopKind,
    block: Block,
}

impl Loop {
    pub fn new(kind: LoopKind, block: Block) -> Loop {
        Loop { kind, block }
    }
}

impl Instruction for Loop {
    fn kind(&self) -> InstrKind {
        self.block.kind()
    }

    fn print(&self) -> String {
        match &self.kind {
            LoopKind::For(var, range) => format!(
                "for {} in {} {}",
                var.name(),
                range.print(),
                self.block.print()
            ),
            LoopKind::While(condition) => {
                format!("while {} {}", condition.print(), self.block.print())
            }
            LoopKind::Loop => format!("loop {}", self.block.print()),
        }
    }

    fn execute(&mut self, interpreter: &mut Interpreter) -> Result<(), BroccoliError> {
        match &mut self.kind {
            LoopKind::Loop => loop {
                self.block.execute(interpreter)
            },
            LoopKind::While(cond) => {
                while cond.as_bool() {
                    self.block.execute(interpreter)
                }
            }
            LoopKind::For(var, range) => {
                let var_name = var.name().to_owned();
                interpreter.scope_enter();

                // FIXME: No unwrap if ugly?
                interpreter.add_variable(std::mem::take(var)).unwrap();

                loop {
                    // Set the value of `var` to the last return value, and execute
                    // the block
                    range.execute(interpreter);

                    // We can unwrap since we added the variable just before
                    if interpreter.get_variable(&var_name).unwrap().as_bool() {
                        break;
                    }

                    self.block.execute(interpreter)
                }

                interpreter.scope_exit();
            }
        }
    }
}
