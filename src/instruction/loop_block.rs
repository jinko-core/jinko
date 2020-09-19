//! The Loop instruction is used for repeating instructions. They can be of three
//! different kinds, `for`, `while` or `loop`.

use crate::{error::JinkoError, interpreter::Interpreter};

use super::{Block, InstrKind, Instruction, Var};

/// What kind of loop the loop block represents: Either a for Loop, with a variable and
/// a range expression, a while loop with just an upper bound, or a loop with no bound
/// at all
#[derive(Clone)]
pub enum LoopKind {
    For(Var, Box<dyn Instruction>),
    While(Box<dyn Instruction>),
    Loop,
}

/// The Loop block struct. Contains the block to execute, as well as the kind of loop
/// it represents.
#[derive(Clone)]
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
                "for {} in {} {}\n",
                var.name(),
                range.print(),
                self.block.print()
            ),
            LoopKind::While(condition) => {
                format!("while {} {}\n", condition.print(), self.block.print())
            }
            LoopKind::Loop => format!("loop {}\n", self.block.print()),
        }
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<(), JinkoError> {
        match &self.kind {
            LoopKind::Loop => loop {
                self.block.execute(interpreter)?;
            },
            LoopKind::While(cond) => {
                while cond.as_bool() {
                    self.block.execute(interpreter)?;
                }
            }
            LoopKind::For(var, range) => {
                let var_name = var.name().to_owned();
                interpreter.scope_enter();

                interpreter.add_variable(var.clone())?;

                loop {
                    range.execute(interpreter)?;

                    // We can unwrap since we added the variable right before
                    if !interpreter.get_variable(&var_name).unwrap().as_bool() {
                        break;
                    }

                    self.block.execute(interpreter)?;
                }

                interpreter.scope_exit();
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::FunctionCall;

    #[test]
    fn pretty_print_loop() {
        let b = Block::new();
        let l = Loop::new(LoopKind::Loop, b);

        assert_eq!(l.print().as_str(), "loop {\n}\n")
    }

    #[test]
    fn pretty_print_for() {
        let r = Box::new(FunctionCall::new("iter".to_owned()));
        let b = Block::new();
        let l = Loop::new(LoopKind::For(Var::new("i".to_owned()), r), b);

        assert_eq!(l.print().as_str(), "for i in iter() {\n}\n")
    }

    #[test]
    fn pretty_print_while() {
        let r = Box::new(Block::new());
        let b = Block::new();
        let l = Loop::new(LoopKind::While(r), b);

        assert_eq!(l.print().as_str(), "while {\n} {\n}\n")
    }
}
