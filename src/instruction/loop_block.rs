//! The Loop instruction is used for repeating instructions. They can be of three
//! different kinds, `for`, `while` or `loop`.

use crate::{ErrKind, Interpreter, JinkoError};

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

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JinkoError> {
        match &self.kind {
            LoopKind::Loop => loop {
                interpreter.debug_step("LOOP ENTER");
                self.block.execute(interpreter)?;
                interpreter.debug_step("LOOP EXIT");
            },
            LoopKind::While(cond) => {
                interpreter.debug_step("WHILE ENTER");
                while cond.as_bool() {
                    self.block.execute(interpreter)?;
                }
                interpreter.debug_step("WHILE EXIT");
            }
            LoopKind::For(_var, _range) => {
                // FIXME:
                //
                // In order to have a correct for implementation, jinko needs ranges,
                // iterators, which implies the need for options, which implies the need
                // for a standard library, which implies the need for code inclusion, etc
                // etc etc... All of that is the goal of the 0.1.1 release.
                //
                // A possible implementation would be to execute the range, assign it
                // to the var, and check that var is a valid iterator. For example,
                // by calling `Iter::ok(var)` (whatever the API might be) and checking
                // that that result, as a boolean, returns true. If it does, execute the
                // body. If it does not, break from the for.

                return Err(JinkoError::new(
                    ErrKind::Interpreter,
                    format!("for loops are currently unimplemented"),
                    None,
                    self.print(),
                ));

                // FIXME: Rework that code
                // interpreter.debug_step("FOR ENTER");
                // let var_name = var.name().to_owned();
                // interpreter.scope_enter();

                // interpreter.add_variable(var.clone())?;

                // loop {
                //     range.execute(interpreter)?;

                //     // We can unwrap since we added the variable right before
                //     if !interpreter.get_variable(&var_name).unwrap().as_bool() {
                //         break;
                //     }

                //     self.block.execute(interpreter)?;
                // }

                // interpreter.scope_exit();
                // interpreter.debug_step("FOR EXIT");
            }
        }

        // FIXME: Add logic. Right now they only return on error, not the actual value
        Ok(InstrKind::Statement)
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
