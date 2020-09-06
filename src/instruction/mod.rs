//! Instructions are used to represent a single expression/statement in broccoli.
//! When using nested instructions, such as `foo = bar();`, you're actually using
//! two instructions: A function call expression, and a variable assignment statement

mod audit;
mod block;
mod function_call;
mod function_declaration;
mod if_else;
mod loop_block;
mod var;
mod var_assignment;

pub use audit::Audit;
pub use block::Block;
pub use function_call::FunctionCall;
pub use function_declaration::{FunctionDec, FunctionDecArg, FunctionKind};
pub use if_else::IfElse;
pub use loop_block::{Loop, LoopKind};
pub use var::Var;
pub use var_assignment::VarAssign;

use crate::interpreter::Interpreter;

/// The type of instructions available
#[derive(Debug, PartialEq)]
pub enum InstrKind {
    Statement,
    Expression,
}

pub trait Instruction {
    /// Execute the instruction, altering the state of the program
    fn execute(&self, _: &mut Interpreter) {
        unreachable!("The execution of this instruction is not implemented yet. This is a bug")
    }

    /// Maybe execute the instruction, transforming it in a Rust bool if possible. It's
    /// only possible to execute as_bool on boolean variables, boolean constants. blocks
    /// returning a boolean and functions returning a boolean.
    fn as_bool(&self) -> bool {
        unreachable!(format!("{}\n -> Cannot get boolean from expression", self.print()))
    }

    /// What is the type of the instruction: a Statement or an Expression
    fn kind(&self) -> InstrKind;

    /// Pretty-print the instruction to valid broccoli code
    fn print(&self) -> String;
}
