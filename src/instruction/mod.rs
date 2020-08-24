//! Instructions are used to represent a single expression/statement in broccoli.
//! When using nested instructions, such as `foo = bar();`, you're actually using
//! two instructions: A function call expression, and a variable assignment statement

mod audit;
mod block;
mod function_call;
mod function_declaration;
mod if_else;
mod var;
mod var_assignment;

pub use audit::Audit;
pub use block::Block;
pub use function_call::FunctionCall;
pub use function_declaration::{FunctionDec, FunctionDecArg, FunctionKind};
pub use if_else::IfElse;
pub use var::Var;
pub use var_assignment::VarAssign;

/// The type of instructions available
#[derive(Debug, PartialEq)]
pub enum InstrKind {
    Statement,
    Expression,
}

pub trait Instruction {
    /// Execute the instruction, altering the state of the program
    fn execute(&self) {
        unreachable!("The execution of this instruction is not implemented yet")
    }

    /// What is the type of the instruction: a Statement or an Expression
    fn kind(&self) -> InstrKind;

    /// Pretty-print the instruction to valid broccoli code
    fn print(&self) -> String;
}
