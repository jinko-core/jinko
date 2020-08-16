//! Instructions are used to represent a single expression/statement in broccoli.
//! When using nested instructions, such as `foo = bar();`, you're actually using
//! two instructions: A function call expression, and a variable assignment statement

mod function_declaration;
mod function_call;
mod var_assignment;

pub use function_declaration::{FunctionDecArg, FunctionDec};
pub use function_call::FunctionCall;
pub use var_assignment::VarAssign;

/// The type of instructions available
pub enum InstrKind {
    Statement,
    Expression,
}

pub trait Instruction {
    /// Execute the instruction, altering the state of the program
    fn execute(&self) {}

    /// What is the type of the instruction: a Statement or an Expression
    fn kind(&self) -> InstrKind;
}
