//! Instructions are used to represent a single expression/statement in broccoli.
//! When using nested instructions, such as `foo = bar();`, you're actually using
//! two instructions: A function call expression, and a variable assignment statement

mod expression;
mod return_kind;
mod statement;
mod var_assignment;

pub use expression::Expression;
pub use return_kind::ReturnKind;
pub use statement::Statement;
pub use var_assignment::VarAssign;

/// The actual "instruction" contained in the Instruction struct
enum InstrType {
    Stmt(Statement),
    Expr(Expression),
}

pub trait Instruction {
    /// Execute the instruction, returning `Something` or `Nothing` inside a Result<>
    fn execute(&self) -> Result<ReturnKind, String>;

    /// What is the type of the instruction: a Statement or an Expression
    fn kind(&self) -> InstrType;
}
