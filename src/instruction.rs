//! Instructions are used to represent a single expression/statement in broccoli.
//! When using nested instructions, such as `foo = bar();`, you're actually using
//! two instructions: A function call expression, and a variable assignment statement

// FIXME: Separate in its own module
use crate::return_kind::ReturnKind;

#[derive(Clone, Copy)]
struct Stmt {
}

#[derive(Clone, Copy)]
struct Expr {
}

/// The actual "instruction" contained in the Instruction struct
union InstrType {
    stmt: Stmt,
    expr: Expr,
}

/// Instructions contain a `ReturnKind`, indicating if they are a statement or an
/// expression
pub struct Instruction {
    // FIXME: Add source code (&str) and spacial information (struct SpaceInfo ?)

    instruction: InstrType,
}

// FIXME: Add error type instead of String

trait InstrTrait {
    /// Execute the instruction, returning `Something` or `Nothing` inside a Result<>
    fn execute() -> Result<ReturnKind, String>;
}
