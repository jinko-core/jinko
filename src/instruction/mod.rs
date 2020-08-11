//! Instructions are used to represent a single expression/statement in broccoli.
//! When using nested instructions, such as `foo = bar();`, you're actually using
//! two instructions: A function call expression, and a variable assignment statement

mod expression;
mod instr_trait;
mod return_kind;
mod statement;

pub use expression::Expression;
pub use instr_trait::InstrTrait;
pub use return_kind::ReturnKind;
pub use statement::Statement;

/// The actual "instruction" contained in the Instruction struct
enum InstrType {
    Stmt(Statement),
    Expr(Expression),
}

/// Instructions contain a `ReturnKind`, indicating if they are a statement or an
/// expression
pub struct Instruction {
    // FIXME: Add source code (&str) and spacial information (struct SpaceInfo ?)
    instruction: InstrType,
}

impl InstrTrait for Instruction {
    fn execute(&self) -> Result<ReturnKind, String> {
        match self.instruction {
            InstrType::Stmt(s) => s.execute(),
            InstrType::Expr(e) => e.execute(),
        }
    }
}
