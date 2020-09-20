//! Binary operations apply an operation on two Instructions. When writing
//! 1 + 2, a BinaryOp will be created containing "1" as a left hand side operand, "2" as
//! a right hand side operand and "+" as the operator.
//!
//! The available operators are `+`, `-`, `*`, `/`, `%`, `&`, `|`, `&&`, `||`, `and`, `or`, `<`,
//! `>`, `==`, `<=`, `>=`, `<<` and `>>`, that is
//! `Add`, `Substract`, `Multiply`, `Divide`, `Modulo`, `BinaryAnd`, `BinaryOr`, `And`,
//! `Or`, `And`, `Or`, `Less than`, `Greater than`, `Equal to`, `Less than or equal to`,
//! `Greater than or equal to`, `Left shift` and `Right shift`.
//!
//! Some of these operators are comparison operators while others are arithmetic operators.
//! Comparison operators can evaluate to booleans while arithmetic ones cannot.

use crate::{error::ErrKind, error::JinkoError, Instruction, instruction::InstrKind};

/// All the binary operators available
#[repr(u8)]
#[derive(Debug, Clone)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BinAnd,
    BinOr,
    And,
    Or,
    Equal,
    LessThan,
    LessOrEqualThan,
    GreaterThan,
    GreaterOrEqualThan,
    LeftShift,
    RightShift,
}

impl Operator {
    /// Create a new operator from a given character
    pub fn new(c: &str) -> Result<Operator, JinkoError> {
        match c {
            "+" => Ok(Operator::Add),
            _ => Err(JinkoError::new(
                ErrKind::Parsing,
                format!("Invalid operator: {}", c),
                None,
                format!("{}", c),
            )),
        }
    }

    /// Return the operator's representation
    pub fn to_str(&self) -> &str {
        match self {
            Operator::Add => "+",
            _ => "???",
        }
    }
}

/// The `BinaryOp` struct contains two expressions and an operator, which can be an arithmetic
/// or a comparison one
#[derive(Clone)]
pub struct BinaryOp {
    lhs: Box<dyn Instruction>,
    rhs: Box<dyn Instruction>,
    op: Operator,
}

impl BinaryOp {
    /// Create a new `BinaryOp` from two instructions and an operator
    pub fn new(lhs: Box<dyn Instruction>, rhs: Box<dyn Instruction>, op: Operator) -> Self {
        BinaryOp { lhs, rhs, op }
    }

    /// Compute the result of the binary operation
    pub fn compute<T>(&self) -> Result<T, JinkoError> {
        match &self.op {
            _ => Err(JinkoError::new(
                    ErrKind::Interpreter,
                    format!("Unknown operator: {:?}", self.op),
                    None,
                    self.print(),
                    )),
        }
    }
}

impl Instruction for BinaryOp {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression
    }

    fn print(&self) -> String {
        format!("{} {} {}", self.lhs.print(), self.op.to_str(), self.rhs.print())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::JinkInt;

    #[test]
    fn t_binop_add() {
        let l = Box::new(JinkInt::from(12));
        let r = Box::new(JinkInt::from(12));
        let op = Operator::new("+").unwrap();

        let binop = BinaryOp::new(l, r, op);

        assert_eq!(binop.compute().unwrap(), 24);
    }
}
