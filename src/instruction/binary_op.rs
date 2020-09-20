//! Binary operations apply an operation on two Instructions. When writing
//! 1 + 2, a BinaryOp will be created containing "1" as a left hand side operand, "2" as
//! a right hand side operand and "+" as the operator.
//!
//! The available operators are `+`, `-`, `*`, `/`, `%`, `&&`, `||`, `and`, `or`, `<`, `>`, `==`, `<=` and `>=`, that is
//! `Add`, `Substract`, `Multiply`, `Divide`, `Modulo`, `And`, `Or`, `And`, `Or`, `Less than`, `Greater than`,
//! `Equal to`, `Less than or equal to `and `Greater than or equal to`
//!
//! Some of these operators are comparison operators while others are arithmetic operators.
//! Comparison operators can evaluate to booleans while arithmetic ones cannot.

use crate::Instruction;

enum Operator {
}

/// The `BinaryOp` struct contains two expressions and an operator, which can be an arithmetic
/// or a comparison one
pub struct BinaryOp {
    lhs: Box<dyn Instruction>,
    rhs: Box<dyn Instruction>,
    operator: Operator,
}
