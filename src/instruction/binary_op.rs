//! Binary operations apply an operation on two Instructions. When writing
//! 1 + 2, a BinaryOp will be created containing "1" as a left hand side operand, "2" as
//! a right hand side operand and "+" as the operator.
//!
//! The available operators are `+`, `-`, `*` and `/`.
//! That is `Add`, `Substract`, `Multiply` and `Divide`.

// FIXME: Separate Operator from BinOp

use crate::value::{JinkFloat, JinkInt, Value};
use crate::{
    error::ErrKind, error::JinkoError, instruction::InstrKind, interpreter::Interpreter,
    FromInstance, Instance, Instruction,
};

/// All the binary operators available
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    LeftParenthesis,
    RightParenthesis,
}

impl Operator {
    /// Create a new operator from a given character
    pub fn new(op_str: &str) -> Operator {
        match op_str {
            "+" => Operator::Add,
            "-" => Operator::Sub,
            "*" => Operator::Mul,
            "/" => Operator::Div,
            "(" => Operator::LeftParenthesis,
            ")" => Operator::RightParenthesis,
            _ => unreachable!("Invalid operator: {}", op_str),
        }
    }

    /// Return the operator's representation
    pub fn to_str(&self) -> &str {
        match self {
            Operator::Add => "+",
            Operator::Sub => "-",
            Operator::Mul => "*",
            Operator::Div => "/",
            Operator::LeftParenthesis => "(",
            Operator::RightParenthesis => ")",
        }
    }

    /// Return the operator's precedence according to the Shunting Yard algorithm
    pub fn precedence(&self) -> u8 {
        match self {
            // Classic SY operator precedence
            Operator::Mul | Operator::Div => 3,
            Operator::Add | Operator::Sub => 2,

            // Special operators. They don't really have a precedence value, and it's
            // never used
            Operator::LeftParenthesis | Operator::RightParenthesis => 0,
        }
    }

    /// Is the operator a left associative one
    pub fn is_left_associative(&self) -> bool {
        // FIXME: Not entirely true
        // - Changes once we add more operators such as the Power one
        match self {
            _ => true,
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

    value: Option<Box<dyn Instruction>>,
}

impl BinaryOp {
    /// Create a new `BinaryOp` from two instructions and an operator
    pub fn new(lhs: Box<dyn Instruction>, rhs: Box<dyn Instruction>, op: Operator) -> Self {
        BinaryOp {
            lhs,
            rhs,
            op,
            value: None,
        }
    }

    /// Return the operator used by the BinaryOp
    #[cfg(test)]
    pub fn operator(&self) -> Operator {
        self.op
    }

    // Get a reference on the left side member of a BinaryOp
    #[cfg(test)]
    pub fn lhs(&self) -> &Box<dyn Instruction> {
        &self.lhs
    }

    /// Get a reference on the right side member of a BinaryOp
    #[cfg(test)]
    pub fn rhs(&self) -> &Box<dyn Instruction> {
        &self.rhs
    }

    // FIXME: Use Interpreter::execute_expression
    /// Execute a node of the binary operation
    fn execute_node(
        &self,
        node: &Box<dyn Instruction>,
        interpreter: &mut Interpreter,
    ) -> Result<Instance, JinkoError> {
        match node.execute(interpreter)? {
            InstrKind::Statement | InstrKind::Expression(None) => Err(JinkoError::new(
                ErrKind::Interpreter,
                format!(
                    "Invalid use of statement in binary operation: {}",
                    self.lhs.print()
                ),
                None,
                self.print(),
            )),
            InstrKind::Expression(Some(v)) => Ok(v),
        }
    }
}

impl Instruction for BinaryOp {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression(None)
    }

    fn print(&self) -> String {
        format!(
            "{} {} {}",
            self.lhs.print(),
            self.op.to_str(),
            self.rhs.print()
        )
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JinkoError> {
        interpreter.debug_step("BINOP ENTER");

        interpreter.debug("OP", self.op.to_str());

        let l_value = self.execute_node(&self.lhs, interpreter)?;
        let r_value = self.execute_node(&self.rhs, interpreter)?;

        if l_value.ty() != r_value.ty() {
            return Err(JinkoError::new(
                ErrKind::Interpreter, // FIXME: Should be a type error
                format!(
                    "Trying to do binary operation on invalid types: {:#?} {} {:#?}",
                    l_value.ty(),
                    self.op.to_str(),
                    r_value.ty() // FIXME: Display correctly
                ),
                None, // FIXME: Fix Location
                self.print(),
            ));
        }

        let return_value;

        // FIXME: DISGUSTING and do not unwap
        match l_value.ty().unwrap().as_str() {
            // FIXME: Absolutely DISGUSTING
            "int" => {
                return_value = InstrKind::Expression(Some(
                    JinkInt::from_instance(&l_value)
                        .do_op(&JinkInt::from_instance(&r_value), self.op)?,
                ));
            }

            "float" => {
                return_value = InstrKind::Expression(Some(
                    JinkFloat::from_instance(&l_value)
                        .do_op(&JinkFloat::from_instance(&r_value), self.op)?,
                ));
            }
            _ => todo!("Implement empty types?"),
        }

        interpreter.debug_step("BINOP EXIT");

        Ok(return_value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::JinkInt;
    use crate::ToInstance;
    use crate::{InstrKind, Interpreter};

    fn binop_assert(l_num: i64, r_num: i64, op_string: &str, res: i64) {
        let l = Box::new(JinkInt::from(l_num));
        let r = Box::new(JinkInt::from(r_num));
        let op = Operator::new(op_string);

        let binop = BinaryOp::new(l, r, op);

        let mut i = Interpreter::new();

        assert_eq!(
            binop.execute(&mut i).unwrap(),
            InstrKind::Expression(Some(JinkInt::from(res).to_instance()))
        );
    }

    #[test]
    fn t_binop_add_same() {
        binop_assert(12, 12, "+", 24);
    }

    #[test]
    fn t_binop_add_l_diff() {
        binop_assert(12, 2, "+", 14);
    }

    #[test]
    fn t_binop_add_r_diff() {
        binop_assert(2, 99, "+", 101);
    }

    #[test]
    fn t_binop_mul_same() {
        binop_assert(12, 12, "*", 144);
    }

    #[test]
    fn t_binop_mul_l_diff() {
        binop_assert(12, 2, "*", 24);
    }

    #[test]
    fn t_binop_mul_r_diff() {
        binop_assert(2, 99, "*", 198);
    }

    #[test]
    fn t_binop_rhs_execute() {
        let r_bin = BinaryOp::new(
            Box::new(JinkInt::from(12)),
            Box::new(JinkInt::from(3)),
            Operator::new("*"),
        );
        let binary_op = BinaryOp::new(
            Box::new(JinkInt::from(9)),
            Box::new(r_bin),
            Operator::new("-"),
        );

        let mut i = Interpreter::new();

        assert_eq!(
            binary_op.rhs().execute(&mut i).unwrap(),
            InstrKind::Expression(Some(JinkInt::from(36).to_instance()))
        );
    }
}
