//! Binary operations apply an operation on two Instructions. When writing
//! 1 + 2, a BinaryOp will be created containing "1" as a left hand side operand, "2" as
//! a right hand side operand and "+" as the operator.
//!
//! The available operators are `+`, `-`, `*` and `/`.
//! That is `Add`, `Substract`, `Multiply` and `Divide`.

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
            // Special operators. They don't really have a precendence value
            Operator::LeftParenthesis => 0,
            Operator::RightParenthesis => 255,

            // Classic SY operator precedence
            Operator::Mul | Operator::Div => 3,
            Operator::Add | Operator::Sub => 2,
        }
    }

    /// Is the operator a left associative one
    pub fn is_left_associative(&self) -> bool {
        self.precedence() == 0
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

    /// Compute the result of the binary operation
    pub fn compute(&self) -> Result<Box<dyn Instruction>, JinkoError> {
        // FIXME: Typecheck self before computing

        match &self.op {
            _ => Err(JinkoError::new(
                ErrKind::Interpreter,
                format!("Unknown operator: {:?}", self.op),
                None,
                self.print(),
            )),
        }
    }

    /// Return the operator used by the BinaryOp
    pub fn operator(&self) -> Operator {
        self.op
    }

    /// Get a reference on the left side member of a BinaryOp
    pub fn lhs(&self) -> &Box<dyn Instruction> {
        &self.lhs
    }

    /// Get a reference on the right side member of a BinaryOp
    pub fn rhs(&self) -> &Box<dyn Instruction> {
        &self.rhs
    }

    /// Set the operator of a BinaryOp
    pub fn set_op(&mut self, op: Operator) {
        self.op = op;
    }

    /// Set the left side member of a BinaryOp
    pub fn set_lhs(&mut self, lhs: Box<dyn Instruction>) {
        self.lhs = lhs;
    }

    /// Set the right side member of a BinaryOp
    pub fn set_rhs(&mut self, rhs: Box<dyn Instruction>) {
        self.rhs = rhs;
    }

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

        // FIXME: Add logic
        Ok(return_value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::JinkInt;

    #[test]
    #[ignore]
    // FIXME: Don't ignore as soon as we can get a value from lhs and rhs
    fn t_binop_add() {
        let l = Box::new(JinkInt::from(12));
        let r = Box::new(JinkInt::from(12));
        let op = Operator::new("+");

        let binop = BinaryOp::new(l, r, op);

        let res = binop.compute();

        match res {
            Err(e) => assert!(false, format!("12 + 12 is a valid operation: {}", e)),
            Ok(v) => assert!(true),
        }
    }
}
