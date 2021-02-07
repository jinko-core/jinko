//! Binary operations apply an operation on two Instructions. When writing
//! 1 + 2, a BinaryOp will be created containing "1" as a left hand side operand, "2" as
//! a right hand side operand and "+" as the operator.
//!
//! The available operators are `+`, `-`, `*` and `/`.
//! That is `Add`, `Substract`, `Multiply` and `Divide`.

use crate::{
    Rename,
    instruction::Operator, FromObjectInstance, InstrKind, Instruction, Interpreter, JkErrKind,
    JkError, JkFloat, JkInt, ObjectInstance, Value,
};

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
    ) -> Result<ObjectInstance, JkError> {
        match node.execute(interpreter)? {
            InstrKind::Statement | InstrKind::Expression(None) => Err(JkError::new(
                JkErrKind::Interpreter,
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

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JkError> {
        interpreter.debug_step("BINOP ENTER");

        interpreter.debug("OP", self.op.to_str());

        let l_value = self.execute_node(&self.lhs, interpreter)?;
        let r_value = self.execute_node(&self.rhs, interpreter)?;

        if l_value.ty() != r_value.ty() {
            return Err(JkError::new(
                JkErrKind::Interpreter, // FIXME: Should be a type error
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
                    JkInt::from_instance(&l_value)
                        .do_op(&JkInt::from_instance(&r_value), self.op)?,
                ));
            }

            "float" => {
                return_value = InstrKind::Expression(Some(
                    JkFloat::from_instance(&l_value)
                        .do_op(&JkFloat::from_instance(&r_value), self.op)?,
                ));
            }
            _ => todo!("Implement empty types?"),
        }

        interpreter.debug_step("BINOP EXIT");

        Ok(return_value)
    }
}

impl Rename for BinaryOp {
    fn prefix(&mut self, prefix: &str) {
        self.lhs.prefix(prefix);
        self.rhs.prefix(prefix);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::JkInt;
    use crate::ToObjectInstance;
    use crate::{InstrKind, Interpreter};

    fn binop_assert(l_num: i64, r_num: i64, op_string: &str, res: i64) {
        let l = Box::new(JkInt::from(l_num));
        let r = Box::new(JkInt::from(r_num));
        let op = Operator::new(op_string);

        let binop = BinaryOp::new(l, r, op);

        let mut i = Interpreter::new();

        assert_eq!(
            binop.execute(&mut i).unwrap(),
            InstrKind::Expression(Some(JkInt::from(res).to_instance()))
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
            Box::new(JkInt::from(12)),
            Box::new(JkInt::from(3)),
            Operator::new("*"),
        );
        let binary_op = BinaryOp::new(
            Box::new(JkInt::from(9)),
            Box::new(r_bin),
            Operator::new("-"),
        );

        let mut i = Interpreter::new();

        assert_eq!(
            binary_op.rhs().execute(&mut i).unwrap(),
            InstrKind::Expression(Some(JkInt::from(36).to_instance()))
        );
    }

    #[test]
    fn t_binop_lhs_execute() {
        let l_bin = BinaryOp::new(
            Box::new(JkInt::from(12)),
            Box::new(JkInt::from(3)),
            Operator::new("*"),
        );
        let binary_op = BinaryOp::new(
            Box::new(l_bin),
            Box::new(JkInt::from(9)),
            Operator::new("-"),
        );

        let mut i = Interpreter::new();

        assert_eq!(binary_op.operator(), Operator::Sub);

        assert_eq!(
            binary_op.lhs().execute(&mut i).unwrap(),
            InstrKind::Expression(Some(JkInt::from(36).to_instance()))
        );
    }
}
