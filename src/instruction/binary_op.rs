//! Binary operations apply an operation on two Instructions. When writing
//! 1 + 2, a BinaryOp will be created containing "1" as a left hand side operand, "2" as
//! a right hand side operand and "+" as the operator.
//!
//! The available operators are `+`, `-`, `*` and `/`.
//! That is `Add`, `Substract`, `Multiply` and `Divide`.

use crate::context::Context;
use crate::error::{ErrKind, Error};
use crate::generics::Generic;
use crate::instance::{FromObjectInstance, ObjectInstance};
use crate::instruction::{InstrKind, Instruction, Operator};
use crate::location::SpanTuple;
use crate::log;
use crate::typechecker::TypeCheck;
use crate::typechecker::{CheckedType, TypeCtx, TypeId};
use crate::value::{JkFloat, JkInt, Value};

/// The `BinaryOp` struct contains two expressions and an operator, which can be an arithmetic
/// or a comparison one
#[derive(Clone)]
pub struct BinaryOp {
    lhs: Box<dyn Instruction>,
    rhs: Box<dyn Instruction>,
    op: Operator,
    cached_type: Option<CheckedType>,
    location: Option<SpanTuple>,
}

impl BinaryOp {
    /// Create a new `BinaryOp` from two instructions and an operator
    pub fn new(lhs: Box<dyn Instruction>, rhs: Box<dyn Instruction>, op: Operator) -> Self {
        BinaryOp {
            lhs,
            rhs,
            op,
            cached_type: None,
            location: None,
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

    // FIXME: Use Context::execute_expression
    /// Execute a node of the binary operation
    fn execute_node(&self, node: &dyn Instruction, ctx: &mut Context) -> Option<ObjectInstance> {
        match node.execute(ctx) {
            None => {
                ctx.error(Error::new(ErrKind::Context).with_msg(format!(
                    "invalid use of statement in binary operation: {}",
                    node.print()
                )));
                None
            }
            Some(v) => Some(v),
        }
    }

    pub fn set_location(&mut self, location: SpanTuple) {
        self.location = Some(location)
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
            self.op.as_str(),
            self.rhs.print()
        )
    }

    fn execute(&self, ctx: &mut Context) -> Option<ObjectInstance> {
        log!("binop enter: op: {}", self.op.as_str());

        let l_value = self.execute_node(&*self.lhs, ctx)?;
        let r_value = self.execute_node(&*self.rhs, ctx)?;

        // FIXME: This produces unhelpful errors for now
        if l_value.ty() != r_value.ty() {
            return None;
        }

        let return_value;

        // At this point, we will already have checked whether or not a binary op
        // is valid type-wise. So we can unwrap at will. If a type is still unknown
        // at this point, this is an interpreter error
        match l_value.ty() {
            CheckedType::Resolved(ty) => match ty.id() {
                "int" => {
                    return_value = JkInt::from_instance(&l_value)
                        .do_op(&JkInt::from_instance(&r_value), self.op)
                        .unwrap();
                }
                "float" => {
                    return_value = JkFloat::from_instance(&l_value)
                        .do_op(&JkFloat::from_instance(&r_value), self.op)
                        .unwrap();
                }
                _ => unreachable!(
                    "attempting binary operation with void type or unknown type AFTER typechecking"
                ),
            },
            _ => unreachable!(
                "attempting binary operation with void type or unknown type AFTER typechecking"
            ),
        }

        log!("BINOP EXIT");

        Some(return_value)
    }

    fn location(&self) -> Option<&SpanTuple> {
        self.location.as_ref()
    }
}

impl TypeCheck for BinaryOp {
    fn resolve_type(&mut self, ctx: &mut TypeCtx) -> CheckedType {
        let l_type = self.lhs.type_of(ctx);
        let r_type = self.rhs.type_of(ctx);

        if l_type != r_type {
            ctx.error(
                Error::new(ErrKind::TypeChecker)
                    .with_msg(format!(
                        "trying to do binary operation on invalid types: {} {} {}",
                        l_type,
                        self.op.as_str(),
                        r_type,
                    ))
                    .with_loc(self.location.clone()),
            );
            return CheckedType::Error;
        }

        match self.op {
            Operator::Lt
            | Operator::Gt
            | Operator::LtEq
            | Operator::GtEq
            | Operator::Equals
            | Operator::NotEquals => CheckedType::Resolved(TypeId::from("bool")),
            _ => l_type,
        }
    }

    fn set_cached_type(&mut self, ty: CheckedType) {
        self.cached_type = Some(ty)
    }

    fn cached_type(&self) -> Option<&CheckedType> {
        self.cached_type.as_ref()
    }
}

impl Generic for BinaryOp {}

// TODO: Add typechecking tests
#[cfg(test)]
mod tests {
    use nom_locate::LocatedSpan;

    use super::*;
    use crate::context::Context;
    use crate::instance::ToObjectInstance;
    use crate::value::JkInt;
    use crate::{jinko, jinko_fail};

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

        let mut i = Context::new();

        assert_eq!(
            binary_op.rhs().execute(&mut i).unwrap(),
            JkInt::from(36).to_instance(),
        );
        assert!(!i.error_handler.has_errors());
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

        let mut i = Context::new();

        assert_eq!(binary_op.operator(), Operator::Sub);

        assert_eq!(
            binary_op.lhs().execute(&mut i).unwrap(),
            JkInt::from(36).to_instance()
        );
        assert!(!i.error_handler.has_errors());
    }

    fn assert_bool(input: &str, value: bool) {
        use crate::value::JkBool;
        let input = LocatedSpan::new_extra(input, None);

        let boxed_output = crate::parser::constructs::expr(input).unwrap().1;
        let output = boxed_output.downcast_ref::<BinaryOp>().unwrap();

        let mut i = Context::new();

        assert_eq!(
            output.execute(&mut i).unwrap(),
            JkBool::from(value).to_instance()
        );
    }

    #[test]
    #[allow(clippy::eq_op)]
    fn comparison_simple_int() {
        assert_bool("1 < 4", 1 < 4);
        assert_bool("4 < 1", 4 < 1);
        assert_bool("1 <= 4", 1 <= 4);
        assert_bool("4 <= 1", 4 <= 1);
        assert_bool("1 == 1", 1 == 1);
        assert_bool("4 != 1", 4 != 1);
    }

    #[test]
    fn comparison_simple_float() {
        assert_bool("1.0 < 4.0", 1.0 < 4.0);
        assert_bool("4.0 < 1.0", 4.0 < 1.0);
        assert_bool("1.0 <= 4.0", 1.0 <= 4.0);
        assert_bool("4.0 <= 1.0", 4.0 <= 1.0);
    }

    #[test]
    #[ignore]
    fn comparison_precedence() {
        assert_bool(
            "1 + 4 * 2 - 1 + 2 * (14 + (2 - 17) * 1) - 12 + 3 / 2 < 45",
            1 + 4 * 2 - 1 + 2 * (14 + (2 - 17) * 1) - 12 + 3 / 2 < 45,
        );
    }

    #[test]
    fn tc_binop_valid() {
        jinko! {
            t0 = 1 + 1;
            t2 = 1.0 + 1.4;
        };
    }

    #[test]
    fn tc_binop_from_func() {
        jinko! {
            func id(x: int) -> int { x }
            t0 = id(1) + id(id(id(id(14))));
        };
    }

    #[test]
    fn tc_binop_mismatched_valid() {
        jinko_fail! {
            t0 = 1 + '4';
            t2 = 1.0 + "hey";
        };
    }

    macro_rules! binop_assert {
        ($expr:expr) => {{
            let mut ctx = Context::new();
            let expr = crate::parser::constructs::expr(nom_locate::LocatedSpan::new_extra(
                stringify!($expr),
                None,
            ))
            .unwrap()
            .1;

            assert_eq!(
                expr.execute(&mut ctx).unwrap(),
                JkInt::from($expr).to_instance()
            );
        }};
    }

    #[test]
    fn t_binop_add_same() {
        binop_assert!(12 + 12);
    }

    #[test]
    fn t_binop_add_l_diff() {
        binop_assert!(12 + 2);
    }

    #[test]
    fn t_binop_add_r_diff() {
        binop_assert!(2 + 99);
    }

    #[test]
    fn t_binop_mul_same() {
        binop_assert!(12 * 12);
    }

    #[test]
    fn t_binop_mul_l_diff() {
        binop_assert!(12 * 2);
    }

    #[test]
    fn t_binop_mul_r_diff() {
        binop_assert!(2 * 99);
    }

    #[test]
    fn binop_parentheses_execute() {
        binop_assert!(4 * (3 + 4))
    }

    #[test]
    fn binop_easy() {
        binop_assert!(5 + 7)
    }

    #[test]
    fn binop_execute_execute_natural_order() {
        binop_assert!(4 + 7 + 3);
    }

    #[test]
    fn binop_execute_execute_mult_priority() {
        binop_assert!(4 + 2 * 3);
    }

    #[test]
    fn binop_execute_execute_mult_natural_priority() {
        binop_assert!(2 * 3 + 4);
    }

    #[test]
    fn binop_execute_valid_add() {
        binop_assert!(1 + 2);
    }

    #[test]
    fn binop_execute_valid_mul() {
        binop_assert!(1 * 2);
    }

    #[test]
    fn binop_execute_valid_normal_priority() {
        binop_assert!(1 * 2 + 3);
    }

    #[test]
    fn binop_execute_valid_back_priority() {
        binop_assert!(3 + 1 * 2);
    }

    #[test]
    fn binop_execute_valid_parentheses_priority() {
        binop_assert!((3 + 1) * 2);
    }

    #[test]
    fn binop_execute_valid_parentheses_priority_reverse() {
        binop_assert!(2 * (3 + 1));
    }

    #[test]
    fn binop_execute_valid_complex_expr() {
        binop_assert!(1 + 4 * 2 - 1 + 2);
    }

    #[test]
    fn binop_execute_valid_multi_expr() {
        binop_assert!(3 + 4 * 2 + 5);
    }

    #[test]
    fn binop_execute_valid_extremely_complex_expr() {
        binop_assert!(1 + 4 * 2 - 1 + 2 * (14 + (2 - 17) * 1) - 12 + 3 / 2);
    }
}
