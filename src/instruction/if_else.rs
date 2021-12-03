//! `IfElse`s are used to represent an if/else statement in the source code. They have
//! a condition, a body and an optional else body.
//!
//! ```ignore
//! if condition {
//!     condition_evaluates_to_true();
//! } else {
//!     all_conditions_are_false();
//! }
//! ```
//!
//! They can be used to return values, just like you would with any block.
//!
//! ```ignore
//! x = if condition { 12 } else { 13 };
//! ```

use crate::instance::FromObjectInstance;
use crate::instruction::{Block, InstrKind, Instruction, TypeId};
use crate::typechecker::TypeCtx;
use crate::value::JkBool;
use crate::{typechecker::CheckedType, Context, ObjectInstance, TypeCheck};
use crate::{ErrKind, Error};

#[derive(Clone)]
pub struct IfElse {
    condition: Box<dyn Instruction>,
    if_body: Block,
    else_body: Option<Block>,
}

impl IfElse {
    /// Create a new IfElse block and return it
    pub fn new(
        condition: Box<dyn Instruction>,
        if_body: Block,
        else_body: Option<Block>,
    ) -> IfElse {
        IfElse {
            condition,
            if_body,
            else_body,
        }
    }
}

impl Instruction for IfElse {
    fn kind(&self) -> InstrKind {
        // We don't check the kind of the else_body, since the typechecker will have
        // approved that the if_body and else_body return the same thing
        self.if_body.kind()
    }

    fn print(&self) -> String {
        let base = format!("if {} {}", self.condition.print(), self.if_body.print());

        match &self.else_body {
            Some(body) => format!("{} else {}", base, body.print()),
            None => base,
        }
    }

    fn execute(&self, ctx: &mut Context) -> Option<ObjectInstance> {
        ctx.debug_step("IF_ELSE ENTER");

        let cond = self.condition.as_bool(ctx)?;
        ctx.debug("COND", &cond.to_string());

        if cond {
            ctx.debug_step("IF ENTER");
            self.if_body.execute(ctx)
        } else {
            ctx.debug_step("ELSE ENTER");
            match &self.else_body {
                Some(b) => b.execute(ctx),
                // FIXME: Fix logic: If an `if` returns something, the else should too.
                // if there is no else, then error out
                None => None,
            }
        }
    }
}

impl TypeCheck for IfElse {
    fn resolve_type(&self, ctx: &mut TypeCtx) -> CheckedType {
        let bool_checkedtype = CheckedType::Resolved(TypeId::from("bool"));
        let cond_ty = self.condition.resolve_type(ctx);
        if cond_ty != bool_checkedtype {
            ctx.error(Error::new(ErrKind::TypeChecker).with_msg(format!(
                "if condition should be a boolean, not a {}",
                cond_ty
            )));
        }

        let if_ty = self.if_body.resolve_type(ctx);
        let else_ty = self
            .else_body
            .as_ref()
            .map(|else_body| else_body.resolve_type(ctx));

        match (if_ty, else_ty) {
            (CheckedType::Void, None) => CheckedType::Void,
            (if_ty, Some(else_ty)) => {
                if if_ty != else_ty {
                    ctx.error(Error::new(ErrKind::TypeChecker).with_msg(format!(
                        "incompatible types for `if` and `else` block: {} and {}",
                        if_ty, else_ty,
                    )));
                    CheckedType::Unknown
                } else {
                    if_ty
                }
            }
            (if_ty, None) => {
                ctx.error(Error::new(ErrKind::TypeChecker).with_msg(format!(
                    "`if` block has a return type ({}) but no else block to match it",
                    if_ty
                )));
                CheckedType::Unknown
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{jinko, jinko_fail};

    #[test]
    fn t_if_print() {
        use crate::value::JkBool;

        let if_block = IfElse::new(Box::new(JkBool::from(true)), Block::new(), None);

        assert_eq!(if_block.print(), "if true {\n}".to_string());
    }

    #[test]
    fn t_if_else_print() {
        use crate::value::JkBool;

        let if_block = IfElse::new(
            Box::new(JkBool::from(true)),
            Block::new(),
            Some(Block::new()),
        );

        assert_eq!(if_block.print(), "if true {\n} else {\n}".to_string());
    }

    #[test]
    fn t_if_kind() {
        use crate::value::JkBool;

        let if_block = IfElse::new(Box::new(JkBool::from(true)), Block::new(), None);

        assert_eq!(if_block.kind(), InstrKind::Statement);
    }

    #[test]
    fn t_if_execute() {
        use crate::instance::ToObjectInstance;
        use crate::value::{JkBool, JkInt};

        let mut ctx = Context::new();

        let mut if_block = Block::new();
        let mut else_block = Block::new();
        if_block.set_statement(false);
        if_block.add_instruction(Box::new(JkInt::from(42)));
        else_block.set_statement(false);
        else_block.add_instruction(Box::new(JkInt::from(69)));

        let if_else = IfElse::new(Box::new(JkBool::from(true)), if_block, Some(else_block));

        assert_eq!(
            if_else.execute(&mut ctx).unwrap(),
            JkInt::from(42).to_instance()
        );
    }

    #[test]
    fn t_else_execute() {
        use crate::instance::ToObjectInstance;
        use crate::value::{JkBool, JkInt};

        let mut ctx = Context::new();

        let mut if_block = Block::new();
        let mut else_block = Block::new();
        if_block.set_statement(false);
        if_block.add_instruction(Box::new(JkInt::from(42)));
        else_block.set_statement(false);
        else_block.add_instruction(Box::new(JkInt::from(69)));

        let if_else = IfElse::new(Box::new(JkBool::from(false)), if_block, Some(else_block));

        assert_eq!(
            if_else.execute(&mut ctx).unwrap(),
            JkInt::from(69).to_instance()
        );
    }

    #[test]
    fn tc_if_else_simple() {
        jinko! {
            if true {
                15
            } else {
                14
            }
        };
    }

    #[test]
    fn tc_if_else_in_func() {
        jinko! {
            func bool_to_int(b: bool) -> int {
                if b {
                    1
                } else {
                    0
                }
            }
        };
    }

    #[test]
    fn tc_if_else_not_bool_in_cond() {
        jinko_fail! {
            if 4.5 {
                15
            } else {
                14
            }
        };
    }

    #[test]
    fn tc_if_else_mismatched_types() {
        jinko_fail! {
            if true {
                1
            } else {
                4.5
            }
        };
    }
}
