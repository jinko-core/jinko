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

use crate::context::Context;
use crate::error::{ErrKind, Error};
use crate::generics::{GenericMap, GenericUser};
use crate::instance::{FromObjectInstance, ObjectInstance};
use crate::instruction::{Block, InstrKind, Instruction};
use crate::location::SpanTuple;
use crate::typechecker::{CheckedType, TypeCheck, TypeCtx, TypeId};
use crate::value::JkBool;

#[derive(Clone)]
pub struct IfElse {
    condition: Box<dyn Instruction>,
    if_body: Block,
    else_body: Option<Block>,
    cached_type: Option<CheckedType>,
    location: Option<SpanTuple>,
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
            cached_type: None,
            location: None,
        }
    }

    pub fn set_location(&mut self, location: SpanTuple) {
        self.location = Some(location)
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
        let cond = self.condition.execute(ctx)?;

        if JkBool::from_instance(&cond).rust_value() {
            self.if_body.execute(ctx)
        } else {
            match &self.else_body {
                Some(b) => b.execute(ctx),
                // FIXME: Fix logic: If an `if` returns something, the else should too.
                // if there is no else, then error out
                None => None,
            }
        }
    }

    fn location(&self) -> Option<&SpanTuple> {
        self.location.as_ref()
    }
}

impl TypeCheck for IfElse {
    fn resolve_type(&mut self, ctx: &mut TypeCtx) -> CheckedType {
        let bool_checkedtype = CheckedType::Resolved(TypeId::from("bool"));
        let cond_ty = self.condition.type_of(ctx);

        if cond_ty != bool_checkedtype {
            ctx.error(
                Error::new(ErrKind::TypeChecker)
                    .with_msg(format!(
                        "if condition should be a boolean, not a `{}`",
                        cond_ty
                    ))
                    .with_loc(self.condition.location().cloned()),
            );
        }

        let if_ty = self.if_body.type_of(ctx);
        let else_ty = self
            .else_body
            .as_mut()
            .map(|else_body| else_body.type_of(ctx));

        match (if_ty, else_ty) {
            (CheckedType::Void, None) => CheckedType::Void,
            (if_ty, Some(else_ty)) => {
                if if_ty != else_ty {
                    ctx.error(
                        Error::new(ErrKind::TypeChecker)
                            .with_msg(format!(
                                "incompatible types for `if` and `else` block: {} and {}",
                                if_ty, else_ty,
                            ))
                            .with_loc(self.location.clone()),
                    );
                    CheckedType::Error
                } else {
                    if_ty
                }
            }
            (if_ty, None) => {
                ctx.error(
                    Error::new(ErrKind::TypeChecker)
                        .with_msg(format!(
                            "`if` block has a return type ({}) but no else block to match it",
                            if_ty
                        ))
                        .with_loc(self.location.clone()),
                );
                CheckedType::Error
            }
        }
    }

    fn set_cached_type(&mut self, ty: CheckedType) {
        self.cached_type = Some(ty)
    }

    fn cached_type(&self) -> Option<&CheckedType> {
        self.cached_type.as_ref()
    }
}

impl GenericUser for IfElse {
    fn resolve_usages(&mut self, type_map: &GenericMap, ctx: &mut TypeCtx) {
        self.condition.resolve_usages(type_map, ctx);
        self.if_body.resolve_usages(type_map, ctx);
        if let Some(b) = &mut self.else_body {
            b.resolve_usages(type_map, ctx)
        };
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

        let mut ctx = Context::new(Box::new(crate::io_trait::JkStdReader {}));

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

        let mut ctx = Context::new(Box::new(crate::io_trait::JkStdReader {}));

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
