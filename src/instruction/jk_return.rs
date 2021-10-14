//! Return construct is used to return early from a function
//! ```
//! return
//! ```
//!
//! It can be used to return values
//!
//! ```
//! return 42
//! ```

use crate::instruction::{InstrKind, Instruction};
use crate::{
    typechecker::{CheckedType, TypeCtx},
    Context, ObjectInstance, TypeCheck,
};

#[derive(Clone)]
pub struct Return {
    value: Option<Box<dyn Instruction>>,
}

impl Return {
    /// Create a new Return instruction
    pub fn new(value: Option<Box<dyn Instruction>>) -> Return {
        Return { value }
    }
}

impl Instruction for Return {
    fn kind(&self) -> InstrKind {
        match &self.value {
            Some(val) => val.kind(),
            None => InstrKind::Statement,
        }
    }

    fn print(&self) -> String {
        let base = "return".to_string();

        match &self.value {
            Some(val) => format!("{} {}", base, val.print()),
            None => base,
        }
    }

    fn execute(&self, ctx: &mut Context) -> Option<ObjectInstance> {
        match &self.value {
            Some(val) => val.execute(ctx),
            None => None,
        }
    }
}

impl TypeCheck for Return {
    fn resolve_type(&self, _ctx: &mut TypeCtx) -> CheckedType {
        /* FIXME: self.value.resolve_type(ctx) */
        CheckedType::Void
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jk_execute;

    #[test]
    fn t_return_kind() {
        let return_inst = Return::new(None);

        assert_eq!(return_inst.kind(), InstrKind::Statement);
    }

    #[test]
    fn t_return_value_kind() {
        use crate::value::JkInt;
        let return_inst = Return::new(Some(Box::new(JkInt::from(42))));

        assert_eq!(return_inst.kind(), JkInt::from(42).kind());
    }

    #[test]
    fn t_return_pretty_print() {
        let return_inst = Return::new(None);

        assert_eq!(return_inst.print(), "return");
    }

    #[test]
    fn t_return_pretty_print_with_value() {
        use crate::value::JkInt;

        let return_inst = Return::new(Some(Box::new(JkInt::from(42))));

        assert_eq!(return_inst.print(), "return 42");
    }

    #[test]
    #[ignore] // FIXME: Do not ignore once parser supports returns
    fn t_return_execute_macro() {
        let res = jk_execute! {
            { return }
        };

        assert_eq!(res, None);
    }

    #[test]
    #[ignore] // FIXME: Do not ignore once parser supports returns
    fn t_return_execute_with_value_macro() {
        use crate::instance::ToObjectInstance;
        use crate::value::JkInt;

        let res = jk_execute! {
            {return 42}
        };

        assert_eq!(res, Some(JkInt::from(42).to_instance()));
    }
}
