//! Return construct is used to return early from a function
//! ```ignore
//! return
//! ```
//!
//! It can be used to return values
//!
//! ```ignore
//! return 42
//! ```

use crate::instruction::{InstrKind, Instruction};
use crate::PrettyPrint;
use crate::SpanTuple;
use crate::{
    typechecker::{CheckedType, TypeCtx},
    Context, Generic, ObjectInstance, TypeCheck,
};

#[derive(Clone)]
pub struct Return {
    value: Option<Box<dyn Instruction>>,
    cached_type: Option<CheckedType>,
    location: Option<SpanTuple>,
}

impl Return {
    /// Create a new Return instruction
    pub fn new(value: Option<Box<dyn Instruction>>) -> Return {
        Return {
            value,
            cached_type: None,
            location: None,
        }
    }

    pub fn set_location(&mut self, location: SpanTuple) {
        self.location = Some(location)
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

    fn location(&self) -> Option<&SpanTuple> {
        self.location.as_ref()
    }
}

impl TypeCheck for Return {
    fn resolve_type(&mut self, ctx: &mut TypeCtx) -> CheckedType {
        match &mut self.value {
            None => CheckedType::Void,
            Some(v) => v.type_of(ctx),
        }
    }

    fn set_cached_type(&mut self, ty: CheckedType) {
        self.cached_type = Some(ty)
    }

    fn cached_type(&self) -> Option<&CheckedType> {
        self.cached_type.as_ref()
    }
}

impl Generic for Return {}

impl PrettyPrint for Return {}

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
    fn t_return_execute_macro() {
        let res = jk_execute! {
            { return }
        };

        assert_eq!(res, None);
    }

    #[test]
    fn t_return_execute_with_value_macro() {
        use crate::instance::ToObjectInstance;
        use crate::value::JkInt;

        let res = jk_execute! {
            {return 42}
        };

        assert_eq!(res, Some(JkInt::from(42).to_instance()));
    }
}
