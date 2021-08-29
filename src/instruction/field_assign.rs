//! FieldAssigns represent the assignment of a value to a given field on an instance
//! This is what is used by the interpreter when modifying an attribute on a given type.
//! Just like variable assignments, the original instance needs to be mutable in order to
//! be assigned a new value. However, unlike variable assignments, there is no "first
//! assignment" for fields, as they should be initialized on the type's instantiation.

use crate::{Context, InstrKind, Instruction, ObjectInstance, Rename};
use super::FieldAccess;

#[derive(Clone)]
pub struct FieldAssign {
    field: FieldAccess,
    value: Box<dyn Instruction>,
}

impl FieldAssign {
    /// Create a new FieldAssign from the FieldAccess you're trying to modify and the value
    /// you're trying to assign to it.
    pub fn new(field: FieldAccess, value: Box<dyn Instruction>) -> FieldAssign {
        FieldAssign {
            field, value
        }
    }
}

impl Rename for FieldAssign {
    fn prefix(&mut self, _prefix: &str) {
        /* FIXME: Add logic */
    }
}

impl Instruction for FieldAssign {
    fn kind(&self) -> InstrKind {
        InstrKind::Statement
    }

    fn print(&self) -> String {
        format!("{} = {}", self.field.print(), self.value.print())
    }

    fn execute(&self, ctx: &mut Context) -> Option<ObjectInstance> {
        let mut instance = self.field.execute(ctx)?;

        // FIXME: Should we check if this is the first time we're setting the field? In
        // that case, error out!
        if let Err(e) = instance.set_field(&self.field.field_name, self.value.execute(ctx)?) {
            ctx.error(e);
        }

        None
    }
}
