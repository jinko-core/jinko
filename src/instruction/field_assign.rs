//! FieldAssigns represent the assignment of a value to a given field on an instance
//! This is what is used by the interpreter when modifying an attribute on a given type.
//! Just like variable assignments, the original instance needs to be mutable in order to
//! be assigned a new value. However, unlike variable assignments, there is no "first
//! assignment" for fields, as they should be initialized on the type's instantiation.

use super::FieldAccess;
use crate::{Context, Error, ErrKind, InstrKind, Instruction, ObjectInstance, Rename};

#[derive(Clone)]
pub struct FieldAssign {
    // FIXME: Figure out how to keep a field access or a variable here
    field: FieldAccess,
    value: Box<dyn Instruction>,
}

impl FieldAssign {
    /// Create a new FieldAssign from the FieldAccess you're trying to modify and the value
    /// you're trying to assign to it.
    pub fn new(field: FieldAccess, value: Box<dyn Instruction>) -> FieldAssign {
        FieldAssign { field, value }
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
        // FIXME: We probably need to keep track of all the instances created somewhere
        // in the context, to garbage collect them later for exemple
        let value = self.value.execute(ctx)?;

        // FIXME: How does this work when we're not dealing with a variable as an instance?
        // Say, `fn_call().attribute = some_other_value` (Hint: It doesn't)
        let instance = match ctx.get_variable(&self.field.instance.print()) {
            Some(var) => &mut var.instance,
            None => {
                ctx.error(Error::new(ErrKind::Context).with_msg(format!(
                    "cannot find variable: `{}`",
                    self.field.instance.print()
                )));
                return None;
            }
        };

        // FIXME: Should we check if this is the first time we're setting the field? In
        // that case, error out!
        if let Err(e) = instance.set_field(&self.field.field_name, value) {
            ctx.error(e);
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use crate::instance::ToObjectInstance;
    use crate::{parser::Construct, Context, JkInt};

    fn setup() -> Context {
        let mut ctx = Context::new();

        let inst = Construct::instruction("type Point(x: int, y: int);")
            .unwrap()
            .1;
        inst.execute(&mut ctx);

        let inst = Construct::instruction("point = Point { x = 15, y = 14 }")
            .unwrap()
            .1;
        inst.execute(&mut ctx);

        assert!(!ctx.error_handler.has_errors());

        ctx
    }

    #[test]
    fn t_valid_field_assign() {
        let mut ctx = setup();

        let f_a = Construct::instruction("point.x = 99").unwrap().1;
        f_a.execute(&mut ctx);

        let f_a_result = Construct::instruction("point.x").unwrap().1;
        let x_value = f_a_result.execute(&mut ctx).unwrap();

        assert!(!ctx.error_handler.has_errors());
        assert_eq!(x_value, JkInt::from(99).to_instance());
    }

    // FIXME: Add tests making sure that we can't modify the fields on something that
    // isn't a variable
}
