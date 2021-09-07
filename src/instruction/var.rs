//! Represents the usage of a variable, for example when returning from
//! a block. In jinko, variables cannot be uninitialized. Therefore, there is no
//! need to keep an option of an instance. A variable is either there, fully initialized,
//! or it's not.

use crate::{Context, ErrKind, Error, InstrKind, Instruction, JkBool, ObjectInstance};

#[derive(Clone)]
pub struct Var {
    name: String,
    mutable: bool,
}

impl Var {
    /// Create a new variable usage with the given name
    pub fn new(name: String) -> Var {
        Var {
            name,
            mutable: false,
        }
    }

    /// Return the name of the variable
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Is a variable mutable or not
    pub fn mutable(&self) -> bool {
        self.mutable
    }

    /// Change the mutability of a variable
    pub fn set_mutable(&mut self, mutable: bool) {
        self.mutable = mutable;
    }
}

impl Instruction for Var {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression(None)
    }

    fn print(&self) -> String {
        self.name.clone()
    }

    fn as_bool(&self, ctx: &mut Context) -> Option<bool> {
        use crate::FromObjectInstance;

        // FIXME: Cleanup

        match self.execute(ctx) {
            Some(instance) => match instance.ty() {
                Some(ty) => match ty.name() {
                    // FIXME:
                    "bool" => Some(JkBool::from_instance(&instance).as_bool(ctx).unwrap()),
                    // We can safely unwrap since we checked the type of the variable
                    _ => {
                        ctx.error(Error::new(ErrKind::Context).with_msg(format!(
                            "var {} cannot be interpreted as boolean",
                            self.name
                        )));
                        None
                    }
                },
                None => todo!(
                    "If the type of the variable hasn't been determined yet,
                    typecheck it and call self.as_bool() again"
                ),
            },
            _ => {
                ctx.error(Error::new(ErrKind::Context).with_msg(format!(
                    "var {} cannot be interpreted as boolean",
                    self.name
                )));
                None
            }
        }
    }

    fn execute<'ctx>(&self, ctx: &'ctx mut Context) -> Option<&'ctx mut ObjectInstance> {
        let var = match ctx.get_variable(self.name()) {
            Some(v) => v,
            None => {
                ctx.error(
                    Error::new(ErrKind::Context)
                        .with_msg(format!("variable has not been declared: {}", self.name)),
                );

                return None;
            }
        };

        ctx.debug("VAR", var.print().as_ref());

        // FIXME: We need to get the instance in some way or another, maybe through the GC?
        // Some(var.instance())
        None
    }
}

impl Default for Var {
    fn default() -> Self {
        Var::new(String::new())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::JkInt;
    use crate::ToObjectInstance;

    #[test]
    fn keep_instance() {
        let mut i = Context::new();
        let mut v = Var::new("a".to_string());

        let instance = JkInt::from(15).to_instance();
        v.set_instance(instance.clone());

        i.add_variable(v.clone()).unwrap();

        assert_eq!(v.execute(&mut i).unwrap(), instance);
    }
}
