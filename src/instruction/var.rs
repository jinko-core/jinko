//! Represents the usage of a variable, for example when returning from
//! a block. In jinko, variables cannot be uninitialized. Therefore, there is no
//! need to keep an option of an instance. A variable is either there, fully initialized,
//! or it's not.

use crate::instruction::TypeDec;
use crate::typechecker::CheckedType;
use crate::{Context, ErrKind, Error, InstrKind, Instruction, JkBool, ObjectInstance, TypeCheck};

#[derive(Clone)]
pub struct Var {
    name: String,
    mutable: bool,
    instance: ObjectInstance,
    // FIXME: Maybe we can refactor this using the instance's type?
    ty: CheckedType,
}

impl Var {
    /// Create a new variable usage with the given name
    pub fn new(name: String) -> Var {
        Var {
            name,
            mutable: false,
            instance: ObjectInstance::empty(),
            ty: CheckedType::Unknown,
        }
    }

    /// Return the name of the variable
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Return a copy of the variable's instance
    pub fn instance(&self) -> ObjectInstance {
        self.instance.clone()
    }

    /// Is a variable mutable or not
    pub fn mutable(&self) -> bool {
        self.mutable
    }

    /// Set the instance contained in a variable
    pub fn set_instance(&mut self, instance: ObjectInstance) {
        self.instance = instance;
    }

    /// Change the mutability of a variable
    pub fn set_mutable(&mut self, mutable: bool) {
        self.mutable = mutable;
    }

    pub fn set_type(&mut self, ty: TypeDec) {
        self.instance.set_ty(Some(ty))
    }
}

impl Instruction for Var {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression(None)
    }

    fn print(&self) -> String {
        format!(
            "{} /* : {} = {} */",
            self.name.clone(),
            self.instance.ty().unwrap_or(&TypeDec::from("")).name(),
            self.instance
        )
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

    fn execute(&self, ctx: &mut Context) -> Option<ObjectInstance> {
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

        Some(var.instance())
    }
}

impl TypeCheck for Var {
    fn resolve_type(&self, ctx: &mut Context) -> CheckedType {
        match ctx.get_variable(self.name()) {
            // FIXME: Is this correct? Does no type correspond to void? Can we use
            // CheckedType for the Instances?
            Some(var) => match var.instance.ty() {
                None => CheckedType::Void,
                Some(ty) => CheckedType::Resolved(ty.into()),
            },
            // FIXME: Is that correct?
            None => CheckedType::Unknown,
        }
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
