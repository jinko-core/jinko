//! Represents the usage of a variable, for example when returning from
//! a block. In jinko, variables cannot be uninitialized. Therefore, there is no
//! need to keep an option of an instance. A variable is either there, fully initialized,
//! or it's not.

use crate::instruction::TypeDec;
use crate::typechecker::{CheckedType, TypeCtx};
use crate::{Context, ErrKind, Error, InstrKind, Instruction, JkBool, ObjectInstance, TypeCheck};

#[derive(Clone)]
pub struct Var {
    name: String,
    mutable: bool,
    pub(crate) instance: ObjectInstance,
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
        self.instance.set_ty(CheckedType::Resolved(ty.into()))
    }
}

impl Instruction for Var {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression(None)
    }

    fn print(&self) -> String {
        let mut base = self.name.clone();
        if let CheckedType::Resolved(ty) = self.instance.ty() {
            base = format!("{} /* : {} */", base, ty.id());
        }

        format!("{} = {}", base, self.instance)
    }

    fn as_bool(&self, ctx: &mut Context) -> Option<bool> {
        use crate::FromObjectInstance;

        // FIXME: Cleanup

        match self.execute(ctx) {
            Some(instance) => match instance.ty() {
                CheckedType::Resolved(ty) => match ty.id() {
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
                _ => todo!(
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

        // FIXME: Re-add once debugging is separate from context #210
        // ctx.debug("VAR", var.print().as_ref());

        Some(var.instance.clone())
    }
}

impl TypeCheck for Var {
    fn resolve_type(&self, ctx: &mut TypeCtx) -> CheckedType {
        match ctx.get_var(self.name()) {
            Some(var_ty) => var_ty.clone(),
            None => {
                ctx.error(
                    Error::new(ErrKind::TypeChecker)
                        .with_msg(format!("use of undeclared variable: `{}`", self.name())),
                );
                CheckedType::Unknown
            }
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
    use crate::{jinko, jinko_fail};

    #[test]
    fn keep_instance() {
        let mut i = Context::new();
        let mut v = Var::new("a".to_string());

        let instance = JkInt::from(15).to_instance();
        v.set_instance(instance.clone());

        i.add_variable(v.clone()).unwrap();

        assert_eq!(v.execute(&mut i).unwrap(), instance);
    }

    #[test]
    fn tc_valid() {
        jinko! {
            a = 15;
            a
        };
    }

    #[test]
    fn tc_invalid() {
        jinko_fail! {
            // undeclared variable
            a
        };
    }
}
