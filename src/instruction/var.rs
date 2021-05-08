//! Represents the usage of a variable, for example when returning from
//! a block. In jinko, variables cannot be uninitialized. Therefore, there is no
//! need to keep an option of an instance. A variable is either there, fully initialized,
//! or it's not.

use crate::instruction::TypeDec;
use crate::{
    InstrKind, Instruction, Interpreter, JkBool, JkErrKind, JkError, ObjectInstance, Rename,
};

#[derive(Clone)]
pub struct Var {
    name: String,
    mutable: bool,
    instance: ObjectInstance,
}

impl Var {
    /// Create a new variable usage with the given name
    pub fn new(name: String) -> Var {
        Var {
            name,
            mutable: false,
            instance: ObjectInstance::empty(),
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

    fn as_bool(&self, i: &mut Interpreter) -> Result<bool, JkError> {
        use crate::FromObjectInstance;

        // FIXME: Cleanup

        match self.execute(i)? {
            InstrKind::Expression(Some(instance)) => match instance.ty() {
                Some(ty) => match ty.name() {
                    // FIXME:
                    "bool" => Ok(JkBool::from_instance(&instance).as_bool(i).unwrap()),
                    // We can safely unwrap since we checked the type of the variable
                    _ => Err(JkError::new(
                        JkErrKind::Interpreter,
                        format!("var {} cannot be interpreted as boolean", self.name),
                        None,
                        self.print(),
                    )),
                },
                None => todo!(
                    "If the type of the variable hasn't been determined yet,
                    typecheck it and call self.as_bool() again"
                ),
            },
            _ => Err(JkError::new(
                JkErrKind::Interpreter,
                format!("var {} cannot be interpreted as boolean", self.name),
                None,
                self.print(),
            )),
        }
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JkError> {
        let var = match interpreter.get_variable(self.name()) {
            Some(v) => v,
            None => {
                return Err(JkError::new(
                    JkErrKind::Interpreter,
                    format!("variable has not been declared: {}", self.name),
                    None,
                    self.name().to_owned(),
                ))
            }
        };

        interpreter.debug("VAR", var.print().as_ref());

        Ok(InstrKind::Expression(Some(var.instance())))
    }
}

impl Rename for Var {
    fn prefix(&mut self, prefix: &str) {
        self.name = format!("{}{}", prefix, self.name)
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
        let mut i = Interpreter::new();
        let mut v = Var::new("a".to_string());

        let instance = JkInt::from(15).to_instance();
        v.set_instance(instance.clone());

        i.add_variable(v.clone()).unwrap();

        assert_eq!(
            v.execute(&mut i).unwrap(),
            InstrKind::Expression(Some(instance))
        );
    }
}
