//! Represents the usage of a variable, for example when returning from
//! a block. In jinko, variables cannot be uninitialized. Therefore, there is no
//! need to keep an option of an instance. A variable is either there, fully initialized,
//! or it's not.

use crate::{ErrKind, Instance, InstrKind, Instruction, Interpreter, JinkoError};

#[derive(Clone)]
pub struct Var {
    name: String,
    instance: Instance,
}

impl Var {
    /// Create a new variable usage with the given name
    pub fn new(name: String) -> Var {
        Var {
            name,
            instance: Instance::empty(),
        }
    }

    /// Return the name of the variable
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Return a copy of the variable's instance
    pub fn instance(&self) -> Instance {
        self.instance.clone()
    }

    /// Set the instance contained in a variable
    #[cfg(test)]
    pub fn set_instance(&mut self, instance: Instance) {
        self.instance = instance
    }
}

impl Instruction for Var {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression(None)
    }

    fn print(&self) -> String {
        self.name.clone()
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JinkoError> {
        interpreter.debug("VAR", self.name());

        let var = match interpreter.get_variable(self.name()) {
            Some(v) => v,
            None => {
                return Err(JinkoError::new(
                    ErrKind::Interpreter,
                    format!("variable has not been declared: {}", self.name),
                    None,
                    self.name().to_owned(),
                ))
            }
        };

        Ok(InstrKind::Expression(Some(var.instance())))
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
    use crate::value::JinkInt;
    use crate::ToInstance;

    #[test]
    fn keep_instance() {
        let mut i = Interpreter::new();
        let mut v = Var::new("a".to_string());

        let instance = JinkInt::from(15).to_instance();
        v.set_instance(instance.clone());

        i.add_variable(v.clone()).unwrap();

        assert_eq!(
            v.execute(&mut i).unwrap(),
            InstrKind::Expression(Some(instance))
        );
    }
}
