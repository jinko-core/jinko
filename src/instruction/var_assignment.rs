//! The VarAssign struct is used when assigning values to variables.

use crate::instruction::{InstrKind, Var};
use crate::{Instruction, Interpreter, JkErrKind, JkError};

#[derive(Clone)]
pub struct VarAssign {
    /// Is the variable mutable ? This is only useful on variable declaration
    mutable: bool,

    /// The "name" of the variable
    symbol: String,

    value: Box<dyn Instruction>,
}

impl VarAssign {
    pub fn new(mutable: bool, symbol: String, value: Box<dyn Instruction>) -> VarAssign {
        VarAssign {
            mutable,
            symbol,
            value,
        }
    }

    /// Get a reference to the symbol of the variable declaration
    pub fn symbol(&self) -> &str {
        &self.symbol
    }

    /// Is a variable is declared as mutable or not
    pub fn mutable(&self) -> bool {
        self.mutable
    }
}

impl Instruction for VarAssign {
    fn kind(&self) -> InstrKind {
        InstrKind::Statement
    }

    fn print(&self) -> String {
        let base = if self.mutable {
            String::from("mut ")
        } else {
            String::new()
        };
        format!("{}{} = {}", base, self.symbol, self.value.print())
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JkError> {
        interpreter.debug("ASSIGN VAR", self.symbol());

        // Are we creating the variable or not
        let mut var_creation = false;

        let mut var = match interpreter.get_variable(&self.symbol) {
            Some(v) => {
                // If `self` is mutable, then it means that we are creating the variable
                // for the first time. However, we entered the match arm because the variable
                // is already present in the interpreter. Error out appropriately.
                if self.mutable() {
                    return Err(JkError::new(
                        JkErrKind::Interpreter,
                        format!("Trying to redefine already defined variable: {}", v.name()),
                        None,
                        self.print(),
                    ));
                }

                v.clone()
            }
            None => {
                let mut new_v = Var::new(self.symbol().to_string());
                new_v.set_mutable(self.mutable());

                var_creation = true;

                new_v
            }
        };

        match var_creation {
            // FIXME:
            // - Cleanup
            // - Merge two true arms together if possible
            true => var.set_instance(self.value.execute_expression(interpreter)?),
            false => match var.mutable() {
                false => {
                    // The variable already exists. So we need to error out if it isn't
                    // mutable
                    return Err(JkError::new(
                        JkErrKind::Interpreter,
                        format!(
                            "Trying to assign value to non mutable variable `{}`: `{}`",
                            var.name(),
                            self.value.print()
                        ),
                        None,
                        self.print(),
                    ));
                }
                true => var.set_instance(self.value.execute_expression(interpreter)?),
            },
        };

        // We can unwrap safely since we checked that the variable does not
        // exist
        interpreter.replace_variable(var).unwrap();

        // A variable assignment is always a statement
        Ok(InstrKind::Statement)
    }

    fn prefix(&mut self, prefix: &str) {
        self.value.prefix(prefix);
        self.symbol = format!("{}{}", prefix, self.symbol)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Construct;
    use crate::value::{JkInt, JkString};
    use crate::ToObjectInstance;

    #[test]
    fn non_mutable() {
        let var_assignment = VarAssign::new(false, "x".to_owned(), Box::new(JkInt::from(12)));

        assert_eq!(var_assignment.print(), "x = 12");
    }

    #[test]
    fn mutable() {
        let var_assignment = VarAssign::new(
            true,
            "some_id_99".to_owned(),
            Box::new(JkString::from("Hey there")),
        );

        assert_eq!(var_assignment.print(), "mut some_id_99 = \"Hey there\"");
    }

    #[test]
    fn assign_mutable() {
        let mut i = Interpreter::new();
        let va_init = Construct::var_assignment("mut a = 13").unwrap().1;
        let va_0 = Construct::var_assignment("a = 15").unwrap().1;

        va_init.execute(&mut i).unwrap();
        va_0.execute(&mut i).unwrap();

        let va_get = Construct::variable("a").unwrap().1;
        assert_eq!(
            va_get.execute(&mut i).unwrap(),
            InstrKind::Expression(Some(JkInt::from(15).to_instance()))
        );
    }

    #[test]
    fn assign_immutable() {
        let mut i = Interpreter::new();
        let va_init = Construct::var_assignment("a = 13").unwrap().1;
        let va_0 = Construct::var_assignment("a = 15").unwrap().1;

        va_init.execute(&mut i).unwrap();
        match va_0.execute(&mut i) {
            Ok(_) => assert!(false, "Can't assign twice to immutable variables"),
            Err(_) => assert!(true),
        }
    }

    #[test]
    fn create_mutable_twice() {
        let mut i = Interpreter::new();
        let va_init = Construct::var_assignment("mut a = 13").unwrap().1;
        let va_0 = Construct::var_assignment("mut a = 15").unwrap().1;

        va_init.execute(&mut i).unwrap();
        match va_0.execute(&mut i) {
            Ok(_) => assert!(false, "Can't create variables twice"),
            Err(_) => assert!(true),
        }
    }
}
