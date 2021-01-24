//! The VarAssign struct is used when assigning values to variables.

use crate::error::{ErrKind, JinkoError};
use crate::interpreter::Interpreter;

use super::{InstrKind, Instruction, Var};

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

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JinkoError> {
        interpreter.debug("ASSIGN VAR", self.symbol());

        // FIXME:
        // - Cleanup
        // - When mapping variables to function arguments, make sure that the correct one
        // is returned:
        //
        // ```
        // a = 15;
        //
        // func add(a: int, b: int) -> int {
        //      a + b // a needs to refer to the argument, not the outer variable
        // }
        // ```
        match interpreter.get_variable(&self.symbol) {
            Some(v) => {
                // If `self` is mutable, then it means that we are creating the variable
                // for the first time. However, we entered the match arm because the variable
                // is already present in the interpreter. Error out appropriately.
                if self.mutable() {
                    return Err(JinkoError::new(
                        ErrKind::Interpreter,
                        format!("Trying to redefine already defined variable: {}", v.name()),
                        None,
                        self.print(),
                    ));
                }

                match v.mutable() {
                    false => {
                        return Err(JinkoError::new(
                            ErrKind::Interpreter,
                            format!(
                                "Trying to assign value to non mutable variable `{}`: `{}`",
                                v.name(),
                                self.value.print()
                            ),
                            None,
                            self.print(),
                        ))
                    }
                    true => {
                        todo!()
                        // let v_value = self.value.execute(interpreter)?;

                        // match v_value {
                        //     InstrKind::Expression(Some(instance)) => v.set_instance(instance),
                        //     InstrKind::Expression(None) | InstrKind::Statement => return Err(JinkoError::new(ErrKind::Interpreter, format!("Trying to assign statement `{}` to variable `{}`", self.value.print(), self.symbol()), None, self.print())),
                        // };
                    }
                }
            }
            // The variable does not exist. We're going to create it, and set its initial
            // value
            None => {
                let mut v = Var::new(self.symbol().to_string());
                v.set_mutable(self.mutable);

                let v_value = self.value.execute(interpreter)?;

                match v_value {
                    InstrKind::Expression(Some(instance)) => v.set_instance(instance),
                    InstrKind::Expression(None) | InstrKind::Statement => {
                        return Err(JinkoError::new(
                            ErrKind::Interpreter,
                            format!(
                                "Trying to assign statement `{}` to variable `{}`",
                                self.value.print(),
                                self.symbol()
                            ),
                            None,
                            self.print(),
                        ))
                    }
                };

                // We can unwrap safely since we checked that the variable does not
                // exist
                interpreter.add_variable(v).unwrap();
            }
        };

        // A variable assignment is always a statement
        Ok(InstrKind::Statement)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::{JinkInt, JinkString};

    #[test]
    fn non_mutable() {
        let var_assignment = VarAssign::new(false, "x".to_owned(), Box::new(JinkInt::from(12)));

        assert_eq!(var_assignment.print(), "x = 12");
    }

    #[test]
    fn mutable() {
        let var_assignment = VarAssign::new(
            true,
            "some_id_99".to_owned(),
            Box::new(JinkString::from("Hey there")),
        );

        assert_eq!(var_assignment.print(), "mut some_id_99 = \"Hey there\"");
    }
}
