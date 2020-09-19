//! The VarAssign struct is used when assigning values to variables.

use crate::error::{ErrKind, JinkoError};
use crate::interpreter::Interpreter;
use crate::value::Constant;

use super::{InstrKind, Instruction};

pub struct VarAssign {
    /// Is the variable mutable ? This is only useful on variable declaration
    mutable: bool,

    /// The "name" of the variable
    symbol: String,

    value: Constant,
}

impl VarAssign {
    pub fn new(mutable: bool, symbol: String, value: Constant) -> VarAssign {
        VarAssign {
            mutable,
            symbol,
            value,
        }
    }

    pub fn mutable(&self) -> bool {
        self.mutable
    }

    pub fn symbol(&self) -> &str {
        &self.symbol
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

    fn execute(&self, interpreter: &mut Interpreter) -> Result<(), JinkoError> {
        match interpreter.get_variable(&self.symbol) {
            Some(v) => match self.mutable {
                // FIXME: Add logic once constant type is cleaned up
                true => unreachable!("Mutating mutable variable {}", v.print()),
                false => Err(JinkoError::new(
                    ErrKind::Interpreter,
                    format!("Trying to mutate immutable variable {}", self.symbol),
                    None,
                    self.print(),
                )),
            },
            // FIXME: Add logic once constant type is cleaned up
            None => unreachable!("First assignment for variable {}", self.symbol),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::constant::*;

    #[test]
    fn non_mutable() {
        let var_assignment = VarAssign::new(
            false,
            "x".to_owned(),
            Constant::new(ConstKind::Int).with_iv(12),
        );

        assert_eq!(var_assignment.print(), "x = 12");
    }

    #[test]
    fn mutable() {
        let var_assignment = VarAssign::new(
            true,
            "some_id_99".to_owned(),
            Constant::new(ConstKind::Str).with_sv("Hey there".to_owned()),
        );

        assert_eq!(var_assignment.print(), "mut some_id_99 = \"Hey there\"");
    }
}
