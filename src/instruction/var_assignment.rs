//! The VarAssign struct is used when assigning values to variables.

use super::{InstrKind, Instruction};

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

    pub fn mutable(&self) -> bool {
        self.mutable
    }

    pub fn symbol(&self) -> &str {
        &self.symbol
    }
}

impl Instruction for VarAssign {
    // FIXME: Add logic
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore]
    fn non_mutable() {
        /*
        let var_assignment = VarAssign::new(
            false,
            "x".to_owned(),
            Constant::new(ConstKind::Int).with_iv(12),
        );

        assert_eq!(var_assignment.print(), "x = 12");
        */
    }

    #[test]
    #[ignore]
    fn mutable() {
        /*
        let var_assignment = VarAssign::new(
            true,
            "some_id_99".to_owned(),
            Constant::new(ConstKind::Str).with_sv("Hey there".to_owned()),
        );

        assert_eq!(var_assignment.print(), "mut some_id_99 = \"Hey there\"");
        */
    }
}
