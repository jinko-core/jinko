//! Represents a boolean in Jinko

use super::{Value, ValueType};
use crate::instruction::{InstrKind, Instruction, Operator};

#[derive(Clone)]
pub struct JinkBool(bool);

impl From<bool> for JinkBool {
    fn from(c: bool) -> Self {
        JinkBool(c)
    }
}

impl Value for JinkBool {
    fn vtype(&self) -> ValueType {
        ValueType::Bool
    }
}

impl Instruction for JinkBool {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression
    }

    fn print(&self) -> String {
        self.0.to_string()
    }

    fn as_bool(&self) -> bool {
        self.0
    }
}
