//! Represents a single character in Jinko

use super::{Value, ValueType};
use crate::instruction::{InstrKind, Instruction};

#[derive(Clone)]
pub struct JinkChar(char);

impl From<char> for JinkChar {
    fn from(c: char) -> Self {
        JinkChar(c)
    }
}

impl Value for JinkChar {
    fn vtype(&self) -> ValueType {
        ValueType::Bool
    }
}

impl Instruction for JinkChar {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression
    }

    fn print(&self) -> String {
        self.0.to_string()
    }
}
