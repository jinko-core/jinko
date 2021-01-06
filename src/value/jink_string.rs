//! Represents a resizeable string in Jinko

use super::{Value, ValueType};
use crate::instruction::{InstrKind, Instruction};
use crate::{Interpreter, JinkoError};

#[derive(Clone)]
pub struct JinkString(String);

impl From<&str> for JinkString {
    fn from(s: &str) -> Self {
        JinkString(s.to_owned())
    }
}

impl Value for JinkString {
    fn vtype(&self) -> ValueType {
        ValueType::Bool
    }
}

impl Instruction for JinkString {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression(None)
    }

    fn print(&self) -> String {
        format!("\"{}\"", self.0.clone())
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JinkoError> {
        interpreter.debug("STR", &self.0.to_string());

        // FIXME: Add logic
        Ok(InstrKind::Expression(None))
    }
}
