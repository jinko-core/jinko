//! Represents a resizeable string in Jinko

use super::{Value, ValueType};
use crate::instruction::{InstrKind, Instruction};

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
        InstrKind::Expression
    }

    fn print(&self) -> String {
        format!("\"{}\"", self.0.clone())
    }
}
