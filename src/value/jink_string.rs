//! Represents a resizeable string in Jinko

use crate::instruction::{Instruction, InstrKind};
use super::Value;

pub struct JinkString(String);

impl From<&str> for JinkString {
    fn from(s: &str) -> Self {
        JinkString(s.to_owned())
    }
}

impl Value for JinkString {}

impl Instruction for JinkString {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression
    }

    fn print(&self) -> String {
        self.0.clone()
    }
}
