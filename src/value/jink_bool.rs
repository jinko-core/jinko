//! Represents a boolean in Jinko

use super::Value;
use crate::instruction::{InstrKind, Instruction};

pub struct JinkBool(bool);

impl From<bool> for JinkBool {
    fn from(c: bool) -> Self {
        JinkBool(c)
    }
}

impl Value for JinkBool {}

impl Instruction for JinkBool {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression
    }

    fn print(&self) -> String {
        self.0.to_string()
    }
}
