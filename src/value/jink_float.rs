//! Represents a floating point number in Jinko. Floating point numbers are always
//! double precision

use super::Value;
use crate::instruction::{InstrKind, Instruction};

pub struct JinkFloat(f64);

impl From<f64> for JinkFloat {
    fn from(f: f64) -> Self {
        JinkFloat(f)
    }
}

impl Value for JinkFloat {}

impl Instruction for JinkFloat {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression
    }

    fn print(&self) -> String {
        self.0.to_string()
    }
}
