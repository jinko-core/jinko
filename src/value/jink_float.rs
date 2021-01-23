//! Represents a floating point number in Jinko. Floating point numbers are always
//! double precision

use super::{Value, ValueType};
use crate::instruction::{InstrKind, Instruction, Operator};
use crate::{Interpreter, JinkoError};

#[derive(Clone)]
pub struct JinkFloat(f64);

impl From<f64> for JinkFloat {
    fn from(f: f64) -> Self {
        JinkFloat(f)
    }
}

impl Value for JinkFloat {
    fn vtype(&self) -> ValueType {
        ValueType::Int
    }

    fn do_op(&self, other: &Self, op: Operator) -> Box<dyn Instruction> {
        match op {
            Operator::Add => Box::new(JinkFloat::from(self.0 + other.0)),
            Operator::Sub => Box::new(JinkFloat::from(self.0 - other.0)),
            Operator::Mul => Box::new(JinkFloat::from(self.0 * other.0)),
            Operator::Div => Box::new(JinkFloat::from(self.0 / other.0)),
            _ => self.no_op(other, op),
        }
    }
}

impl Instruction for JinkFloat {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression(None)
    }

    fn print(&self) -> String {
        self.0.to_string()
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JinkoError> {
        interpreter.debug("FLOAT", &self.0.to_string());

        // FIXME: Add logic
        Ok(InstrKind::Expression(None))
    }
}
