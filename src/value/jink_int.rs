//! Represents an integer in Jinko. All integers are signed 64 bytes

use super::{JinkFloat, Value, ValueType};
use crate::instruction::{InstrKind, Instruction, Operator};
use crate::{Interpreter, JinkoError};

#[derive(Clone)]
pub struct JinkInt(i64);

impl From<i64> for JinkInt {
    fn from(i: i64) -> Self {
        JinkInt(i)
    }
}

impl Value for JinkInt {
    fn vtype(&self) -> ValueType {
        ValueType::Int
    }

    fn do_op(&self, other: &Self, op: Operator) -> Box<dyn Instruction> {
        match op {
            Operator::Add => box JinkInt::from(self.0 + other.0),
            Operator::Sub => box JinkInt::from(self.0 - other.0),
            Operator::Mul => box JinkInt::from(self.0 * other.0),
            Operator::Div => box JinkInt::from(self.0 / other.0),
            _ => self.no_op(other, op),
        }
    }
}

impl Instruction for JinkInt {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression
    }

    fn print(&self) -> String {
        self.0.to_string()
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<(), JinkoError> {
        // FIXME: Add logic
        interpreter.debug("INT", &self.0.to_string());

        Ok(())
    }
}
