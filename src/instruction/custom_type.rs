use super::{DecArg, InstrKind, Instruction};

use crate::error::JinkoError;
use crate::interpreter::Interpreter;

#[derive(Clone)]
pub struct CustomType {
    name: String,
    fields: Vec<DecArg>,
}

impl CustomType {
    pub fn new(name: String, fields: Vec<DecArg>) -> CustomType {
        CustomType { name, fields }
    }
}

impl Instruction for CustomType {
    fn kind(&self) -> InstrKind {
        InstrKind::Statement
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<(), JinkoError> {
        interpreter.debug_step(&format!("CUSTOM TYPE {} ENTER", self.name));

        //TODO

        interpreter.debug_step(&format!("CUSTOM TYPE {} EXIT", self.name));

        Ok(())
    }

    fn print(&self) -> String {
        format!("type {} ( {:?} )", self.name, self.fields)
    }
}
