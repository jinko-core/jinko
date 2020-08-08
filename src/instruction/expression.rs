//! Expressions are instructions that return `Something` upon execution

use super::InstrTrait;
use super::ReturnKind;
use crate::value::Value;

#[derive(Clone, Copy)]
pub struct Expression {
    // ret_val: Box<dyn Value<Contained = T>>,
}

impl InstrTrait for Expression {
    fn execute(&self) -> Result<ReturnKind, String> {
        // FIXME: Actually return something
        Ok(ReturnKind::Something)
    }
}
