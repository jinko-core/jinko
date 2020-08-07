//! The InstrTrait trait allows the definition of an "instruction". An instruction
//! should be able to execute

use super::return_kind::ReturnKind;

pub trait InstrTrait {
    /// Execute the instruction, returning `Something` or `Nothing` inside a Result<>
    fn execute(&self) -> Result<ReturnKind, String>;
}
