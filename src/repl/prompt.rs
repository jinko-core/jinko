//! Creates a prompt based on the interpreter's current status

use crate::interpreter::Interpreter;
use colored::Colorize;

pub struct Prompt;

impl Prompt {
    /// Create the prompt based on the actual interpreter conditions
    pub fn get(_: &Interpreter) -> String {
        format!("jinko {} ", ">".purple())
    }
}
