//! Creates a prompt based on the interpreter's current status

use crate::interpreter::Interpreter;
use colored::Colorize;

pub struct Prompt;

impl Prompt {
    /// Create the prompt based on the actual interpreter conditions
    pub fn get(interpreter: &Interpreter) -> String {
        let base = if interpreter
            .entry_point
            .block()
            .unwrap()
            .instructions()
            .len()
            > 1
        {
            format!("audit")
        } else {
            format!("jinko")
        };

        format!("{} {} ", base, ">".purple())
    }
}
