//! Creates a prompt based on the context's current status

use crate::context::Context;
use colored::Colorize;

pub struct Prompt;

impl Prompt {
    /// Create the prompt based on the actual context conditions
    pub fn get(_: &Context) -> String {
        format!("jinko {} ", ">".purple())
    }
}
