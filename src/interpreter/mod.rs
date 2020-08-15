//! The broccoli interpreter keeps track of variables and functions, and dispatches calls
//! and references to their correct location. It contains a information regarding the
//! registered functions and variables. It also handles garbage collection. Parsing a
//! source file returns an "Interpreter", which is really just a complex structure
//! aggregating the necessary information to run a broccoli program.

use std::collections::HashMap;

pub struct Interpreter;

impl Interpreter {
    /// Create a new empty interpreter
    pub fn new() -> Interpreter {
        Interpreter {}
    }

    /// Add a function to the interpreter. Returns `Ok` if the function was added, `Err`
    /// if it existed already and was not.
    // FIXME: Add semantics error type
    pub fn add_function(&mut self) -> Result<(), String> {
        todo!()
    }

    /// Add a variable to the interpreter. Returns `Ok` if the variable was added, `Err`
    /// if it existed already and was not.
    // FIXME: Add semantics error type
    pub fn add_variable(&mut self) -> Result<(), String> {
        todo!()
    }
}
