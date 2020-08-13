//! The broccoli interpreter keeps track of variables and functions, and dispatches calls
//! and references to their correct location. It contains a information regarding the
//! registered functions and variables. It also handles garbage collection. Parsing a
//! source file returns an "Interpreter", which is really just a complex structure
//! aggregating the necessary information to run a broccoli program.

use std::collections::HashMap;

pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
        }
    }
}
