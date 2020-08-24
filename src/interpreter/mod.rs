//! The broccoli interpreter keeps track of variables and functions, and dispatches calls
//! and references to their correct location. It contains a information regarding the
//! registered functions and variables. It also handles garbage collection. Parsing a
//! source file returns an "Interpreter", which is really just a complex structure
//! aggregating the necessary information to run a broccoli program.

use std::collections::HashMap;

use crate::instruction::{FunctionDec, Var};

pub struct Interpreter {
    /// Is the interpreter in an audit block or not
    pub in_audit: bool,

    /// Functions registered in the interpreter
    pub functions: HashMap<String, FunctionDec>,
    /// Variables registered in the interpreter
    pub variables: HashMap<String, Var>,

    /// Tests registered in the interpreter
    pub tests: HashMap<String, FunctionDec>,
    /// External functions registered in the interpreter
    pub exts: HashMap<String, FunctionDec>,
}

impl Interpreter {
    /// Create a new empty interpreter. Starts in non-audit mode
    pub fn new() -> Interpreter {
        Interpreter {
            in_audit: false,

            functions: HashMap::new(),
            variables: HashMap::new(),

            tests: HashMap::new(),
            exts: HashMap::new(),
        }
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

    /// Pretty-prints valid broccoli code from a given interpreter
    pub fn print(&self) -> String {
        todo!()
    }
}
