//! The jinko interpreter keeps track of variables and functions, and dispatches calls
//! and references to their correct location. It contains a information regarding the
//! registered functions and variables. It also handles garbage collection. Parsing a
//! source file returns an "Interpreter", which is really just a complex structure
//! aggregating the necessary information to run a jinko program.

use std::path::PathBuf;

use colored::Colorize;

mod scope_map;
use scope_map::ScopeMap;

use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::instruction::{Block, FunctionDec, FunctionKind, Instruction, TypeDec, TypeId, Var};
use crate::{JkErrKind, JkError};

/// Type the interpreter uses for keys
type IKey = String;

/// Name of the entry point in jinko
const ENTRY_NAME: &str = "__entry";

/// An interpreter represents the state of a jinko program. It contains functions,
/// variables, tests... and can be optimized, typechecked, executed or
/// serialized/deserialized to bytecode.
pub struct Interpreter {
    /// Is the interpreter in an audit block or not
    pub in_audit: bool,

    /// Is the interpreter in debugging mode or not
    pub debug_mode: bool,

    /// Entry point to the interpreter, the "main" function
    pub entry_point: FunctionDec,

    /// An interpreter corresponds to a single source file (that might include other files)
    /// We need to keep track of its path in order to load files relative to this one
    path: Option<PathBuf>,

    /// Contains the scopes of the interpreter, in which are variables and functions
    scope_map: ScopeMap,

    /// Tests registered in the interpreter
    tests: HashMap<IKey, FunctionDec>,

    /// Sources included by the interpreter
    included: HashSet<PathBuf>,
}

impl Interpreter {
    fn new_entry() -> FunctionDec {
        let mut ep = FunctionDec::new(String::from(ENTRY_NAME), None);

        ep.set_kind(FunctionKind::Func);
        ep.set_block(Block::new());

        ep
    }

    /// Create a new empty interpreter. Starts in non-audit mode
    pub fn new() -> Interpreter {
        let mut i = Interpreter {
            in_audit: false,
            debug_mode: false,
            entry_point: Self::new_entry(),
            path: None,
            scope_map: ScopeMap::new(),
            tests: HashMap::new(),
            included: HashSet::new(),
        };

        i.scope_enter();

        // Add all primitive types as empty types without fields
        crate::instruction::PRIMITIVE_TYPES
            .iter()
            .for_each(|ty_name| i.add_type(TypeDec::from(*ty_name)).unwrap());

        // Include the standard library
        let stdlib_incl =
            crate::instruction::Incl::new(String::from("stdlib"), Some(String::from("")));
        stdlib_incl
            .execute(&mut i)
            .expect("cannot include jinko's standard library - aborting");

        i
    }

    /// Get a rerference to an interpreter's source path
    pub fn path(&self) -> Option<&PathBuf> {
        self.path.as_ref()
    }

    /// Get a rerference to an interpreter's source path
    pub fn set_path(&mut self, path: Option<PathBuf>) {
        self.path = path;

        match &self.path {
            Some(p) => {
                self.included.insert(p.clone());
            }
            None => {}
        };
    }

    /// Set the debug mode of a previously created interpreter
    pub fn set_debug(&mut self, debug: bool) {
        self.debug_mode = debug
    }

    /// Add a function to the interpreter. Returns `Ok` if the function was added, `Err`
    /// if it existed already and was not.
    pub fn add_function(&mut self, function: FunctionDec) -> Result<(), JkError> {
        self.scope_map.add_function(function)
    }

    /// Add a variable to the interpreter. Returns `Ok` if the variable was added, `Err`
    /// if it existed already and was not.
    pub fn add_variable(&mut self, var: Var) -> Result<(), JkError> {
        self.scope_map.add_variable(var)
    }

    /// Add a type to the interpreter. Returns `Ok` if the type was added, `Err`
    /// if it existed already and was not.
    pub fn add_type(&mut self, custom_type: TypeDec) -> Result<(), JkError> {
        self.scope_map.add_type(custom_type)
    }

    /// Remove a variable from the interpreter
    pub fn remove_variable(&mut self, var: &Var) -> Result<(), JkError> {
        self.scope_map.remove_variable(var)
    }

    /// Replace a variable or create it if it does not exist
    pub fn replace_variable(&mut self, var: Var) -> Result<(), JkError> {
        // Remove the variable if it exists
        let _ = self.remove_variable(&var);

        self.add_variable(var)
    }

    /// Get a mutable reference on an existing function
    pub fn get_function(&self, name: &str) -> Option<&Rc<FunctionDec>> {
        self.scope_map.get_function(name)
    }

    /// Get a reference on an existing variable
    pub fn get_variable(&self, name: &str) -> Option<&Var> {
        self.scope_map.get_variable(name)
    }

    /// Get a reference on an existing type
    pub fn get_type(&self, type_id: &TypeId) -> Option<&Rc<TypeDec>> {
        self.scope_map.get_type(type_id.id())
    }

    /// Create a new empty scope
    pub fn scope_enter(&mut self) {
        self.scope_map.scope_enter()
    }

    /// Exit the latest created scope
    pub fn scope_exit(&mut self) {
        self.scope_map.scope_exit()
    }

    /// Enter audit mode
    pub fn audit_enter(&mut self) {
        self.in_audit = true;
    }

    /// Exit audit mode
    pub fn audit_exit(&mut self) {
        self.in_audit = false;
    }

    /// Pretty-prints valid jinko code from a given interpreter
    pub fn print(&self) -> String {
        self.scope_map.print();
        self.entry_point.print()
    }

    /// Print a debug message if the interpreter is in debug mode, according to the
    /// following format:
    ///
    /// `<specifier>: <msg>`
    pub fn debug(&self, specifier: &str, msg: &str) {
        if self.debug_mode {
            println!("{}: {}", specifier.purple(), msg);
        }
    }

    /// Print a debugging step if the interpreter is in debug mode, according to the
    /// following format:
    ///
    /// `<specifier>`
    ///
    /// This is used to indicate "steps" the interpreter is taking where adding a
    /// secondary format is not necesarry. For example, when entering a block: There's
    /// no way to name a block, so no necessity to have more information other than
    /// "ENTER_BLOCK"
    pub fn debug_step(&self, specifier: &str) {
        if self.debug_mode {
            println!("{}", specifier.yellow());
        }
    }

    /// Register a test to be executed by the interpreter
    pub fn add_test(&mut self, test: FunctionDec) -> Result<(), JkError> {
        match self.tests.get(test.name()) {
            Some(test) => Err(JkError::new(
                JkErrKind::Interpreter,
                format!("test function already declared: {}", test.name()),
                None,
                test.name().to_owned(),
            )),
            None => {
                self.tests.insert(test.name().to_owned(), test);
                Ok(())
            }
        }
    }

    /// Check if a source is included or not
    pub fn is_included(&self, source: &PathBuf) -> bool {
        self.included.contains(source)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::JkErrKind;

    #[test]
    fn t_redefinition_of_function() {
        let f0 = FunctionDec::new("f0".to_owned(), None);
        let f0_copy = FunctionDec::new("f0".to_owned(), None);

        let mut i = Interpreter::new();

        assert_eq!(i.add_function(f0), Ok(()));
        assert_eq!(
            i.add_function(f0_copy).err().unwrap().kind(),
            JkErrKind::Interpreter,
        );
    }

    #[test]
    fn t_redefinition_of_variable() {
        let v0 = Var::new("v0".to_owned());
        let v0_copy = Var::new("v0".to_owned());

        let mut i = Interpreter::new();

        assert_eq!(i.add_variable(v0), Ok(()));
        assert_eq!(
            i.add_variable(v0_copy).err().unwrap().kind(),
            JkErrKind::Interpreter,
        );
    }
}
