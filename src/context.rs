//! The jinko context keeps track of variables and functions, and dispatches calls
//! and references to their correct location. It contains information regarding the
//! registered functions and variables. It also handles garbage collection. Parsing a
//! source file returns a "Context", which is really just a complex structure
//! aggregating the necessary information to run a jinko program.

use std::path::{Path, PathBuf};

use colored::Colorize;

mod scope_map;
use scope_map::ScopeMap;

use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::error::{ErrKind, Error, ErrorHandler};
use crate::instruction::{Block, FunctionDec, FunctionKind, Instruction, TypeDec, TypeId, Var};
use crate::{ObjectInstance, TypeCheck};

/// Type the context uses for keys
type CtxKey = String;

/// Name of the entry point in jinko
const ENTRY_NAME: &str = "__entry";

// FIXME: Rework visibility here
/// A context represents the state of a jinko program. It contains functions,
/// variables, tests... and can be optimized, typechecked, executed or
/// serialized/deserialized to bytecode.
pub struct Context {
    /// Is the context in debugging mode or not
    pub debug_mode: bool,

    /// Entry point to the context, the "main" function
    pub entry_point: FunctionDec,

    /// A context corresponds to a single source file (that might include other files)
    /// We need to keep track of its path in order to load files relative to this one
    path: Option<PathBuf>,

    /// Contains the scopes of the context, in which are variables and functions
    scope_map: ScopeMap,

    /// Tests registered in the context
    tests: HashMap<CtxKey, FunctionDec>,

    /// Sources included by the context
    included: HashSet<PathBuf>,

    /// Errors being kept by the context
    pub(crate) error_handler: ErrorHandler,
}

impl Default for Context {
    fn default() -> Context {
        Context::new()
    }
}

impl Context {
    fn new_entry() -> FunctionDec {
        let mut ep = FunctionDec::new(String::from(ENTRY_NAME), None);

        ep.set_kind(FunctionKind::Func);
        ep.set_block(Block::new());

        ep
    }

    /// Create a new empty context. Starts in non-audit mode
    pub fn new() -> Context {
        let mut ctx = Context {
            debug_mode: false,
            entry_point: Self::new_entry(),
            path: None,
            scope_map: ScopeMap::new(),
            tests: HashMap::new(),
            included: HashSet::new(),
            error_handler: ErrorHandler::default(),
        };

        ctx.scope_enter();

        // Add all primitive types as empty types without fields
        crate::instruction::PRIMITIVE_TYPES
            .iter()
            .for_each(|ty_name| ctx.add_type(TypeDec::from(*ty_name)).unwrap());

        // Include the standard library
        let stdlib_incl =
            crate::instruction::Incl::new(String::from("stdlib"), Some(String::from("")));
        stdlib_incl.execute(&mut ctx);

        ctx
    }

    /// Get a reference to a context's source path
    pub fn path(&self) -> Option<&PathBuf> {
        self.path.as_ref()
    }

    /// Get a reference to a context's source path
    pub fn set_path(&mut self, path: Option<PathBuf>) {
        self.path = path;
        // FIXME: Is that correct? Remove that clone()...
        self.error_handler
            .set_path(self.path.clone().unwrap_or_default());

        match &self.path {
            Some(p) => {
                self.included.insert(p.clone());
            }
            None => {}
        };
    }

    /// Add an error to the context
    pub fn error(&mut self, err: Error) {
        self.error_handler.add(err)
    }

    /// Emit all the errors currently kept in the context and remove them
    pub fn emit_errors(&mut self) {
        self.error_handler.emit();
    }

    /// Clear all the errors currently kept in the context and remove them
    pub fn clear_errors(&mut self) {
        self.error_handler.clear();
    }

    /// Set the debug mode of a previously created ctx
    pub fn set_debug(&mut self, debug: bool) {
        self.debug_mode = debug
    }

    /// Add a function to the context. Returns `Ok` if the function was added, `Err`
    /// if it existed already and was not.
    pub fn add_function(&mut self, function: FunctionDec) -> Result<(), Error> {
        self.scope_map.add_function(function)
    }

    /// Add a variable to the context. Returns `Ok` if the variable was added, `Err`
    /// if it existed already and was not.
    pub fn add_variable(&mut self, var: Var) -> Result<(), Error> {
        self.scope_map.add_variable(var)
    }

    /// Add a type to the context. Returns `Ok` if the type was added, `Err`
    /// if it existed already and was not.
    pub fn add_type(&mut self, custom_type: TypeDec) -> Result<(), Error> {
        self.scope_map.add_type(custom_type)
    }

    /// Remove a variable from the context
    pub fn remove_variable(&mut self, var: &Var) -> Result<(), Error> {
        self.scope_map.remove_variable(var)
    }

    /// Replace a variable or create it if it does not exist
    pub fn replace_variable(&mut self, var: Var) -> Result<(), Error> {
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

    /// Pretty-prints valid jinko code from a given ctx
    pub fn print(&self) -> String {
        self.scope_map.print();
        self.entry_point.print()
    }

    /// Print a debug message if the context is in debug mode, according to the
    /// following format:
    ///
    /// `<specifier>: <msg>`
    pub fn debug(&self, specifier: &str, msg: &str) {
        if self.debug_mode {
            println!("{}: {}", specifier.purple(), msg);
        }
    }

    /// Print a debugging step if the context is in debug mode, according to the
    /// following format:
    ///
    /// `<specifier>`
    ///
    /// This is used to indicate "steps" the context is taking where adding a
    /// secondary format is not necesarry. For example, when entering a block: There's
    /// no way to name a block, so no necessity to have more information other than
    /// "ENTER_BLOCK"
    pub fn debug_step(&self, specifier: &str) {
        if self.debug_mode {
            println!("{}", specifier.yellow());
        }
    }

    /// Register a test to be executed by the context
    pub fn add_test(&mut self, test: FunctionDec) -> Result<(), Error> {
        match self.tests.get(test.name()) {
            Some(test) => Err(Error::new(ErrKind::Context)
                .with_msg(format!("test function already declared: {}", test.name()))),
            None => {
                self.tests.insert(test.name().to_owned(), test);
                Ok(())
            }
        }
    }

    /// Check if a source is included or not
    pub fn is_included(&self, source: &Path) -> bool {
        self.included.contains(source)
    }

    pub fn execute(&mut self) -> Result<Option<ObjectInstance>, Error> {
        // The entry point always has a block
        let ep = self.entry_point.block().unwrap().clone();

        ep.resolve_type(self);
        let res = ep.execute(self);

        self.emit_errors();

        match self.error_handler.has_errors() {
            true => Err(Error::new(ErrKind::Context)),
            false => Ok(res),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_redefinition_of_function() {
        let f0 = FunctionDec::new("f0".to_owned(), None);
        let f0_copy = FunctionDec::new("f0".to_owned(), None);

        let mut i = Context::new();

        assert_eq!(i.add_function(f0), Ok(()));
        assert!(i.add_function(f0_copy).is_err());
    }

    #[test]
    fn t_redefinition_of_variable() {
        let v0 = Var::new("v0".to_owned());
        let v0_copy = Var::new("v0".to_owned());

        let mut i = Context::new();

        assert_eq!(i.add_variable(v0), Ok(()));
        assert!(i.add_variable(v0_copy).is_err());
    }
}
