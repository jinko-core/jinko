//! The jinko context keeps track of variables and functions, and dispatches calls
//! and references to their correct location. It contains information regarding the
//! registered functions and variables. It also handles garbage collection. Parsing a
//! source file returns a "Context", which is really just a complex structure
//! aggregating the necessary information to run a jinko program.

use std::path::{Path, PathBuf};

use colored::Colorize;

mod scope_map;
pub use scope_map::{Scope, ScopeMap};

use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::rc::Rc;

use crate::builtins::Builtins;
use crate::error::{ErrKind, Error, ErrorHandler};
use crate::instance::ObjectInstance;
use crate::instruction::{Block, FunctionDec, FunctionKind, Instruction, TypeDec, Var};
use crate::io_trait::JkReader;
use crate::parser;
use crate::typechecker::CheckedType;
use crate::typechecker::{SpecializedNode, TypeCheck, TypeCtx, TypeId};

/// Type the context uses for keys
type CtxKey = String;

/// Name of the entry point in jinko
const ENTRY_NAME: &str = "__entry";

// FIXME: Rework visibility here
/// A context represents the state of a jinko program. It contains functions,
/// variables, tests... and can be optimized, typechecked, executed or
/// serialized/deserialized to bytecode.
pub struct Context {
    /// A context corresponds to a single source file (that might include other files)
    /// We need to keep track of its path in order to load files relative to this one
    path: Option<PathBuf>,
    /// Arguments given to the jinko program
    args: Vec<String>,
    /// Contains the functions shipping with the interpreter
    builtins: Builtins,
    /// Tests registered in the context
    tests: HashMap<CtxKey, FunctionDec>,
    /// Sources included by the context
    included: HashSet<PathBuf>,
    /// External libraries to use via FFI
    #[cfg(feature = "ffi")]
    external_libs: Vec<libloading::Library>,
    /// Contains the scopes of the context, in which are variables and functions
    pub(crate) scope_map: ScopeMap<Var, Rc<FunctionDec>, Rc<TypeDec>>,
    /// Various passes ran by the context
    pub(crate) typechecker: TypeCtx,
    /// Is the context in debugging mode or not
    pub debug_mode: bool,
    /// Source code currently being interpreted by the context
    pub code: Option<String>,
    /// Entry point to the context, the "main" function
    pub entry_point: FunctionDec,
    /// Errors being kept by the context
    pub error_handler: ErrorHandler,
}

impl Context {
    fn new_entry() -> FunctionDec {
        let mut ep = FunctionDec::new(String::from(ENTRY_NAME), None, vec![], vec![]);

        ep.set_kind(FunctionKind::Func);
        ep.set_block(Block::new());

        ep
    }

    /// Create a new empty context without the standard library
    pub fn new(reader: Box<dyn JkReader>) -> Context {
        let mut ctx = Context {
            path: None,
            args: Vec::new(),
            builtins: Builtins::new(),
            tests: HashMap::new(),
            included: HashSet::new(),
            #[cfg(feature = "ffi")]
            external_libs: Vec::new(),
            scope_map: ScopeMap::new(),
            typechecker: TypeCtx::new(reader),
            debug_mode: false,
            code: None,
            entry_point: Self::new_entry(),
            error_handler: ErrorHandler::default(),
        };

        ctx.scope_enter();

        // Add all primitive types as empty types without fields
        crate::typechecker::PRIMITIVE_TYPES
            .iter()
            .for_each(|ty_name| ctx.add_type(TypeDec::from(*ty_name)).unwrap());

        ctx
    }

    /// Get a reference to a context's source path
    pub fn path(&self) -> Option<&PathBuf> {
        self.path.as_ref()
    }

    /// Get a reference to the arguments to give to the program
    pub fn args(&self) -> &Vec<String> {
        &self.args
    }

    /// Set the arguments to give to the program
    pub fn set_args(&mut self, args: Vec<String>) {
        self.args = args;
    }

    /// Set the source code that the context should refer to
    pub fn set_code(&mut self, code: String) {
        self.code = Some(code)
    }

    /// Get a reference to a context's source path
    pub fn set_path(&mut self, path: Option<PathBuf>) {
        // FIXME: Remove that clone...
        self.path = path.clone();
        self.typechecker.set_path(path);

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

    /// Add a function to the context. Returns `Ok` if the function was added, `Err`
    /// if it existed already and was not.
    pub fn add_function(&mut self, function: FunctionDec) -> Result<(), Error> {
        self.scope_map
            .add_function(function.name().to_owned(), Rc::new(function))
    }

    /// Add a variable to the context. Returns `Ok` if the variable was added, `Err`
    /// if it existed already and was not.
    pub fn add_variable(&mut self, var: Var) -> Result<(), Error> {
        self.scope_map.add_variable(var.name().to_owned(), var)
    }

    /// Add a type to the context. Returns `Ok` if the type was added, `Err`
    /// if it existed already and was not.
    pub fn add_type(&mut self, custom_type: TypeDec) -> Result<(), Error> {
        self.scope_map
            .add_type(custom_type.name().to_owned(), Rc::new(custom_type))
    }

    /// Replace a variable or create it if it does not exist
    pub fn replace_variable(&mut self, var: Var) -> Result<(), Error> {
        match self.scope_map.get_variable_mut(var.name()) {
            None => self.add_variable(var)?,
            Some(var_ref) => var_ref.set_instance(var.instance()),
        }

        Ok(())
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
        let mut s = format!("{}\n", self.scope_map);
        s = format!("{}{}", s, self.entry_point.print());

        s
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

    pub fn remove_included(&mut self, source: &Path) {
        self.included.remove(source);
    }

    pub fn type_check(&mut self, instruction: &mut dyn Instruction) -> Result<CheckedType, Error> {
        let res = instruction.type_of(&mut self.typechecker);

        self.error_handler
            .append(&mut self.typechecker.error_handler);

        match self.error_handler.has_errors() {
            true => Err(Error::new(ErrKind::TypeChecker)),
            false => Ok(res?),
        }
    }

    fn inner_check(&mut self, ep: &mut Block) -> Result<(), Error> {
        self.scope_enter();

        ep.type_of(&mut self.typechecker)?;

        self.error_handler
            .append(&mut self.typechecker.error_handler);
        self.emit_errors();

        let new_nodes = self.typechecker.take_specialized_nodes();
        new_nodes.into_iter().for_each(|node| {
            if let Err(e) = match node {
                SpecializedNode::Func(f) => self.add_function(*f),
                SpecializedNode::Type(t) => self.add_type(t),
            } {
                self.error(e);
            }
        });

        match self.error_handler.has_errors() {
            true => Err(Error::new(ErrKind::Context)),
            false => Ok(()),
        }
    }

    pub fn check(&mut self) -> Result<(), Error> {
        // The entry point always has a block
        let mut ep = self.entry_point.block().unwrap().clone();
        self.inner_check(&mut ep)
    }

    pub fn execute(&mut self) -> Result<Option<ObjectInstance>, Error> {
        // The entry point always has a block
        let mut ep = self.entry_point.block().unwrap().clone();
        self.inner_check(&mut ep)?;

        let res = ep
            .instructions()
            .iter()
            .map(|inst| inst.execute(self))
            .last()
            .flatten();

        self.emit_errors();

        match self.error_handler.has_errors() {
            true => Err(Error::new(ErrKind::Context)),
            false => Ok(res),
        }
    }

    pub fn eval(&mut self, input: &str) -> Result<Option<ObjectInstance>, Error> {
        self.entry_point = Context::new_entry();

        parser::parse(self, input, None)?;

        self.execute()
    }

    pub fn has_errors(&self) -> bool {
        self.error_handler.has_errors()
    }

    pub fn is_builtin(&self, name: &str) -> bool {
        self.builtins.contains(name)
    }

    pub fn call_builtin(
        &mut self,
        builtin: &str,
        args: Vec<Box<dyn Instruction>>,
    ) -> Result<Option<ObjectInstance>, Error> {
        match self.builtins.get(builtin) {
            Some(f) => Ok(f(self, args)),
            None => Err(Error::new(ErrKind::Context)),
        }
    }

    /// Add a library to the interpreter
    #[cfg(feature = "ffi")]
    pub fn add_lib(&mut self, lib: libloading::Library) {
        self.external_libs.push(lib)
    }

    /// Get a reference on all shared libraries loaded in the interpreter
    #[cfg(feature = "ffi")]
    pub fn libs(&self) -> &Vec<libloading::Library> {
        &self.external_libs
    }

    /// Includes the standard library in the context
    pub fn init_stdlib(&mut self) -> Result<(), Error> {
        let mut stdlib_incl =
            crate::instruction::Incl::new(String::from("stdlib"), Some(String::from("")));
        stdlib_incl.set_base(PathBuf::new());

        self.entry_point.add_instruction(Box::new(stdlib_incl))?;

        Ok(())
    }

    /// Get a reference to all tests contained in the context
    pub fn tests(&self) -> &HashMap<CtxKey, FunctionDec> {
        &self.tests
    }
}

/// Printer for the context's usage of the ScopeMap
impl<V: Instruction, F: Instruction, T: Instruction> Display for ScopeMap<V, Rc<F>, Rc<T>> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        for stack in self.scopes() {
            writeln!(f, "{}", stack)?;
        }

        Ok(())
    }
}

impl<V: Instruction, F: Instruction, T: Instruction> Display for Scope<V, Rc<F>, Rc<T>> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        for ty in self.types.values() {
            writeln!(f, "{}", ty.print())?;
        }

        for var in self.variables.values() {
            writeln!(f, "{}", var.print())?;
        }

        for func in self.functions.values() {
            writeln!(f, "{}", func.print())?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jinko;

    #[test]
    fn t_redefinition_of_function() {
        let f0 = FunctionDec::new("f0".to_owned(), None, vec![], vec![]);
        let f0_copy = FunctionDec::new("f0".to_owned(), None, vec![], vec![]);

        let mut i = Context::new(Box::new(crate::io_trait::JkStdReader));

        assert_eq!(i.add_function(f0), Ok(()));
        assert!(i.add_function(f0_copy).is_err());
    }

    #[test]
    fn t_redefinition_of_variable() {
        let v0 = Var::new("v0".to_owned());
        let v0_copy = Var::new("v0".to_owned());

        let mut i = Context::new(Box::new(crate::io_trait::JkStdReader));

        assert_eq!(i.add_variable(v0), Ok(()));
        assert!(i.add_variable(v0_copy).is_err());
    }

    #[test]
    fn t_print_scopemap() {
        let mut ctx = Context::new(Box::new(crate::io_trait::JkStdReader));
        ctx.init_stdlib().unwrap();
        ctx.execute().unwrap();

        let output = format!("{}", ctx.scope_map);

        // Let's make sure the output contains type declarations and functions
        assert!(output.contains("type"));
        assert!(output.contains("func"));
    }

    #[test]
    fn t_print_eb() {
        let mut ctx = Context::new(Box::new(crate::io_trait::JkStdReader));

        ctx.add_variable(Var::new(String::from("a_var_named_a")))
            .unwrap();

        let output = ctx.print();

        // Let's make sure the output contains type declarations and functions
        assert!(output.contains("type"));
        assert!(output.contains("func"));
        assert!(output.contains("a_var_named_a"));
    }

    #[test]
    fn t_call_builtin() {
        jinko! {
            "hey".__builtin_string_len();
        };
    }

    #[test]
    fn t_eval() {
        let mut ctx = Context::new(Box::new(crate::io_trait::JkStdReader));
        let input = String::from("my_var = 1");

        ctx.eval(&input).unwrap();
        let output = ctx.print();

        assert!(output.contains("my_var"));
    }

    #[test]
    fn t_double_eval() {
        let mut ctx = Context::new(Box::new(crate::io_trait::JkStdReader));

        let mut input = String::from("my_var = 1");
        ctx.eval(&input).unwrap();

        input = String::from("my_new_var = 2");
        ctx.eval(&input).unwrap();

        let output = ctx.print();

        assert!(output.contains("my_var"));
        assert!(output.contains("my_new_var"));
    }
}
