//! This module is used to parse external code and make it available to other source
//! files.

use std::path::PathBuf;

use crate::{ErrKind, InstrKind, Instruction, Interpreter, JinkoError, Parser};

/// An `Incl` is constituted of a path, an optional alias and contains an interpreter.
/// The interpreter is built from parsing the source file in the path.
/// Aliases are used to potentially rename exported functions.
#[derive(Clone)]
pub struct Incl {
    path: PathBuf,
    alias: Option<String>,
    content: Option<Interpreter>,
}

impl Incl {
    pub fn new(path: PathBuf, alias: Option<String>) -> Incl {
        Incl {
            path, alias, content: None,
        }
    }

    /// Rename all contained code to the correct alias
    fn rename(&mut self) {
        todo!("Implement once namespaces are implemented")
    }

    /// Parse the code and load it in the Incl's interpreter
    fn inner_load(&mut self) -> Result<Interpreter, JinkoError> {
        self.path.push(".jk");

        let input = std::fs::read_to_string(&self.path)?;

        eprintln!("Path: {:#?}", self.path);

        Parser::parse(&input)
    }

    /// Try to load code from the current path where the executable has been launched
    fn load_relative(&mut self) -> Result<Interpreter, JinkoError> {
        self.inner_load()
    }

    /// Try to load code from jinko's installation path
    fn load_jinko_path(&mut self) -> Result<Interpreter, JinkoError> {
        todo!()
    }

    /// Load the source code located at self.path
    ///
    /// There are two ways to look for a source file: First in the includer's path, and
    /// if not available in jinko's installation directory.
    fn load(&mut self) -> Result<(), JinkoError> {
        let interpreter = match self.load_relative() {
            Ok(i) => Ok(i),
            Err(_) => match self.load_jinko_path() {
                Ok(i) => Ok(i),
                Err(_) => Err(JinkoError::new(
                    ErrKind::Interpreter,
                    // FIXME: No debug formatting
                    format!("couldn't include the following code: {:#?}", self.path),
                    None,
                    self.print(),
                )),
            },
        };

        match interpreter {
            Ok(i) => {
                self.content = Some(i);
                Ok(())
            }
            Err(e) => Err(e),
        }
    }
}

impl Instruction for Incl {
    fn kind(&self) -> InstrKind {
        InstrKind::Statement
    }

    fn print(&self) -> String {
        use std::ffi::OsStr;

        let path: &OsStr = self.path.as_ref();
        // FIXME: No unwrap
        let mut base = format!("incl {}", path.to_str().unwrap());

        base = match &self.alias {
            Some(alias) => format!("{} as {}", base, alias),
            None => base,
        };

        base
    }

    fn execute(&self, _interpreter: &mut Interpreter) -> Result<InstrKind, JinkoError> {
        todo!()
    }
}
