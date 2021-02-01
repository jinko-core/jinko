//! This module is used to parse external code and make it available to other source
//! files.

use std::path::PathBuf;

use crate::{ErrKind, InstrKind, Instruction, Interpreter, JinkoError, Parser};

/// An `Incl` is constituted of a path, an optional alias and contains an interpreter.
/// The interpreter is built from parsing the source file in the path.
/// Aliases are used to potentially rename exported functions.
#[derive(Clone)]
pub struct Incl {
    path: String,
    alias: Option<String>,
    content: Option<Interpreter>,
}

impl Incl {
    pub fn new(path: String, alias: Option<String>) -> Incl {
        Incl {
            path,
            alias,
            content: None,
        }
    }

    /// Rename all contained code to the correct alias
    fn rename(&mut self) {
        todo!("Implement once namespaces are implemented")
    }

    /// Parse the code and load it in the Incl's interpreter
    fn inner_load(&self) -> Result<Interpreter, JinkoError> {
        let mut path = self.path.clone();
        path.push_str(".jk");

        let path = PathBuf::from(path);

        dbg!(&path);

        let input = std::fs::read_to_string(path)?;

        Parser::parse(&input)
    }

    /// Try to load code from the current path where the executable has been launched
    fn load_relative(&self) -> Result<Interpreter, JinkoError> {
        self.inner_load()
    }

    /// Try to load code from jinko's installation path
    fn load_jinko_path(&self) -> Result<Interpreter, JinkoError> {
        todo!()
    }

    /// Load the source code located at self.path
    ///
    /// There are two ways to look for a source file: First in the includer's path, and
    /// if not available in jinko's installation directory.
    fn load(&self) -> Result<Interpreter, JinkoError> {
        self.load_relative()

        // let interpreter = match self.load_relative() {
        //     Ok(i) => Ok(i),
        //     Err(_) => match self.load_jinko_path() {
        //         Ok(i) => Ok(i),
        //         Err(_) => Err(JinkoError::new(
        //             ErrKind::Interpreter,
        //             // FIXME: No debug formatting
        //             format!("couldn't include the following code: {:#?}", self.path),
        //             None,
        //             self.print(),
        //         )),
        //     },
        // };

        // interpreter
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

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JinkoError> {
        // FIXME: Store content at some point
        let mut content = match &self.content {
            None => self.load().unwrap(),
            Some(content) => content.clone(),
        };

        println!("{}", content.print());

        let fns = content.global_functions();
        let vars = content.global_variables();

        // FIXME: No unwrap: Do something similar to Blocks
        fns.into_iter().for_each(|f| {
            interpreter.add_function(f).unwrap();
        });
        vars.into_iter().for_each(|v| {
            interpreter.add_variable(v).unwrap();
        });

        // Execute content globally
        content.entry_point.execute(interpreter)?;

        Ok(InstrKind::Statement)
    }
}
