//! This module is used to parse external code and make it available to other source
//! files.

use std::path::PathBuf;

use crate::{parser::Construct, InstrKind, Instruction, Interpreter, JinkoError};

/// An `Incl` is constituted of a path, an optional alias and contains an interpreter.
/// The interpreter is built from parsing the source file in the path.
/// Aliases are used to potentially rename exported functions.
#[derive(Clone)]
pub struct Incl {
    path: String,
    alias: Option<String>,
}

impl Incl {
    pub fn new(path: String, alias: Option<String>) -> Incl {
        Incl { path, alias }
    }

    /// Rename all contained code to the correct alias
    fn rename(&mut self) {
        todo!("Implement once namespaces are implemented")
    }

    /// Parse the code and load it in the Incl's interpreter
    fn inner_load(&self) -> Result<Vec<Box<dyn Instruction>>, JinkoError> {
        let mut path = self.path.clone();
        path.push_str(".jk");

        let path = PathBuf::from(path);

        let input = std::fs::read_to_string(path)?;

        // We can't just parse the input, since it adds the instructions
        // to an entry block in order to execute them. What we can do, is
        // parse many instructions and add them to an empty interpreter
        let (_, instructions) = Construct::many_instructions(input.as_str())?;

        Ok(instructions)
    }

    /// Try to load code from the current path where the executable has been launched
    fn load_relative(&self) -> Result<Vec<Box<dyn Instruction>>, JinkoError> {
        self.inner_load()
    }

    /// Try to load code from jinko's installation path
    fn load_jinko_path(&self) -> Result<Vec<Box<dyn Instruction>>, JinkoError> {
        todo!()
    }

    /// Load the source code located at self.path
    ///
    /// There are two ways to look for a source file: First in the includer's path, and
    /// if not available in jinko's installation directory.
    fn load(&self) -> Result<Vec<Box<dyn Instruction>>, JinkoError> {
        self.load_relative()

        // let interpreter = match self.load_relative() {
        //     Ok(i) => Ok(i),
        //     Err(_) => match self.load_jinko_path() {
        //         Ok(i) => Ok(i),
        //         Err(_) => Err(JinkoError::new(
        //             ErrKind::Vec<Box<dyn Instruction>>,
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
        interpreter.debug("INCL ENTER", &format!("{}", self.print()));

        let content = self.load()?;

        content
            .into_iter()
            .map(|instr| {
                interpreter.debug("INCLUDING", instr.print().as_str());
                instr.execute(interpreter)
            })
            .collect::<Result<Vec<InstrKind>, JinkoError>>()?;

        Ok(InstrKind::Statement)
    }
}
