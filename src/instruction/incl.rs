//! This module is used to parse external code and make it available to other source
//! files.

use std::path::{Path, PathBuf};

use crate::{parser::Construct, InstrKind, Instruction, Interpreter, JinkoError};

/// An `Incl` is constituted of a path, an optional alias and contains an interpreter.
/// The interpreter is built from parsing the source file in the path.
/// Aliases are used to potentially rename exported functions.
#[derive(Clone)]
pub struct Incl {
    path: String,
    alias: Option<String>,
}

/// Default file that gets included when including a directory in jinko source code
const DEFAULT_INCL: &str = "/lib.jk";

impl Incl {
    pub fn new(path: String, alias: Option<String>) -> Incl {
        Incl { path, alias }
    }

    /// Rename all contained code to the correct alias
    fn _rename(&mut self) {
        todo!("Implement once namespaces are implemented")
    }

    fn format_path(&self, base: &Path) -> Result<PathBuf, JinkoError> {
        let mut formatted = PathBuf::from(base);
        let mut formatted_check = formatted.clone();

        let mut path = self.path.clone();
        formatted_check.push(&path);

        // We now face two choices: Either the required path is a folder, and then
        // we need to include the `lib.jk` file inside that folder. Either `<path>.jk` is
        // a file and then we include it. If both are present, error out appropriately.
        match formatted_check.is_dir() {
            true => path.push_str(DEFAULT_INCL),
            false => path.push_str(".jk"),
        };

        // Add the path of the module to load
        formatted.push(&path);

        Ok(formatted)
    }

    /// Parse the code and load it in the Incl's interpreter
    fn inner_load(
        &self,
        base: &Path,
        i: &Interpreter,
    ) -> Result<Vec<Box<dyn Instruction>>, JinkoError> {
        let formatted = self.format_path(base)?;

        i.debug("FINAL PATH", &format!("{:?}", formatted));
        i.debug(
            "CURRENT DIR",
            &format!("{:?}", std::env::current_dir().unwrap()),
        );

        let input = std::fs::read_to_string(formatted)?;

        // We can't just parse the input, since it adds the instructions
        // to an entry block in order to execute them. What we can do, is
        // parse many instructions and add them to an empty interpreter
        let (_, instructions) = Construct::many_instructions(input.as_str())?;

        Ok(instructions)
    }

    /// Try to load code from the current path where the executable has been launched
    fn load_relative(
        &self,
        base: &Path,
        i: &Interpreter,
    ) -> Result<Vec<Box<dyn Instruction>>, JinkoError> {
        self.inner_load(base, i)
    }

    /// Try to load code from jinko's installation path
    fn _load_jinko_path(&self) -> Result<Vec<Box<dyn Instruction>>, JinkoError> {
        todo!()
    }

    /// Load the source code located at self.path
    ///
    /// There are two ways to look for a source file: First in the includer's path, and
    /// if not available in jinko's installation directory.
    fn load(&self, base: &Path, i: &Interpreter) -> Result<Vec<Box<dyn Instruction>>, JinkoError> {
        self.load_relative(base, i)

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

        let base = match interpreter.path() {
            // Get the parent directory of the interpreter's source file. We can unwrap
            // since there's always a base
            Some(path) => path.parent().unwrap(),
            // The interpreter doesn't have an associated source file. Therefore, we
            // load from where the interpreter was started. This is the case if we're
            // in dynamic mode for example
            None => Path::new(""),
        };

        interpreter.debug("BASE DIR", &format!("{:#?}", base));

        let content = self.load(base, interpreter)?;

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
