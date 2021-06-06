//! This module is used to parse external code and make it available to other source
//! files.

use std::path::{Path, PathBuf};

use crate::{parser::Construct, InstrKind, Instruction, Interpreter, JkErrKind, JkError, Rename};

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

    fn format_candidates(&self, base: &Path) -> (PathBuf, PathBuf) {
        let mut format = PathBuf::from(base);
        format.push(&self.path);

        // FIXME: No unwrap
        let format = format.to_str().unwrap().to_string();

        let mut dir_fmt = format.clone();
        let mut file_fmt = format.clone();

        dir_fmt.push_str(&format!("{}", DEFAULT_INCL));
        file_fmt.push_str(".jk");

        (PathBuf::from(dir_fmt), PathBuf::from(file_fmt))
    }

    fn find_include_path(&self, base: &Path) -> Result<PathBuf, JkError> {
        let (dir_candidate, file_candidate) = self.format_candidates(base);

        let (dir_valid, file_valid) = (dir_candidate.is_file(), file_candidate.is_file());

        match (dir_valid, file_valid) {
            // We cannot have both <path>/lib.jk and <path>.jk be valid files
            (true, true) => Err(JkError::new(
                JkErrKind::Interpreter,
                format!(
                    "invalid include: {:?} and {:?} are both valid candidates",
                    dir_candidate, file_candidate
                ),
                None,
                self.print(),
            )),
            (false, false) => Err(JkError::new(
                JkErrKind::Interpreter,
                format!(
                    "no candidate for include: {:?} and {:?} do not exist",
                    dir_candidate, file_candidate
                ),
                None,
                self.print(),
            )),
            (false, true) => Ok(file_candidate),
            (true, false) => Ok(dir_candidate),
        }
    }

    /// Parse the code and load it in the Incl's interpreter
    fn inner_load(
        &self,
        base: &Path,
        i: &Interpreter,
    ) -> Result<(PathBuf, Vec<Box<dyn Instruction>>), JkError> {
        let formatted = self.find_include_path(base)?;

        // If a source has already been included, skip it without returning
        // an error
        if i.is_included(&formatted) {
            return Ok((formatted, vec![]));
        }

        i.debug("FINAL PATH", &format!("{:?}", formatted));

        let input = std::fs::read_to_string(&formatted)?;

        // We can't just parse the input, since it adds the instructions
        // to an entry block in order to execute them. What we can do, is
        // parse many instructions and add them to an empty interpreter
        let (remaining_input, instructions) = Construct::many_instructions(input.as_str())?;
        match remaining_input.len() {
            // The remaining input is empty: We parsed the whole file properly
            0 => Ok((formatted, instructions)),
            _ => Err(JkError::new(
                JkErrKind::Parsing,
                format!(
                    "error when parsing included file: {:?},\non the following input:\n{}",
                    formatted, remaining_input
                ),
                None,
                self.print(),
            )),
        }
    }

    /// Try to load code from the current path where the executable has been launched
    fn load_relative(
        &self,
        base: &Path,
        i: &Interpreter,
    ) -> Result<(PathBuf, Vec<Box<dyn Instruction>>), JkError> {
        self.inner_load(base, i)
    }

    /// Try to load code from jinko's installation path
    fn _load_jinko_path(&self) -> Result<Vec<Box<dyn Instruction>>, JkError> {
        todo!()
    }

    /// Load the source code located at self.path
    ///
    /// There are two ways to look for a source file: First in the includer's path, and
    /// if not available in jinko's installation directory.
    fn load(
        &self,
        base: &Path,
        i: &Interpreter,
    ) -> Result<(PathBuf, Vec<Box<dyn Instruction>>), JkError> {
        self.load_relative(base, i)
    }

    /// Format the correct prefix to include content as. This depends on the presence
    /// of an alias, and also checks for the validity of the prefix
    fn format_prefix(&self) -> Result<String, JkError> {
        let alias = match self.alias.as_ref() {
            Some(alias) => alias,
            // If no alias is given, then return the include path as alias
            None => self.path.as_str(),
        };

        match alias {
            // If the alias is empty, then we're doing a special include from
            // the interpreter itself. Don't add a leading `::`
            "" => Ok(alias.to_string()),
            _ => Ok(format!("{}::", alias)),
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

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JkError> {
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

        let prefix = self.format_prefix()?;

        interpreter.debug("BASE DIR", &format!("{:#?}", base));

        let old_path = interpreter.path().cloned();

        let (new_path, mut content) = self.load(base, interpreter)?;

        // Temporarily change the path of the interpreter
        interpreter.set_path(Some(new_path));

        content
            .iter_mut()
            .map(|instr| {
                instr.prefix(&prefix);

                interpreter.debug("INCLUDING", instr.print().as_str());

                instr.execute(interpreter)
            })
            .collect::<Result<Vec<InstrKind>, JkError>>()?;

        // Reset the old path before leaving the instruction
        interpreter.set_path(old_path);

        Ok(InstrKind::Statement)
    }
}

impl Rename for Incl {
    fn prefix(&mut self, prefix: &str) {
        let alias = match &self.alias {
            None => &self.path,
            Some(alias) => alias,
        };

        self.alias = Some(format!("{}{}", prefix, alias))
    }
}
