//! This module is used to parse external code and make it available to other source
//! files.

use std::path::{Path, PathBuf};

use crate::{
    parser::Construct, ErrKind, Error, InstrKind, Instruction, Interpreter, ObjectInstance, Rename,
};

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
        let mut file_fmt = format;

        dir_fmt.push_str(DEFAULT_INCL.to_string().as_str());
        file_fmt.push_str(".jk");

        (PathBuf::from(dir_fmt), PathBuf::from(file_fmt))
    }

    fn find_include_path(&self, base: &Path) -> Result<PathBuf, Error> {
        let (dir_candidate, file_candidate) = self.format_candidates(base);

        let (dir_valid, file_valid) = (dir_candidate.is_file(), file_candidate.is_file());

        match (dir_valid, file_valid) {
            // We cannot have both <path>/lib.jk and <path>.jk be valid files
            (true, true) => Err(Error::new(ErrKind::Interpreter).with_msg(format!(
                "invalid include: {:?} and {:?} are both valid candidates",
                dir_candidate, file_candidate
            ))),
            (false, false) => Err(Error::new(ErrKind::Interpreter).with_msg(format!(
                "no candidate for include: {:?} and {:?} do not exist",
                dir_candidate, file_candidate
            ))),
            (false, true) => Ok(file_candidate),
            (true, false) => Ok(dir_candidate),
        }
    }

    /// Parse the code and load it in the Incl's interpreter
    fn inner_load(
        &self,
        base: &Path,
        interpreter: &mut Interpreter,
    ) -> Option<(PathBuf, Vec<Box<dyn Instruction>>)> {
        let formatted = match self.find_include_path(base) {
            Ok(f) => f,
            Err(e) => {
                interpreter.error(e);
                return None;
            }
        };

        // If a source has already been included, skip it without returning
        // an error
        if interpreter.is_included(&formatted) {
            return Some((formatted, vec![]));
        }

        interpreter.debug("FINAL PATH", &format!("{:?}", formatted));

        let input = match std::fs::read_to_string(&formatted) {
            Ok(i) => i,
            Err(e) => {
                interpreter.error(Error::from(e));
                return None;
            }
        };

        // We can't just parse the input, since it adds the instructions
        // to an entry block in order to execute them. What we can do, is
        // parse many instructions and add them to an empty interpreter
        let (remaining_input, instructions) = match Construct::many_instructions(input.as_str()) {
            Ok(tuple) => tuple,
            Err(e) => {
                interpreter.error(Error::from(e));
                return None;
            }
        };

        match remaining_input.len() {
            // The remaining input is empty: We parsed the whole file properly
            0 => Some((formatted, instructions)),
            _ => {
                interpreter.error(Error::new(ErrKind::Parsing).with_msg(format!(
                    "error when parsing included file: {:?},\non the following input:\n{}",
                    formatted, remaining_input
                )));
                None
            }
        }
    }

    /// Try to load code from the current path where the executable has been launched
    fn load_relative(
        &self,
        base: &Path,
        i: &mut Interpreter,
    ) -> Option<(PathBuf, Vec<Box<dyn Instruction>>)> {
        self.inner_load(base, i)
    }

    /// Try to load code from jinko's installation path
    fn _load_jinko_path(&self) -> Option<Vec<Box<dyn Instruction>>> {
        todo!()
    }

    /// Load the source code located at self.path
    ///
    /// There are two ways to look for a source file: First in the includer's path, and
    /// if not available in jinko's installation directory.
    fn load(
        &self,
        base: &Path,
        i: &mut Interpreter,
    ) -> Option<(PathBuf, Vec<Box<dyn Instruction>>)> {
        self.load_relative(base, i)
    }

    /// Format the correct prefix to include content as. This depends on the presence
    /// of an alias, and also checks for the validity of the prefix
    fn format_prefix(&self) -> Option<String> {
        let alias = match self.alias.as_ref() {
            Some(alias) => alias,
            // If no alias is given, then return the include path as alias
            None => self.path.as_str(),
        };

        match alias {
            // If the alias is empty, then we're doing a special include from
            // the interpreter itself. Don't add a leading `::`
            "" => Some(alias.to_string()),
            _ => Some(format!("{}::", alias)),
        }
    }

    fn get_base(&self, interpreter: &mut Interpreter) -> PathBuf {
        match interpreter.path() {
            // Get the parent directory of the interpreter's source file. We can unwrap
            // since there's always a base
            Some(path) => path.parent().unwrap().to_owned(),
            // The interpreter doesn't have an associated source file. Therefore, we
            // load from where the interpreter was started. This is the case if we're
            // in dynamic mode for example
            None => PathBuf::new(),
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

    fn execute(&self, interpreter: &mut Interpreter) -> Option<ObjectInstance> {
        interpreter.debug("INCL ENTER", self.print().as_str());

        let base = self.get_base(interpreter);
        let prefix = self.format_prefix()?;

        interpreter.debug("BASE DIR", &format!("{:#?}", base));

        let old_path = interpreter.path().cloned();

        let (new_path, mut content) = self.load(&base, interpreter)?;

        // Temporarily change the path of the interpreter
        interpreter.set_path(Some(new_path));

        content.iter_mut().for_each(|instr| {
            instr.prefix(&prefix);

            interpreter.debug("INCLUDING", instr.print().as_str());

            instr.execute(interpreter);
        });

        // Reset the old path before leaving the instruction
        interpreter.set_path(old_path);

        None
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
