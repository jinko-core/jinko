//! This module is used to parse external code and make it available to other source
//! files.

use std::path::{Path, PathBuf};

use nom_locate::LocatedSpan;

use crate::{
    log,
    parser::constructs,
    typechecker::{CheckedType, TypeCtx},
    Context, ErrKind, Error, Generic, InstrKind, Instruction, ObjectInstance, TypeCheck,
};

/// An `Incl` is constituted of a path, an optional alias and contains a context.
/// The ctx is built from parsing the source file in the path.
/// Aliases are used to potentially rename exported functions.
#[derive(Clone)]
pub struct Incl {
    path: String,
    alias: Option<String>,
    base: Option<PathBuf>,
    typechecked: bool,
    instructions: Vec<Box<dyn Instruction>>,
}

/// Default file that gets included when including a directory in jinko source code
const DEFAULT_INCL: &str = "lib.jk";

impl Incl {
    pub fn new(path: String, alias: Option<String>) -> Incl {
        Incl {
            path,
            alias,
            base: None,
            typechecked: false,
            instructions: vec![],
        }
    }

    fn fetch_instructions(&self, formatted: &Path) -> Result<Vec<Box<dyn Instruction>>, Error> {
        log!("final path: {}", &format!("{:?}", formatted));

        let input = std::fs::read_to_string(&formatted)?;

        // We can't just parse the input, since it adds the instructions
        // to an entry block in order to execute them. What we can do, is
        // parse many instructions and add them to an empty ctx
        let (remaining_input, instructions) =
            constructs::many_expr(LocatedSpan::new(input.as_str()))?;

        match remaining_input.len() {
            // The remaining input is empty: We parsed the whole file properly
            0 => Ok(instructions),
            _ => Err(Error::new(ErrKind::Parsing).with_msg(format!(
                "error when parsing included file: {:?},\non the following input:\n{}",
                formatted, remaining_input
            ))),
        }
    }

    pub fn set_base(&mut self, path: PathBuf) {
        self.base = Some(path);
    }

    fn check_base(&self, base: &Path) -> Result<PathBuf, Error> {
        let (mut dir_candidate, mut file_candidate) = (
            PathBuf::from(base)
                .join(self.path.clone())
                .join(DEFAULT_INCL),
            PathBuf::from(base).join(self.path.clone()),
        );
        dir_candidate.set_extension("jk");
        file_candidate.set_extension("jk");

        match (dir_candidate.is_file(), file_candidate.is_file()) {
            // We cannot have both <path>/lib.jk and <path>.jk be valid files
            (true, true) => Err(Error::new(ErrKind::Context).with_msg(format!(
                "invalid include: {:?} and {:?} are both valid candidates",
                dir_candidate, file_candidate
            ))),
            (false, false) => Err(Error::new(ErrKind::Context).with_msg(format!(
                "no candidate for include: {:?} and {:?} do not exist",
                dir_candidate, file_candidate
            ))),
            (false, true) => Ok(file_candidate),
            (true, false) => Ok(dir_candidate),
        }
    }

    fn load_home_library(&self) -> Result<PathBuf, Error> {
        let home = std::env::var("HOME")?;
        let home_base = PathBuf::from(format!("{}/.jinko/libs/", home));

        self.check_base(&home_base)
    }

    fn load_local_library(&self, base: &Path) -> Result<PathBuf, Error> {
        self.check_base(base)
    }

    pub fn get_final_path(&self, base: &Path) -> Result<PathBuf, (Error, Error)> {
        // Check the local path first
        let local_err = match self.load_local_library(base) {
            Ok(path) => return Ok(path),
            Err(e) => e,
        };

        let home_err = match self.load_home_library() {
            Ok(path) => return Ok(path),
            Err(e) => e,
        };

        Err((local_err, home_err))
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

    fn execute(&self, ctx: &mut Context) -> Option<ObjectInstance> {
        log!("incl enter: {}", self.print().as_str());

        self.instructions.iter().for_each(|instr| {
            // FIXME: Rework prefixing
            // instr.prefix(&prefix);

            instr.execute(ctx);
        });

        None
    }
}

impl TypeCheck for Incl {
    // FIXME: We need to not add the path to the interpreter here
    fn resolve_type(&mut self, ctx: &mut TypeCtx) -> CheckedType {
        let base = match &self.base {
            Some(b) => b.clone(),
            None => match ctx.path() {
                // Get the parent directory of the context's source file. We can unwrap
                // since there's always a base
                Some(path) => path.parent().unwrap().to_owned(),
                // The ctx doesn't have an associated source file. Therefore, we
                // load from where the context was started. This is the case if we're
                // in dynamic mode for example
                None => PathBuf::new(),
            },
        };

        let final_path = match self.get_final_path(&base) {
            Ok(path) => path,
            Err((e1, e2)) => {
                ctx.error(e1);
                ctx.error(e2);
                return CheckedType::Error;
            }
        };

        if ctx.is_included(&final_path) {
            return CheckedType::Void;
        }

        let instructions = match self.fetch_instructions(&final_path) {
            Ok(instructions) => instructions,
            Err(e) => {
                ctx.error(e);
                return CheckedType::Error;
            }
        };

        self.instructions = instructions;

        let old_path = ctx.path().cloned();
        ctx.include(final_path.clone());

        // Temporarily change the path of the context
        ctx.set_path(Some(final_path));

        self.instructions.iter_mut().for_each(|instr| {
            instr.type_of(ctx);
        });

        // Reset the old path before leaving the instruction
        ctx.set_path(old_path);

        CheckedType::Void
    }

    fn set_cached_type(&mut self, _ty: CheckedType) {
        self.typechecked = true
    }

    fn cached_type(&self) -> Option<&CheckedType> {
        match self.typechecked {
            true => Some(&CheckedType::Void),
            false => None,
        }
    }
}

impl Generic for Incl {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{jinko, jinko_fail};

    #[test]
    fn tc_typecheck_stdlib() {
        let mut ctx = Context::new();
        ctx.execute().unwrap();
    }

    #[test]
    fn include_stdlib() {
        jinko! {};
    }

    #[test]
    fn include_non_existant() {
        jinko_fail! {
            incl does_not_exist_at_all;
        };
    }

    #[test]
    fn include_already_included() {
        jinko! {
            incl stdlib;
            incl stdlib as std;
        };
    }
}
