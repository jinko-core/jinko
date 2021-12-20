//! This module is used to parse external code and make it available to other source
//! files.

use std::path::{Path, PathBuf};

use crate::{
    log,
    parser::constructs,
    typechecker::{CheckedType, TypeCtx},
    Context, ErrKind, Error, InstrKind, Instruction, ObjectInstance, TypeCheck,
};

/// An `Incl` is constituted of a path, an optional alias and contains a context.
/// The ctx is built from parsing the source file in the path.
/// Aliases are used to potentially rename exported functions.
#[derive(Clone)]
pub struct Incl {
    path: String,
    alias: Option<String>,
    base: Option<PathBuf>,
}

/// Default file that gets included when including a directory in jinko source code
const DEFAULT_INCL: &str = "/lib.jk";

impl Incl {
    pub fn new(path: String, alias: Option<String>) -> Incl {
        Incl {
            path,
            alias,
            base: None,
        }
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

    /// Parse the code and load it in the Incl's ctx
    fn inner_load(
        &self,
        base: &Path,
        ctx: &mut Context,
    ) -> Result<(PathBuf, Vec<Box<dyn Instruction>>), Error> {
        let formatted = self.find_include_path(base)?;

        // If a source has already been included, skip it without returning
        // an error
        if ctx.is_included(&formatted) {
            return Ok((formatted, vec![]));
        }

        log!("final path: {}", &format!("{:?}", formatted));

        let input = std::fs::read_to_string(&formatted)?;

        // We can't just parse the input, since it adds the instructions
        // to an entry block in order to execute them. What we can do, is
        // parse many instructions and add them to an empty ctx
        let (remaining_input, instructions) = constructs::many_expr(input.as_str())?;

        match remaining_input.len() {
            // The remaining input is empty: We parsed the whole file properly
            0 => Ok((formatted, instructions)),
            _ => Err(Error::new(ErrKind::Parsing).with_msg(format!(
                "error when parsing included file: {:?},\non the following input:\n{}",
                formatted, remaining_input
            ))),
        }
    }

    /// Try to load code from the current path where the executable has been launched
    fn load_relative(
        &self,
        base: &Path,
        ctx: &mut Context,
    ) -> Result<(PathBuf, Vec<Box<dyn Instruction>>), Error> {
        self.inner_load(base, ctx)
    }

    /// Try to load code from jinko's installation path
    fn load_jinko_path(
        &self,
        ctx: &mut Context,
    ) -> Result<(PathBuf, Vec<Box<dyn Instruction>>), Error> {
        let home = std::env::var("HOME")?;

        let base = PathBuf::from(format!("{}/.jinko/libs/", home));
        self.inner_load(&base, ctx)
    }

    /// Load the source code located at self.path
    ///
    /// There are two ways to look for a source file: First in the includer's path, and
    /// if not available in jinko's installation directory.
    fn load(&self, base: &Path, ctx: &mut Context) -> Option<(PathBuf, Vec<Box<dyn Instruction>>)> {
        // If we can load from the current path, we return early
        let relative_err = match self.load_relative(base, ctx) {
            Ok(tuple) => return Some(tuple),
            Err(e) => e,
        };

        let jk_path_err = match self.load_jinko_path(ctx) {
            Ok(tuple) => return Some(tuple),
            Err(e) => e,
        };

        ctx.error(relative_err);
        ctx.error(jk_path_err);

        None
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
            // the context itself. Don't add a leading `::`
            "" => Some(alias.to_string()),
            _ => Some(format!("{}::", alias)),
        }
    }

    fn get_base(&self, ctx: &mut Context) -> PathBuf {
        // If the incl block contains a given base, return this instead
        match &self.base {
            Some(b) => b.clone(), // FIXME: Remove clone
            None => match ctx.path() {
                // Get the parent directory of the context's source file. We can unwrap
                // since there's always a base
                Some(path) => path.parent().unwrap().to_owned(),
                // The ctx doesn't have an associated source file. Therefore, we
                // load from where the context was started. This is the case if we're
                // in dynamic mode for example
                None => PathBuf::new(),
            },
        }
    }

    pub fn set_base(&mut self, path: PathBuf) {
        self.base = Some(path);
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

        let base = self.get_base(ctx);
        let _prefix = self.format_prefix()?;

        log!("base dir: {}", &format!("{:#?}", base));

        let old_path = ctx.path().cloned();

        let (new_path, mut content) = self.load(&base, ctx)?;

        // Temporarily change the path of the context
        ctx.set_path(Some(new_path));

        content.iter_mut().for_each(|instr| {
            // FIXME: Rework prefixing
            // instr.prefix(&prefix);

            instr.execute(ctx);
        });

        // Reset the old path before leaving the instruction
        ctx.set_path(old_path);

        None
    }
}

impl TypeCheck for Incl {
    // FIXME: We need to not add the path to the interpreter here
    fn resolve_type(&self, _ctx: &mut TypeCtx) -> CheckedType {
        // FIXME: Maybe the inclusion part should be done separately ahead of typechecking
        // and execution?
        // FIXME: This is a lot of code in common with execute()
        // let base = self.get_base(ctx.context);

        // let old_path = ctx.context.path().cloned();

        // let (new_path, mut content) = match self.load(&base, ctx.context) {
        //     None => return CheckedType::Unknown,
        //     Some(tuple) => tuple,
        // };

        // // Temporarily change the path of the context
        // ctx.context.set_path(Some(new_path));

        // content.iter_mut().for_each(|instr| {
        //     instr.resolve_type(ctx);
        // });

        // // Reset the old path before leaving the instruction
        // ctx.context.set_path(old_path);

        CheckedType::Void
    }
}

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
