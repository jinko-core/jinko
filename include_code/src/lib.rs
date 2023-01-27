//! This module is used to parse external code and make it available to other source
//! files.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use ast::{Ast, Node, Visitor};
use error::{ErrKind, Error};
use location::{Source, SourceOwned, SpanTuple};
use symbol::Symbol;

pub trait IncludeCode {
    fn resolve_includes(self) -> Self;
}

impl IncludeCode for Ast {
    fn resolve_includes(self) -> Ast {
        let mut ctx = IncludeCtx {
            included: HashSet::new(),
        };

        // Basically what we want to do is visit each node. When we see a `Node::Incl`, we simply fetch its content and return a new node.
        // At this point, we assume the AST is valid. Right? No! We can return an Err::Incl if something goes south.
        ctx.visit(self)
    }
}

struct IncludeCtx {
    included: HashSet<PathBuf>,
}

/// Default file that gets included when including a directory in jinko source code
const DEFAULT_INCL: &str = "lib.jk";

fn check_base(base: &Path, location: &SpanTuple, path: &Path) -> Result<PathBuf, Error> {
    let (mut dir_candidate, mut file_candidate) = (
        PathBuf::from(base).join(path).join(DEFAULT_INCL),
        PathBuf::from(base).join(path),
    );
    dir_candidate.set_extension("jk");
    file_candidate.set_extension("jk");

    match (dir_candidate.is_file(), file_candidate.is_file()) {
        // We cannot have both <path>/lib.jk and <path>.jk be valid files
        (true, true) => Err(Error::new(ErrKind::Context)
            .with_msg(format!(
                "invalid include: `{:?}` and `{:?}` are both valid candidates",
                dir_candidate, file_candidate
            ))
            .with_loc(Some(location.clone()))),
        (false, false) => Err(Error::new(ErrKind::Context)
            .with_msg(format!(
                "no candidate for include: `{:?}` and `{:?}` do not exist",
                dir_candidate, file_candidate
            ))
            .with_loc(Some(location.clone()))),
        (false, true) => Ok(file_candidate),
        (true, false) => Ok(dir_candidate),
    }
}

fn load_home_library(location: &SpanTuple, path: &Path) -> Result<PathBuf, Error> {
    let home = std::env::var("HOME")?;
    let home_base = PathBuf::from(format!("{}/.jinko/libs/", home));

    check_base(&home_base, location, path)
}

fn load_local_library(base: &Path, location: &SpanTuple, path: &Path) -> Result<PathBuf, Error> {
    check_base(base, location, path)
}

pub fn get_final_path(
    base: &Path,
    location: &SpanTuple,
    path: &Path,
) -> Result<PathBuf, (Error, Error)> {
    // Check the local path first
    let local_err = match load_local_library(base, location, path) {
        Ok(path) => return Ok(path),
        Err(e) => e,
    };

    let home_err = match load_home_library(location, path) {
        Ok(path) => return Ok(path),
        Err(e) => e,
    };

    Err((local_err, home_err))
}

fn get_base(location: &SpanTuple) -> PathBuf {
    match location.source() {
        // Get the parent directory of the context's source file. We can unwrap
        // since there's always a base
        SourceOwned::Path(p) => p.parent().unwrap().to_owned(),
        // The ctx doesn't have an associated source file. Therefore, we
        // load from where the context was started. This is the case if we're
        // in dynamic mode for example
        SourceOwned::Input(_) | SourceOwned::Empty => PathBuf::new(),
    }
}

// fn resolve_type(&mut self, ctx: &mut TypeCtx) -> Result<CheckedType, Error> {

//     let final_path = match self.get_final_path(&base) {
//         Ok(path) => path,
//         Err((e1, e2)) => {
//             ctx.error(e1);
//             ctx.error(e2);
//             return Err(Error::new(ErrKind::TypeChecker));
//         }
//     };

//     if ctx.is_included(&final_path) {
//         return Ok(CheckedType::Void);
//     }

//     let instructions = self.fetch_instructions(&final_path, ctx.reader())?;

//     self.instructions = instructions;

//     let old_path = ctx.path().cloned();
//     ctx.include(final_path.clone());

//     // Temporarily change the path of the context
//     ctx.set_path(Some(final_path));

//     self.instructions.iter_mut().for_each(|instr| {
//         if let Err(e) = instr.type_of(ctx) {
//             ctx.error(e);
//         }
//     });

//     // Reset the old path before leaving the instruction
//     ctx.set_path(old_path);

//     Ok(CheckedType::Void)
// }

fn fetch_ast_node(path: &Path) -> Result<Ast, Error> {
    let input = std::fs::read_to_string(path)?;
    let ast = xparser::parse(&input, Source::Path(path))?;

    Ok(ast)
}

impl Visitor for IncludeCtx {
    fn visit_incl(&mut self, location: SpanTuple, source: Symbol, _as_path: Option<Symbol>) -> Ast {
        let base = get_base(&location);
        let to_include = PathBuf::from(source.access());

        let final_path = match get_final_path(&base, &location, &to_include) {
            Ok(path) => path,
            Err((e1, e2)) => {
                e1.emit();
                e2.emit();
                PathBuf::new()
                // return Err(Error::new(ErrKind::TypeChecker));
            }
        };

        if self.included.contains(&final_path) {
            return Ast {
                location,
                node: Node::Empty,
            };
        }

        // FIXME: No unwrap
        fetch_ast_node(&final_path).unwrap()
    }
}
