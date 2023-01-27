//! This module is used to parse external code and make it available to other source
//! files.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use ast::{Ast, Node, Visitor};
use error::{ErrKind, Error};
use location::{Source, SourceOwned, SpanTuple};
use symbol::Symbol;

pub trait IncludeCode: Sized {
    fn resolve_includes(self) -> Result<Self, Error>;
}

impl IncludeCode for Ast {
    fn resolve_includes(self) -> Result<Ast, Error> {
        let mut ctx = IncludeCtx {
            included: HashSet::new(),
        };

        // Basically what we want to do is visit each node. When we see a
        // [`Node::Incl`], we simply fetch its content and return a new node,
        // if that path hasn't been previously included.
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

impl Visitor<Error> for IncludeCtx {
    fn visit_incl(
        &mut self,
        location: SpanTuple,
        source: Symbol,
        _as_path: Option<Symbol>,
    ) -> Result<Ast, Error> {
        let base = get_base(&location);
        let to_include = PathBuf::from(source.access());

        let final_path = match get_final_path(&base, &location, &to_include) {
            Ok(path) => path,
            Err((e1, e2)) => {
                e1.emit();
                e2.emit();
                return Err(Error::new(ErrKind::Include));
            }
        };

        if self.included.contains(&final_path) {
            return Ok(Ast {
                location,
                node: Node::Empty,
            });
        }

        let path: &Path = &final_path;
        let input = std::fs::read_to_string(path)?;

        let ast = xparser::parse(&input, Source::Path(path))?;
        self.included.insert(final_path);

        self.visit(ast)
    }
}

#[cfg(test)]
mod test {
    // FIXME: Add some tests
}
