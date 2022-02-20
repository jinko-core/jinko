//! The Error module contains helpful wrapper around possible errors in jinko. They
//! are used by the context as well as the parser.

use std::fmt::{Display, Formatter};
use std::path::PathBuf;

use colored::Colorize;

use crate::{ParseInput, SpanTuple};

/// The role of the error handler is to keep track of errors and emit them properly
/// once done
#[derive(Default)]
pub struct ErrorHandler {
    errors: Vec<Error>,
    file: PathBuf,
}

impl ErrorHandler {
    /// Emit all the errors contained in a handler
    pub fn emit(&self) {
        if let Some(first_err) = self.errors.first() {
            first_err.emit();
        }
        self.errors.iter().for_each(|e| {
            eprintln!("----------------------------------------------------------------");
            e.emit()
        });
    }

    /// Add a new error to the handler
    pub fn add(&mut self, err: Error) {
        self.errors.push(err)
    }

    /// Drains all the errors contained in another handler in order to accumulate them
    /// in one place
    pub fn append(&mut self, other: &mut ErrorHandler) {
        self.errors.append(&mut other.errors)
    }

    /// Remove all the errors contained in the handler
    pub fn clear(&mut self) {
        self.errors.clear()
    }

    /// Set the file that should be used by the error handler. This function should be
    /// removed once locations are kept properly in the different instructions
    pub fn set_path(&mut self, file: PathBuf) {
        self.file = file;
    }

    /// Has the error handler seen errors or not
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
#[repr(u8)]
pub enum ErrKind {
    Parsing,
    Context,
    TypeChecker,
    Generics,
    ExternFunc,
    IO,
}

impl ErrKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            ErrKind::Parsing => "Parsing",
            ErrKind::Context => "Interpreter",
            ErrKind::TypeChecker => "Typechecker",
            ErrKind::Generics => "Generics",
            ErrKind::IO => "I/O",
            ErrKind::ExternFunc => "External Function",
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Error {
    kind: ErrKind,
    msg: Option<String>,
    loc: Option<SpanTuple>,
}

impl Error {
    fn emit_full_loc(&self, loc: &SpanTuple) {
        let (before_ctx, after_ctx) = loc.generate_context();

        if let Some(msg) = &self.msg {
            if let Some(path) = loc.path() {
                let kind_str = self.kind.as_str();

                eprintln!("{}: {}", "error type".black().on_yellow(), kind_str);
                eprintln!(
                    "{}:{}:{}: {}",
                    path.display().to_string().yellow(),
                    loc.start().line(),
                    loc.start().column(),
                    msg
                );
                eprintln!();
            }
        }

        if let Some(ctx) = before_ctx {
            ctx.emit('|', '_')
        };
        loc.emit(">".red().bold(), "-".purple());
        after_ctx.emit('|', '_');
    }

    pub fn emit(&self) {
        if let Some(loc) = &self.loc {
            self.emit_full_loc(loc);
        } else if let Some(msg) = &self.msg {
            eprintln!("{}", msg)
        }
    }

    pub fn new(kind: ErrKind) -> Error {
        Error {
            kind,
            msg: None,
            loc: None,
        }
    }

    pub fn with_msg(self, msg: String) -> Error {
        Error {
            msg: Some(msg),
            ..self
        }
    }

    // FIXME: Should this really take an Option<Location>?
    pub fn with_loc(self, loc: Option<SpanTuple>) -> Error {
        Error { loc, ..self }
    }

    pub fn exit(&self) {
        // The exit code depends on the kind of error
        std::process::exit(self.kind as i32 + 1);
    }
}

use std::convert::From;
use std::io;

/// I/O errors keep their messages
impl From<io::Error> for Error {
    fn from(e: std::io::Error) -> Error {
        Error::new(ErrKind::IO).with_msg(e.to_string())
    }
}

/// Nom errors are automatically parsing errors
impl<'i> From<nom::Err<(ParseInput<'i>, nom::error::ErrorKind)>> for Error {
    fn from(e: nom::Err<(ParseInput<'i>, nom::error::ErrorKind)>) -> Error {
        // FIXME: Is this correct?
        Error::new(ErrKind::Parsing).with_msg(e.to_string())
    }
}

/// Likewise, if we need to convert from a nom::Err<jinko::Error> to a jinko::Error.
/// While this pattern may seem weird, nom sometimes requires you to wrap errors in an
/// Error or Failure state in order to specify to parse combinators how to proceed. You
/// can see this being used with the `NomError` alias throughout the project. Thus, we
/// might need to lower the wrapped errors back into our regular errors
impl From<nom::Err<Error>> for Error {
    fn from(e: nom::Err<Error>) -> Error {
        match e {
            nom::Err::Incomplete(_) => Error::new(ErrKind::Parsing),
            nom::Err::Error(inner) | nom::Err::Failure(inner) => inner,
        }
    }
}

impl<'i> nom::error::ParseError<ParseInput<'i>> for Error {
    fn from_error_kind(span: ParseInput<'i>, _: nom::error::ErrorKind) -> Error {
        // FIXME: Add better location here in order to print whole line and
        // display specific hint about parse error
        Error::new(ErrKind::Parsing).with_loc(Some(SpanTuple::new(
            span.extra,
            span.into(),
            span.into(),
        )))
    }

    fn append(span: ParseInput<'i>, _: nom::error::ErrorKind, _other: Error) -> Error {
        // FIXME: Should we accumulate errors this way?
        // let other_msg = match other.msg {
        //     Some(msg) => format!("{}\n", msg),
        //     None => String::new(),
        // };

        Error::new(ErrKind::Parsing).with_loc(Some(SpanTuple::new(
            span.extra,
            span.into(),
            span.into(),
        )))
        // /* FIXME  */ with_msg(format!("{}{}", other_msg, input))
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.kind.as_str())?;
        if self.msg.is_some() {
            write!(f, ": {}", self.msg.as_ref().unwrap())?;
        }

        if let Some(loc) = &self.loc {
            writeln!(
                f,
                " at line {} column {}",
                loc.start().line(),
                loc.start().column()
            )?;
        }

        Ok(())
    }
}

#[cfg(feature = "ffi")]
impl std::convert::From<libloading::Error> for Error {
    fn from(e: libloading::Error) -> Error {
        Error::new(ErrKind::ExternFunc).with_msg(e.to_string())
    }
}

impl std::convert::From<std::env::VarError> for Error {
    fn from(e: std::env::VarError) -> Self {
        Error::new(ErrKind::ExternFunc).with_msg(e.to_string())
    }
}

impl std::error::Error for Error {}
