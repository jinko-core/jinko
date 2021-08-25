//! The Error module contains helpful wrapper around possible errors in jinko. They
//! are used by the interpreter as well as the parser.

use std::path::{Path, PathBuf};

use colored::Colorize;

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
        self.errors.iter().for_each(|e| e.emit(&self.file));
    }

    /// Add a new error to the handler
    pub fn add(&mut self, err: Error) {
        self.errors.push(err)
    }

    /// Remove all the errors contained in the handler
    pub fn clear(&mut self) {
        self.errors.clear()
    }

    // FIXME: Remove this once location is implemented
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

// FIXME: Location should not be in the error part only
/// Contains indications vis-a-vis the error's location in the source file
#[derive(Debug, PartialEq, Clone)]
pub struct ErrSpaceLocation {
    pub line: usize,
    pub offset: usize,
    pub input: &'static str,
}

// FIXME: Add better API?
impl ErrSpaceLocation {
    pub fn new(line: usize, offset: usize, input: &'static str) -> ErrSpaceLocation {
        ErrSpaceLocation {
            line,
            offset,
            input,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
#[repr(u8)]
pub enum ErrKind {
    Parsing,
    Interpreter,
    TypeChecker,
    IO,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Error {
    pub(crate) kind: ErrKind,
    msg: Option<String>,
    loc: Option<ErrSpaceLocation>,
}

impl Error {
    pub fn emit(&self, file: &Path) {
        let kind_str = match self.kind {
            ErrKind::Parsing => "Parsing",
            ErrKind::Interpreter => "Interpreter",
            ErrKind::TypeChecker => "Typechecker",
            ErrKind::IO => "I/O",
        };

        eprintln!("Error type: {}", kind_str.red());
        eprintln!(" ===> {}", file.to_string_lossy().green());

        // FIXME: Is the formatting correct?
        eprintln!("    |");
        for line in self.msg.as_deref().unwrap_or("").lines() {
            eprintln!("    | {}", line);
        }
        eprintln!("    |");
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

    pub fn with_loc(self, loc: ErrSpaceLocation) -> Error {
        Error {
            loc: Some(loc),
            ..self
        }
    }

    pub fn exit(&self) {
        // The exit code depends on the kind of error
        std::process::exit(self.kind as i32 + 1);
    }
}

/// I/O errors keep their messages
impl std::convert::From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Error {
        Error::new(ErrKind::IO).with_msg(e.to_string())
    }
}

/// Nom errors are automatically parsing errors
impl std::convert::From<nom::Err<(&str, nom::error::ErrorKind)>> for Error {
    fn from(e: nom::Err<(&str, nom::error::ErrorKind)>) -> Error {
        Error::new(ErrKind::Parsing).with_msg(e.to_string())
    }
}

/// Likewise, if we need to convert from a nom::Err<jinko::Error> to a jinko::Error
impl std::convert::From<nom::Err<Error>> for Error {
    fn from(e: nom::Err<Error>) -> Error {
        match e {
            nom::Err::Incomplete(_) => Error::new(ErrKind::Parsing),
            nom::Err::Error(inner) | nom::Err::Failure(inner) => inner,
        }
    }
}

impl nom::error::ParseError<&str> for Error {
    fn from_error_kind(input: &str, _: nom::error::ErrorKind) -> Error {
        Error::new(ErrKind::Parsing).with_msg(String::from(input))
    }

    fn append(input: &str, _: nom::error::ErrorKind, _other: Error) -> Error {
        // FIXME: Should we accumulate errors this way?
        // let other_msg = match other.msg {
        //     Some(msg) => format!("{}\n", msg),
        //     None => String::new(),
        // };

        Error::new(ErrKind::Parsing).with_msg(String::from(input)) // /* FIXME  */ with_msg(format!("{}{}", other_msg, input))
    }
}
