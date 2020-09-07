//! The Error module contains helpful wrapper around possible errors in broccoli. They
//! are used by the interpreter as well as the parser.

use colored::Colorize;

/// What kind of error we are dealing with: Either a parsing error, or a behavioural one.
#[derive(Debug)]
pub enum ErrKind {
    Parsing,
    Interpreter,
    IO,
}

/// Contains indications vis-a-vis the error's location in the source file
pub struct SpaceLocation(pub usize, pub usize);

impl SpaceLocation {
    /// Line of the error
    pub fn line(&self) -> usize {
        self.0
    }

    /// Offset of the error on the contained line
    pub fn offset(&self) -> usize {
        self.1
    }
}

/// The actual error type
pub struct BroccoliError {
    kind: ErrKind,
    msg: String,

    loc: Option<SpaceLocation>,
}

impl BroccoliError {
    /// Create a new error and return it
    pub fn new(kind: ErrKind, msg: String, loc: Option<SpaceLocation>) -> BroccoliError {
        BroccoliError { kind, msg, loc }
    }
}

impl std::fmt::Display for BroccoliError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // FIXME: Add better formatting
        write!(f, "ErrorKind: {:?}\nInfo: {}", self.kind, self.msg.red())
    }
}

impl std::convert::From<std::io::Error> for BroccoliError {
    fn from(e: std::io::Error) -> Self {
        BroccoliError::new(ErrKind::IO, e.to_string(), None)
    }
}
