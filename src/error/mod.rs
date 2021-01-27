//! The Error module contains helpful wrapper around possible errors in jinko. They
//! are used by the interpreter as well as the parser.

use colored::Colorize;

/// What kind of error we are dealing with: Either a parsing error, or a behavioural one.
#[derive(Copy, Clone, Debug, PartialEq)]
#[repr(u8)]
pub enum JkErrKind {
    Parsing,
    Interpreter,
    IO,
}

/// Contains indications vis-a-vis the error's location in the source file
#[derive(Debug, PartialEq)]
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
// FIXME: Remove `Option` once input tracking is implemented
#[derive(Debug, PartialEq)]
pub struct JkError {
    kind: JkErrKind,
    msg: String,

    loc: Option<SpaceLocation>,
    input: String,
}

impl JkError {
    /// Create a new error and return it
    pub fn new(
        kind: JkErrKind,
        msg: String,
        loc: Option<SpaceLocation>,
        input: String,
    ) -> JkError {
        JkError {
            kind,
            msg,
            loc,
            input,
        }
    }

    /// Display the error on stderr before exiting the program
    pub fn exit(&self) {
        eprintln!("{}", self.to_string());

        // The exit code depends on the kind of error
        std::process::exit(self.kind as i32 + 1);
    }

    /// What kind of error the error is
    #[cfg(test)]
    pub fn kind(&self) -> JkErrKind {
        self.kind
    }
}

impl std::fmt::Display for JkError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // FIXME: Add better formatting
        write!(f, "ErrorKind: {:?}\nInfo: {}", self.kind, self.msg.red())
    }
}

impl std::convert::From<std::io::Error> for JkError {
    fn from(e: std::io::Error) -> Self {
        JkError::new(JkErrKind::IO, e.to_string(), None, "".to_owned())
    }
}

// FIXME: Improve formatting, current output is barren
impl std::convert::From<nom::Err<(&str, nom::error::ErrorKind)>> for JkError {
    fn from(e: nom::Err<(&str, nom::error::ErrorKind)>) -> Self {
        JkError::new(JkErrKind::Parsing, e.to_string(), None, "".to_owned())
    }
}
