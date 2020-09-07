//! The Error module contains helpful wrapper around possible errors in broccoli. They
//! are used by the interpreter as well as the parser.

use colored::Colorize;

/// What kind of error we are dealing with: Either a parsing error, or a behavioural one.
#[derive(Copy, Clone, Debug, PartialEq)]
#[repr(u8)]
pub enum ErrKind {
    Parsing,
    Interpreter,
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
pub struct BroccoliError<'err> {
    kind: ErrKind,
    msg: String,

    loc: Option<SpaceLocation>,
    input: Option<&'err str>,
}

impl<'err> BroccoliError<'err> {
    /// Create a new error and return it
    pub fn new(
        kind: ErrKind,
        msg: String,
        loc: Option<SpaceLocation>,
        input: Option<&'err str>,
    ) -> BroccoliError {
        BroccoliError {
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
    pub fn kind(&self) -> ErrKind {
        self.kind
    }
}

impl std::fmt::Display for BroccoliError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // FIXME: Add better formatting
        write!(
            f,
            "Input: {}\nErrorKind: {:?}\nInfo: {}",
            self.input.unwrap(), // FIXME: Remove unwrap()
            self.kind,
            self.msg.red()
        )
    }
}
