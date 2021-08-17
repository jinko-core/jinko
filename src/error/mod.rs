//! The Error module contains helpful wrapper around possible errors in jinko. They
//! are used by the interpreter as well as the parser.

// FIXME: Add an error handler to the interpreter to pass around to functions and to use
// to generate errors and maybe exit with a specific error code. The error handler can
// also accumulate errors instead of always emitting them

use colored::Colorize;

// FIXME: Location should not be in the error part only
/// Contains indications vis-a-vis the error's location in the source file
#[derive(Debug, PartialEq)]
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
    IO,
}

#[derive(Debug)]
pub struct Error {
    kind: ErrKind,
    msg: Option<String>,
    loc: Option<ErrSpaceLocation>,
}

impl Error {
    pub fn emit(&self) {
        let kind_str = match self.kind {
            ErrKind::Parsing => "parsing",
            ErrKind::Interpreter => "interpreter",
            ErrKind::IO => "i/o",
        };

        eprintln!("error type: {}", kind_str.red());
        eprintln!("{}", self.msg.as_deref().unwrap_or(""));

        // FIXME: Use somehow, somewhere
        // The exit code depends on the kind of error
        // std::process::exit(self.kind as i32 + 1);
    }

    pub fn new(kind: ErrKind) -> Error {
        Error {
            kind,
            msg: None,
            loc: None,
        }
    }

    // FIXME: Work out something better...
    pub fn with_msg<T: std::fmt::Display>(self, msg: T) -> Error {
        Error {
            msg: Some(format!("{}", msg)),
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

impl std::convert::From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Error {
        let err = Error::new(ErrKind::IO).with_msg(e.to_string());

        err.emit();
        err
    }
}

// FIXME: Improve formatting, current output is barren
impl std::convert::From<nom::Err<(&str, nom::error::ErrorKind)>> for Error {
    fn from(e: nom::Err<(&str, nom::error::ErrorKind)>) -> Error {
        let err = Error::new(ErrKind::Parsing).with_msg(e.to_string());

        err.emit();
        err
    }
}

// /// What kind of error we are dealing with: Either a parsing error, or a behavioural one.
// #[derive(Copy, Clone, Debug, PartialEq)]
// #[repr(u8)]
// pub enum ErrErrKind {
//     Parsing,
//     Interpreter,
//     IO,
// }
// 
// /// The actual error type
// // FIXME: Remove `Option` once input tracking is implemented
// #[derive(Debug, PartialEq)]
// pub struct Error {
//     kind: ErrErrKind,
//     msg: String,
//
//     loc: Option<ErrSpaceLocation>,
//     input: String,
// }
//
// impl Error {
//     /// Create a new error and return it
//     pub fn new(
//         kind: ErrErrKind,
//         msg: String,
//         loc: Option<ErrSpaceLocation>,
//         input: String,
//     ) -> Error {
//         Error {
//             kind,
//             msg,
//             loc,
//             input,
//         }
//     }
//
//     /// Display the error on stderr before exiting the program
//     pub fn exit(&self) {
//         eprintln!("{}", self.to_string());
//
//         // The exit code depends on the kind of error
//         std::process::exit(self.kind as i32 + 1);
//     }
//
//     /// What kind of error the error is
//     #[cfg(test)]
//     pub fn kind(&self) -> ErrErrKind {
//         self.kind
//     }
//
//     /// Message contained in the error
//     #[cfg(test)]
//     pub fn msg(&self) -> &str {
//         &self.msg
//     }
// }
//
// impl std::fmt::Display for Error {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         // FIXME: Add better formatting
//         write!(f, "ErrorErrKind: {:?}\nInfo: {}", self.kind, self.msg.red())
//     }
// }
