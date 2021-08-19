//! The Error module contains helpful wrapper around possible errors in jinko. They
//! are used by the interpreter as well as the parser.

// FIXME: Add an error handler to the interpreter to pass around to functions and to use
// to generate errors and maybe exit with a specific error code. The error handler can
// also accumulate errors instead of always emitting them

use colored::Colorize;

#[derive(Default)]
pub struct ErrorHandler {
    errors: Vec<Error>,
}

impl ErrorHandler {
    pub fn emit(&self) {
        self.errors.iter().for_each(|e| e.emit());
    }

    pub fn add(&mut self, err: Error) {
        self.errors.push(err)
    }

    pub fn clear(&mut self) {
        self.errors.clear()
    }
}

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

#[derive(Debug, PartialEq)]
pub struct Error {
    pub(crate) kind: ErrKind,
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

impl std::convert::From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Error {
        Error::new(ErrKind::IO).with_msg(e.to_string())
    }
}

impl std::convert::From<nom::Err<(&str, nom::error::ErrorKind)>> for Error {
    fn from(e: nom::Err<(&str, nom::error::ErrorKind)>) -> Error {
        Error::new(ErrKind::Parsing).with_msg(e.to_string())
    }
}

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
