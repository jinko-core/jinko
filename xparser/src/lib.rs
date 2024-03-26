mod constructs;
mod tokens;

pub use constructs::*;
pub use tokens::*;

use ast::Ast;
use location::Source;

use ast::Node;
use nom_locate::position;
use nom_locate::LocatedSpan;

// FIXME: This is missing location info
// FIXME: Message info as well?
#[derive(Debug, PartialEq, Eq)]
pub enum Error<'i> {
    Msg(String),
    Nom(nom::Err<(ParseInput<'i>, nom::error::ErrorKind)>),
    Incomplete(nom::Needed),
    Mult(Vec<Error<'i>>),
}

impl<'i> Error<'i> {
    pub fn emit(&self) {
        match self {
            Error::Msg(m) => eprintln!("parsing error: {m}"),
            Error::Nom(e) => eprintln!("nom error: {e:?}"),
            Error::Incomplete(needed) => eprintln!("parsing error: incomplete: {needed:?}"),
            Error::Mult(v) => v.iter().for_each(|e| e.emit()),
        }
    }
}

// TODO: Rename type?
pub type ParseInput<'i> = LocatedSpan<&'i str, Source<'i>>;
// TODO: Rename type?
pub type ParseResult<'i, I, T> = nom::IResult<I, T, Error<'i>>;

/// Parses the entire user input into a vector of instructions in the context
pub fn parse<'i>(input: &'i str, source: Source<'i>) -> Result<Ast, Error<'i>> {
    let input = LocatedSpan::new_extra(input, source);

    let (input, start) = position::<ParseInput, Error>(input)?;
    let (input, stmts) = constructs::many_exprs(input)?;
    let (input, end) = position::<ParseInput, Error>(input)?;

    // TODO: How does this work with the last statement being an expression?
    Ok(Ast {
        location: constructs::pos_to_loc(input, start, end),
        node: Node::Block {
            stmts,
            last_is_expr: true,
        }, // FIXME: Is that valid?
    })
}

/// Helpful macro for unit testing the parser
#[cfg(test)]
#[macro_export]
macro_rules! span {
    ($s:literal) => {
        nom_locate::LocatedSpan::new_extra($s, location::Source::Input($s))
    };
}

/// Nom errors are automatically parsing errors
impl<'i> From<nom::Err<(ParseInput<'i>, nom::error::ErrorKind)>> for Error<'i> {
    fn from(e: nom::Err<(ParseInput<'i>, nom::error::ErrorKind)>) -> Error<'i> {
        // FIXME: Is this correct?
        // Error::new(ErrKind::Parsing).with_msg(e.to_string())
        Error::Nom(e)
    }
}

/// Likewise, if we need to convert from a nom::Err<jinko::Error> to a jinko::Error.
/// While this pattern may seem weird, nom sometimes requires you to wrap errors in an
/// Error or Failure state in order to specify to parse combinators how to proceed. You
/// can see this being used with the `NomError` alias throughout the project. Thus, we
/// might need to lower the wrapped errors back into our regular errors
impl<'i> From<nom::Err<Error<'i>>> for Error<'i> {
    fn from(e: nom::Err<Error<'i>>) -> Error<'i> {
        match e {
            // nom::Err::Incomplete(_) => Error::new(ErrKind::Parsing),
            nom::Err::Incomplete(n) => Error::Incomplete(n),
            nom::Err::Error(inner) | nom::Err::Failure(inner) => inner,
        }
    }
}

impl<'i> nom::error::ParseError<ParseInput<'i>> for Error<'i> {
    fn from_error_kind(span: ParseInput<'i>, k: nom::error::ErrorKind) -> Error {
        // FIXME: Add better location here in order to print whole line and
        // display specific hint about parse error
        // Error::new(ErrKind::Parsing).with_loc(Some(SpanTuple::with_source_ref(
        //     span.extra,
        //     span.into(),
        //     span.into(),
        // )))
        Error::Nom(nom::Err::Error((span, k)))
    }

    fn append(span: ParseInput<'i>, k: nom::error::ErrorKind, other: Error<'i>) -> Error<'i> {
        // FIXME: Should we accumulate errors this way?
        // let other_msg = match other.msg {
        //     Some(msg) => format!("{}\n", msg),
        //     None => String::new(),
        // };

        // Error::new(ErrKind::Parsing).with_loc(Some(SpanTuple::with_source_ref(
        //     span.extra,
        //     span.into(),
        //     span.into(),
        // )))
        // /* FIXME  */ with_msg(format!("{}{}", other_msg, input))

        let mut vec = match other {
            Error::Mult(v) => v,
            e => vec![e],
        };

        vec.push(Error::from_error_kind(span, k));

        Error::Mult(vec)
    }
}

/// Parse a list of token trees to a jinko [`ast::Ast`]
#[macro_export]
macro_rules! ast {
        ($($tok:tt)*) => {
            {
                let ast = xparser::parse(
                    stringify!($($tok)*),
                    location::Source::Input(stringify!($($tok)*)))
                .unwrap();

                ast
            }
        }
    }
