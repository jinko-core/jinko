mod constructs;
mod tokens;

use ast::Ast;
use location::Source;

use ast::Node;
use nom_locate::position;
use nom_locate::LocatedSpan;

// FIXME: This is missing location info
// FIXME: Message info as well?
#[derive(Debug, PartialEq)]
pub struct Error;

// TODO: Rename type?
pub type ParseInput<'i> = LocatedSpan<&'i str, Source<'i>>;
// TODO: Rename type?
pub type ParseResult<T, I> = nom::IResult<T, I, Error>;

/// Parses the entire user input into a vector of instructions in the context
pub fn parse(input: &str, source: Source) -> Result<Ast, Error> {
    let input = LocatedSpan::new_extra(input, source);

    let (input, start) = position::<ParseInput, Error>(input)?;
    let (input, stmts) = constructs::many_exprs(input)?;
    let (input, end) = position::<ParseInput, Error>(input)?;

    // TODO: How does this work with the last statement being an expression?
    Ok(Ast {
        location: constructs::pos_to_loc(input, start, end),
        node: Node::Block {
            stmts,
            last_is_expr: false,
        }, // FIXME: Invalid
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
impl<'i> From<nom::Err<(ParseInput<'i>, nom::error::ErrorKind)>> for Error {
    fn from(_e: nom::Err<(ParseInput<'i>, nom::error::ErrorKind)>) -> Error {
        // FIXME: Is this correct?
        // Error::new(ErrKind::Parsing).with_msg(e.to_string())
        Error
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
            // nom::Err::Incomplete(_) => Error::new(ErrKind::Parsing),
            nom::Err::Incomplete(_) => Error,
            nom::Err::Error(inner) | nom::Err::Failure(inner) => inner,
        }
    }
}

impl<'i> nom::error::ParseError<ParseInput<'i>> for Error {
    fn from_error_kind(_span: ParseInput<'i>, _: nom::error::ErrorKind) -> Error {
        // FIXME: Add better location here in order to print whole line and
        // display specific hint about parse error
        // Error::new(ErrKind::Parsing).with_loc(Some(SpanTuple::with_source_ref(
        //     span.extra,
        //     span.into(),
        //     span.into(),
        // )))
        Error
    }

    fn append(_span: ParseInput<'i>, _: nom::error::ErrorKind, _other: Error) -> Error {
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
        Error
    }
}
