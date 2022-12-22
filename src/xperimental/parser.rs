use crate::error::Error;
use crate::location::Source;
use crate::ErrKind;

pub use super::tokens::Token;
use ast::Ast;
use ast::Call;
use ast::Declaration;
use ast::FunctionKind;
use ast::GenericArgument;
use ast::LoopKind;
use ast::Node;
use ast::Operator;
use ast::TypeArgument;
use ast::TypeKind;
use ast::TypedValue;
use ast::Value;
use location::Location;
use location::SpanTuple;
use nom::bytes::complete::take;
use nom::Err::Error as NomError;
use nom::Slice;
use nom_locate::position;
// pub use constant_construct::ConstantConstruct;
use nom_locate::LocatedSpan;
use symbol::Symbol;

use nom::{
    branch::alt, character::complete::multispace0, combinator::opt, multi::many0,
    sequence::delimited, sequence::pair, sequence::preceded, sequence::terminated,
};

pub type ParseInput<'i> = LocatedSpan<&'i str, Source<'i>>;
pub type ParseResult<T, I> = nom::IResult<T, I, Error>;

/// Parses the entire user input into a vector of instructions in the context
pub fn parse(input: &str, source: Source) -> Result<Ast, Error> {
    todo!()
}

// FIXME: Refactor, rework, remove
fn pos_to_loc(
    input: ParseInput,
    start: impl Into<Location>,
    end: impl Into<Location>,
) -> SpanTuple {
    SpanTuple::with_source_ref(input.extra, start.into(), end.into())
}

pub(crate) fn char_constant(input: ParseInput) -> ParseResult<ParseInput, Ast> {
    let (input, start) = position(input)?;
    let (input, char_value) = Token::char_constant(input)?;
    let (input, end) = position(input)?;

    Ok((
        input,
        Ast {
            location: pos_to_loc(input, start, end),
            node: Node::Constant(Value::Char(char_value)),
        },
    ))
}

pub(crate) fn string_constant(input: ParseInput) -> ParseResult<ParseInput, Ast> {
    let (input, start_loc) = position(input)?;
    let (input, _) = Token::double_quote(input)?;
    let (input, inner) = inner_string(input, start_loc.into())?;
    let (input, end_loc) = position(input)?;
    let location = pos_to_loc(input, start_loc, end_loc);
    let string = inner.unwrap_or_else(|| Ast {
        location,
        node: Node::Constant(Value::Str(String::new())),
    });

    Ok((input, string))
}

/// inner_string = '"'
///              | special string
fn inner_string(input: ParseInput, start_loc: Location) -> ParseResult<ParseInput, Option<Ast>> {
    if let Ok((input, _)) = Token::double_quote(input) {
        Ok((input, None))
    } else {
        let (input, special) = special(input, start_loc.clone())?;
        let (input, next) = inner_string(input, start_loc)?;

        Ok((input, Some(concat(special, next))))
    }
}

/// | '{' expr '}'
/// | '\' CHAR
/// | CHAR (* anything except "{\ *)
fn special(input: ParseInput, start_loc: Location) -> ParseResult<ParseInput, Ast> {
    let special_chars = &['"', '{', '\\'];

    if let Ok((_input, _)) = Token::left_curly_bracket(input) {
        // FIXME: terminated(expr, Token::right_curly_bracket)(input)
        todo!()
    } else if let Ok((input, _)) = Token::backslash(input) {
        if input.is_empty() {
            return special(input, start_loc);
        }
        let (input, special) = take(1usize)(input)?;
        let escaped = match *special.fragment() {
            "\"" => "\"",
            "{" => "{",
            "}" => "}",
            "n" => "\n",
            "r" => "\r",
            "t" => "\t",
            _ => {
                return Err(NomError(
                    Error::new(ErrKind::Parsing).with_msg(String::from("Unknown character escape")),
                ))
            }
        };

        let (input, end_loc) = position(input)?;
        let string = Ast {
            location: pos_to_loc(input, start_loc, end_loc),
            node: Node::Constant(Value::Str(String::from(escaped))),
        };
        Ok((input, string))
    } else if let Some(index) = input.find(special_chars) {
        let (input, end_loc) = position(input)?;
        let string = Ast {
            location: pos_to_loc(input, start_loc, end_loc),
            node: Node::Constant(Value::Str(String::from(&input[..index]))),
        };
        Ok((input.slice(index..), string))
    } else {
        Err(NomError(
            Error::new(ErrKind::Parsing).with_msg(String::from("undelimited string")),
        ))
    }
}

fn concat(left: Ast, right: Option<Ast>) -> Ast {
    match right {
        None => left,
        Some(right) => Ast {
            location: left.location.clone(),
            node: Node::MethodCall {
                instance: Box::new(left),
                call: Call {
                    to: Symbol::from("concat"),
                    generics: vec![],
                    args: vec![right],
                },
            },
        },
    }
}

pub(crate) fn float_constant(input: ParseInput) -> ParseResult<ParseInput, Ast> {
    let (input, start) = position(input)?;
    let (input, value) = Token::float_constant(input)?;
    let (input, end) = position(input)?;

    Ok((
        input,
        Ast {
            location: pos_to_loc(input, start, end),
            node: Node::Constant(Value::Float(value)),
        },
    ))
}

pub(crate) fn int_constant(input: ParseInput) -> ParseResult<ParseInput, Ast> {
    let (input, start) = position(input)?;
    let (input, value) = Token::int_constant(input)?;
    let (input, end) = position(input)?;

    Ok((
        input,
        Ast {
            location: pos_to_loc(input, start, end),
            node: Node::Constant(Value::Integer(value)),
        },
    ))
}

pub(crate) fn bool_constant(input: ParseInput) -> ParseResult<ParseInput, Ast> {
    let (input, start) = position(input)?;
    let (input, value) = Token::bool_constant(input)?;
    let (input, end) = position(input)?;

    Ok((
        input,
        Ast {
            location: pos_to_loc(input, start, end),
            node: Node::Constant(Value::Bool(value)),
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span;

    #[test]
    fn empty_string() {
        let input = span!("\"\"");

        let (input, expr) = string_constant(input).unwrap();
        assert_eq!(*input.fragment(), "");
        let string = match expr.node {
            Node::Constant(Value::Str(s)) => s,
            _ => unreachable!(),
        };
        assert_eq!(string, "");
    }

    #[test]
    fn basic_string() {
        let input = span!("\"hello\"");

        let (input, expr) = string_constant(input).unwrap();
        assert_eq!(*input.fragment(), "");
        let string = match expr.node {
            Node::Constant(Value::Str(s)) => s,
            _ => unreachable!(),
        };
        assert_eq!(string, "hello");
    }

    #[test]
    fn formatted_once() {
        let input = span!("\"hello {world}\"");

        let (input, expr) = string_constant(input).unwrap();
        assert_eq!(*input.fragment(), "");
        assert!(matches!(expr.node, Node::MethodCall { .. }));
    }

    #[test]
    fn formatted_middle() {
        let input = span!("\"hello {world} !\"");

        let (input, expr) = string_constant(input).unwrap();
        assert_eq!(*input.fragment(), "");
        assert!(matches!(expr.node, Node::MethodCall { .. }));
    }

    #[test]
    fn formatted_start() {
        let input = span!("\"{10 + 9} is 21\"");

        let (input, expr) = string_constant(input).unwrap();
        assert_eq!(*input.fragment(), "");
        assert!(matches!(expr.node, Node::MethodCall { .. }));
    }

    #[test]
    fn escape_start() {
        let input = span!("\"\\neat\"");

        let (input, expr) = string_constant(input).unwrap();
        assert_eq!(*input.fragment(), "");
        assert!(matches!(expr.node, Node::MethodCall { .. }));
    }

    #[test]
    fn escape_end() {
        let input = span!("\"hello\\n\"");

        let (input, expr) = string_constant(input).unwrap();
        assert_eq!(*input.fragment(), "");
        assert!(matches!(expr.node, Node::MethodCall { .. }));
    }

    #[test]
    fn escape_formatting() {
        let input = span!("\"hello \\{world\\}\"");

        let (input, expr) = string_constant(input).unwrap();
        assert_eq!(*input.fragment(), "");
        assert!(matches!(expr.node, Node::MethodCall { .. }));
    }

    #[test]
    fn formatted_not_delimited() {
        let input = span!("\"hello {world}");

        assert!(string_constant(input).is_err());
    }

    #[test]
    fn basic_not_delimited() {
        let input = span!("\"Rust Transmute Task Force");

        assert!(string_constant(input).is_err());
    }

    #[test]
    fn escape_unexpected_end_of_string() {
        let input = span!("\"Rust Transmute Task Force\\");

        assert!(string_constant(input).is_err());
    }

    #[test]
    fn invalid_escape() {
        let input = span!("\"Rust Transmute \\a Task Force\"");

        assert!(string_constant(input).is_err());
    }
}

// BREAK

/// Parse as many instructions as possible
/// many_expr = ( expr_semicolon )*
pub fn many_expr(mut input: ParseInput) -> ParseResult<ParseInput, Vec<Ast>> {
    let mut exprs = vec![];
    loop {
        input = next(input);
        if input.is_empty() {
            return Ok((input, exprs));
        }
        let (new_input, expr) = expr_semicolon(input)?;
        input = new_input;
        exprs.push(expr);
    }
}

/// Parse an instruction and maybe the semicolon that follows.
///
/// expr_semicolon = expr [ ';' ]
pub fn expr_semicolon(input: ParseInput) -> ParseResult<ParseInput, Ast> {
    let (input, expr) = expr(input)?;
    let (input, _) = opt(Token::semicolon)(input)?;

    Ok((input, expr))
}

// FIXME: Rename, Rework
#[deprecated]
pub fn parse_operator(input: ParseInput) -> Operator {
    let op = match *input.fragment() {
        "+" => Operator::Add,
        "-" => Operator::Sub,
        "*" => Operator::Mul,
        "/" => Operator::Div,
        "<" => Operator::Lt,
        ">" => Operator::Gt,
        "<=" => Operator::LtEq,
        ">=" => Operator::GtEq,
        "==" => Operator::Equals,
        "!=" => Operator::NotEquals,
        "(" => Operator::LeftParenthesis,
        ")" => Operator::RightParenthesis,
        s => unreachable!("Invalid operator: {}", s),
    };

    op
}

#[deprecated]
pub fn function_kind(input: ParseInput) -> FunctionKind {
    match *input {
        "func" => FunctionKind::Func,
        "test" => FunctionKind::Test,
        "mock" => FunctionKind::Mock,
        "ext" => FunctionKind::Extern,
        s => unreachable!("invalid function kind: {}", s),
    }
}

// expr = cmp ( '<' cmp | '>' cmp | '<=' cmp | '>=' cmp | '==' cmp | '!=' cmp)*
pub fn expr(input: ParseInput) -> ParseResult<ParseInput, Ast> {
    let input = next(input);
    let (input, start_loc) = position(input)?;
    let (mut input, mut expr) = cmp(input)?;
    while let Ok((new_input, op)) = alt((
        Token::lt_eq,
        Token::gt_eq,
        Token::equals,
        Token::not_equals,
        Token::lt,
        Token::gt,
    ))(input)
    {
        let (new_input, rhs) = cmp(new_input)?;
        let (new_input, end_loc) = position(new_input)?;
        input = new_input;
        let b_op = Ast {
            location: pos_to_loc(input, start_loc, end_loc),
            node: Node::BinaryOp(parse_operator(op), Box::new(expr), Box::new(rhs)),
        };
        expr = b_op;
    }
    Ok((input, expr))
}

pub fn cmp(input: ParseInput) -> ParseResult<ParseInput, Ast> {
    let input = next(input);
    let (input, start_loc) = position(input)?;
    let (mut input, mut expr) = term(input)?;
    while let Ok((new_input, op)) = alt((Token::add, Token::sub))(input) {
        let (new_input, rhs) = term(new_input)?;
        let (new_input, end_loc) = position(new_input)?;
        input = new_input;
        let b_op = Ast {
            location: pos_to_loc(input, start_loc, end_loc),
            node: Node::BinaryOp(parse_operator(op), Box::new(expr), Box::new(rhs)),
        };
        expr = b_op;
    }
    Ok((input, expr))
}

/// term = factor next ( '*' factor next | '/' factor next )*
fn term(input: ParseInput) -> ParseResult<ParseInput, Ast> {
    let input = next(input);
    let (input, start_loc) = position(input)?;
    let (input, mut term) = factor(input)?;
    let mut input = next(input);
    while let Ok((new_input, op)) = alt((Token::mul, Token::div))(input) {
        let (new_input, rhs) = factor(new_input)?;
        let (new_input, end_loc) = position(new_input)?;
        let new_input = next(new_input);
        input = new_input;
        let b_op = Ast {
            location: pos_to_loc(input, start_loc, end_loc),
            node: Node::BinaryOp(parse_operator(op), Box::new(term), Box::new(rhs)),
        };
        term = b_op;
    }
    Ok((input, term))
}

/// factor = next unit factor_rest
fn factor(input: ParseInput) -> ParseResult<ParseInput, Ast> {
    let input = next(input);
    let (input, start_loc) = position(input)?;
    let (input, unit) = unit(input)?;
    factor_rest(input, unit, start_loc.into())
}

/// factor_rest = '.' IDENTIFIER next method_or_field factor_rest
///             | ε
fn factor_rest(input: ParseInput, expr: Ast, start_loc: Location) -> ParseResult<ParseInput, Ast> {
    match Token::dot(input) {
        Ok((input, _)) => {
            let (input, id) = Token::identifier(input)?;
            let input = next(input);
            let (input, expr) = method_or_field(input, expr, id, start_loc.clone())?;
            factor_rest(input, expr, start_loc)
        }
        _ => Ok((input, expr)),
    }
}

/// method_or_field = '(' next args
///                 | ε
fn method_or_field(
    input: ParseInput,
    expr: Ast,
    id: String,
    start_loc: Location,
) -> ParseResult<ParseInput, Ast> {
    if let Ok((input, _)) = Token::left_parenthesis(input) {
        let input = next(input);
        let (input, args) = args(input)?;
        let (input, end_loc) = position(input)?;
        let method_call = Ast {
            location: pos_to_loc(input, start_loc, end_loc),
            node: Node::MethodCall {
                instance: Box::new(expr),
                call: Call {
                    to: Symbol::from(id),
                    generics: vec![],
                    args,
                },
            },
        };

        Ok((input, method_call))
    } else if let Ok((input, _)) = Token::left_bracket(input) {
        let input = next(input);
        let (input, generics) = generic_list(input)?;
        let input = next(input);
        let (input, _) = Token::left_parenthesis(input)?;
        let input = next(input);
        let (input, args) = args(input)?;
        let (input, end_loc) = position(input)?;
        let method_call = Ast {
            location: pos_to_loc(input, start_loc, end_loc),
            node: Node::MethodCall {
                instance: Box::new(expr),
                call: Call {
                    to: Symbol::from(id),
                    generics,
                    args,
                },
            },
        };

        Ok((input, method_call))
    } else {
        let (input, end_loc) = position(input)?;
        let f_a = Ast {
            location: pos_to_loc(input, start_loc, end_loc),
            node: Node::FieldAccess(Box::new(expr), Symbol::from(id)),
        };

        Ok((input, f_a))
    }
}

/// ```ignore
/// unit = '_f' expr block next [ 'else' next block ]
///      | 'while' expr block
///      | 'loop' next block
///      | 'for' spaced_identifier '_n' expr block
///
///      | 'func' function_declaration block
///      | 'test' function_declaration block
///      | 'mock' function_declaration block
///
///      | 'type' type_id '(' named_args
///      | 'incl' spaced_identifier [ 'as' next IDENTIFIER ]
///      | 'mut' spaced_identifier '=' expr (* mutable variable assigment *)
///      | '@' spaced_identifier '(' args
///
///      | 'extern' 'func' function_declaration ';'
///      | 'return' expr
///      | '{' next inner_block
///      | '(' expr ')'
///
///      | 'true'
///      | 'false'
///      | "'" CHAR "'"
///      | '"' [^"] '"'
///      | INT
///      | DOUBLE
///
///      | IDENTIFIER next func_type_or_var
/// ```
fn unit(input: ParseInput) -> ParseResult<ParseInput, Ast> {
    let (input, start_loc) = position(input)?;
    if let Ok((input, _)) = Token::if_tok(input) {
        unit_if(input, start_loc.into())
    } else if let Ok((input, _)) = Token::while_tok(input) {
        unit_while(input, start_loc.into())
    } else if let Ok((input, _)) = Token::loop_tok(input) {
        unit_loop(input, start_loc.into())
    } else if let Ok((input, _)) = Token::for_tok(input) {
        unit_for(input, start_loc.into())
    } else if let Ok((input, kind)) =
        alt((Token::func_tok, Token::test_tok, Token::mock_tok))(input)
    {
        unit_func(input, kind, start_loc.into())
    } else if let Ok((input, _)) = Token::incl_tok(input) {
        unit_incl(input, start_loc.into())
    } else if let Ok((input, _)) = Token::type_tok(input) {
        unit_type_decl(input, start_loc.into())
    } else if let Ok((input, _)) = Token::mut_tok(input) {
        unit_mut_var(input)
    } else if let Ok((input, _)) = Token::at_sign(input) {
        unit_jk_inst(input, start_loc.into())
    } else if let Ok((input, _)) = Token::ext_tok(input) {
        unit_extern(input, start_loc.into())
    } else if let Ok((input, _)) = Token::return_tok(input) {
        unit_return(input, start_loc.into())
    } else if let Ok((input, _)) = Token::left_curly_bracket(input) {
        unit_block(input, start_loc.into())
    } else if let Ok((input, _)) = Token::left_parenthesis(input) {
        terminated(expr, Token::right_parenthesis)(input)
    } else if let Ok(res) = constant(input) {
        Ok(res)
    } else {
        let (input, id) = Token::identifier(input)?;
        let input = next(input);
        func_type_or_var(input, id, start_loc.into())
    }
}

fn unit_if(input: ParseInput, start_loc: Location) -> ParseResult<ParseInput, Ast> {
    let (input, cond) = expr(input)?;
    let (input, success) = block(input)?;
    let input = next(input);
    if let Ok((input, _)) = Token::else_tok(input) {
        let input = next(input);
        let (input, else_body) = block(input)?;
        let (input, end_loc) = position(input)?;

        let if_else = Ast {
            location: pos_to_loc(input, start_loc, end_loc),
            node: Node::IfElse {
                if_condition: Box::new(cond),
                if_block: Box::new(success),
                else_block: Some(Box::new(else_body)),
            },
        };

        Ok((input, if_else))
    } else {
        let (input, end_loc) = position(input)?;
        let if_else = Ast {
            location: pos_to_loc(input, start_loc, end_loc),
            node: Node::IfElse {
                if_condition: Box::new(cond),
                if_block: Box::new(success),
                else_block: None,
            },
        };

        Ok((input, if_else))
    }
}

fn unit_while(input: ParseInput, start_loc: Location) -> ParseResult<ParseInput, Ast> {
    let (input, (cond, block)) = pair(expr, block)(input)?;
    let (input, end_loc) = position(input)?;
    let while_loop = Node::Loop(LoopKind::While(Box::new(cond)), Box::new(block));

    Ok((
        input,
        Ast {
            location: pos_to_loc(input, start_loc, end_loc),
            node: while_loop,
        },
    ))
}

fn unit_loop(input: ParseInput, start_loc: Location) -> ParseResult<ParseInput, Ast> {
    let input = next(input);
    let (input, block) = block(input)?;
    let (input, end_loc) = position(input)?;
    let loop_loop = Node::Loop(LoopKind::Infinite, Box::new(block));

    Ok((
        input,
        Ast {
            location: pos_to_loc(input, start_loc, end_loc),
            node: loop_loop,
        },
    ))
}

fn unit_for(input: ParseInput, start_loc: Location) -> ParseResult<ParseInput, Ast> {
    let (input, (id, iterator_end_loc)) = spaced_identifier(input)?;
    let (input, _) = Token::in_tok(input)?;
    let (input, expr) = expr(input)?;
    let (input, block) = block(input)?;
    let (input, end_loc) = position(input)?;
    let var = Node::Var(Symbol::from(id));
    let for_loop = Node::Loop(
        LoopKind::For {
            iterator: Box::new(Ast {
                location: pos_to_loc(input, start_loc.clone(), iterator_end_loc),
                node: var,
            }),
            range: Box::new(expr),
        },
        Box::new(block),
    );

    Ok((
        input,
        Ast {
            location: pos_to_loc(input, start_loc, end_loc),
            node: for_loop,
        },
    ))
}

fn unit_func<'i>(
    input: ParseInput<'i>,
    kind: ParseInput<'i>,
    start_loc: Location,
) -> ParseResult<ParseInput<'i>, Ast> {
    let (input, decl) = func_declaration(input)?;
    let kind = function_kind(kind);
    let input = next(input);
    let (input, block) = block(input)?;
    let (input, end_loc) = position(input)?;
    let function = Node::Function {
        kind,
        decl,
        block: Some(Box::new(block)),
    };

    Ok((
        input,
        Ast {
            location: pos_to_loc(input, start_loc, end_loc),
            node: function,
        },
    ))
}

fn unit_incl(input: ParseInput, start_loc: Location) -> ParseResult<ParseInput, Ast> {
    let (input, (path, id_loc)) = spaced_identifier(input)?;
    if let Ok((input, _)) = Token::az_tok(input) {
        let (input, alias) = preceded(nom_next, Token::identifier)(input)?;
        let (input, end_loc) = position(input)?;
        let incl = Node::Incl {
            source: Symbol::from(path),
            as_path: Some(Symbol::from(alias)),
        };

        Ok((
            input,
            Ast {
                location: pos_to_loc(input, start_loc, end_loc),
                node: incl,
            },
        ))
    } else {
        let end_loc = Location::new(id_loc.line(), id_loc.column() + path.len());
        let incl = Node::Incl {
            source: Symbol::from(path),
            as_path: None,
        };
        let location = pos_to_loc(input, start_loc, end_loc);
        Ok((
            input,
            Ast {
                location,
                node: incl,
            },
        ))
    }
}

fn type_id(input: ParseInput) -> ParseResult<ParseInput, TypeArgument> {
    fn arg_types(input: ParseInput) -> ParseResult<ParseInput, Vec<TypeArgument>> {
        if let Ok((input, _)) = Token::right_parenthesis(input) {
            return Ok((input, vec![]));
        }

        let (input, first_arg) = type_id(input)?;
        let (input, mut args) = many0(preceded(Token::comma, type_id))(input)?;
        let (input, _) = Token::right_parenthesis(input)?;

        args.insert(0, first_arg);

        Ok((input, args))
    }

    fn return_type(input: ParseInput) -> ParseResult<ParseInput, TypeArgument> {
        let input = next(input);
        let (input, _) = Token::arrow(input)?;
        let input = next(input);
        type_id(input)
    }

    let input = next(input);
    if let Ok((input, _)) = Token::func_tok(input) {
        let (input, generics) = maybe_generic_application(input)?;
        let (input, _) = Token::left_parenthesis(input)?;
        let (input, args) = arg_types(input)?;
        let (input, ret_ty) = opt(return_type)(input)?;

        let kind = TypeKind::FunctionLike(args, ret_ty.map(Box::new));

        Ok((input, TypeArgument { kind, generics }))
    } else {
        let (input, (id, _)) = spaced_identifier(input)?;
        let kind = TypeKind::Ty(Symbol::from(id));

        let (input, generics) = maybe_generic_application(input)?;

        Ok((input, TypeArgument { kind, generics }))
    }
}

/// type_id '(' type_inst_arg (',' type_inst_arg)* ')'
fn unit_type_decl(input: ParseInput, start_loc: Location) -> ParseResult<ParseInput, Ast> {
    // FIXME: This needs to use TypeIds
    let (input, (name, _)) = spaced_identifier(input)?;
    let (input, generics) = maybe_generic_arguments(input)?;
    let (input, type_dec) = if let Ok((input, _)) = Token::left_parenthesis(input) {
        let (input, first_field) = typed_arg(input)?;
        let (input, mut fields) = many0(preceded(Token::comma, typed_arg))(input)?;
        let (input, _) = Token::right_parenthesis(input)?;

        fields.insert(0, first_field);

        (
            input,
            Node::Type {
                name: Symbol::from(name),
                generics,
                fields,
                with: None, // TODO: Parse `with` block properly
            },
        )
    } else {
        (
            input,
            Node::Type {
                name: Symbol::from(name),
                generics,
                fields: vec![],
                with: None,
            },
        )
    };
    let (input, end_loc) = position(input)?;

    Ok((
        input,
        Ast {
            location: pos_to_loc(input, start_loc, end_loc),
            node: type_dec,
        },
    ))
}

/// spaced_identifier '=' expr
fn unit_mut_var(input: ParseInput) -> ParseResult<ParseInput, Ast> {
    let (input, (symbol, start_loc)) = spaced_identifier(input)?;
    let (input, _) = Token::equal(input)?;
    let (input, value) = expr(input)?;
    let (input, end_loc) = position(input)?;

    let assignment = Node::VarAssign {
        mutable: true,
        to_assign: Symbol::from(symbol),
        value: Box::new(value),
    };

    Ok((
        input,
        Ast {
            location: pos_to_loc(input, start_loc, end_loc),
            node: assignment,
        },
    ))
}

/// IDENTIFIER next '(' next args
fn unit_jk_inst(input: ParseInput, start_loc: Location) -> ParseResult<ParseInput, Ast> {
    // let (input, name) = delimited(nom_next, Token::identifier, nom_next)(input)?;
    // let (input, _) = Token::left_parenthesis(input)?;
    // let (input, args) = args(next(input))?;
    // let (input, end_loc) = position(input)?;

    // // A jk_inst will never contain generics
    // let mut call = FunctionCall::new(name, vec![], args);
    // call.set_location(SpanTuple::with_source_ref(
    //     input.extra,
    //     start_loc,
    //     end_loc.into(),
    // ));
    // match JkInst::from_function_call(&call) {
    //     Ok(inst) => Ok((input, Box::new(inst))),
    //     Err(err) => Err(NomError(err)),
    // }
    todo!()
}

/// 'func' function_declaration ';'
fn unit_extern(input: ParseInput, start_loc: Location) -> ParseResult<ParseInput, Ast> {
    let input = next(input);
    let (input, decl) = delimited(Token::func_tok, func_declaration, Token::semicolon)(input)?;
    let (input, end_loc) = position(input)?;

    Ok((
        input,
        Ast {
            location: pos_to_loc(input, start_loc, end_loc),
            node: Node::Function {
                kind: FunctionKind::Extern,
                decl,
                block: None,
            },
        },
    ))
}

///  [ expr ]                      (* Not LL(1) but this entry is subject to change *)
fn unit_return(input: ParseInput, start_loc: Location) -> ParseResult<ParseInput, Ast> {
    let (input, expr) = opt(expr)(input)?;
    let (input, end_loc) = position(input)?;

    let ret = Node::Return(expr.map(Box::new));

    Ok((
        input,
        Ast {
            location: pos_to_loc(input, start_loc, end_loc),
            node: ret,
        },
    ))
}

fn unit_block(input: ParseInput, start_loc: Location) -> ParseResult<ParseInput, Ast> {
    let (input, block) = inner_block(next(input))?;
    let (input, end_loc) = position(input)?;

    Ok((
        input,
        Ast {
            location: pos_to_loc(input, start_loc, end_loc),
            node: block,
        },
    ))
}

// FIXME: generic_list, generic_arguments and the maybe* versions need to be improved and refactored
fn generic_list(input: ParseInput) -> ParseResult<ParseInput, Vec<TypeArgument>> {
    fn whitespace_plus_type(input: ParseInput) -> ParseResult<ParseInput, TypeArgument> {
        let input = next(input);
        let (input, ty) = type_id(input)?;
        let input = next(input);

        Ok((input, ty))
    }

    let (input, first_type) = whitespace_plus_type(input)?;
    let (input, mut generics) = many0(preceded(Token::comma, whitespace_plus_type))(input)?;
    let (input, _) = Token::right_bracket(input)?;

    generics.insert(0, first_type);
    Ok((input, generics))
}

// FIXME: This does not parse default generic types yet (`func f[T = int]()`)
fn generic_arguments(input: ParseInput) -> ParseResult<ParseInput, Vec<GenericArgument>> {
    fn whitespace_plus_generic(input: ParseInput) -> ParseResult<ParseInput, GenericArgument> {
        let input = next(input);
        let (input, (id, _)) = spaced_identifier(input)?;
        let input = next(input);

        Ok((
            input,
            GenericArgument {
                name: Symbol::from(id),
                default: None,
            },
        ))
    }

    let (input, first_type) = whitespace_plus_generic(input)?;
    let (input, mut generics) = many0(preceded(Token::comma, whitespace_plus_generic))(input)?;
    let (input, _) = Token::right_bracket(input)?;

    generics.insert(0, first_type);
    Ok((input, generics))
}

fn maybe_generic_arguments(input: ParseInput) -> ParseResult<ParseInput, Vec<GenericArgument>> {
    if let Ok((input, _)) = Token::left_bracket(input) {
        Ok(generic_arguments(input)?)
    } else {
        Ok((input, vec![]))
    }
}

fn maybe_generic_application(input: ParseInput) -> ParseResult<ParseInput, Vec<TypeArgument>> {
    if let Ok((input, _)) = Token::left_bracket(input) {
        Ok(generic_list(input)?)
    } else {
        Ok((input, vec![]))
    }
}

/// function_declaration = next spaced_identifier [ next '[' spaced_identifier ( ',' spaced_identifier )* ']' ] next '(' next typed_arg next return_type
fn func_declaration(input: ParseInput) -> ParseResult<ParseInput, Declaration> {
    let input = next(input);
    let (input, (id, _)) = spaced_identifier(input)?;
    let input = next(input);

    let (input, generics) = maybe_generic_arguments(input)?;

    let (input, _) = Token::left_parenthesis(input)?;
    let input = next(input);
    let (input, args) = typed_args(input)?;
    let input = next(input);
    let (input, return_type) = return_type(input)?;

    let decl = Declaration {
        name: Symbol::from(id),
        generics,
        args,
        return_type,
    };

    Ok((input, decl))
}

/// return_type = '->' type_id
///             | ε
fn return_type(input: ParseInput) -> ParseResult<ParseInput, Option<TypeArgument>> {
    match Token::arrow(input) {
        Ok((input, _)) => {
            let (input, ty_id) = type_id(input)?;
            Ok((input, Some(ty_id)))
        }
        _ => Ok((input, None)),
    }
}

/// block = '{' next inner_block
pub fn block(input: ParseInput) -> ParseResult<ParseInput, Ast> {
    let (input, start_loc) = position(input)?;
    let (input, _) = Token::left_curly_bracket(input)?;
    let input = next(input);
    let (input, block) = inner_block(input)?;
    let (input, end_loc) = position(input)?;

    Ok((
        input,
        Ast {
            location: pos_to_loc(input, start_loc, end_loc),
            node: block,
        },
    ))
}

/// inner_block = '}'
///             | expr '}'                  (* The only case where block is an expr *)
///             | expr ';' next inner_block
fn inner_block(input: ParseInput) -> ParseResult<ParseInput, Node> {
    if let Ok((input, _)) = Token::right_curly_bracket(input) {
        return Ok((input, Node::Block(vec![])));
    }

    let (input, inst) = expr(input)?;
    if let Ok((input, _)) = Token::right_curly_bracket(input) {
        return Ok((input, Node::Block(vec![inst])));
    }

    let (input, block) = preceded(Token::semicolon, preceded(nom_next, inner_block))(input)?;
    // block.push_front_instruction(inst);
    todo!();
    Ok((input, block))
}

/// func_type_or_var = '(' next func_or_type_inst_args
///                  | '=' expr                   (* variable assigment *)
///                  | ε                          (* variable or empty type instantiation *)
fn func_type_or_var(
    input: ParseInput,
    id: String,
    start_loc: Location,
) -> ParseResult<ParseInput, Ast> {
    if let Ok((input, _)) = Token::left_bracket(input) {
        generic_func_or_type_inst_args(next(input), id, start_loc)
    } else if let Ok((input, _)) = Token::left_parenthesis(input) {
        func_or_type_inst_args(next(input), id, vec![], start_loc)
    } else if let Ok((input, _)) = Token::equal(input) {
        let (input, value) = expr(input)?;
        let (input, end_loc) = position(input)?;
        let var_assign = Node::VarAssign {
            mutable: false,
            to_assign: Symbol::from(id),
            value: Box::new(value),
        };
        Ok((
            input,
            Ast {
                location: pos_to_loc(input, start_loc, end_loc),
                node: var_assign,
            },
        ))
    } else {
        let (input, end_loc) = position(input)?;
        let var_or_et = Node::VarOrEmptyType(Symbol::from(id));
        Ok((
            input,
            Ast {
                location: pos_to_loc(input, start_loc, end_loc),
                node: var_or_et,
            },
        ))
    }
}

/// func_or_type_inst_args = IDENTIFIER next ':' expr (',' type_inst_arg )* ')'  (* type_instantiation *)
///                  | args                                            (* function_call *)
fn func_or_type_inst_args(
    input: ParseInput,
    id: String,
    generics: Vec<TypeArgument>,
    start_loc: Location,
) -> ParseResult<ParseInput, Ast> {
    let (input, first_attr_start_loc) = position(input)?;
    if let Ok((input, first_attr)) =
        terminated(terminated(Token::identifier, nom_next), Token::colon)(input)
    {
        let input = next(input);
        let (input, first_attr_val) = expr(input)?;
        let (input, first_attr_end_loc) = position(input)?;
        let (input, mut attrs) = many0(preceded(Token::comma, type_inst_arg))(input)?;
        let (input, _) = Token::right_parenthesis(input)?;
        let (input, end_loc) = position(input)?;

        attrs.insert(
            0,
            Ast {
                location: pos_to_loc(input, first_attr_start_loc, first_attr_end_loc),
                node: Node::VarAssign {
                    mutable: false,
                    to_assign: Symbol::from(first_attr),
                    value: Box::new(first_attr_val),
                },
            },
        );

        let type_inst = Node::TypeInstantiation(Call {
            to: Symbol::from(id),
            generics,
            args: attrs,
        });

        Ok((
            input,
            Ast {
                location: pos_to_loc(input, start_loc, end_loc),
                node: type_inst,
            },
        ))
    } else {
        let (input, args) = args(input)?;
        let (input, end_loc) = position(input)?;
        let func_call = Node::FunctionCall(Call {
            to: Symbol::from(id),
            generics,
            args,
        });

        Ok((
            input,
            Ast {
                location: pos_to_loc(input, start_loc, end_loc),
                node: func_call,
            },
        ))
    }
}

fn generic_func_or_type_inst_args(
    input: ParseInput,
    id: String,
    start_loc: Location,
) -> ParseResult<ParseInput, Ast> {
    // FIXME: Assign generics to FunctionCall and TypeInstantiation
    let (input, generics) = generic_list(input)?;
    let input = next(input);
    let (input, _) = Token::left_parenthesis(input)?;

    func_or_type_inst_args(next(input), id, generics, start_loc)
}

///
/// ARGS
///

/// args = expr ( ',' expr )* ')'
///      | ')'
fn args(input: ParseInput) -> ParseResult<ParseInput, Vec<Ast>> {
    if let Ok((input, _)) = Token::right_parenthesis(input) {
        return Ok((input, vec![]));
    }
    let (input, first_arg) = expr(input)?;
    let (input, mut args) = many0(preceded(Token::comma, expr))(input)?;
    let (input, _) = Token::right_parenthesis(input)?;

    args.insert(0, first_arg);
    Ok((input, args))
}

/// typed_args = typed_arg ( ',' typed_arg )* ')'
///           | ')'
fn typed_args(input: ParseInput) -> ParseResult<ParseInput, Vec<TypedValue>> {
    if let Ok((input, _)) = Token::right_parenthesis(input) {
        return Ok((input, vec![]));
    }
    let (input, first_arg) = typed_arg(input)?;
    let (input, mut args) = many0(preceded(Token::comma, typed_arg))(input)?;
    let (input, _) = Token::right_parenthesis(input)?;

    args.insert(0, first_arg);
    Ok((input, args))
}

// FIXME: This should not return a String
fn multi_type(input: ParseInput) -> ParseResult<ParseInput, TypeArgument> {
    // FIXME: Remove with #342
    fn whitespace_plus_type_id(input: ParseInput) -> ParseResult<ParseInput, TypeArgument> {
        delimited(nom_next, type_id, nom_next)(input)
    }

    // FIXME: We want to allow generic types here later on
    let (input, first_type) = whitespace_plus_type_id(input)?;

    let (input, mut types) = many0(preceded(Token::pipe, whitespace_plus_type_id))(input)?;

    // FIXME: Remove clone once we have a proper MultiType struct to return
    types.insert(0, first_type.clone());

    Ok((input, first_type))
}

/// typed_arg = spaced_identifier ':' spaced_identifier
fn typed_arg(input: ParseInput) -> ParseResult<ParseInput, TypedValue> {
    let (input, (id, start_loc)) = spaced_identifier(input)?;
    let (input, _) = Token::colon(input)?;
    let input = next(input);
    let (input, ty) = multi_type(input)?;
    let input = next(input);
    let (input, end_loc) = position(input)?;

    let dec_arg = TypedValue {
        location: pos_to_loc(input, start_loc, end_loc),
        symbol: Symbol::from(id),
        ty,
    };

    Ok((input, dec_arg))
}

/// type_inst_arg = spaced_identifier ':' expr
fn type_inst_arg(input: ParseInput) -> ParseResult<ParseInput, Ast> {
    let input = next(input);
    let (input, start_loc) = position(input)?;
    let (input, (id, _)) = spaced_identifier(input)?;
    let (input, _) = Token::colon(input)?;
    let input = next(input);
    let (input, value) = expr(input)?;
    let (input, end_loc) = position(input)?;
    let input = next(input);

    let location = pos_to_loc(input, start_loc, end_loc);
    let node = Node::VarAssign {
        mutable: false,
        to_assign: Symbol::from(id),
        value: Box::new(value),
    };

    Ok((input, Ast { location, node }))
}

fn spaced_identifier(input: ParseInput) -> ParseResult<ParseInput, (String, Location)> {
    let input = next(input);
    let (input, loc) = position(input)?;
    let (input, id) = Token::identifier(input)?;
    let input = next(input);

    Ok((input, (id, loc.into())))
}

/// next = extra*
/// extra = WHITESPACE
///       | '/*' [^'*/'] '*/'
///       | '//' [^\n]   '\n'
///       | '#'  [^\n]   '\n'
pub fn next(input: ParseInput) -> ParseInput {
    // FIXME: Do not unwrap
    let (input, _) = multispace0::<_, Error>(input).unwrap();
    match Token::consume_comment(input) {
        Ok((input, _)) => next(input),
        _ => input,
    }
}

fn nom_next(input: ParseInput) -> ParseResult<ParseInput, ()> {
    Ok((next(input), ()))
}

/// Constants are raw values in the source code. For example, `"string"`, `12` and
/// `0.5`.
///
/// `'<any_char>' | "<any_char>*" | <num>? | <num>?.<num>?`
pub(crate) fn constant(input: ParseInput) -> ParseResult<ParseInput, Ast> {
    let constant = alt((
        char_constant,
        string_constant,
        float_constant,
        int_constant,
        bool_constant,
    ))(input)?;

    Ok(constant)
}
