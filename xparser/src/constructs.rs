//! A `Construct` is a complex set of tokens. For example, `fn()` is an identifier, a
//! left parenthesis and a right parenthesis. Together, they constitute a function call.
//! In the same vein, `x = 12;` is 4 tokens used to represent variable assignment.
//! Therefore, constructs use tokens while the parser only uses constructs. This is an
//! abstraction for all possible ways to parse a line in jinko.
//!
//! Each of the functions in that module contain the grammar they represent above their
//! name. The syntax used for the grammar is loosely based on regular instructions and
//! globbing. One can use * to indicate 0 or more, ? to indicate 1 or more, etc etc.
//! Optional parameters are included between brackets. For example,
//!
//! `[mut] <identifier> = <const> | <function_call> | <block> | <identifier>`
//!
//! is the grammar for a variable assignment.

use super::tokens;
use super::{Error, ParseInput, ParseResult};

use ast::Call;
use ast::Declaration;
use ast::FunctionKind;
use ast::GenericParameter;
use ast::LoopKind;
use ast::Node;
use ast::Type;
use ast::TypeKind;
use ast::TypedValue;
use ast::Value;
use ast::{Ast, TypeContent};
use location::Location;
use location::SpanTuple;
use nom::sequence::tuple;
use nom::Err::Error as NomError;
use nom::Parser;
use nom::Slice;
use nom_locate::position;
use symbol::Symbol;

use nom::{
    branch::alt, bytes::complete::take, character::complete::multispace0, combinator::opt,
    multi::many0, sequence::delimited, sequence::pair, sequence::preceded, sequence::terminated,
};

// FIXME: Refactor, rework, remove, depreciate
pub(crate) fn pos_to_loc(
    input: ParseInput,
    start: impl Into<Location>,
    end: impl Into<Location>,
) -> SpanTuple {
    SpanTuple::with_source_ref(input.extra, start.into(), end.into())
}

/// Parse as many instructions as possible
/// many_expr = ( expr_semicolon )*
pub fn many_exprs(mut input: ParseInput) -> ParseResult<ParseInput, Vec<Ast>> {
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
    let (input, _) = opt(tokens::semicolon)(input)?;

    Ok((input, expr))
}

// expr = cmp ( '<' cmp | '>' cmp | '<=' cmp | '>=' cmp | '==' cmp | '!=' cmp)*
pub fn expr(input: ParseInput) -> ParseResult<ParseInput, Ast> {
    let input = next(input);
    let (input, start_loc) = position(input)?;
    let (mut input, mut expr) = cmp(input)?;
    while let Ok((new_input, op)) = alt((
        tokens::lt_eq,
        tokens::gt_eq,
        tokens::equals,
        tokens::not_equals,
        tokens::lt,
        tokens::gt,
    ))(input)
    {
        let (new_input, rhs) = cmp(new_input)?;
        let (new_input, end_loc) = position(new_input)?;
        input = new_input;
        let b_op = Ast {
            location: pos_to_loc(input, start_loc, end_loc),
            node: Node::BinaryOp(op, Box::new(expr), Box::new(rhs)),
        };
        expr = b_op;
    }
    Ok((input, expr))
}

pub fn cmp(input: ParseInput) -> ParseResult<ParseInput, Ast> {
    let input = next(input);
    let (input, start_loc) = position(input)?;
    let (mut input, mut expr) = term(input)?;
    while let Ok((new_input, op)) = alt((tokens::add, tokens::sub))(input) {
        let (new_input, rhs) = term(new_input)?;
        let (new_input, end_loc) = position(new_input)?;
        input = new_input;
        let b_op = Ast {
            location: pos_to_loc(input, start_loc, end_loc),
            node: Node::BinaryOp(op, Box::new(expr), Box::new(rhs)),
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
    while let Ok((new_input, op)) = alt((tokens::mul, tokens::div))(input) {
        let (new_input, rhs) = factor(new_input)?;
        let (new_input, end_loc) = position(new_input)?;
        let new_input = next(new_input);
        input = new_input;
        let b_op = Ast {
            location: pos_to_loc(input, start_loc, end_loc),
            node: Node::BinaryOp(op, Box::new(term), Box::new(rhs)),
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
    match tokens::dot(input) {
        Ok((input, _)) => {
            let (input, id) = tokens::identifier(input)?;
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
    if let Ok((input, _)) = tokens::left_parenthesis(input) {
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
    } else if let Ok((input, _)) = tokens::left_bracket(input) {
        let input = next(input);
        let (input, generics) = generic_list(input)?;
        let input = next(input);
        let (input, _) = tokens::left_parenthesis(input)?;
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
    if let Ok((input, _)) = tokens::if_tok(input) {
        unit_if(input, start_loc.into())
    } else if let Ok((input, _)) = tokens::while_tok(input) {
        unit_while(input, start_loc.into())
    } else if let Ok((input, _)) = tokens::loop_tok(input) {
        unit_loop(input, start_loc.into())
    } else if let Ok((input, _)) = tokens::for_tok(input) {
        unit_for(input, start_loc.into())
    } else if let Ok((input, kind)) =
        alt((tokens::func_tok, tokens::test_tok, tokens::mock_tok))(input)
    {
        unit_func(input, kind, start_loc.into())
    } else if let Ok((input, _)) = tokens::incl_tok(input) {
        unit_incl(input, start_loc.into())
    } else if let Ok((input, _)) = tokens::type_tok(input) {
        unit_type_decl(input, start_loc.into())
    } else if let Ok((input, _)) = tokens::where_tok(input) {
        unit_var_decl(input)
    } else if let Ok((input, _)) = tokens::at_sign(input) {
        unit_jk_inst(input, start_loc.into())
    } else if let Ok((input, _)) = tokens::ext_tok(input) {
        unit_extern(input, start_loc.into())
    } else if let Ok((input, _)) = tokens::return_tok(input) {
        unit_return(input, start_loc.into())
    } else if let Ok((input, _)) = tokens::left_curly_bracket(input) {
        unit_block(input, start_loc.into())
    } else if let Ok((input, _)) = tokens::left_parenthesis(input) {
        terminated(expr, tokens::right_parenthesis)(input)
    } else if let Ok(res) = constant(input) {
        Ok(res)
    } else {
        let (input, id) = tokens::identifier(input)?;
        let input = next(input);
        func_type_or_var(input, id, start_loc.into())
    }
}

fn unit_if(input: ParseInput, start_loc: Location) -> ParseResult<ParseInput, Ast> {
    let (input, cond) = expr(input)?;
    let (input, success) = block(input)?;
    let input = next(input);
    if let Ok((input, _)) = tokens::else_tok(input) {
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
    let (input, (id, _)) = spaced_identifier(input)?;
    let (input, _) = tokens::in_tok(input)?;
    let (input, expr) = expr(input)?;
    let (input, block) = block(input)?;
    let (input, end_loc) = position(input)?;
    let iterator = Symbol::from(id);
    let for_loop = Node::Loop(
        LoopKind::For {
            iterator,
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

fn unit_func(
    input: ParseInput,
    kind: FunctionKind,
    start_loc: Location,
) -> ParseResult<ParseInput, Ast> {
    let (input, decl) = func_declaration(input)?;
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
    let (input, (path, (_, end_loc))) = spaced_identifier(input)?;
    if let Ok((input, _)) = tokens::az_tok(input) {
        let (input, alias) = preceded(nom_next, tokens::identifier)(input)?;
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

fn type_id(input: ParseInput) -> ParseResult<ParseInput, Type> {
    fn arg_types(input: ParseInput) -> ParseResult<ParseInput, Vec<Type>> {
        if let Ok((input, _)) = tokens::right_parenthesis(input) {
            return Ok((input, vec![]));
        }

        let (input, first_arg) = type_id(input)?;
        let (input, mut args) = many0(preceded(tokens::comma, type_id))(input)?;
        let (input, _) = tokens::right_parenthesis(input)?;

        args.insert(0, first_arg);

        Ok((input, args))
    }

    fn return_type(input: ParseInput) -> ParseResult<ParseInput, Type> {
        let input = next(input);
        let (input, _) = tokens::arrow(input)?;
        let input = next(input);
        type_id(input)
    }

    fn type_id_no_multi(input: ParseInput) -> ParseResult<ParseInput, Type> {
        let input = next(input);
        let (input, start_loc) = position(input)?;
        if let Ok((input, _)) = tokens::func_tok(input) {
            let (input, generics) = maybe_generic_application(input)?;
            let (input, _) = tokens::left_parenthesis(input)?;
            let (input, args) = arg_types(input)?;
            let (input, ret_ty) = opt(return_type)(input)?;
            let (input, end_loc) = position(input)?;

            let kind = TypeKind::FunctionLike(args, ret_ty.map(Box::new));

            Ok((
                input,
                Type {
                    kind,
                    generics,
                    location: pos_to_loc(input, start_loc, end_loc),
                },
            ))
        } else {
            fn literal(input: ParseInput) -> ParseResult<ParseInput, Type> {
                let (input, (location, kind)) = string_constant
                    .or(float_constant)
                    .or(int_constant)
                    .map(|literal| {
                        (
                            literal.location.clone(),
                            TypeKind::Literal(Box::new(literal)),
                        )
                    })
                    .parse(input)?;

                Ok((
                    input,
                    Type {
                        kind,
                        generics: vec![],
                        location,
                    },
                ))
            }

            fn type_symbol(input: ParseInput) -> ParseResult<ParseInput, Type> {
                let (input, (kind, (start_loc, end_loc))) = spaced_identifier
                    .map(|(id, loc)| (TypeKind::Simple(Symbol::from(id)), loc))
                    .parse(input)?;

                let (input, generics) = maybe_generic_application(input)?;

                // FIXME: Refactor
                let end_loc = if !generics.is_empty() {
                    position(input)?.1.into()
                } else {
                    end_loc
                };

                Ok((
                    input,
                    Type {
                        kind,
                        generics,
                        location: pos_to_loc(input, start_loc, end_loc),
                    },
                ))
            }

            type_symbol.or(literal).parse(input)
        }
    }

    let (input, start_loc) = position(input)?;
    let (input, ty_arg) = type_id_no_multi(input)?;

    if delimited(nom_next, tokens::pipe, nom_next)(input).is_ok() {
        let (input, types) = many0(tuple((nom_next, tokens::pipe, type_id_no_multi)))(input)?;
        let mut multi = vec![ty_arg];
        // FIXME: butt ugly
        multi.extend(types.into_iter().map(|tuple| tuple.2));
        let (input, end_loc) = position(input)?;

        Ok((
            input,
            Type {
                kind: TypeKind::Multi(multi),
                generics: vec![],
                location: pos_to_loc(input, start_loc, end_loc),
            },
        ))
    } else {
        Ok((input, ty_arg))
    }
}

/// ```text
/// generic_arg ::= identifier [ "=" type ]?
/// id_and_generics ::= identifier [ "[" generic_arg+ "]" ]?
///
/// # int
/// # int | ComplexType
/// type ::= id_and_generics | type "|" id_and_generics
///
/// type_declaration ::=
///     // record type
///     | "type" id_and_generics "(" [id_and_generics ":" type ]","* ")" ";"
///     // tuple type
///     | "type" id_and_generics "(" [type]","* ")"
///     // type alias
///     | "type" id_and_generics = type ";"
/// ```
fn unit_type_decl(input: ParseInput, start_loc: Location) -> ParseResult<ParseInput, Ast> {
    // FIXME: This needs to use TypeIds
    let (input, (name, _)) = spaced_identifier(input)?;
    let (input, generics) = maybe_generic_arguments(input)?;

    let (input, type_dec) = if let Ok((input, _)) = tokens::left_parenthesis(input) {
        let (input, first_field) = typed_arg(input)?;
        let (input, mut fields) = many0(preceded(tokens::comma, typed_arg))(input)?;
        let (input, _) = tokens::right_parenthesis(input)?;

        fields.insert(0, first_field);

        (
            input,
            Node::Type {
                name: Symbol::from(name),
                generics,
                fields: TypeContent::Record(fields),
                with: None, // TODO: Parse `with` block properly
            },
        )
    } else if let Ok((input, _)) = tokens::equal(input) {
        let input = next(input);
        let (input, type_aliased) = type_id(input)?;
        (
            input,
            Node::Type {
                name: Symbol::from(name),
                generics,
                // FIXME: this is invalid
                fields: TypeContent::Alias(type_aliased),
                with: None,
            },
        )
    } else {
        (
            input,
            Node::Type {
                name: Symbol::from(name),
                generics,
                fields: TypeContent::None,
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

/// mut? [`spaced_identifier`] '=' expr
fn unit_var_decl(input: ParseInput) -> ParseResult<ParseInput, Ast> {
    let input = next(input);
    let (input, mutable) = opt(tokens::mut_tok)(input)?;
    let mutable = mutable.is_some();

    let (input, (symbol, (start_loc, _))) = spaced_identifier(input)?;
    let (input, _) = tokens::equal(input)?;
    let (input, value) = expr(input)?;
    let (input, end_loc) = position(input)?;

    let assignment = Node::VarDeclaration {
        mutable,
        to_declare: Symbol::from(symbol),
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
fn unit_jk_inst(_input: ParseInput, _start_loc: Location) -> ParseResult<ParseInput, Ast> {
    // let (input, name) = delimited(nom_next, tokens::identifier, nom_next)(input)?;
    // let (input, _) = tokens::left_parenthesis(input)?;
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
    let (input, decl) = delimited(tokens::func_tok, func_declaration, tokens::semicolon)(input)?;
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
fn generic_list(input: ParseInput) -> ParseResult<ParseInput, Vec<Type>> {
    fn whitespace_plus_type(input: ParseInput) -> ParseResult<ParseInput, Type> {
        let input = next(input);
        let (input, ty) = type_id(input)?;
        let input = next(input);

        Ok((input, ty))
    }

    let (input, first_type) = whitespace_plus_type(input)?;
    let (input, mut generics) = many0(preceded(tokens::comma, whitespace_plus_type))(input)?;
    let (input, _) = tokens::right_bracket(input)?;

    generics.insert(0, first_type);
    Ok((input, generics))
}

// FIXME: This does not parse default generic types yet (`func f[T = int]()`)
fn generic_arguments(input: ParseInput) -> ParseResult<ParseInput, Vec<GenericParameter>> {
    fn whitespace_plus_generic(input: ParseInput) -> ParseResult<ParseInput, GenericParameter> {
        let input = next(input);
        let (input, (id, _)) = spaced_identifier(input)?;
        let input = next(input);

        Ok((
            input,
            GenericParameter {
                name: Symbol::from(id),
                default: None,
            },
        ))
    }

    let (input, first_type) = whitespace_plus_generic(input)?;
    let (input, mut generics) = many0(preceded(tokens::comma, whitespace_plus_generic))(input)?;
    let (input, _) = tokens::right_bracket(input)?;

    generics.insert(0, first_type);
    Ok((input, generics))
}

fn maybe_generic_arguments(input: ParseInput) -> ParseResult<ParseInput, Vec<GenericParameter>> {
    if let Ok((input, _)) = tokens::left_bracket(input) {
        Ok(generic_arguments(input)?)
    } else {
        Ok((input, vec![]))
    }
}

fn maybe_generic_application(input: ParseInput) -> ParseResult<ParseInput, Vec<Type>> {
    if let Ok((input, _)) = tokens::left_bracket(input) {
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

    let (input, _) = tokens::left_parenthesis(input)?;
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
fn return_type(input: ParseInput) -> ParseResult<ParseInput, Option<Type>> {
    match tokens::arrow(input) {
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
    let (input, _) = tokens::left_curly_bracket(input)?;
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
    if let Ok((input, _)) = tokens::right_curly_bracket(input) {
        return Ok((
            input,
            Node::Block {
                stmts: vec![],
                last_is_expr: false,
            },
        ));
    }

    let (input, inst) = expr(input)?;
    if let Ok((input, _)) = tokens::right_curly_bracket(input) {
        return Ok((
            input,
            Node::Block {
                stmts: vec![inst],
                last_is_expr: true, // FIXME: Is that right?
            },
        ));
    }

    let (input, block) = preceded(tokens::semicolon, preceded(nom_next, inner_block))(input)?;
    let (mut stmts, last_is_expr) = match block {
        Node::Block {
            stmts,
            last_is_expr,
        } => (stmts, last_is_expr),
        _ => unreachable!(),
    };

    stmts.insert(0, inst);

    Ok((
        input,
        Node::Block {
            stmts,
            last_is_expr,
        },
    ))
}

/// func_type_or_var = '(' next func_or_type_inst_args
///                  | '=' expr                   (* variable assigment *)
///                  | ε                          (* variable or empty type instantiation *)
fn func_type_or_var(
    input: ParseInput,
    id: String,
    start_loc: Location,
) -> ParseResult<ParseInput, Ast> {
    if let Ok((input, _)) = tokens::left_bracket(input) {
        generic_func_or_type_inst_args(next(input), id, start_loc)
    } else if let Ok((input, _)) = tokens::left_parenthesis(input) {
        func_or_type_inst_args(next(input), id, vec![], start_loc)
    } else if let Ok((input, _)) = tokens::equal(input) {
        let (input, value) = expr(input)?;
        let (input, end_loc) = position(input)?;
        let var_assign = Node::VarAssign {
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
    generics: Vec<Type>,
    start_loc: Location,
) -> ParseResult<ParseInput, Ast> {
    let (input, first_attr_start_loc) = position(input)?;
    if let Ok((input, first_attr)) =
        terminated(terminated(tokens::identifier, nom_next), tokens::colon)(input)
    {
        let input = next(input);
        let (input, first_attr_val) = expr(input)?;
        let (input, first_attr_end_loc) = position(input)?;
        let (input, mut attrs) = many0(preceded(tokens::comma, type_inst_arg))(input)?;
        let (input, _) = tokens::right_parenthesis(input)?;
        let (input, end_loc) = position(input)?;

        attrs.insert(
            0,
            Ast {
                location: pos_to_loc(input, first_attr_start_loc, first_attr_end_loc),
                node: Node::VarAssign {
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
    let (input, _) = tokens::left_parenthesis(input)?;

    func_or_type_inst_args(next(input), id, generics, start_loc)
}

///
/// ARGS
///

/// args = expr ( ',' expr )* ')'
///      | ')'
fn args(input: ParseInput) -> ParseResult<ParseInput, Vec<Ast>> {
    if let Ok((input, _)) = tokens::right_parenthesis(input) {
        return Ok((input, vec![]));
    }
    let (input, first_arg) = expr(input)?;
    let (input, mut args) = many0(preceded(tokens::comma, expr))(input)?;
    let (input, _) = tokens::right_parenthesis(input)?;

    args.insert(0, first_arg);
    Ok((input, args))
}

/// typed_args = typed_arg ( ',' typed_arg )* ')'
///           | ')'
fn typed_args(input: ParseInput) -> ParseResult<ParseInput, Vec<TypedValue>> {
    if let Ok((input, _)) = tokens::right_parenthesis(input) {
        return Ok((input, vec![]));
    }
    let (input, first_arg) = typed_arg(input)?;
    let (input, mut args) = many0(preceded(tokens::comma, typed_arg))(input)?;
    let (input, _) = tokens::right_parenthesis(input)?;

    args.insert(0, first_arg);
    Ok((input, args))
}

/// typed_arg = spaced_identifier ':' type_id
fn typed_arg(input: ParseInput) -> ParseResult<ParseInput, TypedValue> {
    let (input, (id, (start_loc, _))) = spaced_identifier(input)?;
    let (input, _) = tokens::colon(input)?;
    let input = next(input);
    let (input, ty) = type_id(input)?;
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
    let (input, _) = tokens::colon(input)?;
    let input = next(input);
    let (input, value) = expr(input)?;
    let (input, end_loc) = position(input)?;
    let input = next(input);

    let location = pos_to_loc(input, start_loc, end_loc);
    let node = Node::VarAssign {
        to_assign: Symbol::from(id),
        value: Box::new(value),
    };

    Ok((input, Ast { location, node }))
}

fn spaced_identifier(input: ParseInput) -> ParseResult<ParseInput, (String, (Location, Location))> {
    let input = next(input);
    let (input, start_loc) = position(input)?;
    let (input, id) = tokens::identifier(input)?;
    let (input, end_loc) = position(input)?;
    let input = next(input);

    Ok((input, (id, (start_loc.into(), end_loc.into()))))
}

/// next = extra*
/// extra = WHITESPACE
///       | '/*' [^'*/'] '*/'
///       | '//' [^\n]   '\n'
///       | '#'  [^\n]   '\n'
pub fn next(input: ParseInput) -> ParseInput {
    // FIXME: Do not unwrap
    let (input, _) = multispace0::<_, Error>(input).unwrap();
    match tokens::consume_comment(input) {
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

pub(crate) fn char_constant(input: ParseInput) -> ParseResult<ParseInput, Ast> {
    let (input, start) = position(input)?;
    let (input, char_value) = tokens::char_constant(input)?;
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
    let (input, _) = tokens::double_quote(input)?;
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
    if let Ok((input, _)) = tokens::double_quote(input) {
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

    if let Ok((input, _)) = tokens::left_curly_bracket(input) {
        terminated(expr, tokens::right_curly_bracket)(input)
    } else if let Ok((input, _)) = tokens::backslash(input) {
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
            unknown => {
                return Err(NomError(
                    // FIXME: Reuse this
                    // Error::new(ErrKind::Parsing).with_msg(String::from("Unknown character escape")),
                    Error::Msg(format!("unknown character escape: `{unknown}`")),
                ));
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
            // FIXME: Reuse this
            // Error::new(ErrKind::Parsing).with_msg(String::from("undelimited string")),
            Error::Msg("undelimited string".to_string()),
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
    let (input, value) = tokens::float_constant(input)?;
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
    let (input, value) = tokens::int_constant(input)?;
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
    let (input, value) = tokens::bool_constant(input)?;
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

    use ast::Node::*;
    use ast::Operator;
    use ast::Value::*;

    #[test]
    fn block_expr() {
        let (_, ast) = expr(span!("{ a }")).unwrap();

        let last_is_expr = match ast.node {
            Block { last_is_expr, .. } => last_is_expr,
            _ => unreachable!(),
        };

        assert!(last_is_expr);

        let (_, ast) = expr(span!("{ }")).unwrap();

        let last_is_expr = match ast.node {
            Block { last_is_expr, .. } => last_is_expr,
            _ => unreachable!(),
        };

        assert!(!last_is_expr);

        let (_, ast) = expr(span!("{ a; b; c; }")).unwrap();

        let last_is_expr = match ast.node {
            Block { last_is_expr, .. } => last_is_expr,
            _ => unreachable!(),
        };

        assert!(!last_is_expr);

        let (_, ast) = expr(span!("{ a; b; c }")).unwrap();

        let last_is_expr = match ast.node {
            Block { last_is_expr, .. } => last_is_expr,
            _ => unreachable!(),
        };

        assert!(last_is_expr);
    }

    #[test]
    fn consume_whitespace() {
        assert_eq!(
            nom_next(span!("   input")).map(|(input, _)| *input.fragment()),
            Ok("input")
        );
        assert_eq!(
            nom_next(span!(" \t input")).map(|(input, _)| *input.fragment()),
            Ok("input")
        );
    }

    #[test]
    fn simple_int_sum() {
        let (input, expr) = expr(span!(" 401 + 809 ")).unwrap();

        let (op, lhs, rhs) = match expr.node {
            BinaryOp(op, lhs, rhs) => match (lhs.node, rhs.node) {
                (Constant(Integer(lhs)), Constant(Integer(rhs))) => (op, lhs, rhs),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };

        assert_eq!(*input.fragment(), "");
        assert_eq!(op, Operator::Add);
        assert_eq!(lhs, 401);
        assert_eq!(rhs, 809);
    }

    #[test]
    fn simple_float_mul() {
        let (input, expr) = expr(span!("3.15 * 9.999")).unwrap();

        let (op, lhs, rhs) = match expr.node {
            BinaryOp(op, lhs, rhs) => match (lhs.node, rhs.node) {
                (Constant(Float(lhs)), Constant(Float(rhs))) => (op, lhs, rhs),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };

        assert_eq!(*input.fragment(), "");
        assert_eq!(op, Operator::Mul);
        assert_eq!(lhs, 3.15);
        assert_eq!(rhs, 9.999);
    }

    //     #[test]
    //     fn chained_sum_sub() {
    //         let (input, expr) = expr(span!("239 + 809 - 1004")).unwrap();
    //         let (sub_op, add_op, first, second, third) = match expr.node {
    //             BinaryOp(sub_op, lhs, third) => match lhs.node {
    //                 BinaryOp(add_op, first, second) => (sub_op, add_op, first, second, third),
    //                 _ => unreachable!(),
    //             },
    //             _ => unreachable!(),
    //         };

    //         assert_eq!(*input.fragment(), "");
    //         assert_eq!(sub_op, Operator::Sub);
    //         assert_eq!(add_op, Operator::Add);
    //         assert_eq!(first, 239);
    //         assert_eq!(second, 809);
    //         assert_eq!(third, 1004);
    //     }

    //     #[test]
    //     fn math_precedence() {
    //         let (input, expr) = expr(span!("5.9 + 128 / 809.1 - 1004")).unwrap();
    //         let sub: &BinaryOp = expr.downcast_ref().unwrap();
    //         let add: &BinaryOp = sub.lhs().downcast_ref().unwrap();
    //         let div: &BinaryOp = add.rhs().downcast_ref().unwrap();

    //         let first: &JkFloat = add.lhs().downcast_ref().unwrap();
    //         let second: &JkInt = div.lhs().downcast_ref().unwrap();
    //         let third: &JkFloat = div.rhs().downcast_ref().unwrap();
    //         let fourth: &JkInt = sub.rhs().downcast_ref().unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert_eq!(sub.operator(), Operator::Sub);
    //         assert_eq!(add.operator(), Operator::Add);
    //         assert_eq!(div.operator(), Operator::Div);
    //         assert_eq!(first.print(), "5.9");
    //         assert_eq!(second.print(), "128");
    //         assert_eq!(third.print(), "809.1");
    //         assert_eq!(fourth.print(), "1004");
    //     }

    //     #[test]
    //     fn tricky_math_precedence() {
    //         let (input, expr) = expr(span!("5.9 + 128 / 809.1 - 1 * 1.1")).unwrap();
    //         let sub: &BinaryOp = expr.downcast_ref().unwrap();
    //         let add: &BinaryOp = sub.lhs().downcast_ref().unwrap();
    //         let mul: &BinaryOp = sub.rhs().downcast_ref().unwrap();
    //         let div: &BinaryOp = add.rhs().downcast_ref().unwrap();

    //         let first: &JkFloat = add.lhs().downcast_ref().unwrap();
    //         let second: &JkInt = div.lhs().downcast_ref().unwrap();
    //         let third: &JkFloat = div.rhs().downcast_ref().unwrap();
    //         let fourth: &JkInt = mul.lhs().downcast_ref().unwrap();
    //         let fifth: &JkFloat = mul.rhs().downcast_ref().unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert_eq!(sub.operator(), Operator::Sub);
    //         assert_eq!(add.operator(), Operator::Add);
    //         assert_eq!(mul.operator(), Operator::Mul);
    //         assert_eq!(div.operator(), Operator::Div);
    //         assert_eq!(first.print(), "5.9");
    //         assert_eq!(second.print(), "128");
    //         assert_eq!(third.print(), "809.1");
    //         assert_eq!(fourth.print(), "1");
    //         assert_eq!(fifth.print(), "1.1");
    //     }

    //     #[test]
    //     fn method_call_no_arg() {
    //         let (input, expr) = expr(span!("a.call( )")).unwrap();

    //         assert!(expr.downcast_ref::<MethodCall>().is_some());
    //         assert_eq!(*input.fragment(), "");
    //     }

    //     #[test]
    //     fn method_call_one_arg() {
    //         let (input, expr) = expr(span!("a.call(\"hello\")")).unwrap();

    //         assert!(expr.downcast_ref::<MethodCall>().is_some());
    //         assert_eq!(*input.fragment(), "");
    //     }

    //     #[test]
    //     fn method_call_many_args() {
    //         let (input, expr) = expr(span!("a.call(\"hello\", 1   , variable)")).unwrap();

    //         assert!(expr.downcast_ref::<MethodCall>().is_some());
    //         assert_eq!(*input.fragment(), "");
    //     }

    //     #[test]
    //     fn method_call_many() {
    //         let (input, expr) = expr(span!("a.call(\"hello\").sub().subsub(1, 20)")).unwrap();

    //         assert!(expr.downcast_ref::<MethodCall>().is_some());
    //         assert_eq!(*input.fragment(), "");
    //     }

    //     #[test]
    //     fn method_call_on_bool() {
    //         let (input, expr) = expr(span!("true.call( )")).unwrap();

    //         assert!(expr.downcast_ref::<MethodCall>().is_some());
    //         assert_eq!(*input.fragment(), "");
    //     }

    //     #[test]
    //     fn field_access_single() {
    //         let (input, expr) = expr(span!("a.attribute")).unwrap();

    //         assert!(expr.downcast_ref::<FieldAccess>().is_some());
    //         assert_eq!(*input.fragment(), "");
    //     }

    //     #[test]
    //     fn field_access_many() {
    //         let (input, expr) = expr(span!("a.attr.sub_attr.subsub")).unwrap();

    //         assert!(expr.downcast_ref::<FieldAccess>().is_some());
    //         assert_eq!(*input.fragment(), "");
    //     }

    //     #[test]
    //     fn field_access_many_newline() {
    //         let (input, expr) = expr(span!("a\n.attr\n.sub_attr\n.subsub")).unwrap();

    //         assert!(expr.downcast_ref::<FieldAccess>().is_some());
    //         assert_eq!(*input.fragment(), "");
    //     }

    //     #[test]
    //     fn if_no_else() {
    //         let (input, expr) = expr(span!("if 1 + 1 { 10 / 2 }")).unwrap();

    //         assert!(expr.downcast_ref::<IfElse>().is_some());
    //         assert_eq!(*input.fragment(), "");
    //     }

    //     #[test]
    //     fn if_else() {
    //         let (input, expr) = expr(span!("if 1 + 1 { 10 / 2 } else { var }")).unwrap();

    //         assert!(expr.downcast_ref::<IfElse>().is_some());
    //         assert_eq!(*input.fragment(), "");
    //     }

    //     #[test]
    //     fn while_loop() {
    //         let (input, expr) = expr(span!("while true { var + 10 }")).unwrap();

    //         assert!(expr.downcast_ref::<Loop>().is_some());
    //         assert_eq!(*input.fragment(), "");
    //     }

    //     #[test]
    //     fn for_loop() {
    //         let (input, expr) = expr(span!("for entry in collection { entry.print() }")).unwrap();

    //         assert!(expr.downcast_ref::<Loop>().is_some());
    //         assert_eq!(*input.fragment(), "");
    //     }

    //     #[test]
    //     fn loop_basic() {
    //         let (input, expr) = expr(span!("loop { variable.get() + 10 }")).unwrap();

    //         assert!(expr.downcast_ref::<Loop>().is_some());
    //         assert_eq!(*input.fragment(), "");
    //     }

    //     #[test]
    //     fn funcion_dec_no_arg() {
    //         let (input, expr) = expr(span!("func a ( ) { 1 }")).unwrap();
    //         let func = expr.downcast_ref::<FunctionDec>().unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert!(func.args().is_empty());
    //     }

    //     #[test]
    //     fn funcion_dec_one_arg() {
    //         let (input, expr) = expr(span!("func id ( arg: int ) { arg }")).unwrap();
    //         let func = expr.downcast_ref::<FunctionDec>().unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert!(func.args().len() == 1);
    //         assert!(func.args()[0].name() == "arg");
    //         assert!(func.args()[0].get_type().id() == "int");
    //     }

    //     #[test]
    //     fn funcion_dec_many_args() {
    //         let (input, expr) = expr(span!(
    //             "func concat ( arg1: char , arg2: int,arg3:float, arg4: string) { arg1 + arg2 + arg3 }"
    //         ))
    //         .unwrap();
    //         let func = expr.downcast_ref::<FunctionDec>().unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert!(func.args().len() == 4);
    //         assert!(func.args()[0].name() == "arg1");
    //         assert!(func.args()[3].name() == "arg4");
    //     }

    //     #[test]
    //     fn funcion_dec_many_args_with_return() {
    //         let (input, expr) =
    //             expr(span!("func concat ( arg1: char , arg2: int,arg3:float, arg4: string) -> string { arg1 + arg2 + arg3 }")).unwrap();
    //         let func = expr.downcast_ref::<FunctionDec>().unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert_eq!(func.args().len(), 4);
    //         assert_eq!(func.ty().unwrap().id(), "string");
    //         assert_eq!(func.args()[0].name(), "arg1");
    //         assert_eq!(func.args()[3].name(), "arg4");
    //     }

    //     #[test]
    //     fn funcion_dec_no_arg_with_return() {
    //         let (input, expr) = expr(span!("func a ( ) -> int { 1 }")).unwrap();
    //         let func = expr.downcast_ref::<FunctionDec>().unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert_eq!(func.ty().unwrap().id(), "int");
    //         assert!(func.args().is_empty());
    //     }

    //     #[test]
    //     fn test_dec_no_arg() {
    //         let (input, expr) = expr(span!("test a ( ) { true }")).unwrap();
    //         let func = expr.downcast_ref::<FunctionDec>().unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert_eq!(func.fn_kind(), FunctionKind::Test);
    //     }

    //     #[test]
    //     fn mock_dec_one_arg() {
    //         let (input, expr) = expr(span!("mock id ( arg: int ) { arg }")).unwrap();
    //         let func = expr.downcast_ref::<FunctionDec>().unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert_eq!(func.fn_kind(), FunctionKind::Mock);
    //     }

    //     #[test]
    //     fn block_empty() {
    //         let (input, expr) = expr(span!("{ }")).unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert!(expr.downcast_ref::<Block>().is_some());
    //     }

    //     #[test]
    //     fn block_one_inst() {
    //         let (input, expr) = expr(span!("{ var }")).unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert!(expr.downcast_ref::<Block>().is_some());
    //     }

    //     #[test]
    //     fn block_many_inst() {
    //         let (input, expr) = expr(span!(
    //             "{
    //             var = 1 + 1;
    //             var = var - 2;
    //             var
    //                                  }"
    //         ))
    //         .unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert!(expr.downcast_ref::<Block>().is_some());
    //     }

    //     #[test]
    //     fn block_statement() {
    //         let (input, expr) = expr(span!(
    //             "{
    //             var = 1 + 1;
    //             var = var - 2;
    //             var;
    //                                  }"
    //         ))
    //         .unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert!(expr.downcast_ref::<Block>().is_some());
    //     }

    //     #[test]
    //     fn block_missing_closing() {
    //         assert!(expr(span!(
    //             "{
    //             var = 1 + 1;
    //             var = var - 2;
    //             var
    //             "
    //         ))
    //         .is_err())
    //     }

    //     #[test]
    //     fn type_dec_one_field() {
    //         let (input, expr) = expr(span!("type Num ( val : int )")).unwrap();
    //         let dec = expr.downcast_ref::<TypeDec>().unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert_eq!(dec.name(), "Num");
    //         assert_eq!(dec.fields().len(), 1);
    //     }

    //     #[test]
    //     fn type_dec_multiple_field() {
    //         let (input, expr) = expr(span!("type Point( x : int , y: int )")).unwrap();
    //         let dec = expr.downcast_ref::<TypeDec>().unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert_eq!(dec.name(), "Point");
    //         assert_eq!(dec.fields().len(), 2);
    //     }

    //     #[test]
    //     fn type_dec_incomplete() {
    //         assert!(expr(span!("type Point( x:int , y: )")).is_err());
    //     }

    //     #[test]
    //     fn include_simple() {
    //         let (input, expr) = expr(span!("incl pair")).unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert!(expr.downcast_ref::<Incl>().is_some());
    //     }

    //     #[test]
    //     fn include_with_alias() {
    //         let (input, expr) = expr(span!("incl numpy as np")).unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert!(expr.downcast_ref::<Incl>().is_some());
    //     }

    //     #[test]
    //     fn include_with_alias_missing_path() {
    //         assert!(expr(span!("incl as uoh")).is_err());
    //     }

    //     #[test]
    //     fn var_assignment() {
    //         let (input, expr) = expr(span!("var = 'a'")).unwrap();
    //         let assign = expr.downcast_ref::<VarAssign>().unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert_eq!(assign.symbol(), "var");
    //         assert!(!assign.mutable());
    //     }

    //     #[test]
    //     fn var_assigment_tricky() {
    //         let (input, expr) = expr(span!("n1=b.call() + 1")).unwrap();
    //         let assign = expr.downcast_ref::<VarAssign>().unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert_eq!(assign.symbol(), "n1");
    //         assert!(!assign.mutable());
    //     }

    //     #[test]
    //     fn mut_var_assigment() {
    //         let (input, expr) = expr(span!("mut var = b.call() + 1")).unwrap();
    //         let assign = expr.downcast_ref::<VarAssign>().unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert_eq!(assign.symbol(), "var");
    //         assert!(assign.mutable());
    //     }

    //     #[test]
    //     fn t_comparison_op_valid() {
    //         assert!(expr(span!("a <   12 ")).is_ok());
    //         assert!(expr(span!("some() > 12.1")).is_ok());
    //     }

    //     #[test]
    //     fn t_binary_op_invalid() {
    //         let (input, expr) = expr(span!("a ? 12")).unwrap();

    //         assert!(expr.downcast_ref::<BinaryOp>().is_none());
    //         assert_eq!(*input.fragment(), "? 12");
    //     }

    //     #[test]
    //     fn var_assigment_mut_in_name() {
    //         let (input, expr) = expr(span!("mut_var = b.call() + 1")).unwrap();
    //         let assign = expr.downcast_ref::<VarAssign>().unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert_eq!(assign.symbol(), "mut_var");
    //         assert!(!assign.mutable());
    //     }

    //     #[test]
    //     fn mut_var_assigment_mut_in_name() {
    //         let (input, expr) = expr(span!("mut mut_var = b.call() + 1")).unwrap();
    //         let assign = expr.downcast_ref::<VarAssign>().unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert_eq!(assign.symbol(), "mut_var");
    //         assert!(assign.mutable());
    //     }

    //     #[test]
    //     fn jk_inst_no_arg() {
    //         let (input, expr) = expr(span!("@quit ( )")).unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert!(expr.downcast_ref::<JkInst>().is_some());
    //     }

    //     #[test]
    //     fn jk_inst_arg() {
    //         let (input, expr) = expr(span!("@dump ( thing )")).unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert!(expr.downcast_ref::<JkInst>().is_some());
    //     }

    //     #[test]
    //     fn jk_inst_non_existant() {
    //         assert!(expr(span!("@crab ( thing )")).is_err());
    //     }

    //     #[test]
    //     fn extern_no_args() {
    //         let (input, expr) = expr(span!("ext func exit () ;")).unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert!(expr.downcast_ref::<FunctionDec>().is_some());
    //     }

    //     #[test]
    //     fn extern_many_args() {
    //         let (input, expr) = expr(span!("ext func memcpy(dst: char, src: char, n: int);")).unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert!(expr.downcast_ref::<FunctionDec>().is_some());
    //     }

    //     #[test]
    //     fn extern_missing_semicolon() {
    //         assert!(expr(span!("ext func memcpy(dst: char , n: int) ")).is_err());
    //     }

    //     #[test]
    //     fn return_nothing() {
    //         let (input, expr) = expr(span!("return")).unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert!(expr.downcast_ref::<Return>().is_some());
    //     }

    //     #[test]
    //     fn return_sum() {
    //         let (input, expr) = expr(span!("return 10 + 9")).unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert!(expr.downcast_ref::<Return>().is_some());
    //     }

    //     /// Mimic previous parsers behaviour
    //     #[test]
    //     #[ignore]
    //     fn return_malformed() {
    //         let (input, expr) = expr(span!("return 10 +")).unwrap();

    //         assert_eq!(*input.fragment(), "10 +");
    //         assert!(expr.downcast_ref::<Return>().is_none());
    //     }

    //     #[test]
    //     fn function_call_no_arg() {
    //         let (input, expr) = expr(span!("call ( )")).unwrap();
    //         let func = expr.downcast_ref::<FunctionCall>().unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert_eq!(func.name(), "call");
    //         assert!(func.args().is_empty());
    //     }

    //     #[test]
    //     fn function_call_one() {
    //         let (input, expr) = expr(span!("id ( 10 )")).unwrap();
    //         let func = expr.downcast_ref::<FunctionCall>().unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert_eq!(func.name(), "id");
    //         assert_eq!(func.args().len(), 1);
    //     }

    //     #[test]
    //     fn function_call_many() {
    //         let (input, expr) = expr(span!("concat( 'h','e', 'l' , 'l', 'o')")).unwrap();
    //         let func = expr.downcast_ref::<FunctionCall>().unwrap();

    //         assert_eq!(*input.fragment(), "");
    //         assert_eq!(func.name(), "concat");
    //         assert_eq!(func.args().len(), 5);
    //     }

    #[test]
    fn function_call_missing_paren() {
        assert!(expr(span!("concat( 'h','e', 'l' , 'l', 'o'")).is_err());
    }

    #[test]
    fn function_call_double_paren() {
        assert!(expr(span!("fn((")).is_err());
    }

    #[test]
    fn multi_comment_multi_line() {
        let input = span!(
            r#"/**
        * This function does nothing
        */
        func void() { }"#
        );

        let (input, expr) = expr(input).unwrap();
        assert!(matches!(expr.node, Function { .. }));

        assert_eq!(*input.fragment(), "");
    }

    #[test]
    fn sing_comment_multi_line() {
        let input = span!(
            r#" // Comment
        func void() { }"#
        );

        let (input, expr) = expr(input).unwrap();
        assert!(matches!(expr.node, Function { .. }));

        assert_eq!(*input.fragment(), "");
    }

    #[test]
    fn hashtag_comment_multi_line() {
        let input = span!(
            r##"# Comment
    func void() { }"##
        );

        let (input, expr) = expr(input).unwrap();
        assert!(matches!(expr.node, Function { .. }));

        assert_eq!(*input.fragment(), "");
    }

    #[test]
    fn multiple_different_comments() {
        let input = span!(
            r##"# Comment
    # Another one
                /**
                   * Some documentation
                   */
        func void() { }"##
        );

        let (input, expr) = expr(input).unwrap();
        assert!(matches!(expr.node, Function { .. }));

        assert_eq!(*input.fragment(), "");
    }

    #[test]
    fn multiple_different_comments_close() {
        let input = span!(
            r##"# Comment
    # Another one
                /**
                   * Some documentation
                   *//* Some more */
        func void() { }"##
        );

        let (input, expr) = expr(input).unwrap();
        assert!(matches!(expr.node, Function { .. }));

        assert_eq!(*input.fragment(), "");
    }

    #[test]
    fn lt_exprs() {
        assert!(expr(span!("a < b")).is_ok())
    }

    #[test]
    fn gt_exprs() {
        assert!(expr(span!("a > b")).is_ok())
    }

    #[test]
    fn lte_exprs() {
        assert!(expr(span!("lhs <= rhs")).is_ok())
    }

    #[test]
    fn gte_exprs() {
        assert!(expr(span!("lhs >= rhs")).is_ok())
    }

    #[test]
    fn exprs_equals() {
        assert!(expr(span!("lhs == rhs")).is_ok())
    }

    #[test]
    fn exprs_not_equals() {
        assert!(expr(span!("lhs != rhs")).is_ok())
    }

    #[test]
    fn expr_with_parenthesis() {
        assert!(expr(span!("lhs + (rhs - lhs)")).is_ok())
    }

    #[test]
    fn parentheses() {
        let (input, expr) = expr(span!("4 * (3 + 5)")).unwrap();
        assert!(matches!(expr.node, BinaryOp { .. }));

        assert_eq!(*input.fragment(), "");
    }

    #[test]
    fn multi_type_2() {
        assert!(expr(span!("func takes_mt(a: int | string) {}")).is_ok())
    }

    #[test]
    fn multi_type_unclosed() {
        assert!(expr(span!("func invalid_mt(a: int |) {}")).is_err())
    }

    #[test]
    fn multi_type_long() {
        assert!(expr(span!(
            "func long_mt(a: int | string | float | char | bool) {}"
        ))
        .is_ok())
    }

    #[test]
    fn multi_type_in_type_dec() {
        assert!(expr(span!(
            "type MultiTy(a: int | string | float | char | bool);"
        ))
        .is_ok())
    }

    #[test]
    fn func_dec_one_generic() {
        assert!(expr(span!("func a[T]() {}")).is_ok())
    }

    #[test]
    fn func_dec_multiple_generic() {
        assert!(expr(span!("func a[T, U, V]() {}")).is_ok())
    }

    #[test]
    fn func_dec_generic_and_whitespace() {
        assert!(expr(span!("func a[    T]() {}")).is_ok());
        assert!(expr(span!("func a[    T  ]() {}")).is_ok());
        assert!(expr(span!("func a[   T , U    , V]() {}")).is_ok())
    }

    #[test]
    fn func_dec_empty_generic_list() {
        assert!(expr(span!("func a[]() {}")).is_err())
    }

    #[test]
    fn func_dec_no_generic_delimiter() {
        assert!(expr(span!("func a[T, U() {}")).is_err())
    }

    #[test]
    fn func_call_generics_one() {
        assert!(expr(span!("fn_call[T]()")).is_ok());
    }

    #[test]
    fn func_call_generics_multi() {
        assert!(expr(span!("fn_call[T, U, V]()")).is_ok());
    }

    #[test]
    fn func_call_generics_multi_and_args() {
        assert!(expr(span!("fn_call[T, U, V](a, b, c)")).is_ok());
    }

    #[test]
    fn type_inst_generics_multi_and_args() {
        assert!(expr(span!("TypeInst[T, U, V](a: 0, b: 1, c: 2)")).is_ok());
    }

    #[test]
    fn empty_type_declaration() {
        assert!(expr(span!("type CustomType;")).is_ok())
    }

    #[test]
    fn empty_type_instantiation() {
        assert!(expr(span!("a = CustomType;")).is_ok())
    }

    #[test]
    fn generic_type_decl() {
        assert!(expr(span!("type Generic[T](inner: T);")).is_ok());
    }

    #[test]
    fn multi_generic_type_decl() {
        assert!(expr(span!(
            "type Generic[T, U, V](inner: T, outer: int, something: W);"
        ))
        .is_ok());
    }

    #[test]
    fn generic_empty_type_decl() {
        assert!(expr(span!("type Generic[T];")).is_ok());
    }

    #[test]
    fn generic_method_call() {
        assert!(expr(span!("expr.call[T, U, V](arg0, arg1)")).is_ok());
        assert!(expr(span!("call[W, Y, Z]().call[T, U, V](arg0, arg1)")).is_ok());
        assert!(expr(span!("value.call[primitive]()")).is_ok());
        assert!(expr(span!("value.call  [primitive]   ()")).is_ok());
    }

    #[test]
    fn type_id_simple() {
        assert!(type_id(span!("int")).is_ok());
        assert!(type_id(span!("Custom")).is_ok());
    }

    #[test]
    fn type_id_functor_without_args() {
        assert!(type_id(span!("func")).is_err());
    }

    #[test]
    fn type_id_generic() {
        assert!(type_id(span!("Vec[T]")).is_ok());
    }

    #[test]
    fn type_id_functor() {
        assert!(type_id(span!("func(int, string)")).is_ok());
    }

    #[test]
    fn type_id_generic_functor() {
        assert!(type_id(span!("func[T, U, V](int, string)")).is_ok());
    }

    #[test]
    fn type_id_functor_return_type() {
        assert!(type_id(span!("func() -> A")).is_ok());
        assert!(type_id(span!("func[T, U, V](A, B) -> A")).is_ok());
    }

    #[test]
    fn type_id_functor_generic_return_type() {
        assert!(type_id(span!("func[T, U, V](A, B) -> A[T]")).is_ok());
    }

    #[test]
    fn type_id_functor_functor_return_type() {
        assert!(type_id(span!("func[T, U, V](A, B) -> func(T) -> A[T]")).is_ok());
    }

    #[test]
    fn generic_func_arg_533() {
        assert!(expr(span!("ext func __builtin_vec_len[T](vec: Vec[T]);")).is_ok());
    }

    #[test]
    fn complex_function_declaration() {
        assert!(expr(span!("func f_to_f[F1, F2](f1: func[F1](A) -> B, f2: func[F2](B) -> func(A) -> B, p: string) -> func[F1, F2](F1, F2, string) -> Pair[A, B] { /* todo :) */ }")).is_ok());
    }

    #[test]
    fn nested_generic_type() {
        assert!(expr(span!("a = Pair[Pair[int], int](a: 15, b: 14)")).is_ok());
    }

    #[test]
    fn assign_call_to_generic_fn() {
        assert!(expr(span!("int_size = size_of[int](15)")).is_ok());
        assert!(expr(span!("int_size = size_of [int] (15)")).is_ok());
    }

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

    #[test]
    fn immutable_declaration() {
        let input = span!("where x = 14");

        assert!(expr(input).is_ok());
    }

    #[test]
    fn mutable_declaration() {
        let input = span!("where mut x = 14");

        assert!(expr(input).is_ok());
    }

    #[test]
    fn mutable_declaration_complex() {
        let input = span!("where mut x = if value { 14 } else { 15 }");

        assert!(expr(input).is_ok());
    }

    #[test]
    fn type_alias() {
        let input = span!("type Foo = Bar");

        assert!(expr(input).is_ok());
    }

    #[test]
    fn type_alias_multi_type() {
        let input = span!("type Foo = Bar | Baz | Qux");

        assert!(expr(input).is_ok());
    }

    #[test]
    fn parse_type_id() {
        let input = span!("Bar");

        assert!(matches!(
            type_id(input).unwrap().1.kind,
            TypeKind::Simple(_)
        ));
    }

    #[test]
    fn parse_type_id_multi() {
        let input = span!("Bar | Baz | Qux");

        assert!(matches!(type_id(input).unwrap().1.kind, TypeKind::Multi(v) if v.len() == 3));
    }

    #[test]
    fn parse_type_id_func() {
        let input = span!("func(Bar) -> Baz");

        assert!(matches!(
            type_id(input).unwrap().1.kind,
            TypeKind::FunctionLike(..)
        ));
    }

    #[test]
    fn parse_module_type() {
        let input = span!("type foo = source[\"foo\"]");

        assert!(expr(input).is_ok());
    }

    #[test]
    fn parse_literal_type_in_function_arg() {
        let input = span!("func only_accepts_foo_15(a: \"foo\", b: 15) {}");

        assert!(expr(input).is_ok());
    }

    #[test]
    fn parse_literal_type_in_generic_application() {
        let input = span!("func foo(a: Foo[\"foo\"], b: Bar[15, 15.2]) {}");

        assert!(expr(input).is_ok());
    }
}
