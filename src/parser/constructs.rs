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

use nom::Err::Error as NomError;
use nom::{
    branch::alt, combinator::opt, multi::many0, sequence::delimited, sequence::pair,
    sequence::preceded, sequence::terminated,
};

use crate::instruction::{
    BinaryOp, Block, DecArg, FieldAccess, FunctionCall, FunctionDec, FunctionKind, IfElse, Incl,
    Instruction, JkInst, Loop, LoopKind, MethodCall, Operator, Return, TypeDec, TypeId,
    TypeInstantiation, Var, VarAssign, VarOrEmptyType,
};
use crate::parser::{ConstantConstruct, ParseResult, Token};

/// Parse as many instructions as possible
/// many_expr = ( expr_semicolon )*
pub fn many_expr(mut input: &str) -> ParseResult<&str, Vec<Box<dyn Instruction>>> {
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
pub fn expr_semicolon(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
    let (input, expr) = expr(input)?;
    let (input, _) = opt(Token::semicolon)(input)?;

    Ok((input, expr))
}

// expr = cmp ( '<' cmp | '>' cmp | '<=' cmp | '>=' cmp | '==' cmp | '!=' cmp)*
pub fn expr(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
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
        input = new_input;
        expr = Box::new(BinaryOp::new(expr, rhs, Operator::new(op)));
    }
    Ok((input, expr))
}

pub fn cmp(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
    let (mut input, mut expr) = term(input)?;
    while let Ok((new_input, op)) = alt((Token::add, Token::sub))(input) {
        let (new_input, rhs) = term(new_input)?;
        input = new_input;
        expr = Box::new(BinaryOp::new(expr, rhs, Operator::new(op)));
    }
    Ok((input, expr))
}

/// term = factor next ( '*' factor next | '/' factor next )*
fn term(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
    let (input, mut term) = factor(input)?;
    let mut input = next(input);
    while let Ok((new_input, op)) = alt((Token::mul, Token::div))(input) {
        let (new_input, rhs) = factor(new_input)?;
        let new_input = next(new_input);
        input = new_input;
        term = Box::new(BinaryOp::new(term, rhs, Operator::new(op)));
    }
    Ok((input, term))
}

/// factor = next unit factor_rest
fn factor(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
    let input = next(input);
    let (input, unit) = unit(input)?;
    factor_rest(input, unit)
}

/// factor_rest = '.' IDENTIFIER next method_or_field factor_rest
///             | ε
fn factor_rest(input: &str, expr: Box<dyn Instruction>) -> ParseResult<&str, Box<dyn Instruction>> {
    match Token::dot(input) {
        Ok((input, _)) => {
            let (input, id) = Token::identifier(input)?;
            let input = next(input);
            let (input, expr) = method_or_field(input, expr, id)?;
            factor_rest(input, expr)
        }
        _ => Ok((input, expr)),
    }
}

/// method_or_field = '(' next args
///                 | ε
fn method_or_field(
    input: &str,
    expr: Box<dyn Instruction>,
    id: String,
) -> ParseResult<&str, Box<dyn Instruction>> {
    match Token::left_parenthesis(input) {
        Ok((input, _)) => {
            let input = next(input);
            let (input, args) = args(input)?;
            let method_call = MethodCall::new(expr, FunctionCall::new(id, args));
            Ok((input, Box::new(method_call)))
        }
        _ => Ok((input, Box::new(FieldAccess::new(expr, id)))),
    }
}

/// ```ignore
/// unit = 'if' expr block next [ 'else' next block ]
///      | 'while' expr block
///      | 'loop' next block
///      | 'for' spaced_identifier 'in' expr block
///
///      | 'func' function_declaration block
///      | 'test' function_declaration block
///      | 'mock' function_declaration block
///
///      | 'type' spaced_identifier '(' named_args
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
fn unit(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
    if let Ok((input, _)) = Token::if_tok(input) {
        unit_if(input)
    } else if let Ok((input, _)) = Token::while_tok(input) {
        unit_while(input)
    } else if let Ok((input, _)) = Token::loop_tok(input) {
        unit_loop(input)
    } else if let Ok((input, _)) = Token::for_tok(input) {
        unit_for(input)
    } else if let Ok((input, kind)) =
        alt((Token::func_tok, Token::test_tok, Token::mock_tok))(input)
    {
        unit_func(input, kind)
    } else if let Ok((input, _)) = Token::incl_tok(input) {
        unit_incl(input)
    } else if let Ok((input, _)) = Token::type_tok(input) {
        unit_type_decl(input)
    } else if let Ok((input, _)) = Token::mut_tok(input) {
        unit_mut_var(input)
    } else if let Ok((input, _)) = Token::at_sign(input) {
        unit_jk_inst(input)
    } else if let Ok((input, _)) = Token::ext_tok(input) {
        unit_extern(input)
    } else if let Ok((input, _)) = Token::return_tok(input) {
        unit_return(input)
    } else if let Ok((input, _)) = Token::left_curly_bracket(input) {
        unit_block(input)
    } else if let Ok((input, _)) = Token::left_parenthesis(input) {
        terminated(expr, Token::right_parenthesis)(input)
    } else if let Ok(res) = constant(input) {
        Ok(res)
    } else {
        let (input, id) = Token::identifier(input)?;
        let input = next(input);
        func_type_or_var(input, id)
    }
}

fn unit_if(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
    let (input, cond) = expr(input)?;
    let (input, success) = block(input)?;
    let input = next(input);
    if let Ok((input, _)) = Token::else_tok(input) {
        let input = next(input);
        let (input, else_body) = block(input)?;
        Ok((input, Box::new(IfElse::new(cond, success, Some(else_body)))))
    } else {
        Ok((input, Box::new(IfElse::new(cond, success, None))))
    }
}

fn unit_while(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
    let (input, (cond, block)) = pair(expr, block)(input)?;
    Ok((input, Box::new(Loop::new(LoopKind::While(cond), block))))
}

fn unit_loop(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
    let input = next(input);
    let (input, block) = block(input)?;
    Ok((input, Box::new(Loop::new(LoopKind::Loop, block))))
}

fn unit_for(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
    let (input, id) = spaced_identifier(input)?;
    let (input, _) = Token::in_tok(input)?;
    let (input, expr) = expr(input)?;
    let (input, block) = block(input)?;
    let var = Var::new(id);
    Ok((input, Box::new(Loop::new(LoopKind::For(var, expr), block))))
}

fn unit_func<'a>(input: &'a str, kind: &str) -> ParseResult<&'a str, Box<dyn Instruction>> {
    let (input, mut function) = func_declaration(input)?;
    let (input, body) = block(input)?;
    function.set_block(body);
    function.set_kind(FunctionKind::from(kind));
    Ok((input, Box::new(function)))
}

fn unit_incl(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
    let (input, path) = spaced_identifier(input)?;
    if let Ok((input, _)) = Token::az_tok(input) {
        let (input, alias) = preceded(nom_next, Token::identifier)(input)?;
        Ok((input, Box::new(Incl::new(path, Some(alias)))))
    } else {
        Ok((input, Box::new(Incl::new(path, None))))
    }
}

/// spaced_identifier '(' type_inst_arg (',' type_inst_arg)* ')'
fn unit_type_decl(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
    let (input, name) = spaced_identifier(input)?;
    let (input, _generics) = maybe_generic_list(input)?;
    let (input, type_dec) = if let Ok((input, _)) = Token::left_parenthesis(input) {
        let (input, first_arg) = typed_arg(input)?;
        let (input, mut args) = many0(preceded(Token::comma, typed_arg))(input)?;
        let (input, _) = Token::right_parenthesis(input)?;

        args.insert(0, first_arg);

        (input, TypeDec::new(name, args))
    } else {
        (input, TypeDec::new(name, vec![]))
    };

    Ok((input, Box::new(type_dec)))
}

/// spaced_identifier '=' expr
fn unit_mut_var(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
    let (input, symbol) = spaced_identifier(input)?;
    let (input, _) = Token::equal(input)?;
    let (input, value) = expr(input)?;

    Ok((input, Box::new(VarAssign::new(true, symbol, value))))
}

/// IDENTIFIER next '(' next args
fn unit_jk_inst(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
    let (input, name) = delimited(nom_next, Token::identifier, nom_next)(input)?;
    let (input, _) = Token::left_parenthesis(input)?;
    let (input, args) = args(next(input))?;

    let call = FunctionCall::new(name, args);
    match JkInst::from_function_call(&call) {
        Ok(inst) => Ok((input, Box::new(inst))),
        Err(err) => Err(NomError(err)),
    }
}

/// 'func' function_declaration ';'
fn unit_extern(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
    let input = next(input);
    let (input, mut dec) = delimited(Token::func_tok, func_declaration, Token::semicolon)(input)?;

    dec.set_kind(FunctionKind::Ext);
    Ok((input, Box::new(dec)))
}

///  [ expr ]                      (* Not LL(1) but this entry is subject to change *)
fn unit_return(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
    let (input, expr) = opt(expr)(input)?;

    Ok((input, Box::new(Return::new(expr))))
}

fn unit_block(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
    let (input, block) = inner_block(next(input))?;
    Ok((input, Box::new(block)))
}

// FIXME: This does not parse default generic types yet (`func f<T = int>()`)
fn generic_list(input: &str) -> ParseResult<&str, Vec<TypeId>> {
    fn whitespace_plus_id(input: &str) -> ParseResult<&str, String> {
        let input = next(input);
        let (input, id) = Token::identifier(input)?;
        let input = next(input);

        Ok((input, id))
    }

    let (input, first_type) = whitespace_plus_id(input)?;
    let (input, mut generics) = many0(preceded(Token::comma, whitespace_plus_id))(input)?;
    let (input, _) = Token::right_bracket(input)?;

    generics.insert(0, first_type);
    Ok((input, generics.into_iter().map(TypeId::new).collect()))
}

fn maybe_generic_list(input: &str) -> ParseResult<&str, Vec<TypeId>> {
    if let Ok((input, _)) = Token::left_bracket(input) {
        Ok(generic_list(input)?)
    } else {
        Ok((input, vec![]))
    }
}

/// function_declaration = next spaced_identifier [ next '[' spaced_identifier ( ',' spaced_identifier )* ']' ] next '(' next typed_arg next return_type
fn func_declaration(input: &str) -> ParseResult<&str, FunctionDec> {
    let input = next(input);
    let (input, id) = spaced_identifier(input)?;
    let input = next(input);

    let (input, _generics) = maybe_generic_list(input)?;

    let (input, _) = Token::left_parenthesis(input)?;
    let input = next(input);
    let (input, args) = typed_args(input)?;
    let input = next(input);
    let (input, return_type) = return_type(input)?;

    let mut function = FunctionDec::new(id, return_type);
    function.set_args(args);
    Ok((input, function))
}

/// return_type = '->' spaced_identifier
///             | ε
fn return_type(input: &str) -> ParseResult<&str, Option<TypeId>> {
    match Token::arrow(input) {
        Ok((input, _)) => {
            let (input, ty) = spaced_identifier(input)?;
            Ok((input, Some(TypeId::from(ty.as_str()))))
        }
        _ => Ok((input, None)),
    }
}

/// block = '{' next inner_block
pub fn block(input: &str) -> ParseResult<&str, Block> {
    let (input, _) = Token::left_curly_bracket(input)?;
    let input = next(input);
    inner_block(input)
}

/// inner_block = '}'
///             | expr '}'                  (* The only case where block is an expr *)
///             | expr ';' next inner_block
fn inner_block(input: &str) -> ParseResult<&str, Block> {
    if let Ok((input, _)) = Token::right_curly_bracket(input) {
        return Ok((input, Block::new()));
    }

    let (input, inst) = expr(input)?;
    if let Ok((input, _)) = Token::right_curly_bracket(input) {
        let mut block = Block::new();
        block.add_instruction(inst);
        block.set_statement(false);
        return Ok((input, block));
    }

    let (input, mut block) = preceded(Token::semicolon, preceded(nom_next, inner_block))(input)?;
    block.push_front_instruction(inst);
    Ok((input, block))
}

/// func_type_or_var = '(' next func_or_type_inst_args
///                  | '=' expr                   (* variable assigment *)
///                  | ε                          (* variable or empty type instantiation *)
fn func_type_or_var(input: &str, id: String) -> ParseResult<&str, Box<dyn Instruction>> {
    if let Ok((input, _)) = Token::left_bracket(input) {
        generic_func_or_type_inst_args(next(input), id)
    } else if let Ok((input, _)) = Token::left_parenthesis(input) {
        func_or_type_inst_args(next(input), id, None)
    } else if let Ok((input, _)) = Token::equal(input) {
        let (input, value) = expr(input)?;
        Ok((input, Box::new(VarAssign::new(false, id, value))))
    } else {
        Ok((input, Box::new(VarOrEmptyType::new(id))))
    }
}

/// func_or_type_inst_args = IDENTIFIER next ':' expr (',' type_inst_arg )* ')'  (* type_instantiation *)
///                  | args                                            (* function_call *)
fn func_or_type_inst_args(
    input: &str,
    id: String,
    _generics: Option<Vec<TypeId>>,
) -> ParseResult<&str, Box<dyn Instruction>> {
    if let Ok((input, first_attr)) =
        terminated(terminated(Token::identifier, nom_next), Token::colon)(input)
    {
        let (input, first_attr_val) = expr(input)?;
        let (input, attrs) = many0(preceded(Token::comma, type_inst_arg))(input)?;
        let (input, _) = Token::right_parenthesis(input)?;

        let mut type_inst = TypeInstantiation::new(TypeId::new(id));
        type_inst.add_field(VarAssign::new(false, first_attr, first_attr_val));
        attrs.into_iter().for_each(|attr| type_inst.add_field(attr));

        // type_inst.set_generics(_generics);

        Ok((input, Box::new(type_inst)))
    } else {
        let (input, args) = args(input)?;
        let func_call = FunctionCall::new(id, args);

        // TODO: Once we start doing more than just parsing...
        // let mut func_call = FunctionCall::new(id, args);
        // func_call.set_generics(_generics);

        Ok((input, Box::new(func_call)))
    }
}

fn generic_func_or_type_inst_args(
    input: &str,
    id: String,
) -> ParseResult<&str, Box<dyn Instruction>> {
    // FIXME: Assign generics to FunctionCall and TypeInstantiation
    let (input, generics) = generic_list(input)?;
    let (input, _) = Token::left_parenthesis(input)?;

    func_or_type_inst_args(next(input), id, Some(generics))
}

///
/// ARGS
///

/// args = expr ( ',' expr )* ')'
///      | ')'
fn args(input: &str) -> ParseResult<&str, Vec<Box<dyn Instruction>>> {
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
fn typed_args(input: &str) -> ParseResult<&str, Vec<DecArg>> {
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
fn multi_type(input: &str) -> ParseResult<&str, String> {
    // FIXME: Remove with #342
    fn whitespace_plus_id(input: &str) -> ParseResult<&str, String> {
        delimited(nom_next, Token::identifier, nom_next)(input)
    }

    // FIXME: We want to allow generic types here later on
    let (input, first_type) = whitespace_plus_id(input)?;

    let (input, mut types) = many0(preceded(Token::pipe, whitespace_plus_id))(input)?;

    // FIXME: Remove clone once we have a proper MultiType struct to return
    types.insert(0, first_type.clone());

    Ok((input, first_type))
}

/// typed_arg = spaced_identifier ':' spaced_identifier
fn typed_arg(input: &str) -> ParseResult<&str, DecArg> {
    let (input, id) = spaced_identifier(input)?;
    let (input, _) = Token::colon(input)?;
    let input = next(input);
    let (input, types) = multi_type(input)?;
    let input = next(input);

    Ok((input, DecArg::new(id, TypeId::new(types))))
}

/// type_inst_arg = spaced_identifier ':' expr
fn type_inst_arg(input: &str) -> ParseResult<&str, VarAssign> {
    let (input, id) = spaced_identifier(input)?;
    let (input, _) = Token::colon(input)?;
    let input = next(input);
    let (input, value) = expr(input)?;
    let input = next(input);

    Ok((input, VarAssign::new(false, id, value)))
}

fn spaced_identifier(input: &str) -> ParseResult<&str, String> {
    delimited(nom_next, Token::identifier, nom_next)(input)
}

/// next = extra*
/// extra = WHITESPACE
///       | '/*' [^'*/'] '*/'
///       | '//' [^\n]   '\n'
///       | '#'  [^\n]   '\n'
pub fn next(input: &str) -> &str {
    let input = input.trim_start();
    match Token::consume_comment(input) {
        Ok((input, _)) => next(input),
        _ => input,
    }
}

fn nom_next(input: &str) -> ParseResult<&str, ()> {
    Ok((next(input), ()))
}

/// Constants are raw values in the source code. For example, `"string"`, `12` and
/// `0.5`.
///
/// `'<any_char>' | "<any_char>*" | <num>? | <num>?.<num>?`
pub(crate) fn constant(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
    let constant = alt((
        ConstantConstruct::char_constant,
        ConstantConstruct::string_constant,
        ConstantConstruct::float_constant,
        ConstantConstruct::int_constant,
        ConstantConstruct::bool_constant,
    ))(input)?;

    Ok(constant)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{JkFloat, JkInt};

    #[test]
    fn consume_whitespace() {
        assert_eq!(nom_next("   input"), Ok(("input", ())));
        assert_eq!(nom_next(" \t input"), Ok(("input", ())));
    }

    #[test]
    fn simple_int_sum() {
        let (input, expr) = expr(" 401 + 809 ").unwrap();
        let operation: &BinaryOp = expr.downcast_ref().unwrap();
        let lhs: &JkInt = operation.lhs().downcast_ref().unwrap();
        let rhs: &JkInt = operation.rhs().downcast_ref().unwrap();

        assert_eq!(input, "");
        assert_eq!(operation.operator(), Operator::Add);
        assert_eq!(lhs.print(), "401");
        assert_eq!(rhs.print(), "809");
    }

    #[test]
    fn simple_float_mul() {
        let (input, expr) = expr("3.14 * 9.999").unwrap();
        let operation: &BinaryOp = expr.downcast_ref().unwrap();
        let lhs: &JkFloat = operation.lhs().downcast_ref().unwrap();
        let rhs: &JkFloat = operation.rhs().downcast_ref().unwrap();

        assert_eq!(input, "");
        assert_eq!(operation.operator(), Operator::Mul);
        assert_eq!(lhs.print(), "3.14");
        assert_eq!(rhs.print(), "9.999");
    }

    #[test]
    fn chained_sum_sub() {
        let (input, expr) = expr("239 + 809 - 1004").unwrap();
        let sub: &BinaryOp = expr.downcast_ref().unwrap();
        let add: &BinaryOp = sub.lhs().downcast_ref().unwrap();

        let first: &JkInt = add.lhs().downcast_ref().unwrap();
        let second: &JkInt = add.rhs().downcast_ref().unwrap();
        let third: &JkInt = sub.rhs().downcast_ref().unwrap();

        assert_eq!(input, "");
        assert_eq!(sub.operator(), Operator::Sub);
        assert_eq!(add.operator(), Operator::Add);
        assert_eq!(first.print(), "239");
        assert_eq!(second.print(), "809");
        assert_eq!(third.print(), "1004");
    }

    #[test]
    fn math_precedence() {
        let (input, expr) = expr("5.9 + 128 / 809.1 - 1004").unwrap();
        let sub: &BinaryOp = expr.downcast_ref().unwrap();
        let add: &BinaryOp = sub.lhs().downcast_ref().unwrap();
        let div: &BinaryOp = add.rhs().downcast_ref().unwrap();

        let first: &JkFloat = add.lhs().downcast_ref().unwrap();
        let second: &JkInt = div.lhs().downcast_ref().unwrap();
        let third: &JkFloat = div.rhs().downcast_ref().unwrap();
        let fourth: &JkInt = sub.rhs().downcast_ref().unwrap();

        assert_eq!(input, "");
        assert_eq!(sub.operator(), Operator::Sub);
        assert_eq!(add.operator(), Operator::Add);
        assert_eq!(div.operator(), Operator::Div);
        assert_eq!(first.print(), "5.9");
        assert_eq!(second.print(), "128");
        assert_eq!(third.print(), "809.1");
        assert_eq!(fourth.print(), "1004");
    }

    #[test]
    fn tricky_math_precedence() {
        let (input, expr) = expr("5.9 + 128 / 809.1 - 1 * 1.1").unwrap();
        let sub: &BinaryOp = expr.downcast_ref().unwrap();
        let add: &BinaryOp = sub.lhs().downcast_ref().unwrap();
        let mul: &BinaryOp = sub.rhs().downcast_ref().unwrap();
        let div: &BinaryOp = add.rhs().downcast_ref().unwrap();

        let first: &JkFloat = add.lhs().downcast_ref().unwrap();
        let second: &JkInt = div.lhs().downcast_ref().unwrap();
        let third: &JkFloat = div.rhs().downcast_ref().unwrap();
        let fourth: &JkInt = mul.lhs().downcast_ref().unwrap();
        let fifth: &JkFloat = mul.rhs().downcast_ref().unwrap();

        assert_eq!(input, "");
        assert_eq!(sub.operator(), Operator::Sub);
        assert_eq!(add.operator(), Operator::Add);
        assert_eq!(mul.operator(), Operator::Mul);
        assert_eq!(div.operator(), Operator::Div);
        assert_eq!(first.print(), "5.9");
        assert_eq!(second.print(), "128");
        assert_eq!(third.print(), "809.1");
        assert_eq!(fourth.print(), "1");
        assert_eq!(fifth.print(), "1.1");
    }

    #[test]
    fn method_call_no_arg() {
        let (input, expr) = expr("a.call( )").unwrap();

        assert!(expr.downcast_ref::<MethodCall>().is_some());
        assert_eq!(input, "");
    }

    #[test]
    fn method_call_one_arg() {
        let (input, expr) = expr("a.call(\"hello\")").unwrap();

        assert!(expr.downcast_ref::<MethodCall>().is_some());
        assert_eq!(input, "");
    }

    #[test]
    fn method_call_many_args() {
        let (input, expr) = expr("a.call(\"hello\", 1   , variable)").unwrap();

        assert!(expr.downcast_ref::<MethodCall>().is_some());
        assert_eq!(input, "");
    }

    #[test]
    fn method_call_many() {
        let (input, expr) = expr("a.call(\"hello\").sub().subsub(1, 20)").unwrap();

        assert!(expr.downcast_ref::<MethodCall>().is_some());
        assert_eq!(input, "");
    }

    #[test]
    fn method_call_on_bool() {
        let (input, expr) = expr("true.call( )").unwrap();

        assert!(expr.downcast_ref::<MethodCall>().is_some());
        assert_eq!(input, "");
    }

    #[test]
    fn field_access_single() {
        let (input, expr) = expr("a.attribute").unwrap();

        assert!(expr.downcast_ref::<FieldAccess>().is_some());
        assert_eq!(input, "");
    }

    #[test]
    fn field_access_many() {
        let (input, expr) = expr("a.attr.sub_attr.subsub").unwrap();

        assert!(expr.downcast_ref::<FieldAccess>().is_some());
        assert_eq!(input, "");
    }

    #[test]
    fn field_access_many_newline() {
        let (input, expr) = expr("a\n.attr\n.sub_attr\n.subsub").unwrap();

        assert!(expr.downcast_ref::<FieldAccess>().is_some());
        assert_eq!(input, "");
    }

    #[test]
    fn if_no_else() {
        let (input, expr) = expr("if 1 + 1 { 10 / 2 }").unwrap();

        assert!(expr.downcast_ref::<IfElse>().is_some());
        assert_eq!(input, "");
    }

    #[test]
    fn if_else() {
        let (input, expr) = expr("if 1 + 1 { 10 / 2 } else { var }").unwrap();

        assert!(expr.downcast_ref::<IfElse>().is_some());
        assert_eq!(input, "");
    }

    #[test]
    fn while_loop() {
        let (input, expr) = expr("while true { var + 10 }").unwrap();

        assert!(expr.downcast_ref::<Loop>().is_some());
        assert_eq!(input, "");
    }

    #[test]
    fn for_loop() {
        let (input, expr) = expr("for entry in collection { entry.print() }").unwrap();

        assert!(expr.downcast_ref::<Loop>().is_some());
        assert_eq!(input, "");
    }

    #[test]
    fn loop_basic() {
        let (input, expr) = expr("loop { variable.get() + 10 }").unwrap();

        assert!(expr.downcast_ref::<Loop>().is_some());
        assert_eq!(input, "");
    }

    #[test]
    fn funcion_dec_no_arg() {
        let (input, expr) = expr("func a ( ) { 1 }").unwrap();
        let func = expr.downcast_ref::<FunctionDec>().unwrap();

        assert_eq!(input, "");
        assert!(func.args().is_empty());
    }

    #[test]
    fn funcion_dec_one_arg() {
        let (input, expr) = expr("func id ( arg: int ) { arg }").unwrap();
        let func = expr.downcast_ref::<FunctionDec>().unwrap();

        assert_eq!(input, "");
        assert!(func.args().len() == 1);
        assert!(func.args()[0].name() == "arg");
        assert!(func.args()[0].get_type().id() == "int");
    }

    #[test]
    fn funcion_dec_many_args() {
        let (input, expr) = expr(
            "func concat ( arg1: char , arg2: int,arg3:float, arg4: string) { arg1 + arg2 + arg3 }",
        )
        .unwrap();
        let func = expr.downcast_ref::<FunctionDec>().unwrap();

        assert_eq!(input, "");
        assert!(func.args().len() == 4);
        assert!(func.args()[0].name() == "arg1");
        assert!(func.args()[3].name() == "arg4");
    }

    #[test]
    fn funcion_dec_many_args_with_return() {
        let (input, expr) =
            expr("func concat ( arg1: char , arg2: int,arg3:float, arg4: string) -> string { arg1 + arg2 + arg3 }").unwrap();
        let func = expr.downcast_ref::<FunctionDec>().unwrap();

        assert_eq!(input, "");
        assert_eq!(func.args().len(), 4);
        assert_eq!(func.ty().unwrap().id(), "string");
        assert_eq!(func.args()[0].name(), "arg1");
        assert_eq!(func.args()[3].name(), "arg4");
    }

    #[test]
    fn funcion_dec_no_arg_with_return() {
        let (input, expr) = expr("func a ( ) -> int { 1 }").unwrap();
        let func = expr.downcast_ref::<FunctionDec>().unwrap();

        assert_eq!(input, "");
        assert_eq!(func.ty().unwrap().id(), "int");
        assert!(func.args().is_empty());
    }

    #[test]
    fn test_dec_no_arg() {
        let (input, expr) = expr("test a ( ) { true }").unwrap();
        let func = expr.downcast_ref::<FunctionDec>().unwrap();

        assert_eq!(input, "");
        assert_eq!(func.fn_kind(), FunctionKind::Test);
    }

    #[test]
    fn mock_dec_one_arg() {
        let (input, expr) = expr("mock id ( arg: int ) { arg }").unwrap();
        let func = expr.downcast_ref::<FunctionDec>().unwrap();

        assert_eq!(input, "");
        assert_eq!(func.fn_kind(), FunctionKind::Mock);
    }

    #[test]
    fn block_empty() {
        let (input, expr) = expr("{ }").unwrap();

        assert_eq!(input, "");
        assert!(expr.downcast_ref::<Block>().is_some());
    }

    #[test]
    fn block_one_inst() {
        let (input, expr) = expr("{ var }").unwrap();

        assert_eq!(input, "");
        assert!(expr.downcast_ref::<Block>().is_some());
    }

    #[test]
    fn block_many_inst() {
        let (input, expr) = expr(
            "{
            var = 1 + 1;
            var = var - 2;
            var
                                 }",
        )
        .unwrap();

        assert_eq!(input, "");
        assert!(expr.downcast_ref::<Block>().is_some());
    }

    #[test]
    fn block_statement() {
        let (input, expr) = expr(
            "{
            var = 1 + 1;
            var = var - 2;
            var;
                                 }",
        )
        .unwrap();

        assert_eq!(input, "");
        assert!(expr.downcast_ref::<Block>().is_some());
    }

    #[test]
    fn block_missing_closing() {
        assert!(expr(
            "{
            var = 1 + 1;
            var = var - 2;
            var
            "
        )
        .is_err())
    }

    #[test]
    fn type_dec_one_field() {
        let (input, expr) = expr("type Num ( val : int )").unwrap();
        let dec = expr.downcast_ref::<TypeDec>().unwrap();

        assert_eq!(input, "");
        assert_eq!(dec.name(), "Num");
        assert_eq!(dec.fields().len(), 1);
    }

    #[test]
    fn type_dec_multiple_field() {
        let (input, expr) = expr("type Point( x : int , y: int )").unwrap();
        let dec = expr.downcast_ref::<TypeDec>().unwrap();

        assert_eq!(input, "");
        assert_eq!(dec.name(), "Point");
        assert_eq!(dec.fields().len(), 2);
    }

    #[test]
    fn type_dec_incomplete() {
        assert!(expr("type Point( x:int , y: )").is_err());
    }

    #[test]
    fn include_simple() {
        let (input, expr) = expr("incl pair").unwrap();

        assert_eq!(input, "");
        assert!(expr.downcast_ref::<Incl>().is_some());
    }

    #[test]
    fn include_with_alias() {
        let (input, expr) = expr("incl numpy as np").unwrap();

        assert_eq!(input, "");
        assert!(expr.downcast_ref::<Incl>().is_some());
    }

    #[test]
    fn include_with_alias_missing_path() {
        assert!(expr("incl as uoh").is_err());
    }

    #[test]
    fn var_assignment() {
        let (input, expr) = expr("var = 'a'").unwrap();
        let assign = expr.downcast_ref::<VarAssign>().unwrap();

        assert_eq!(input, "");
        assert_eq!(assign.symbol(), "var");
        assert!(!assign.mutable());
    }

    #[test]
    fn var_assigment_tricky() {
        let (input, expr) = expr("n1=b.call() + 1").unwrap();
        let assign = expr.downcast_ref::<VarAssign>().unwrap();

        assert_eq!(input, "");
        assert_eq!(assign.symbol(), "n1");
        assert!(!assign.mutable());
    }

    #[test]
    fn mut_var_assigment() {
        let (input, expr) = expr("mut var = b.call() + 1").unwrap();
        let assign = expr.downcast_ref::<VarAssign>().unwrap();

        assert_eq!(input, "");
        assert_eq!(assign.symbol(), "var");
        assert!(assign.mutable());
    }

    #[test]
    fn t_comparison_op_valid() {
        assert!(expr("a <   12 ").is_ok());
        assert!(expr("some() > 12.1").is_ok());
    }

    #[test]
    fn t_binary_op_invalid() {
        let (input, expr) = expr("a ? 12").unwrap();

        assert!(expr.downcast_ref::<BinaryOp>().is_none());
        assert_eq!(input, "? 12");
    }

    #[test]
    fn var_assigment_mut_in_name() {
        let (input, expr) = expr("mut_var = b.call() + 1").unwrap();
        let assign = expr.downcast_ref::<VarAssign>().unwrap();

        assert_eq!(input, "");
        assert_eq!(assign.symbol(), "mut_var");
        assert!(!assign.mutable());
    }

    #[test]
    fn mut_var_assigment_mut_in_name() {
        let (input, expr) = expr("mut mut_var = b.call() + 1").unwrap();
        let assign = expr.downcast_ref::<VarAssign>().unwrap();

        assert_eq!(input, "");
        assert_eq!(assign.symbol(), "mut_var");
        assert!(assign.mutable());
    }

    #[test]
    fn jk_inst_no_arg() {
        let (input, expr) = expr("@quit ( )").unwrap();

        assert_eq!(input, "");
        assert!(expr.downcast_ref::<JkInst>().is_some());
    }

    #[test]
    fn jk_inst_arg() {
        let (input, expr) = expr("@dump ( thing )").unwrap();

        assert_eq!(input, "");
        assert!(expr.downcast_ref::<JkInst>().is_some());
    }

    #[test]
    fn jk_inst_non_existant() {
        assert!(expr("@crab ( thing )").is_err());
    }

    #[test]
    fn extern_no_args() {
        let (input, expr) = expr("ext func exit () ;").unwrap();

        assert_eq!(input, "");
        assert!(expr.downcast_ref::<FunctionDec>().is_some());
    }

    #[test]
    fn extern_many_args() {
        let (input, expr) = expr("ext func memcpy(dst: char, src: char, n: int);").unwrap();

        assert_eq!(input, "");
        assert!(expr.downcast_ref::<FunctionDec>().is_some());
    }

    #[test]
    fn extern_missing_semicolon() {
        assert!(expr("ext func memcpy(dst: char , n: int) ").is_err());
    }

    #[test]
    fn return_nothing() {
        let (input, expr) = expr("return").unwrap();

        assert_eq!(input, "");
        assert!(expr.downcast_ref::<Return>().is_some());
    }

    #[test]
    fn return_sum() {
        let (input, expr) = expr("return 10 + 9").unwrap();

        assert_eq!(input, "");
        assert!(expr.downcast_ref::<Return>().is_some());
    }

    /// Mimic previous parsers behaviour
    #[test]
    #[ignore]
    fn return_malformed() {
        let (input, expr) = expr("return 10 +").unwrap();

        assert_eq!(input, "10 +");
        assert!(expr.downcast_ref::<Return>().is_none());
    }

    #[test]
    fn function_call_no_arg() {
        let (input, expr) = expr("call ( )").unwrap();
        let func = expr.downcast_ref::<FunctionCall>().unwrap();

        assert_eq!(input, "");
        assert_eq!(func.name(), "call");
        assert!(func.args().is_empty());
    }

    #[test]
    fn function_call_one() {
        let (input, expr) = expr("id ( 10 )").unwrap();
        let func = expr.downcast_ref::<FunctionCall>().unwrap();

        assert_eq!(input, "");
        assert_eq!(func.name(), "id");
        assert_eq!(func.args().len(), 1);
    }

    #[test]
    fn function_call_many() {
        let (input, expr) = expr("concat( 'h','e', 'l' , 'l', 'o')").unwrap();
        let func = expr.downcast_ref::<FunctionCall>().unwrap();

        assert_eq!(input, "");
        assert_eq!(func.name(), "concat");
        assert_eq!(func.args().len(), 5);
    }

    #[test]
    fn function_call_missing_paren() {
        assert!(expr("concat( 'h','e', 'l' , 'l', 'o'").is_err());
    }

    #[test]
    fn function_call_double_paren() {
        assert!(expr("fn((").is_err());
    }

    #[test]
    fn multi_comment_multi_line() {
        let input = r#"/**
    * This function does nothing
    */
    func void() { }"#;

        let (input, expr) = expr(input).unwrap();
        expr.downcast_ref::<FunctionDec>().unwrap();

        assert_eq!(input, "");
    }

    #[test]
    fn sing_comment_multi_line() {
        let input = r#" // Comment
    func void() { }"#;

        let (input, expr) = expr(input).unwrap();
        expr.downcast_ref::<FunctionDec>().unwrap();

        assert_eq!(input, "");
    }

    #[test]
    fn hashtag_comment_multi_line() {
        let input = r##"# Comment
func void() { }"##;

        let (input, expr) = expr(input).unwrap();
        expr.downcast_ref::<FunctionDec>().unwrap();

        assert_eq!(input, "");
    }

    #[test]
    fn multiple_different_comments() {
        let input = r##"# Comment
# Another one 
            /**
               * Some documentation
               */
    func void() { }"##;

        let (input, expr) = expr(input).unwrap();
        expr.downcast_ref::<FunctionDec>().unwrap();

        assert_eq!(input, "");
    }

    #[test]
    fn multiple_different_comments_close() {
        let input = r##"# Comment
# Another one 
            /**
               * Some documentation
               *//* Some more */
    func void() { }"##;

        let (input, expr) = expr(input).unwrap();
        expr.downcast_ref::<FunctionDec>().unwrap();

        assert_eq!(input, "");
    }

    #[test]
    fn lt_exprs() {
        assert!(expr("a < b").is_ok())
    }

    #[test]
    fn gt_exprs() {
        assert!(expr("a > b").is_ok())
    }

    #[test]
    fn lte_exprs() {
        assert!(expr("lhs <= rhs").is_ok())
    }

    #[test]
    fn gte_exprs() {
        assert!(expr("lhs >= rhs").is_ok())
    }

    #[test]
    fn exprs_equals() {
        assert!(expr("lhs == rhs").is_ok())
    }

    #[test]
    fn exprs_not_equals() {
        assert!(expr("lhs != rhs").is_ok())
    }

    #[test]
    fn expr_with_parenthesis() {
        assert!(expr("lhs + (rhs - lhs)").is_ok())
    }

    #[test]
    fn parentheses() {
        let (input, expr) = expr("4 * (3 + 5)").unwrap();
        expr.downcast_ref::<BinaryOp>().unwrap();

        assert_eq!(input, "");
    }

    #[test]
    fn multi_type_2() {
        assert!(expr("func takes_mt(a: int | string) {}").is_ok())
    }

    #[test]
    fn multi_type_unclosed() {
        assert!(expr("func invalid_mt(a: int |) {}").is_err())
    }

    #[test]
    fn multi_type_long() {
        assert!(expr("func long_mt(a: int | string | float | char | bool) {}").is_ok())
    }

    #[test]
    fn multi_type_in_type_dec() {
        assert!(expr("type MultiTy(a: int | string | float | char | bool);").is_ok())
    }

    #[test]
    fn func_dec_one_generic() {
        assert!(expr("func a[T]() {}").is_ok())
    }

    #[test]
    fn func_dec_multiple_generic() {
        assert!(expr("func a[T, U, V]() {}").is_ok())
    }

    #[test]
    fn func_dec_generic_and_whitespace() {
        assert!(expr("func a[    T]() {}").is_ok());
        assert!(expr("func a[    T  ]() {}").is_ok());
        assert!(expr("func a[   T , U    , V]() {}").is_ok())
    }

    #[test]
    fn func_dec_empty_generic_list() {
        assert!(expr("func a[]() {}").is_err())
    }

    #[test]
    fn func_dec_no_generic_delimiter() {
        assert!(expr("func a[T, U() {}").is_err())
    }

    #[test]
    fn func_call_generics_one() {
        assert!(expr("fn_call[T]()").is_ok());
    }

    #[test]
    fn func_call_generics_multi() {
        assert!(expr("fn_call[T, U, V]()").is_ok());
    }

    #[test]
    fn func_call_generics_multi_and_args() {
        assert!(expr("fn_call[T, U, V](a, b, c)").is_ok());
    }

    #[test]
    fn type_inst_generics_multi_and_args() {
        assert!(expr("TypeInst[T, U, V](a: 0, b: 1, c: 2)").is_ok());
    }

    #[test]
    fn empty_type_declaration() {
        assert!(expr("type CustomType;").is_ok())
    }

    #[test]
    fn empty_type_instantiation() {
        assert!(expr("a = CustomType;").is_ok())
    }

    #[test]
    fn generic_type_decl() {
        assert!(expr("type Generic[T](inner: T);").is_ok());
    }

    #[test]
    fn multi_generic_type_decl() {
        assert!(expr("type Generic[T, U, V](inner: T, outer: int, something: W);").is_ok());
    }

    #[test]
    fn generic_empty_type_decl() {
        assert!(expr("type Generic[T];").is_ok());
    }
}
