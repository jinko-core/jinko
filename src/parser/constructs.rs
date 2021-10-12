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
    branch::alt, combinator::opt, multi::many0, sequence::pair, sequence::preceded,
    sequence::terminated,
};

use crate::error::{ErrKind, Error};
use crate::instruction::{
    BinaryOp, Block, DecArg, ExtraContent, FieldAccess, FunctionCall, FunctionDec, FunctionKind,
    IfElse, Incl, Instruction, JkInst, Loop, LoopKind, MethodCall, Operator, Return, TypeDec,
    TypeId, TypeInstantiation, Var, VarAssign,
};
use crate::parser::{BoxConstruct, ConstantConstruct, ParseResult, ShuntingYard, Token};

type Instructions = Vec<Box<dyn Instruction>>;
type MaybeInstruction = Option<Box<dyn Instruction>>;

/// Parse as many instructions as possible
/// exprs = ( expr_scl )*
pub fn exprs(input: &str) -> ParseResult<&str, Vec<Box<dyn Instruction>>> {
    many0(expr_scl)(input)
}

/// Parse an instruction and maybe the semicolon that follows.
///
/// expr_scl = expr [ ';' ]
pub fn expr_scl(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
    let (input, expr) = expr(input)?;
    let (input, _) = opt(Token::semicolon)(input)?;

    Ok((input, expr))
}

/// expr = term ( '+' term | '-' term )*
pub fn expr(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
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
            let method_call = MethodCall::new(expr, FunctionCall::with_args(id, args));
            Ok((input, Box::new(method_call)))
        }
        _ => Ok((input, Box::new(FieldAccess::new(expr, id)))),
    }
}

/// unit = 'if' expr block next [ 'else' next block ]
///      | 'while' expr block
///      | 'loop' next block
///      | 'for' next IDENTIFIER next 'in' expr block
///
///      | 'func' function_declaration block
///      | 'test' function_declaration block
///      | 'mock' function_declaration block
///
///      | 'type' next IDENTIFIER next '{' named_args                           // TODO
///      | 'incl' next IDENTIFIER next [ 'as' next IDENTIFIER ]                 // TODO
///      | 'mut' next IDENTIFIER next '=' expr (* mutable variable assigment *) // TODO
///      | '@' next IDENTIFIER next '(' args                                    // TODO
///
///      | 'extern' 'func' function_declaration ';'                             // TODO
///      | 'return' expr                                                        // TODO
///      | '{' next inner_block         
///
///      | 'true'
///      | 'false'
///      | "'" CHAR "'"
///      | '"' [^"] '"'
///      | INT
///      | DOUBLE
///
///      | IDENTIFIER next func_type_or_var
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
    } else if let Ok((input, _)) = Token::left_curly_bracket(input) {
        unit_box(input)
    } else if let Ok(res) = Construct::constant(input) {
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
        let (input, failure) = block(input)?;
        Ok((input, Box::new(IfElse::new(cond, success, Some(failure)))))
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
    let input = next(input);
    let (input, id) = Token::identifier(input)?;
    let input = next(input);
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

fn unit_box(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
    let (input, block) = inner_block(next(input))?;
    Ok((input, Box::new(block)))
}

/// function_declaration = next IDENTIFIER next '(' next func_args next return_type
fn func_declaration(input: &str) -> ParseResult<&str, FunctionDec> {
    let input = next(input);
    let (input, id) = Token::identifier(input)?;
    let input = next(input);
    let (input, _) = Token::left_parenthesis(input)?;
    let input = next(input);
    let (input, args) = func_args(input)?;
    let input = next(input);
    let (input, return_type) = return_type(input)?;

    let mut function = FunctionDec::new(id, return_type);
    function.set_args(args);
    Ok((input, function))
}

/// return_type = '->' next IDENTIFIER next
///             | ε
fn return_type(input: &str) -> ParseResult<&str, Option<TypeId>> {
    match Token::arrow(input) {
        Ok((input, _)) => {
            let input = next(input);
            let (input, ty) = Token::identifier(input)?;
            let input = next(input);
            Ok((input, Some(TypeId::from(ty.as_str()))))
        }
        _ => Ok((input, None)),
    }
}

/// block = '{' next inner_block
fn block(input: &str) -> ParseResult<&str, Block> {
    let (input, _) = Token::left_curly_bracket(input)?;
    let input = next(input);
    inner_block(input)
}

/// inner_block = expr ( ';' expr )* '}'
///             | '}'
fn inner_block(input: &str) -> ParseResult<&str, Block> {
    let mut block = Block::new();
    if let Ok((input, _)) = Token::right_curly_bracket(input) {
        return Ok((input, block));
    }

    let (mut input, inst) = expr(input)?;
    block.add_instruction(inst);
    while let Ok((new_input, _)) = Token::semicolon(input) {
        let (new_input, inst) = expr(new_input)?;
        block.add_instruction(inst);
        input = new_input;
    }
    let (input, _) = Token::right_curly_bracket(input)?;

    Ok((input, block))
}

/// func_type_or_var = '(' next func_or_type_args
///                  | '=' expr                   (* variable assigment *)
///                  | ε                          (* variable *)
fn func_type_or_var(input: &str, id: String) -> ParseResult<&str, Box<dyn Instruction>> {
    if let Ok((input, _)) = Token::left_parenthesis(input) {
        func_or_type_args(next(input), id)
    } else if let Ok((input, _)) = Token::equal(input) {
        let (input, value) = expr(input)?;
        Ok((input, Box::new(VarAssign::new(false, id, value))))
    } else {
        Ok((input, Box::new(Var::new(id))))
    }
}

/// func_or_type_args = IDENTIFIER next ':' expr (',' type_arg )* ')'  (* type_instantiation *)
///                  | args                                            (* function_call *)
fn func_or_type_args(input: &str, id: String) -> ParseResult<&str, Box<dyn Instruction>> {
    if let Ok((input, first_attr)) =
        terminated(terminated(Token::identifier, nom_next), Token::colon)(input)
    {
        let (input, first_attr_val) = expr(input)?;
        let (input, attrs) = many0(preceded(Token::comma, type_arg))(input)?;
        let (input, _) = Token::right_parenthesis(input)?;

        let mut type_inst = TypeInstantiation::new(TypeId::new(id));
        type_inst.add_field(VarAssign::new(false, first_attr, first_attr_val));
        attrs.into_iter().for_each(|attr| type_inst.add_field(attr));

        Ok((input, Box::new(type_inst)))
    } else {
        let (input, args) = args(input)?;
        Ok((input, Box::new(FunctionCall::with_args(id, args))))
    }
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

/// func_args = func_arg ( ',' func_arg )* ')'
///           | ')'
fn func_args(input: &str) -> ParseResult<&str, Vec<DecArg>> {
    if let Ok((input, _)) = Token::right_parenthesis(input) {
        return Ok((input, vec![]));
    }
    let (input, first_arg) = func_arg(input)?;
    let (input, mut args) = many0(preceded(Token::comma, func_arg))(input)?;
    let (input, _) = Token::right_parenthesis(input)?;

    args.insert(0, first_arg);
    Ok((input, args))
}

/// func_arg = next IDENTIFIER next ':' next IDENTIFIER next
fn func_arg(input: &str) -> ParseResult<&str, DecArg> {
    let input = next(input);
    let (input, id) = Token::identifier(input)?;
    let input = next(input);
    let (input, _) = Token::colon(input)?;
    let input = next(input);
    let (input, ty) = Token::identifier(input)?;
    let input = next(input);

    Ok((input, DecArg::new(id, TypeId::new(ty))))
}

/// type_arg = next IDENTIFIER next ':' expr
fn type_arg(input: &str) -> ParseResult<&str, VarAssign> {
    let input = next(input);
    let (input, id) = Token::identifier(input)?;
    let input = next(input);
    let (input, _) = Token::colon(input)?;
    let input = next(input);
    let (input, value) = expr(input)?;
    let input = next(input);

    Ok((input, VarAssign::new(false, id, value)))
}

/// next = extra*
/// extra = WHITESPACE
///       | '/*' [^'*/'] '*/'
///       | '//' [^\n]   '\n'
///       | '#'  [^\n]   '\n'
pub fn next(input: &str) -> &str {
    let input = input.trim_start();
    let mut comments = alt((
        Token::consume_shebang_comment,
        Token::consume_single_comment,
        Token::consume_multi_comment,
    ));
    if let Ok((input, _)) = comments(input) {
        next(input)
    } else {
        input
    }
}

fn nom_next(input: &str) -> ParseResult<&str, ()> {
    Ok((next(input), ()))
}

pub struct Construct;

impl Construct {
    /// Parse any valid jinko instruction. This can be a function call, a variable,
    /// a block declaration...
    pub fn instruction(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
        // FIXME: If input is empty, return an error or do nothing
        // FIXME: We need to parse the remaining input after a correct instruction
        // has been parsed
        let (input, value) = alt((
            BoxConstruct::extra,
            BoxConstruct::function_declaration,
            BoxConstruct::type_declaration,
            BoxConstruct::ext_declaration,
            BoxConstruct::test_declaration,
            BoxConstruct::mock_declaration,
            BoxConstruct::incl,
            BoxConstruct::if_else,
            BoxConstruct::any_loop,
            BoxConstruct::jinko_inst,
            BoxConstruct::mut_var_assignment,
            // binary_op must be parsed before variables, constants, function calls and
            // method_calls
            Construct::binary_op,
            Construct::method_call_or_field_access,
            Construct::type_inst_or_function_call_or_var,
            BoxConstruct::block,
            Construct::constant,
        ))(input)?;

        Ok((input, value))
    }

    /// Parse a method call or a field access
    ///
    /// method_call_or_field_access = instance '.' identifier method_call
    ///                             | instance '.' identifier field_access
    pub fn method_call_or_field_access(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
        let (input, instance) = Construct::instance(input)?;
        let (input, _) = Token::dot(input)?;
        let (input, field_name) = Token::identifier(input)?;
        Construct::method_call(input, instance.clone(), &field_name)
            .or_else(|_| Construct::field_access(input, instance, field_name))
    }

    /// Parse a method call
    ///
    /// function_call
    pub fn method_call<'a>(
        input: &'a str,
        caller: Box<dyn Instruction>,
        field_name: &str,
    ) -> ParseResult<&'a str, Box<dyn Instruction>> {
        let (input, method) = Construct::function_call(input, field_name)?;

        Ok((input, Box::new(MethodCall::new(caller, method))))
    }

    /// Parse a field access
    ///
    /// ( '.' identifier )*
    pub fn field_access(
        input: &str,
        first_fa: Box<dyn Instruction>,
        field_name: String,
    ) -> ParseResult<&str, Box<dyn Instruction>> {
        let (input, dot_field_vec) = many0(preceded(Token::dot, Token::identifier))(input)?;

        let mut current_fa = FieldAccess::new(first_fa, field_name);
        for field_name in dot_field_vec {
            let fa = FieldAccess::new(Box::new(current_fa), field_name);
            current_fa = fa;
        }

        Ok((input, Box::new(current_fa)))
    }

    /// Parse a type instantiation, a function call, a var assignment or a variable
    ///
    /// type_inst_or_function_call_or_variable = identifier function_call
    ///                                        | identifier type_instantiation
    ///                                        | identifier var_assignment
    ///                                        | identifier (* variable *)
    pub fn type_inst_or_function_call_or_var(
        input: &str,
    ) -> ParseResult<&str, Box<dyn Instruction>> {
        let (input, identifier) = Token::identifier(input)?;
        let (trimmed, _) = Token::maybe_consume_extra(input)?;
        BoxConstruct::function_call(trimmed, &identifier)
            .or_else(|_| Construct::type_instanciation(trimmed, &identifier))
            .or_else(|_| BoxConstruct::var_assignment(trimmed, &identifier))
            .or_else(|_| Ok((input, Box::new(Var::new(identifier)))))
    }

    /// Parse a function call
    /// When a function is called in the source code.
    ///
    /// ```
    /// fn(); // Function call
    /// fn() // Call the function `fn` and use the return result as an instruction
    /// x = fn(); // Assign the result of the function call to the variable x
    /// ```
    ///
    /// '(' maybe_consume_extra [ args_list ] ')'
    pub fn function_call<'a>(input: &'a str, fn_name: &str) -> ParseResult<&'a str, FunctionCall> {
        let (input, _) = Token::left_parenthesis(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, arg_vec) = opt(Construct::args_list)(input)?;
        let (input, _) = Token::right_parenthesis(input)?;

        let mut fn_call = FunctionCall::new(fn_name.to_owned());
        if let Some(arg_vec) = arg_vec {
            arg_vec.into_iter().for_each(|arg| fn_call.add_arg(arg));
        }

        Ok((input, fn_call))
    }

    /// Parse a type instantiation
    ///
    /// maybe_consume_extra '{' named_arg_list '}'
    pub fn type_instanciation<'a>(
        input: &'a str,
        type_name: &str,
    ) -> ParseResult<&'a str, Box<dyn Instruction>> {
        let (input, _) = Token::left_curly_bracket(input)?;
        let (input, arg_vec) = Construct::named_arg_list(input)?;
        let (input, _) = Token::right_curly_bracket(input)?;

        let type_id = TypeId::new(type_name.to_owned());
        let mut type_instantiation = TypeInstantiation::new(type_id);
        arg_vec
            .into_iter()
            .for_each(|field| type_instantiation.add_field(field));

        Ok((input, Box::new(type_instantiation)))
    }

    /// Parse a var assigment
    /// When a variable is assigned a value. Ideally, a variable cannot be assigned the
    /// `void` type.
    ///
    /// ```
    /// x = 12; // Store 12 into the variable `x`
    /// x = 456; // Forbidden, `x` is immutable
    /// ```
    ///
    /// A variable assignment is a Statement. It cannot be used as an Expression
    ///
    /// ```
    /// {
    ///     x = 12; // Block returns void
    /// }
    /// {
    ///     x = 12 // Forbidden
    /// }
    /// {
    ///     x = call();
    ///     x // Okay
    /// }
    /// ```
    ///
    /// maybe_consume_extra '=' maybe_consume_extra instruction
    pub fn var_assignment<'a>(input: &'a str, var_name: &str) -> ParseResult<&'a str, VarAssign> {
        let (input, _) = Token::equal(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, value) = Construct::instruction(input)?;

        Ok((input, VarAssign::new(false, var_name.to_owned(), value)))
    }

    pub fn early_return(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
        let (input, value) = BoxConstruct::jk_return(input)?;

        Ok((input, value))
    }

    /// Parse an instruction and maybe the semicolon that follows.
    ///
    /// `<instruction> [ ; ]`
    pub fn instruction_maybe_semicolon(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
        let (input, expr) = Construct::instruction(input)?;
        let (input, _) = opt(Token::semicolon)(input)?;

        Ok((input, expr))
    }

    /// Parse as many instructions as possible
    pub fn many_instructions(input: &str) -> ParseResult<&str, Vec<Box<dyn Instruction>>> {
        let instrs = many0(Construct::instruction_maybe_semicolon)(input)?;

        Ok(instrs)
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

    fn extra_shebang(input: &str) -> ParseResult<&str, ExtraContent> {
        let (input, comment) = Token::consume_shebang_comment(input)?;

        Ok((input, ExtraContent::new_shebang(comment.to_owned())))
    }

    fn extra_single(input: &str) -> ParseResult<&str, ExtraContent> {
        let (input, comment) = Token::consume_single_comment(input)?;

        Ok((input, ExtraContent::new_single_line(comment.to_owned())))
    }

    fn extra_multi(input: &str) -> ParseResult<&str, ExtraContent> {
        let (input, comment) = Token::consume_multi_comment(input)?;

        Ok((input, ExtraContent::new_multi_line(comment.to_owned())))
    }

    fn extra_whitespaces(input: &str) -> ParseResult<&str, ExtraContent> {
        let (input, comment) = Token::consume_whitespaces(input)?;

        Ok((input, ExtraContent::new_whitespaces(comment.to_owned())))
    }

    /// Extra content is whitespaces and comments
    pub fn extra(input: &str) -> ParseResult<&str, ExtraContent> {
        let extra = alt((
            Construct::extra_whitespaces,
            Construct::extra_shebang,
            Construct::extra_single,
            Construct::extra_multi,
        ))(input)?;

        Ok(extra)
    }

    /// Parse an argument given to a function. Consumes the whitespaces before and after
    /// the argument
    fn arg(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
        let (input, _) = Token::maybe_consume_extra(input)?;

        let (input, constant) = Construct::instruction(input)?;

        let (input, _) = Token::maybe_consume_extra(input)?;

        Ok((input, constant))
    }

    /// Parse a list of arguments separated by comma
    fn args_list(input: &str) -> ParseResult<&str, Vec<Box<dyn Instruction>>> {
        /// Parse a comma and the argument that follows it
        fn comma_and_arg(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
            let (input, _) = Token::comma(input)?;
            let (input, _) = Token::maybe_consume_extra(input)?;
            let (input, constant) = Construct::instruction(input)?;
            let (input, _) = Token::maybe_consume_extra(input)?;

            Ok((input, constant))
        }

        // Get first arg
        let (input, first_arg) = Construct::arg(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        // Get 0 or more arguments with a comma to the function call
        let (input, mut arg_vec) = many0(comma_and_arg)(input)?;
        arg_vec.insert(0, first_arg);

        Ok((input, arg_vec))
    }

    /// Parse a named argument. The syntax is similar to variable assignments, but they
    /// are only used during type instantiation, and maybe later during function calls.
    ///
    /// `<identifier> = <instruction>`
    fn named_arg(input: &str) -> ParseResult<&str, VarAssign> {
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, id) = Token::identifier(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::equal(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, value) = Construct::instruction(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        Ok((input, VarAssign::new(false, id, value)))
    }

    fn named_arg_list(input: &str) -> ParseResult<&str, Vec<VarAssign>> {
        fn named_arg_and_comma(input: &str) -> ParseResult<&str, VarAssign> {
            let (input, constant) = Construct::named_arg(input)?;
            let (input, _) = Token::comma(input)?;

            Ok((input, constant))
        }

        // Get 0 or more arguments with a comma to the function call
        let (input, mut arg_vec) = many0(named_arg_and_comma)(input)?;

        // Parse the last argument, which does not have a comma. There needs to be
        // at least one argument, which can be this one
        let (input, last_arg) = Construct::named_arg(input)?;

        arg_vec.push(last_arg);

        Ok((input, arg_vec))
    }

    /// When a type is instantiated in the source code.
    ///
    /// ```
    /// type A(n: int); // Declare type A
    /// val = A(1); // Instantiate a new A type variable
    /// ```
    /// `<arg_list> := [(<constant> | <variable> | <expression>)*]`
    /// `<identifier> ( <arg_list> )`
    pub fn type_instantiation(input: &str) -> ParseResult<&str, TypeInstantiation> {
        let (input, type_name) = Token::identifier(input)?;
        let type_id = TypeId::new(type_name);
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::left_curly_bracket(input)?;

        let mut type_instantiation = TypeInstantiation::new(type_id);

        let (input, mut arg_vec) = Construct::named_arg_list(input)?;
        let (input, _) = Token::right_curly_bracket(input)?;

        arg_vec
            .drain(0..)
            .for_each(|field| type_instantiation.add_field(field));

        Ok((input, type_instantiation))
    }

    /// When a variable is assigned a value. Ideally, a variable cannot be assigned the
    /// `void` type.
    ///
    /// ```
    /// mut n = 12; // Store 12 into `n`, a mutable variable
    /// n = 1586; // Allowed
    /// ```
    ///
    /// A variable assignment is a Statement. It cannot be used as an Expression
    ///
    /// ```
    /// {
    ///     x = 12; // Block returns void
    /// }
    /// {
    ///     x = 12 // Forbidden
    /// }
    /// {
    ///     x = call();
    ///     x // Okay
    /// } // But it's easier to just...
    /// {
    ///     call()
    /// }
    /// ```
    ///
    /// 'mut' maybe_consume_extra identifier var_assignment
    pub(crate) fn mut_var_assignment(input: &str) -> ParseResult<&str, VarAssign> {
        let (input, _) = Token::mut_tok(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, id) = Token::identifier(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, mut assignment) = Construct::var_assignment(input, &id)?;

        assignment.set_mutable(true);

        Ok((input, assignment))
    }

    /// Parse a valid variable name
    ///
    /// `<identifier>`
    pub(crate) fn variable(input: &str) -> ParseResult<&str, Var> {
        let (input, name) = Token::identifier(input)?;

        Ok((input, Var::new(name)))
    }

    /// Parse a statement and the semicolon that follows
    ///
    /// `<instruction> ;`
    fn stmt_semicolon(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, expr) = Construct::instruction(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::semicolon(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        Ok((input, expr))
    }

    /// Parse multiple statements and a possible return Instruction
    fn stmts_and_maybe_last(input: &str) -> ParseResult<&str, (Instructions, MaybeInstruction)> {
        let (input, instructions) = many0(Construct::stmt_semicolon)(input)?;
        let (input, last_expr) =
            opt(alt((Construct::early_return, Construct::instruction)))(input)?;

        Ok((input, (instructions, last_expr)))
    }

    /// Parses the statements in a block as well as a possible last instruction
    fn block_instructions(input: &str) -> ParseResult<&str, (Instructions, MaybeInstruction)> {
        let (input, _) = Token::left_curly_bracket(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        let (input, (instructions, last)) = Construct::stmts_and_maybe_last(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        if let (_, Some(dead_code)) = opt(Construct::instruction)(input)? {
            // FIXME: This should be a warning
            return Err(NomError(Error::new(ErrKind::Parsing).with_msg(format!(
                "Dead code after early return, starting at {}",
                dead_code.print()
            ))));
        }

        let (input, _) = Token::right_curly_bracket(input)?;

        Ok((input, (instructions, last)))
    }

    /// A block of code is a new inner scope that contains instructions. You can use
    /// them in If/Else blocks, in function declarations, or just as is.
    ///
    /// ```
    /// func return_nothing() {
    ///     compute_stuff();
    /// } // Block returns void, so does the function
    ///
    /// x = {
    ///     12
    /// } // Block returns 12
    ///
    /// x = {
    ///     compute_stuff();
    ///     some_other_stuff();
    ///     12
    /// } // Block returns 12 after having called two functions
    /// ```
    ///
    /// There can only be one returning instruction, and it must be the last one
    /// in the block.
    ///
    /// `{ [ <instruction> ; ]* [ <instruction> ] }`
    pub(crate) fn block(input: &str) -> ParseResult<&str, Block> {
        let (input, (instructions, last)) = Construct::block_instructions(input)?;

        let mut block = Block::new();
        block.set_instructions(instructions);
        block.set_last(last);

        Ok((input, block))
    }

    /// Parse an empty argument declaration list
    ///
    /// `( )`
    fn args_dec_empty(input: &str) -> ParseResult<&str, Vec<DecArg>> {
        let (input, _) = Token::left_parenthesis(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::right_parenthesis(input)?;

        Ok((input, vec![]))
    }

    /// Parse an identifier then its type
    ///
    /// `<identifier> : <type>`
    fn identifier_type(input: &str) -> ParseResult<&str, DecArg> {
        let (input, id) = Token::identifier(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::colon(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, ty) = Token::identifier(input)?;

        Ok((input, DecArg::new(id, TypeId::new(ty))))
    }

    /// Parse an identifer as well as the type and comma that follows
    ///
    /// `<identifer> : <type> ,`
    fn identifier_type_comma(input: &str) -> ParseResult<&str, DecArg> {
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, arg) = Construct::identifier_type(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::comma(input)?;

        Ok((input, arg))
    }

    /// Parse a non empty argument declaration list
    ///
    /// `( [ <identifier> : <type> ]* )`
    fn args_dec_non_empty(input: &str) -> ParseResult<&str, Vec<DecArg>> {
        let (input, _) = Token::left_parenthesis(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        let (input, mut args) = many0(Construct::identifier_type_comma)(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        // Parse the last argument which does not have a comma
        let (input, last_arg) = Construct::identifier_type(input)?;
        args.push(last_arg);

        let (input, _) = Token::right_parenthesis(input)?;

        Ok((input, args))
    }

    /// Parse a list (maybe empty) of argument declarations
    fn args_dec(input: &str) -> ParseResult<&str, Vec<DecArg>> {
        let args = alt((Construct::args_dec_empty, Construct::args_dec_non_empty))(input)?;

        Ok(args)
    }

    /// Parse the void return type of a function, checking that no arrow is present
    fn return_type_void(input: &str) -> ParseResult<&str, Option<TypeId>> {
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, arrow) = opt(Token::arrow)(input)?;

        match arrow {
            Some(_) => Err(NomError(
                Error::new(ErrKind::Parsing).with_msg(String::from(input)),
            )),
            None => Ok((input, None)),
        }
    }

    /// Parse a non-void return type
    fn return_type_non_void(input: &str) -> ParseResult<&str, Option<TypeId>> {
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::arrow(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, ty) = Token::identifier(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        Ok((input, Some(TypeId::from(ty.as_str()))))
    }

    /// Parse the return type of a function. Can be void
    fn return_type(input: &str) -> ParseResult<&str, Option<TypeId>> {
        let ty = alt((Construct::return_type_non_void, Construct::return_type_void))(input)?;

        Ok(ty)
    }

    /// Parses the content of a function declaration
    ///
    /// `<identifier> <args_dec> <return_type> <block>`
    fn function_content(input: &str) -> ParseResult<&str, FunctionDec> {
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, fn_name) = Token::identifier(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        let (input, args) = Construct::args_dec(input)?;
        let (input, ty) = Construct::return_type(input)?;
        let (input, block) = Construct::block(input)?;

        let mut function = FunctionDec::new(fn_name, ty);

        function.set_args(args);
        function.set_block(block);

        Ok((input, function))
    }

    /// Parse a function declaration. This includes the function's signature and the
    /// associated code block
    ///
    /// ```
    /// func fn_name(arg0: int) -> int {
    ///     do_something(arg0);
    ///
    ///     12
    /// }
    /// ```
    ///
    /// `<func_arg_list> := [ (<identifier> : <type>)* ]
    /// `<func> <identifier> ( <func_arg_list> ) [ -> <type> ] <block>`
    pub(crate) fn function_declaration(input: &str) -> ParseResult<&str, FunctionDec> {
        let (input, _) = Token::func_tok(input)?;

        let (input, mut function) = Construct::function_content(input)?;
        function.set_kind(FunctionKind::Func);

        Ok((input, function))
    }

    /// Parse a test declaration. This returns a FunctionDec as well, but of
    /// kind `FunctionDec::Test`.
    /// test functions are non-callable by the programmer. Only the context can
    /// invoke them. Therefore, naming the test the same as the tested function is fine
    /// and is not any form of overloading whatsoever.
    ///
    /// ```
    /// test add() {
    ///     assert_eq(12 + 2, add(12, 2));
    /// }
    /// ```
    ///
    /// `<test> <identifier> ( ) <block>
    pub(crate) fn test_declaration(input: &str) -> ParseResult<&str, FunctionDec> {
        let (input, _) = Token::test_tok(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, fn_name) = Token::identifier(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        let (input, args) = Construct::args_dec(input)?;
        let (input, ty) = Construct::return_type_void(input)?;
        let (input, block) = Construct::block(input)?;

        let mut function = FunctionDec::new(fn_name, ty);

        function.set_args(args);
        function.set_block(block);

        function.set_kind(FunctionKind::Test);

        Ok((input, function))
    }

    /// Parse a mock declaration. This returns a FunctionDec as well, but of
    /// kind `FunctionDec::Mock`.
    ///
    ///
    /// ```
    /// mock add(lhs: int, rhs: int) -> int {
    ///     mock_stuff()
    /// }
    /// ```
    ///
    /// `<mock> <identifier> ( <func_arg_list> ) [ -> <type> ] <block>
    pub(crate) fn mock_declaration(input: &str) -> ParseResult<&str, FunctionDec> {
        let (input, _) = Token::mock_tok(input)?;

        let (input, mut function) = Construct::function_content(input)?;
        function.set_kind(FunctionKind::Mock);

        Ok((input, function))
    }

    /// Parse an external function declaration.
    ///
    /// External functions cannot have an associated block. The function's code resides
    /// in a native program, for example a shared C library or a Rust crate.
    ///
    /// `<ext> <func> <identifier> ( <func_arg_list> ) [ -> <type> ] ;`
    pub(crate) fn ext_declaration(input: &str) -> ParseResult<&str, FunctionDec> {
        let (input, _) = Token::ext_tok(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::func_tok(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        let (input, fn_name) = Token::identifier(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        let (input, args) = Construct::args_dec(input)?;
        let (input, ty) = Construct::return_type(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::semicolon(input)?;

        let mut function = FunctionDec::new(fn_name, ty);

        function.set_args(args);

        function.set_kind(FunctionKind::Ext);

        Ok((input, function))
    }

    /// Parse an `else` plus the associated block
    fn else_block(input: &str) -> ParseResult<&str, Block> {
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::else_tok(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        Construct::block(input)
    }

    /// Parse an if/else construct. This parses the entirety of the if/else. Therefore
    /// consuming the first `if` and the remaining optional `else`.
    ///
    /// `<if> <block> [ <else> <block> ]`
    pub(crate) fn if_else(input: &str) -> ParseResult<&str, IfElse> {
        let (input, _) = Token::if_tok(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        let (input, condition) = Construct::instruction(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        let (input, if_body) = Construct::block(input)?;

        let (input, else_body) = opt(Construct::else_block)(input)?;

        let if_else = IfElse::new(condition, if_body, else_body);

        Ok((input, if_else))
    }

    /// Parse return construct. Consumes a return with its potential value
    ///
    /// `<return> [ <xxx> ]`
    pub(crate) fn jk_return(input: &str) -> ParseResult<&str, Return> {
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::return_tok(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        let (input, val) = opt(Construct::instruction)(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = opt(Token::semicolon)(input)?;

        Ok((input, Return::new(val)))
    }

    /// Parse a loop block, meaning the `loop` keyword and a corresponding block
    ///
    /// `<loop> <block>`
    fn loop_block(input: &str) -> ParseResult<&str, Loop> {
        let (input, _) = Token::loop_tok(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, block) = Construct::block(input)?;

        Ok((input, Loop::new(LoopKind::Loop, block)))
    }

    /// Parse a while block. A while block consists of a high bound, or instruction, as
    /// well as a block
    ///
    /// `<while> <instruction> <block>`
    fn while_block(input: &str) -> ParseResult<&str, Loop> {
        let (input, _) = Token::while_tok(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, condition) = Construct::instruction(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, block) = Construct::block(input)?;

        Ok((input, Loop::new(LoopKind::While(condition), block)))
    }

    /// Construct a for block, which consists of a variable, a range instruction, and
    /// a block to execute
    ///
    /// `<for> <variable> <in> <instruction> <block>`
    fn for_block(input: &str) -> ParseResult<&str, Loop> {
        let (input, _) = Token::for_tok(input)?;

        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, variable) = Construct::variable(input)?;

        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::in_tok(input)?;

        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, instruction) = Construct::instruction(input)?;

        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, block) = Construct::block(input)?;

        Ok((
            input,
            Loop::new(LoopKind::For(variable, instruction), block),
        ))
    }

    /// Parse any loop construct: For, While or Loop
    pub(crate) fn any_loop(input: &str) -> ParseResult<&str, Loop> {
        let lup = alt((
            Construct::loop_block,
            Construct::for_block,
            Construct::while_block,
        ))(input)?;

        Ok(lup)
    }

    /// Parse a context directive. There are only a few of them, listed in
    /// the `JkInst` module
    ///
    /// `@<jinko_inst><args_list>`
    pub(crate) fn jinko_inst(input: &str) -> ParseResult<&str, JkInst> {
        let (input, _) = Token::at_sign(input)?;
        let (input, id) = Token::identifier(input)?;
        let (input, fc) = Construct::function_call(input, &id)?;

        // FIXME: No unwrap(), use something else than just the name
        //        this is very awkward, we have a Error coming up and we shouldn't be creating
        //        new JkInst.
        let inst = JkInst::from_function_call(fc).unwrap();

        Ok((input, inst))
    }

    /// Parse a binary operation. A binary operation is composed of an instruction, an
    /// operator and another instruction
    ///
    /// `<expr> <op> <expr>`
    ///
    /// ```
    /// x + y; // Add x and y together
    /// a << 2; // Shift a by 2 bits
    /// a > 2; // Is a greater than 2?
    /// ```
    pub(crate) fn binary_op(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
        ShuntingYard::parse(input)
    }

    /// Parse a user-defined custom type
    ///
    /// `<type> <TypeName> ( <func_arg_list> ) ;`
    pub(crate) fn type_declaration(input: &str) -> ParseResult<&str, TypeDec> {
        let (input, _) = Token::_type_tok(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        let (input, type_name) = Token::identifier(input)?;

        let (input, _) = Token::maybe_consume_extra(input)?;

        let (input, fields) = Construct::args_dec_non_empty(input)?;

        let type_declaration = TypeDec::new(type_name, fields);

        Ok((input, type_declaration))
    }

    /// Parses a path for code inclusion
    fn path(input: &str) -> ParseResult<&str, String> {
        let (input, _) = Token::maybe_consume_extra(input)?;

        let (input, path) = Token::identifier(input)?;

        let (input, _) = Token::maybe_consume_extra(input)?;

        Ok((input, path))
    }

    // Parse the `as` keyword and the following identifier if present
    fn az_identifier(input: &str) -> ParseResult<&str, Option<String>> {
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, id) = match opt(Token::az_tok)(input)? {
            (input, Some(_)) => {
                let (input, _) = Token::maybe_consume_extra(input)?;
                let (input, id) = Token::identifier(input)?;

                (input, Some(id))
            }
            (input, None) => (input, None),
        };

        let (input, _) = Token::maybe_consume_extra(input)?;

        Ok((input, id))
    }

    /// Parse an include statement and its possible aliasing
    ///
    /// `<incl> <path> [ <as> <alias> ]
    pub(crate) fn incl(input: &str) -> ParseResult<&str, Incl> {
        let (input, _) = Token::incl_tok(input)?;
        let (input, path) = Construct::path(input)?;

        let (input, rename) = Construct::az_identifier(input)?;

        let (input, _) = Token::maybe_consume_extra(input)?;

        let incl = Incl::new(path, rename);

        Ok((input, incl))
    }

    /// Parse a viable instance for a field access.
    /// This is similar to Construct::method_caller, without allowing constants, as they
    /// contain no fields.
    fn instance(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
        let instance = alt((
            BoxConstruct::type_instantiation,
            Construct::function_call_or_var,
            BoxConstruct::if_else,
            BoxConstruct::block,
            BoxConstruct::any_loop,
            BoxConstruct::jinko_inst,
            // DESIGN CHANGE, we can do "hello".field which fails later on like
            // "hello".invalid_method() would
            // Previously "hello".field was a parser error
            Construct::constant,
        ))(input)?;

        Ok(instance)
    }

    pub fn function_call_or_var(input: &str) -> ParseResult<&str, Box<dyn Instruction>> {
        let (input, id) = Token::identifier(input)?;
        BoxConstruct::function_call(input, &id).or_else(|_| Ok((input, Box::new(Var::new(id)))))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::JkInstKind;
    use crate::{JkBool, JkChar, JkFloat, JkInt, JkString};

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
    fn var_assignment() {
        let (input, expr) = expr("var = 'a'").unwrap();
        let assign = expr.downcast_ref::<VarAssign>().unwrap();

        assert_eq!(input, "");
        assert_eq!(assign.symbol(), "var");
    }

    #[test]
    fn var_assigment_tricky() {
        let (input, expr) = expr("n1=b.call() + 1").unwrap();
        let assign = expr.downcast_ref::<VarAssign>().unwrap();

        assert_eq!(input, "");
        assert_eq!(assign.symbol(), "n1");
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
        dbg!(input);
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

    /*

        fn function_call(input: &str) -> ParseResult<&str, FunctionCall> {
            let (input, fn_id) = Token::identifier(input)?;
            let (input, _) = Token::maybe_consume_extra(input).unwrap();
            Construct::function_call(input, &fn_id)
        }

        #[test]
        fn t_var_assign_valid() {
            let (input, id) = Token::identifier("x = 12;").unwrap();
            let (input, _) = Token::maybe_consume_extra(input).unwrap();
            assert_eq!(
                Construct::var_assignment(input, &id).unwrap().1.mutable(),
                false
            );
            let (input, id) = Token::identifier("x = 12;").unwrap();
            let (input, _) = Token::maybe_consume_extra(input).unwrap();
            assert_eq!(
                Construct::var_assignment(input, &id).unwrap().1.symbol(),
                "x"
            );

            assert_eq!(
                Construct::mut_var_assignment("mut x_99 = 129;")
                    .unwrap()
                    .1
                    .mutable(),
                true
            );
            assert_eq!(
                Construct::mut_var_assignment("mut x_99 = 129;")
                    .unwrap()
                    .1
                    .symbol(),
                "x_99"
            );

            let (input, id) = Token::identifier("mut_x_99 = 129;").unwrap();
            let (input, _) = Token::maybe_consume_extra(input).unwrap();
            assert_eq!(
                Construct::var_assignment(input, &id).unwrap().1.mutable(),
                false
            );
            let (input, id) = Token::identifier("mut_x_99 = 129;").unwrap();
            let (input, _) = Token::maybe_consume_extra(input).unwrap();
            assert_eq!(
                Construct::var_assignment(input, &id).unwrap().1.symbol(),
                "mut_x_99"
            );

            assert_eq!(
                Construct::mut_var_assignment("mut mut_x_99 = 129;")
                    .unwrap()
                    .1
                    .mutable(),
                true
            );
            assert_eq!(
                Construct::mut_var_assignment("mut mut_x_99 = 129;")
                    .unwrap()
                    .1
                    .symbol(),
                "mut_x_99"
            );

            assert_eq!(
                Construct::mut_var_assignment("mut\nname = 129;")
                    .unwrap()
                    .1
                    .mutable(),
                true
            );

            assert!(Construct::mut_var_assignment("mut x=12;").is_ok());
            assert!(Construct::mut_var_assignment("mut x= 12;").is_ok());
            assert!(Construct::mut_var_assignment("mut x =12;").is_ok());
        }

        #[test]
        fn t_var_assign_invalid() {
            let (input, id) = Token::identifier("mutable x = 12").unwrap();
            assert!(Construct::var_assignment(input, &id).is_err());
        }

        #[test]
        fn t_function_call_no_args_valid() {
            assert_eq!(function_call("fn()").unwrap().1.name(), "fn");
            assert_eq!(function_call("fn()").unwrap().1.args().len(), 0);

            assert_eq!(function_call("fn(    )").unwrap().1.name(), "fn");
            assert_eq!(function_call("fn(    )").unwrap().1.args().len(), 0);
        }

        #[test]
        fn t_function_call_valid() {
            assert_eq!(function_call("fn(2)").unwrap().1.name(), "fn");
            assert_eq!(function_call("fn(2)").unwrap().1.args().len(), 1);

            assert_eq!(function_call("fn(1, 2, 3)").unwrap().1.name(), "fn");
            assert_eq!(function_call("fn(a, hey(), 3.12)").unwrap().1.name(), "fn");
            assert_eq!(function_call("fn(1, 2, 3)").unwrap().1.args().len(), 3);

            assert_eq!(function_call("fn(1   , 2,3)").unwrap().1.name(), "fn");
            assert_eq!(function_call("fn(1   , 2,3)").unwrap().1.args().len(), 3);
            assert_eq!(function_call("fn     ()").unwrap().1.name(), "fn");
        }

        #[test]
        fn t_function_call_invalid() {
            assert!(function_call("fn(").is_err());
            assert!(function_call("fn))").is_err());
            assert!(function_call("fn((").is_err());
            assert!(function_call("fn((").is_err());
            assert!(function_call("fn((").is_err());
        }

        #[test]
        fn t_function_call_multiarg_invalid() {
            assert!(function_call("fn(1, 2, 3, 4,)").is_err());
            assert!(function_call("fn(1, 2, 3, 4,   )").is_err());
        }

        #[test]
        fn t_block_empty() {
            assert_eq!(Construct::block("{}").unwrap().1.instructions().len(), 0);
        }

        #[test]
        fn t_block_valid_oneline() {
            assert_eq!(
                Construct::block("{ 12a; }").unwrap().1.instructions().len(),
                1
            );
            assert_eq!(
                Construct::block("{ 12a; 14a; }")
                    .unwrap()
                    .1
                    .instructions()
                    .len(),
                2
            );
            assert_eq!(
                Construct::block("{ 12a; 14a }")
                    .unwrap()
                    .1
                    .instructions()
                    .len(),
                1
            );

            assert!(Construct::block("{ 12a; 14a }").unwrap().1.last().is_some());
        }

        #[test]
        fn t_id_type_valid() {
            assert_eq!(
                Construct::identifier_type("name: some_type")
                    .unwrap()
                    .1
                    .name(),
                "name"
            );
            assert_eq!(
                Construct::identifier_type("name: some_type")
                    .unwrap()
                    .1
                    .get_type()
                    .id(),
                "some_type"
            );

            assert_eq!(
                Construct::identifier_type("name     :some_type")
                    .unwrap()
                    .1
                    .name(),
                "name"
            );
            assert_eq!(
                Construct::identifier_type("name     :some_type")
                    .unwrap()
                    .1
                    .get_type()
                    .id(),
                "some_type"
            );
        }

        #[test]
        fn t_args_dec_empty() {
            assert_eq!(Construct::args_dec("()").unwrap().1.len(), 0);
        }

        #[test]
        fn t_args_dec_one_arg() {
            assert_eq!(Construct::args_dec("(name :ty)").unwrap().1.len(), 1);
        }

        #[test]
        fn t_args_dec_valid() {
            assert_eq!(
                Construct::args_dec("(name :ty, name1      : type1)")
                    .unwrap()
                    .1
                    .len(),
                2
            );
        }

        #[test]
        fn t_return_type_void() {
            assert_eq!(Construct::return_type(""), Ok(("", None)));
            assert_eq!(Construct::return_type("    "), Ok(("", None)));
            assert_eq!(
                Construct::return_type("        { 12 }"),
                Ok(("{ 12 }", None))
            );
        }

        #[test]
        fn t_block_invalid_oneline() {
            assert!(Construct::block("{ 12a;").is_err());
            assert!(Construct::block("{ 12a").is_err());
            assert!(Construct::block("{ 12a; 13a").is_err());
            assert!(Construct::block("12a; 13a }").is_err());
        }

        #[test]
        fn t_block_valid_multiline() {
            let input = r#"{
                    12a;
                    12a;
                    13a;
                }"#;

            assert_eq!(Construct::block(input).unwrap().1.instructions().len(), 3);

            let input = r#"{
                    12a;
                    12a;
                    13a;
                    14a
                }"#;

            assert_eq!(Construct::block(input).unwrap().1.instructions().len(), 3);

            let input = r#"{
                    true;
                    false
                }"#;

            assert_eq!(Construct::block(input).unwrap().1.instructions().len(), 1);
        }

        #[test]
        fn t_return_type_non_void() {
            assert_eq!(
                Construct::return_type("-> int"),
                Ok(("", Some(TypeId::from("int"))))
            );
            assert_eq!(
                Construct::return_type("   ->    int   {"),
                Ok(("{", Some(TypeId::from("int"))))
            );
        }

        #[test]
        fn t_function_declaration_valid_simple() {
            let func = Construct::function_declaration("func something() {}")
                .unwrap()
                .1;

            assert_eq!(func.name(), "something");
            assert_eq!(func.ty(), None);
            assert_eq!(func.args().len(), 0);
            assert_eq!(func.fn_kind(), FunctionKind::Func);
        }

        #[test]
        fn t_function_declaration_valid_space() {
            let func = Construct::function_declaration("func something        \t() {}")
                .unwrap()
                .1;

            assert_eq!(func.name(), "something");
            assert_eq!(func.ty(), None);
            assert_eq!(func.args().len(), 0);
            assert_eq!(func.fn_kind(), FunctionKind::Func);
        }

        #[test]
        fn t_function_declaration_valid() {
            let func = Construct::function_declaration("func add(lhs: ty, rhs: ty) -> ty {}")
                .unwrap()
                .1;

            assert_eq!(func.name(), "add");
            assert_eq!(func.ty(), Some(&TypeId::from("ty")));
            assert_eq!(func.args().len(), 2);
            assert_eq!(func.fn_kind(), FunctionKind::Func);
        }

        #[test]
        fn t_test_valid() {
            let test = Construct::test_declaration("test add() {}").unwrap().1;

            assert_eq!(test.name(), "add");
            assert_eq!(test.ty(), None);
            assert_eq!(test.fn_kind(), FunctionKind::Test);
        }

        #[test]
        fn t_test_invalid() {
            assert!(Construct::test_declaration("test add(a: int) -> int {}").is_err());
        }

        #[test]
        fn t_mock_valid() {
            let test = Construct::mock_declaration("mock add(lhs: ty, rhs: ty) {}")
                .unwrap()
                .1;

            assert_eq!(test.name(), "add");
            assert_eq!(test.ty(), None);
            assert_eq!(test.fn_kind(), FunctionKind::Mock);
        }

        #[test]
        fn t_ext_valid() {
            let test = Construct::ext_declaration("ext func add(lhs: ty, rhs: ty) -> ty;")
                .unwrap()
                .1;

            assert_eq!(test.name(), "add");
            assert_eq!(test.ty(), Some(&TypeId::from("ty")));
            assert_eq!(test.fn_kind(), FunctionKind::Ext);
        }

        #[test]
        fn t_ext_valid_void() {
            let test = Construct::ext_declaration("ext func add(lhs: ty, rhs: ty);")
                .unwrap()
                .1;

            assert_eq!(test.name(), "add");
            assert_eq!(test.ty(), None);
            assert_eq!(test.fn_kind(), FunctionKind::Ext);
        }

        #[test]
        fn t_ext_invalid() {
            assert!(Construct::ext_declaration("ext func add(a: int) -> int {}").is_err());
        }

        #[test]
        fn t_if_else_just_if() {
            let ie = Construct::if_else("if condition {}");

            assert!(&ie.is_ok());
        }

        #[test]
        fn t_if_else() {
            assert!(Construct::if_else("if condition {} else {}").is_ok());
        }

        #[test]
        fn t_loop_valid() {
            assert!(Construct::loop_block("loop {}").is_ok());
        }

        #[test]
        fn t_loop_invalid() {
            assert!(Construct::loop_block("loo {}").is_err());

            assert!(Construct::loop_block("loop").is_err());
        }

        #[test]
        fn t_while_valid() {
            assert!(Construct::while_block("while x_99 {}").is_ok());
        }

        #[test]
        fn t_while_invalid() {
            assert!(Construct::while_block("while {}").is_err());

            assert!(Construct::while_block("while").is_err());
        }

        #[test]
        fn t_for_valid() {
            assert!(Construct::for_block("for x_99 in x_99 {}").is_ok());
        }

        #[test]
        fn t_for_invalid() {
            assert!(Construct::for_block("for {}").is_err());

            assert!(Construct::for_block("for x99 in {}").is_err());

            assert!(Construct::for_block("for x99 in { { { inner_block() } } }").is_err());
        }

        #[test]
        fn t_jinko_inst_valid() {
            let (_, dump_inst) = Construct::jinko_inst("@dump()").unwrap();
            assert_eq!(dump_inst.jk_inst_kind(), &JkInstKind::Dump);

            let (_, quit_inst) = Construct::jinko_inst("@quit(something, something_else)").unwrap();
            assert_eq!(quit_inst.jk_inst_kind(), &JkInstKind::Quit);
        }

        #[test]
        fn t_binary_op_valid() {
            assert!(Construct::binary_op("a *   12 ").is_ok());
            assert!(Construct::binary_op("some() + 12.1").is_ok());
        }

        #[test]
        fn t_binary_op_invalid() {
            assert!(Construct::binary_op("a ? 12").is_err());
        }

        #[test]
        fn t_type_declaration_simple() {
            assert!(Construct::type_declaration("type Int(v: int);").is_ok());
            assert!(Construct::type_declaration("type Ints(a: int, b: int);").is_ok());
            assert!(Construct::type_declaration("type Compound(i: int, s: str);").is_ok());
            assert!(Construct::type_declaration(
                "type Custom(v: int, a: SomeType, b: Another, c: lower_case);",
            )
            .is_ok());
        }

        #[test]
        fn t_type_declaration_empty() {
            assert!(Construct::type_declaration("type Empty();").is_err());
        }

        #[test]
        fn t_type_declaration_invalid() {
            assert!(Construct::type_declaration("type ExtraComma(a: int, b: int,);").is_err());
        }

        #[test]
        fn t_type_instantiation_valid() {
            assert!(Construct::type_instantiation("Custom { a = 1 }").is_ok());
        }

        #[test]
        fn t_type_instantiation_valid_multi() {
            assert!(Construct::type_instantiation("Custom { a = 1, b= 2, c = { 's' } }").is_ok());
        }

        #[test]
        fn t_type_instantiation_invalid() {
            assert!(Construct::type_instantiation("Custom { ").is_err());
        }

        #[test]
        fn t_type_instantiation_no_name() {
            assert!(Construct::type_instantiation("{ 1 }").is_err());
        }

        #[test]
        fn t_type_instantiation_no_named_arg() {
            assert!(Construct::type_instantiation("CustomType { 1 }").is_err());
        }

        #[test]
        fn t_func_dec_binop() {
            assert!(Construct::function_declaration("func a(a: int, b:int) -> int { a + b }").is_ok());
        }

        #[test]
        fn t_func_dec_return_arg() {
            assert!(Construct::function_declaration("func a(a: int, b:int) -> int { a }").is_ok());
        }

        #[test]
        fn t_func_dec_return_arg_plus_stmt() {
            assert!(
                Construct::function_declaration("func a(a: int, b:int) -> int { something(); a }")
                    .is_ok()
            );
        }

        #[test]
        fn t_func_dec_return_binop_plus_stmt() {
            assert!(Construct::function_declaration(
                "func a(a: int, b:int) -> int { something(); a + b }"
            )
            .is_ok());
        }

        #[test]
        fn t_func_dec_return_binop_as_var() {
            assert!(Construct::function_declaration(
                "func a(a: int, b:int) -> int { res = a + b; res }"
            )
            .is_ok());
        }

        #[test]
        fn t_func_call_with_true_arg_is_func_call() {
            let res = Construct::instruction("h(true)").unwrap().1;
            res.downcast_ref::<FunctionCall>().unwrap();

            // There might be a bug that a function call with just a boolean argument gets
            // parsed as a variable. This test aims at correcting that regression. If it
            // fails, then it means the function call did not get parsed as a function call
        }

        #[test]
        fn t_named_argument_valid() {
            assert!(Construct::named_arg("a = b").is_ok());
            assert!(Construct::named_arg("a = 2").is_ok());
            assert!(Construct::named_arg("a = { 2 }").is_ok());
            assert!(Construct::named_arg("a = call()").is_ok());
        }

        #[test]
        fn t_named_argument_invalid() {
            assert!(Construct::named_arg("a =").is_err(), "No second member");
            assert!(Construct::named_arg("= 2").is_err(), "No first member");
            assert!(
                Construct::named_arg("a { 2 }").is_err(),
                "Missing equal sign"
            );
        }

        #[test]
        fn t_incl_valid() {
            assert!(Construct::incl("incl simple").is_ok());
        }

        #[test]
        fn t_incl_valid_plus_rename() {
            assert!(Construct::incl("incl a as b").is_ok());
        }

        #[test]
        fn t_incl_invalid() {
            assert!(Construct::incl("incl").is_err());
        }

        #[test]
        fn t_incl_plus_rename_invalid() {
            assert!(Construct::incl("incl a as").is_err());
        }

        #[test]
        fn t_method_call_simple() {
            assert!(
                Construct::method_call_or_field_access("a.b()").is_ok(),
                "Valid to have simple identifiers"
            );
            assert!(
                Construct::method_call_or_field_access("135.method()").is_ok(),
                "Valid to have constant as caller"
            );
            assert!(
                Construct::method_call_or_field_access("{ hey }.method()").is_ok(),
                "Valid to have block as caller"
            );
            assert!(
                Construct::method_call_or_field_access("func_call().method()").is_ok(),
                "Valid to have call as caller"
            );
            assert!(
                Construct::instruction("    \ta.b()").is_ok(),
                "Valid to have simple identifiers"
            );
        }

        #[test]
        fn t_method_call_invalid() {
            let (input, _) =
                Construct::method_call_or_field_access("a.b(").expect("Not parsed as field");
            assert_eq!(input, "(", "Missing parentheses");
            assert!(
                Construct::method_call_or_field_access("a.()").is_err(),
                "Missing method name"
            );
            assert!(Construct::instance(".method()").is_err(), "Missing caller");
        }

        #[test]
        fn t_sy_eager_consume() {
            // https://github.com/CohenArthur/jinko/issues/172

            assert_eq!(Construct::instruction("1 2").unwrap().0, " 2");
            assert_eq!(Construct::instruction("a b").unwrap().0, " b");
        }

        #[test]
        fn t_field_access_valid() {
            assert!(Construct::method_call_or_field_access("s.a").is_ok());
            assert!(Construct::method_call_or_field_access("longer_identifier.a").is_ok());
            assert!(Construct::method_call_or_field_access("longer_identifier.with_numbers32").is_ok());
        }

        #[test]
        fn t_field_access_from_type_instantiation() {
            assert!(Construct::method_call_or_field_access("CustomType { k = a, v = b }.c").is_ok());
        }

        #[test]
        fn t_field_access_with_constant_field() {
            assert!(Construct::method_call_or_field_access("s.1").is_err());
            assert!(Construct::method_call_or_field_access("a.\"string\"").is_err());
        }

        // TODO Remove if design change accepted
        /*#[test]
        fn t_field_access_invalid_with_constant_instance() {
            assert!(Construct::method_call_or_field_access("1.a").is_err());
            assert!(Construct::method_call_or_field_access("\"string\".a").is_err());
        }
        */

        #[test]
        fn t_field_access_with_spaces() {
            assert!(Construct::method_call_or_field_access("sdot. space").is_err());
            assert!(Construct::method_call_or_field_access("s .dotspace").is_err());
        }

        // In the following tests about comments, we always need an extra call to `instruction`
        // in order to get ride of the newline after the comment


    #[test]
    fn t_multi_field_access_3() {
        assert!(Construct::method_call_or_field_access("top.middle.bottom").is_ok());
        assert_eq!(
            Construct::method_call_or_field_access("top.middle.bottom")
                .unwrap()
                .0,
            ""
        );
    }

    #[test]
    fn t_multi_field_access_4() {
        assert!(Construct::method_call_or_field_access("top.middle.bottom.last").is_ok());
        assert_eq!(
            Construct::method_call_or_field_access("top.middle.bottom.last")
                .unwrap()
                .0,
            ""
        );
    }

    #[test]
    fn t_multi_field_access_n() {
        assert!(Construct::method_call_or_field_access(
            "jk.jk.jk.jk.jk.jk.jk.jk.jk.jk.jk.jk.jk.jk.jk"
        )
        .is_ok());
        assert_eq!(
            Construct::method_call_or_field_access("jk.jk.jk.jk.jk.jk.jk.jk.jk.jk.jk.jk.jk.jk.jk")
                .unwrap()
                .0,
            ""
        );
    }

    #[test]
    fn t_naked_return() {
        let ie = Construct::jk_return("return");

        assert!(&ie.is_ok());

        let res = ie.unwrap().1;
        println!("Test {}", res.print());
    }

    #[test]
    fn t_return_something() {
        let ie = Construct::jk_return("return 42");

        assert!(&ie.is_ok());
    }

    #[test]
    fn t_return_block_dead_code() {
        let ie = Construct::instruction("{ return 42; 5; }");

        // This should fail, ``5;`` is dead code because of the ``return``
        assert!(&ie.is_err());
    }
    */
}
