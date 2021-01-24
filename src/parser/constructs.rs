//! A `Construct` is a complex set of tokens. For example, `fn()` is an identifier, a
//! left parenthesis and a right parenthesis. Together, they constitute a function call.
//! In the same vein, `x = 12;` is 4 tokens used to represent variable assignment.
//! Therefore, constructs use tokens while the parser only uses constructs. This is an
//! abstraction for all possible ways to parse a line in jinko.
//!
//! Each of the functions in that module contain the grammar they represent above their
//! name. The syntax used for the grammar is loosely based on regular expressions and
//! globbing. One can use * to indicate 0 or more, ? to indicate 1 or more, etc etc.
//! Optional parameters are included between brackets. For example,
//!
//! `[mut] <identifier> = <const> | <function_call> | <block> | <identifier>`
//!
//! is the grammar for a variable assignment.

use nom::{branch::alt, combinator::opt, multi::many0, IResult};

use super::{
    box_construct::BoxConstruct, constant_construct::ConstantConstruct, jinko_insts::JinkoInst,
    shunting_yard::ShuntingYard, tokens::Token,
};
use crate::instruction::{
    Audit, BinaryOp, Block, FunctionCall, FunctionDec, FunctionDecArg, FunctionKind, IfElse,
    Instruction, Loop, LoopKind, Var, VarAssign,
};

pub struct Construct;

impl Construct {
    /// Constants are raw values in the source code. For example, `"string"`, `12` and
    /// `0.5`.
    ///
    /// `'<any_char>' | "<any_char>*" | <num>? | <num>?.<num>?`
    pub fn constant(input: &str) -> IResult<&str, Box<dyn Instruction>> {
        alt((
            ConstantConstruct::c_char_constant,
            ConstantConstruct::c_string_constant,
            ConstantConstruct::c_float_constant,
            ConstantConstruct::c_int_constant,
            ConstantConstruct::c_bool_constant,
        ))(input)
    }

    /// Parse a function call with no arguments
    ///
    /// `<identifier> ( )`
    fn function_call_no_args(input: &str) -> IResult<&str, FunctionCall> {
        let (input, fn_id) = Token::identifier(input)?;
        let (input, _) = Token::left_parenthesis(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::right_parenthesis(input)?;

        Ok((input, FunctionCall::new(fn_id.to_owned())))
    }

    /// Parse an argument given to a function. Consumes the whitespaces before and after
    /// the argument
    fn arg(input: &str) -> IResult<&str, Box<dyn Instruction>> {
        let (input, _) = Token::maybe_consume_extra(input)?;

        let (input, constant) = Construct::expression(input)?;

        let (input, _) = Token::maybe_consume_extra(input)?;

        Ok((input, constant))
    }

    fn arg_and_comma(input: &str) -> IResult<&str, Box<dyn Instruction>> {
        let (input, constant) = Construct::expression(input)?;
        let (input, _) = Token::comma(input)?;

        Ok((input, constant))
    }

    /// Parse a list of arguments separated by comma
    fn args_list(input: &str) -> IResult<&str, Vec<Box<dyn Instruction>>> {
        // Get 1 or more arguments with a comma to the function call
        let (input, mut arg_vec) = many0(Construct::arg_and_comma)(input)?;

        // Parse the last argument, which does not have a comma. There needs to be
        // at least one argument, which can be this one
        let (input, last_arg) = Construct::arg(input)?;

        arg_vec.push(last_arg);

        Ok((input, arg_vec))
    }

    /// Parse a function call with arguments
    fn function_call_args(input: &str) -> IResult<&str, FunctionCall> {
        let (input, fn_id) = Token::identifier(input)?;
        let (input, _) = Token::left_parenthesis(input)?;

        let mut fn_call = FunctionCall::new(fn_id.to_owned());

        let (input, mut arg_vec) = Construct::args_list(input)?;
        let (input, _) = Token::right_parenthesis(input)?;

        arg_vec.drain(0..).for_each(|arg| fn_call.add_arg(arg));

        Ok((input, fn_call))
    }

    /// When a function is called in the source code.
    ///
    /// ```
    /// fn(); // Function call
    /// fn() // Call the function `fn` and use the return result as an expression
    /// x = fn(); // Assign the result of the function call to the variable x
    /// ```
    ///
    /// `<arg_list> := [(<constant> | <variable> | <expression>)*]`
    /// `<identifier> ( <arg_list> )`
    pub fn function_call(input: &str) -> IResult<&str, FunctionCall> {
        alt((
            Construct::function_call_no_args,
            Construct::function_call_args,
        ))(input)
    }

    /// When a variable is assigned a value. Ideally, a variable cannot be assigned the
    /// `void` type.
    ///
    /// ```
    /// x = 12; // Store 12 into the variable `x`
    /// x = 456; // Forbidden, `x` is immutable
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
    /// `[mut] <identifier> = ( <constant> | <function_call> ) ;`
    pub fn var_assignment(input: &str) -> IResult<&str, VarAssign> {
        let (input, mut_opt) = opt(Token::mut_tok)(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        let (input, id) = Token::identifier(input)?;
        let (input, _) = opt(Token::consume_whitespaces)(input)?;
        let (input, _) = Token::equal(input)?;
        let (input, _) = opt(Token::consume_whitespaces)(input)?;
        let (input, value) = Construct::expression(input)?;
        let (input, _) = Token::semicolon(input)?;

        match mut_opt {
            Some(_) => Ok((input, VarAssign::new(true, id.to_owned(), value))),
            None => Ok((input, VarAssign::new(false, id.to_owned(), value))),
        }
    }

    /// Parse a valid variable name
    ///
    /// `<identifier>`
    pub fn variable(input: &str) -> IResult<&str, Var> {
        let (input, name) = Token::identifier(input)?;

        Ok((input, Var::new(name.to_owned())))
    }

    /// Parse any valid jinko expression. This can be a function call, a variable,
    /// a block declaration...
    pub fn expression(input: &str) -> IResult<&str, Box<dyn Instruction>> {
        let (input, _) = Token::maybe_consume_extra(input)?;

        // FIXME: If input is empty, return an error or do nothing
        let (input, value) = alt((
            BoxConstruct::function_declaration,
            BoxConstruct::ext_declaration,
            BoxConstruct::function_call,
            BoxConstruct::if_else,
            BoxConstruct::any_loop,
            BoxConstruct::jinko_inst,
            BoxConstruct::block,
            BoxConstruct::var_assignment,
            Construct::binary_op,
            BoxConstruct::variable,
            Construct::constant,
        ))(input)?;

        let (input, _) = Token::maybe_consume_extra(input)?;

        Ok((input, value))
    }

    fn stmt_semicolon(input: &str) -> IResult<&str, Box<dyn Instruction>> {
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, expr) = Construct::expression(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::semicolon(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        Ok((input, expr))
    }

    /// Parses the statements in a block as well as a possible last expression
    fn instructions(
        input: &str,
    ) -> IResult<&str, (Vec<Box<dyn Instruction>>, Option<Box<dyn Instruction>>)> {
        let (input, _) = Token::left_curly_bracket(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        let (input, instructions) = many0(Construct::stmt_semicolon)(input)?;
        let (input, last_expr) = opt(Construct::expression)(input)?;

        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::right_curly_bracket(input)?;

        Ok((input, (instructions, last_expr)))
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
    /// `{ [ <expression> ; ]* [ <expression> ] }`
    pub fn block(input: &str) -> IResult<&str, Block> {
        let (input, (instructions, last)) = Construct::instructions(input)?;

        let mut block = Block::new();
        block.set_instructions(instructions);
        block.set_last(last);

        Ok((input, block))
    }

    fn args_dec_empty(input: &str) -> IResult<&str, Vec<FunctionDecArg>> {
        let (input, _) = Token::left_parenthesis(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::right_parenthesis(input)?;

        Ok((input, vec![]))
    }

    /// Parse an identifier then its type
    ///
    /// `<identifier> : <identifier>
    fn identifier_type(input: &str) -> IResult<&str, FunctionDecArg> {
        let (input, id) = Token::identifier(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::colon(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, ty) = Token::identifier(input)?;

        Ok((input, FunctionDecArg::new(id.to_owned(), ty.to_owned())))
    }

    fn identifier_type_comma(input: &str) -> IResult<&str, FunctionDecArg> {
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, arg) = Construct::identifier_type(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::comma(input)?;

        Ok((input, arg))
    }

    fn args_dec_non_empty(input: &str) -> IResult<&str, Vec<FunctionDecArg>> {
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
    fn args_dec(input: &str) -> IResult<&str, Vec<FunctionDecArg>> {
        alt((Construct::args_dec_empty, Construct::args_dec_non_empty))(input)
    }

    fn return_type_void(input: &str) -> IResult<&str, Option<String>> {
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, arrow) = opt(Token::arrow)(input)?;

        match arrow {
            Some(_) => Err(nom::Err::Error((input, nom::error::ErrorKind::OneOf))),
            None => Ok((input, None)),
        }
    }

    /// Parse a non-void return type
    fn return_type_non_void(input: &str) -> IResult<&str, Option<String>> {
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::arrow(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, ty) = Token::identifier(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        Ok((input, Some(ty.to_owned())))
    }

    /// Parse the return type of a function. Can be void
    fn return_type(input: &str) -> IResult<&str, Option<String>> {
        alt((Construct::return_type_non_void, Construct::return_type_void))(input)
    }

    fn function_content(input: &str) -> IResult<&str, FunctionDec> {
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, fn_name) = Token::identifier(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        let (input, args) = Construct::args_dec(input)?;
        let (input, ty) = Construct::return_type(input)?;
        let (input, block) = Construct::block(input)?;

        let mut function = FunctionDec::new(fn_name.to_owned(), ty);

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
    /// `<typed_arg_list> := [ (<identifier> : <type>)* ]
    /// `<func> <identifier> ( <typed_arg_list> ) [ -> <type> ] <block>`
    pub fn function_declaration(input: &str) -> IResult<&str, FunctionDec> {
        let (input, _) = Token::func_tok(input)?;

        let (input, mut function) = Construct::function_content(input)?;
        function.set_kind(FunctionKind::Func);

        Ok((input, function))
    }

    /// Parse a test declaration. This returns a FunctionDec as well, but of
    /// kind `FunctionDec::Test`.
    /// test functions are non-callable by the programmer. Only the interpreter can
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
    pub fn test_declaration(input: &str) -> IResult<&str, FunctionDec> {
        let (input, _) = Token::test_tok(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, fn_name) = Token::identifier(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        let (input, args) = Construct::args_dec(input)?;
        let (input, ty) = Construct::return_type_void(input)?;
        let (input, block) = Construct::block(input)?;

        let mut function = FunctionDec::new(fn_name.to_owned(), ty);

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
    /// `<mock> <identifier> ( <typed_arg_list> ) [ -> <type> ] <block>
    pub fn mock_declaration(input: &str) -> IResult<&str, FunctionDec> {
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
    /// `<ext> <func> <identifier> ( <typed_arg_list> ) [ -> <type> ] ;`
    pub fn ext_declaration(input: &str) -> IResult<&str, FunctionDec> {
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

        let mut function = FunctionDec::new(fn_name.to_owned(), ty);

        function.set_args(args);

        function.set_kind(FunctionKind::Ext);

        Ok((input, function))
    }

    /// Parse an `else` plus the associated block
    fn else_block(input: &str) -> IResult<&str, Block> {
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::else_tok(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        Construct::block(input)
    }

    /// Parse an if/else construct. This parses the entirety of the if/else. Therefore
    /// consuming the first `if` and the remaining optional `else`.
    ///
    /// `<if> <block> [ <else> <block> ]`
    pub fn if_else(input: &str) -> IResult<&str, IfElse> {
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::if_tok(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        let (input, condition) = Construct::expression(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        let (input, if_body) = Construct::block(input)?;

        let (input, else_body) = opt(Construct::else_block)(input)?;

        let if_else = IfElse::new(condition, if_body, else_body);

        Ok((input, if_else))
    }

    /// Parse an audit block. This consists in the the audit keyword and the following
    /// block. Audit blocks are useful to relax the interpreter and develop faster. For
    /// example, you're allowed to ignore return values in an audit block.
    ///
    /// `<audit> <block>`
    pub fn audit(input: &str) -> IResult<&str, Audit> {
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::audit_tok(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, block) = Construct::block(input)?;

        Ok((input, Audit::new(block)))
    }

    /// Parse a loop block, meaning the `loop` keyword and a corresponding block
    ///
    /// `<loop> <block>`
    pub fn loop_block(input: &str) -> IResult<&str, Loop> {
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::loop_tok(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, block) = Construct::block(input)?;

        Ok((input, Loop::new(LoopKind::Loop, block)))
    }

    /// Parse a while block. A while block consists of a high bound, or expression, as
    /// well as a block
    ///
    /// `<while> <expression> <block>`
    pub fn while_block(input: &str) -> IResult<&str, Loop> {
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::while_tok(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, condition) = Construct::expression(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, block) = Construct::block(input)?;

        Ok((input, Loop::new(LoopKind::While(condition), block)))
    }

    /// Construct a for block, which consists of a variable, a range expression, and
    /// a block to execute
    ///
    /// `<for> <variable> <in> <expression> <block>`
    pub fn for_block(input: &str) -> IResult<&str, Loop> {
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::for_tok(input)?;

        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, variable) = Construct::variable(input)?;

        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::in_tok(input)?;

        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, expression) = Construct::expression(input)?;

        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, block) = Construct::block(input)?;

        Ok((input, Loop::new(LoopKind::For(variable, expression), block)))
    }

    /// Parse any loop construct: For, While or Loop
    pub fn any_loop(input: &str) -> IResult<&str, Loop> {
        alt((
            Construct::loop_block,
            Construct::for_block,
            Construct::while_block,
        ))(input)
    }

    /// Parse an interpreter directive. There are only a few of them, listed in
    /// the `JinkoInst` module
    ///
    /// `@<jinko_inst><args_list>`
    pub fn jinko_inst(input: &str) -> IResult<&str, JinkoInst> {
        let (input, _) = Token::at_sign(input)?;
        let (input, fc) = Construct::function_call(input)?;

        // FIXME: No unwrap(), use something else than just the name
        let inst = JinkoInst::from_str(fc.name()).unwrap();

        Ok((input, inst))
    }

    /// Parse a binary operation. A binary operation is composed of an expression, an
    /// operator and another expression
    ///
    /// `<expr> <op> <expr>`
    ///
    /// ```
    /// x + y; // Add x and y together
    /// a << 2; // Shift a by 2 bits
    /// a > 2; // Is a greater than 2?
    /// ```
    pub fn binary_op(input: &str) -> IResult<&str, Box<dyn Instruction>> {
        ShuntingYard::parse(input)
    }

    /// Parse a user-defined custom type
    ///
    /// `<type> <TypeName> ( <typed_arg_list> ) ;`
    pub fn custom_type(input: &str) -> IResult<&str, &str> {
        let (input, _) = Token::maybe_consume_extra(input)?;
        let (input, _) = Token::type_tok(input)?;
        let (input, _) = Token::maybe_consume_extra(input)?;

        let (input, type_name) = Token::identifier(input)?;

        let (input, _) = Token::maybe_consume_extra(input)?;

        let (input, fields) = Construct::args_dec_non_empty(input)?;

        // FIXME: Add Type creation and return it
        Ok((input, ""))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]

    fn t_constant_valid() {
        /*
        assert_eq!(Construct::constant("12").unwrap().1.kind(), ConstKind::Int);
        assert_eq!(
            Construct::constant("12.2").unwrap().1.kind(),
            ConstKind::Float
        );
        assert_eq!(
            Construct::constant("'a'").unwrap().1.kind(),
            ConstKind::Char
        );
        assert_eq!(
            Construct::constant("\"a\"").unwrap().1.kind(),
            ConstKind::Str
        );
        */
    }

    #[test]
    fn t_var_assign_valid() {
        assert_eq!(
            Construct::var_assignment("x = 12;").unwrap().1.mutable(),
            false
        );
        assert_eq!(
            Construct::var_assignment("x = 12;").unwrap().1.symbol(),
            "x"
        );

        assert_eq!(
            Construct::var_assignment("mut x_99 = 129;")
                .unwrap()
                .1
                .mutable(),
            true
        );
        assert_eq!(
            Construct::var_assignment("mut x_99 = 129;")
                .unwrap()
                .1
                .symbol(),
            "x_99"
        );

        assert_eq!(
            Construct::var_assignment("mut_x_99 = 129;")
                .unwrap()
                .1
                .mutable(),
            false
        );
        assert_eq!(
            Construct::var_assignment("mut_x_99 = 129;")
                .unwrap()
                .1
                .symbol(),
            "mut_x_99"
        );

        assert_eq!(
            Construct::var_assignment("mut mut_x_99 = 129;")
                .unwrap()
                .1
                .mutable(),
            true
        );
        assert_eq!(
            Construct::var_assignment("mut mut_x_99 = 129;")
                .unwrap()
                .1
                .symbol(),
            "mut_x_99"
        );

        assert_eq!(
            Construct::var_assignment("mut\nname = 129;")
                .unwrap()
                .1
                .mutable(),
            true
        );

        match Construct::var_assignment("mut x=12;") {
            Ok(_) => assert!(true),
            Err(_) => assert!(false, "Equal stuck to id is allowed"),
        }
        match Construct::var_assignment("mut x= 12;") {
            Ok(_) => assert!(true),
            Err(_) => assert!(false, "Equal stuck to id is allowed"),
        }
        match Construct::var_assignment("mut x =12;") {
            Ok(_) => assert!(true),
            Err(_) => assert!(false, "Equal stuck to value is allowed"),
        }
    }

    #[test]
    fn t_var_assign_invalid() {
        match Construct::var_assignment("mutable x = 12") {
            Ok(_) => assert!(false, "Mutable isn't mut"),
            Err(_) => assert!(true),
        }
        match Construct::var_assignment("mut x = 12") {
            Ok(_) => assert!(false, "No semicolon"),
            Err(_) => assert!(true),
        }
        match Construct::var_assignment("mut_x = 12") {
            Ok(_) => assert!(false, "No semicolon"),
            Err(_) => assert!(true),
        }
    }

    #[test]
    fn t_function_call_no_args_valid() {
        assert_eq!(Construct::function_call("fn()").unwrap().1.name(), "fn");
        assert_eq!(Construct::function_call("fn()").unwrap().1.args().len(), 0);

        assert_eq!(Construct::function_call("fn(    )").unwrap().1.name(), "fn");
        assert_eq!(
            Construct::function_call("fn(    )").unwrap().1.args().len(),
            0
        );
    }

    #[test]
    fn t_function_call_valid() {
        assert_eq!(Construct::function_call("fn(2)").unwrap().1.name(), "fn");
        assert_eq!(Construct::function_call("fn(2)").unwrap().1.args().len(), 1);

        assert_eq!(
            Construct::function_call("fn(1, 2, 3)").unwrap().1.name(),
            "fn"
        );
        assert_eq!(
            Construct::function_call("fn(a, hey(), 3.12)")
                .unwrap()
                .1
                .name(),
            "fn"
        );
        assert_eq!(
            Construct::function_call("fn(1, 2, 3)")
                .unwrap()
                .1
                .args()
                .len(),
            3
        );

        assert_eq!(
            Construct::function_call("fn(1   , 2,3)").unwrap().1.name(),
            "fn"
        );
        assert_eq!(
            Construct::function_call("fn(1   , 2,3)")
                .unwrap()
                .1
                .args()
                .len(),
            3
        );
    }

    #[test]
    fn t_function_call_invalid() {
        match Construct::function_call("fn(") {
            Ok(_) => assert!(false, "Unterminated parenthesis"),
            Err(_) => assert!(true),
        }
        match Construct::function_call("fn))") {
            Ok(_) => assert!(false, "Wrong parenthesis"),
            Err(_) => assert!(true),
        }
        match Construct::function_call("fn((") {
            Ok(_) => assert!(false, "Wrong parenthesis again"),
            Err(_) => assert!(true),
        }
        match Construct::function_call("fn((") {
            Ok(_) => assert!(false, "Wrong parenthesis again"),
            Err(_) => assert!(true),
        }
        match Construct::function_call("fn((") {
            Ok(_) => assert!(false, "Wrong parenthesis again"),
            Err(_) => assert!(true),
        }
    }

    #[test]
    fn t_function_call_multiarg_invalid() {
        match Construct::function_call("fn(1, 2, 3, 4,)") {
            Ok(_) => assert!(false, "Unterminated arglist"),
            Err(_) => assert!(true),
        }
        match Construct::function_call("fn(1, 2, 3, 4,   )") {
            Ok(_) => assert!(false, "Unterminated arglist"),
            Err(_) => assert!(true),
        }
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

        match Construct::block("{ 12a; 14a }").unwrap().1.last() {
            Some(_) => assert!(true),
            None => assert!(false, "Last expression here is valid")
        }
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
                .ty(),
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
                .ty(),
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
        match Construct::block("{ 12a;") {
            Ok(_) => assert!(false, "Unterminated bracket"),
            Err(_) => assert!(true),
        }

        match Construct::block("{ 12a") {
            Ok(_) => assert!(false, "Unterminated bracket but on expression"),
            Err(_) => assert!(true),
        }

        match Construct::block("{ 12a; 13a") {
            Ok(_) => assert!(false, "Unterminated bracket but on second expression"),
            Err(_) => assert!(true),
        }

        match Construct::block("12a; 13a }") {
            Ok(_) => assert!(false, "Not starting with a bracket"),
            Err(_) => assert!(true),
        }
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
            Ok(("", Some("int".to_owned())))
        );
        assert_eq!(
            Construct::return_type("   ->    int   {"),
            Ok(("{", Some("int".to_owned())))
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
    fn t_function_declaration_valid() {
        let func = Construct::function_declaration("func add(lhs: ty, rhs: ty) -> ty {}")
            .unwrap()
            .1;

        assert_eq!(func.name(), "add");
        assert_eq!(func.ty(), Some(&"ty".to_owned()));
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
        match Construct::test_declaration("test add(a: int) -> int {}") {
            Ok(_) => assert!(false, "Can't have arguments to a test"),
            Err(_) => assert!(true),
        };
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
        assert_eq!(test.ty(), Some(&"ty".to_owned()));
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
        match Construct::ext_declaration("ext func add(a: int) -> int {}") {
            Ok(_) => assert!(false, "Can't have a block for an ext function"),
            Err(_) => assert!(true),
        };
    }

    #[test]
    fn t_if_else_just_if() {
        let ie = Construct::if_else("if condition {}");

        match &ie {
            Ok(_) => assert!(true),
            Err(_) => assert!(false, "Valid to only have if"),
        };
    }

    #[test]
    fn t_if_else() {
        match Construct::if_else("if condition {} else {}") {
            Ok((input, _)) => assert_eq!(input, ""),
            Err(_) => assert!(false, "Valid to have empty blocks"),
        };
    }

    #[test]
    fn t_audit_simple() {
        match Construct::audit("audit {}") {
            Ok(_) => assert!(true),
            Err(_) => assert!(false, "Valid audit syntax"),
        }
    }

    #[test]
    fn t_loop_valid() {
        match Construct::loop_block("loop {}") {
            Ok((i, _)) => assert_eq!(i, ""),
            Err(_) => assert!(false, "Valid empty loop"),
        }
    }

    #[test]
    fn t_loop_invalid() {
        match Construct::loop_block("loo {}") {
            Ok(_) => assert!(false, "`loo` is not the keyword"),
            Err(_) => assert!(true),
        };

        match Construct::loop_block("loop") {
            Ok(_) => assert!(false, "A block is required"),
            Err(_) => assert!(true),
        };
    }

    #[test]
    fn t_while_valid() {
        match Construct::while_block("while x_99 {}") {
            Ok((i, _)) => assert_eq!(i, ""),
            Err(_) => assert!(false, "Valid empty while"),
        }
    }

    #[test]
    fn t_while_invalid() {
        match Construct::while_block("while {}") {
            Ok(_) => assert!(false, "Need a condition"),
            Err(_) => assert!(true),
        };

        match Construct::while_block("while") {
            Ok(_) => assert!(false, "A block is required"),
            Err(_) => assert!(true),
        };
    }

    #[test]
    fn t_for_valid() {
        match Construct::for_block("for x_99 in x_99 {}") {
            Ok((i, _)) => assert_eq!(i, ""),
            Err(_) => assert!(false, "Valid empty for"),
        }
    }

    #[test]
    fn t_for_invalid() {
        match Construct::for_block("for {}") {
            Ok(_) => assert!(false, "Need a variable and range"),
            Err(_) => assert!(true),
        };

        match Construct::for_block("for x99 in {}") {
            Ok(_) => assert!(false, "A range is required"),
            Err(_) => assert!(true),
        };

        match Construct::for_block("for x99 in { { { inner_block() } } }") {
            Ok(_) => assert!(false, "A range is required"),
            Err(_) => assert!(true),
        };
    }

    #[test]
    fn t_jinko_inst_valid() {
        assert_eq!(Construct::jinko_inst("@dump()"), Ok(("", JinkoInst::Dump)));
        assert_eq!(
            Construct::jinko_inst("@quit(something, something_else)"),
            Ok(("", JinkoInst::Quit))
        );
    }

    #[test]
    fn t_binary_op_valid() {
        match Construct::binary_op("a *   12 ") {
            Ok(_) => assert!(true),
            Err(_) => assert!(false, "Valid to have multi spaces"),
        };
        match Construct::binary_op("some() + 12.1") {
            Ok(_) => assert!(true),
            Err(_) => assert!(false, "Valid to have multiple expression types"),
        };
    }

    #[test]
    fn t_binary_op_invalid() {
        match Construct::binary_op("a ? 12") {
            Ok(_) => assert!(false, "? is not a binop"),
            Err(_) => assert!(true),
        };
    }

    #[test]
    fn t_custom_type_simple() {
        match Construct::custom_type("type Int(v: int);") {
            Ok(_) => assert!(true),
            Err(_) => assert!(false, "Just one int is valid"),
        };
        match Construct::custom_type("type Ints(a: int, b: int);") {
            Ok(_) => assert!(true),
            Err(_) => assert!(false, "Two integers is valid"),
        };
        match Construct::custom_type("type Compound(i: int, s: str);") {
            Ok(_) => assert!(true),
            Err(_) => assert!(false, "Different types are valid"),
        };
        match Construct::custom_type("type Custom(v: int, a: SomeType, b: Another, c: lower_case);")
        {
            Ok(_) => assert!(true),
            Err(_) => assert!(false, "Custom types in custom types are valid"),
        };
    }

    #[test]
    fn t_custom_type_empty() {
        match Construct::custom_type("type Empty();") {
            Ok(_) => assert!(false, "Can't have empty types"),
            Err(_) => assert!(true),
        }
    }

    #[test]
    fn t_custom_type_invalid() {
        match Construct::custom_type("type ExtraComma(a: int, b: int,);") {
            Ok(_) => assert!(false, "Extra comma in type definition"),
            Err(_) => assert!(true),
        }
    }
}
