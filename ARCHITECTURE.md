# `src/context`

A `Context` is a structure keeping track of data during code execution, such as declared types,
functions and variables. Its purpose is to run through code, typechecking and optimizing it as
necessary. You'll find the context's main implementation, which relies on the use of scope maps.
Scope maps allow the use of programming scopes and are a core feature of jinko.

# `src/parser`

The parser takes care of giving sense to the random characters written in a jinko file. It produces
valid, interpretable rust structures according to various rules defined in [the syntax](SYNTAX.md).
The parser directory is split in 4 important files:

- `tokens.rs`: Tokens are keywords, identifiers and various tokens that the interpreter
should recognize. A method in `Token` should, ultimately, return a string if it matches certain
rules. For example, `Token::func_tok` will return the string "func" if it is directly present in
the given input. `Token::identifer` will return an identifier if one is found, matching different
set of rules such as the necessity for a character, the possibility of using underscores, or even
namespace separators. Tokens do NOT return complex data structures. They operate on a string input,
and return part of that input.

- `constructs.rs`: A Construct is a high level, semantically important block. Using some
tokens (that you could consider as low-level building bricks), a Construct will transform a string
input into a valid, executable instruction. For example, `Construct::func_call` expects an
identifier and a set of parentheses, plus a possible list of arguments. Therefore, this function
makes use of `Token::left_parenthesis`, `Token::right_parenthesis`, `Token::identifier`...

- `shunting_yard.rs`: The goal of Shunting Yard is to parse valid mathematical expressions
and return them as interpretable code. The implementation is pretty standard, first creating a RPN
of the input before translating this into a single binary operation.

- `box_construct.rs`: This file provides useful functions used to wrap the result of
`Construct` functions into boxes. Thus, a `FunctionCall` or a `VarAssign` will get abstracted
further to a `Box<dyn Instruction>`. This is useful for "entry" functions such as
`Construct::instruction`.

# `src/repl`

The REPL module takes care of evaluating input in a command line manner, providing an interface to
the interpreter. REPL stands for `Read Eval Print Loop`.

# `src/instance`

An instance represents a jinko value in rust code. They are strongly typed, and represented in
memory. They can be converted and created from primitive types, and are created manually when a
custom type is instanciated. An instance is comprised of a vector of bytes, which holds the actual
data of the jinko object. It is then converted back and forth if necessary.

# `src/value`

Values are used to represent types that can undergo arithmetic operations, such as floats and
integers. This module also houses the definitions of the constants, and their conversion to
instances.

# `src/instruction`

The core of the interpreter. An Instruction corresponds to a jinko construct that can be executed,
typechecked and optimized. Each part of the syntax corresponds to a specific `Instruction`, so you
have a `FunctionCall`, a `TypeDeclaration`, a `TypeInstantiation`... Most of the work is done here
when it comes to code execution.

# `src/typechecker.rs`

The typechecker is responsible for verifying that jinko code conforms to using proper, static,
valid types. The main part of this module is the `TypeChecker` trait, which is implemented on all
executable types.

# `src/error`

Houses the `jinko::Error` type, which is used accross the project to propagate errors and display
them.

# `src/utils`

Small wrappers around Rust collections in order to create `Stacks` and `Queues`
