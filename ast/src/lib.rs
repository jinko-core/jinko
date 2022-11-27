//! Abstract Syntax Tree representation of jinko's source code

use location::Location;
use symbol::Symbol;

/// A type argument, i.e. when performing a specific generic call or specifying a variable's type
///
/// ```ignore
/// a: int = id<int>(15);
///
/// a: int = 15;
///
/// func f(arg: Vector<int>) {}
/// func g(arg: Tuple<int, float>) {}
/// func h(arg: Tuple<int, Tuple<Vector<float>, string>>) {}
/// ```
#[derive(Debug)]
pub struct TypeArgument {
    pub name: Symbol,
    pub generics: Vec<TypeArgument>,
}

/// A value with its associated type. This is used for function arguments or type fields
#[derive(Debug)]
pub struct TypedValue(Symbol, TypeArgument);

/// A generic argument declaration
/// ```ignore
/// //      v
/// func id[T](value: T) -> T { value }
/// //      ^
///
/// //      vvvvvvv
/// func id[T = int](value: T) -> T { value }
/// //      ^^^^^^^
/// ```
pub struct GenericArgument {
    pub name: Symbol,
    pub default: Option<Symbol>,
}

#[derive(Debug)]
pub enum Operator {}

#[derive(Debug)]
pub enum FunctionKind {}

/// Common parts of a "call", to a function, method, or a type. This does not differentiate between
/// a function call or type instantiation and does not reflect the differences in syntax.
#[derive(Debug)]
pub struct Call {
    pub to: Symbol,
    pub generics: Vec<TypeArgument>,
    pub args: Vec<Ast>,
}

#[derive(Debug)]
pub enum LoopKind {
    Infinite,
    While(Box<Ast>),
    For { iterator: Box<Ast>, range: Box<Ast> },
}

// FIXME: How to keep location in there? How to do it ergonomically?
// As a "Smart pointer" type? E.g by having it implement `Deref<T = AstInner>`?
// Would that even work? If it does, it is ergonomic but boy is it not idiomatic
#[derive(Debug)]
pub enum AstNode {
    Block(Vec<Ast>),
    Incl {
        source: Symbol,
        as_path: Option<Symbol>,
    },
    Function {
        kind: FunctionKind,
        name: Symbol,
        generics: Vec<Symbol>,
        arguments: Vec<TypedValue>,
        block: Box<Ast>,
    },
    Type {
        name: Symbol,
        fields: Vec<TypedValue>,
        with: Option<Box<Ast>>,
    },
    TypeInstantiation(Call),
    FunctionCall(Call),
    MethodCall {
        instance: Box<Ast>,
        call: Call,
    },
    BinaryOp(Operator, Box<Ast>, Box<Ast>),
    FieldAccess(Box<Ast>, Symbol),
    IfElse {
        if_condition: Box<Ast>,
        if_block: Box<Ast>,
        else_block: Option<Box<Ast>>,
    },
    VarAssign {
        mutable: bool,
        to_assign: Box<Ast>,
        value: Box<Ast>,
    },
    Var(Symbol),
    VarOrEmptyType(Symbol),
    Loop(LoopKind, Box<Ast>),
    Return(Box<Ast>),
}

/// The [`Ast`] structure is a wrapper around the [`AstNode`] sum type, which contains
/// extra information such as the node's [`Location`]
#[derive(Debug)]
pub struct Ast {
    pub location: Location,
    pub node: AstNode,
}
