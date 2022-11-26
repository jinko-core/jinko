//! Abstract Syntax Tree representation of jinko's source code

use symbol::Symbol;

/// A type argument, i.e. when performing a specific generic call or specifying a variable's type
///
/// a: int = id<int>(15);
///
/// a: int = 15;
///
/// func f(arg: Vector<int>) {}
/// func g(arg: Tuple<int, float>) {}
/// func h(arg: Tuple<int, Tuple<Vector<float>, string>>) {}
pub struct TypeArgument {
    pub name: Symbol,
    pub generics: Vec<TypeArgument>,
}

pub struct TypedValue(Symbol, TypeArgument);

pub struct GenericArgument {
    pub name: Symbol,
    pub default: Option<Symbol>,
}

pub enum Operator {}
pub enum FunctionKind {}

/// Common parts of a "call", to a function, method, or a type. This does not differentiate between
/// a function call or type instantiation and does not reflect the differences in syntax.
pub struct Call {
    pub to: Symbol,
    pub generics: Vec<TypeArgument>,
    pub args: Vec<Ast>,
}

pub enum LoopKind {
    Infinite,
    While(Box<Ast>),
    For { iterator: Box<Ast>, range: Box<Ast> },
}

// FIXME: How to keep location in there? How to do it ergonomically?
// As a "Smart pointer" type? E.g by having it implement `Deref<T = AstInner>`?
// Would that even work? If it does, it is ergonomic but boy is it not idiomatic
pub enum Ast {
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
