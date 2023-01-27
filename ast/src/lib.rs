//! Abstract Syntax Tree representation of jinko's source code

use location::SpanTuple;
use symbol::Symbol;

#[derive(Debug, Clone)]
pub enum TypeKind {
    Ty(Symbol),
    FunctionLike(Vec<TypeArgument>, Option<Box<TypeArgument>>),
}

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
/// func apply_fn(arg: int, fn: func(int) -> int) -> int { fn(arg) }
/// ```
#[derive(Debug, Clone)]
pub struct TypeArgument {
    pub generics: Vec<TypeArgument>,
    pub kind: TypeKind,
    // FIXME: Missing location member
}

/// A value with its associated type. This is used for function arguments or type fields
// FIXME: This needs a location right?
#[derive(Debug)]
pub struct TypedValue {
    pub location: SpanTuple,
    pub symbol: Symbol,
    pub ty: TypeArgument,
    // FIXME: Missing location member
}

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
#[derive(Debug)]
pub struct GenericArgument {
    pub name: Symbol,
    pub default: Option<Symbol>,
    // FIXME: This should be an Option<TypeArgument>
    // FIXME: Missing location member
}

#[derive(Debug, PartialEq, Eq)]
pub enum Operator {
    // FIXME: Should this have location as well?
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Gt,
    LtEq,
    GtEq,
    Equals,
    NotEquals,
    LeftParenthesis,
    RightParenthesis,
}

#[derive(Debug)]
pub struct Declaration {
    pub name: Symbol,
    pub generics: Vec<GenericArgument>,
    pub args: Vec<TypedValue>,
    pub return_type: Option<TypeArgument>,
}

#[derive(Debug)]
pub enum FunctionKind {
    Func,
    Test,
    Mock,
    Extern,
}

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

#[derive(Debug)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Char(char),
    Bool(bool),
    Str(String),
}

// FIXME: How to keep location in there? How to do it ergonomically?
// As a "Smart pointer" type? E.g by having it implement `Deref<T = AstInner>`?
// Would that even work? If it does, it is ergonomic but boy is it not idiomatic
#[derive(Debug)]
pub enum Node {
    Block {
        stmts: Vec<Ast>,
        last_is_expr: bool,
    },
    Incl {
        source: Symbol,
        as_path: Option<Symbol>,
    },
    Function {
        kind: FunctionKind,
        decl: Declaration,
        block: Option<Box<Ast>>,
    },
    Type {
        name: Symbol,
        generics: Vec<GenericArgument>,
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
        to_assign: Symbol,
        value: Box<Ast>,
    },
    Var(Symbol),
    VarOrEmptyType(Symbol),
    Loop(LoopKind, Box<Ast>),
    Return(Option<Box<Ast>>),
    Constant(Value),
    Empty,
}

/// The [`Ast`] structure is a wrapper around the [`AstNode`] sum type, which contains
/// extra information such as the node's [`Location`]
#[derive(Debug)]
pub struct Ast {
    pub location: SpanTuple,
    pub node: Node,
}

// FIXME: Add documentation
pub trait Visitor<E> {
    fn visit_incl(
        &mut self,
        location: SpanTuple,
        source: Symbol,
        as_path: Option<Symbol>,
    ) -> Result<Ast, E> {
        // This is a terminal visitor
        Ok(Ast {
            location,
            node: Node::Incl { source, as_path },
        })
    }

    fn visit_block(
        &mut self,
        location: SpanTuple,
        stmts: Vec<Ast>,
        last_is_expr: bool,
    ) -> Result<Ast, E> {
        let stmts = stmts
            .into_iter()
            .map(|stmt| self.visit(stmt))
            .collect::<Result<Vec<Ast>, E>>()?;

        Ok(Ast {
            location,
            node: Node::Block {
                stmts,
                last_is_expr,
            },
        })
    }

    fn visit(&mut self, ast: Ast) -> Result<Ast, E> {
        match ast.node {
            Node::Block {
                stmts,
                last_is_expr,
            } => self.visit_block(ast.location, stmts, last_is_expr),
            Node::Incl { source, as_path } => self.visit_incl(ast.location, source, as_path),
            Node::Function {
                kind: _,
                decl: _,
                block: _,
            } => todo!(),
            Node::Type {
                name: _,
                generics: _,
                fields: _,
                with: _,
            } => todo!(),
            Node::TypeInstantiation(_) => todo!(),
            Node::FunctionCall(_) => todo!(),
            Node::MethodCall {
                instance: _,
                call: _,
            } => todo!(),
            Node::BinaryOp(_, _, _) => todo!(),
            Node::FieldAccess(_, _) => todo!(),
            Node::IfElse {
                if_condition: _,
                if_block: _,
                else_block: _,
            } => todo!(),
            Node::VarAssign {
                mutable: _,
                to_assign: _,
                value: _,
            } => todo!(),
            Node::Var(_) => todo!(),
            Node::VarOrEmptyType(_) => todo!(),
            Node::Loop(_, _) => todo!(),
            Node::Return(_) => todo!(),
            Node::Constant(_) => todo!(),
            Node::Empty => Ok(ast),
        }
    }
}
