//! Abstract Syntax Tree representation of jinko's source code

use error::{ErrKind, Error};
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
pub struct GenericArgument {
    pub name: Symbol,
    pub default: Option<Symbol>,
    // FIXME: This should be an Option<TypeArgument>
    // FIXME: Missing location member
}

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, Clone)]
pub struct Declaration {
    pub name: Symbol,
    pub generics: Vec<GenericArgument>,
    pub args: Vec<TypedValue>,
    pub return_type: Option<TypeArgument>,
}

#[derive(Debug, Clone)]
pub enum FunctionKind {
    Func,
    Test,
    Mock,
    Extern,
}

/// Common parts of a "call", to a function, method, or a type. This does not differentiate between
/// a function call or type instantiation and does not reflect the differences in syntax.
#[derive(Debug, Default, Clone)]
pub struct Call {
    pub to: Symbol,
    pub generics: Vec<TypeArgument>,
    pub args: Vec<Ast>,
}

#[derive(Debug, Clone)]
pub enum LoopKind {
    Infinite,
    While(Box<Ast>),
    For { iterator: Symbol, range: Box<Ast> },
}

#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
pub struct Ast {
    pub location: SpanTuple,
    pub node: Node,
}

#[doc(hidden)]
trait VecExt<T> {
    fn with(self, elt: T) -> Self;
}

impl<T> VecExt<T> for Vec<T> {
    fn with(mut self, elt: T) -> Vec<T> {
        self.push(elt);

        self
    }
}

// FIXME: Add documentation
pub trait Visitor {
    fn visit_incl(
        &mut self,
        location: SpanTuple,
        source: Symbol,
        as_path: Option<Symbol>,
    ) -> Result<Ast, Error> {
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
    ) -> Result<Ast, Error> {
        let (stmts, errs) = stmts.into_iter().map(|stmt| self.visit(stmt)).fold(
            (Vec::new(), Vec::new()),
            |(acc, err_acc), stmt| match stmt {
                Ok(stmt) => (acc.with(stmt), err_acc),
                Err(e) => (acc, err_acc.with(e)),
            },
        );

        if !errs.is_empty() {
            Err(Error::new(ErrKind::Multiple(errs)))
        } else {
            Ok(Ast {
                location,
                node: Node::Block {
                    stmts,
                    last_is_expr,
                },
            })
        }
    }

    // This default visitor does not need `block` to be boxed, but further specializations
    // may require it
    #[allow(clippy::boxed_local)]
    fn visit_loop(
        &mut self,
        location: SpanTuple,
        kind: LoopKind,
        block: Box<Ast>,
    ) -> Result<Ast, Error> {
        let block = self.visit(*block)?;

        Ok(Ast {
            location,
            node: Node::Loop(kind, Box::new(block)),
        })
    }

    #[allow(clippy::boxed_local)]
    fn visit_function(
        &mut self,
        location: SpanTuple,
        kind: FunctionKind,
        decl: Declaration,
        block: Option<Box<Ast>>,
    ) -> Result<Ast, Error> {
        let block = self.visit_optional(block)?;

        Ok(Ast {
            location,
            node: Node::Function { kind, decl, block },
        })
    }

    fn visit_optional(&mut self, ast: Option<Box<Ast>>) -> Result<Option<Box<Ast>>, Error> {
        match ast {
            Some(ast) => self.visit(*ast).map(|ast| Some(Box::new(ast))),
            None => Ok(None),
        }
    }

    fn visit(&mut self, ast: Ast) -> Result<Ast, Error> {
        match ast.node {
            Node::Block {
                stmts,
                last_is_expr,
            } => self.visit_block(ast.location, stmts, last_is_expr),
            Node::Incl { source, as_path } => self.visit_incl(ast.location, source, as_path),
            Node::Function { kind, decl, block } => {
                self.visit_function(ast.location, kind, decl, block)
            }
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
            Node::Loop(kind, block) => self.visit_loop(ast.location, kind, block),
            Node::Return(_) => todo!(),
            Node::Constant(_) => todo!(),
            Node::Empty => Ok(ast),
        }
    }
}
