//! Abstract Syntax Tree representation of jinko's source code

use error::{ErrKind, Error};
use location::SpanTuple;
use symbol::Symbol;

#[derive(Debug, Clone)]
pub enum TypeKind {
    // TODO: Should we consider a simple type as a variant of multi type with only one type argument?
    // probably simpler, right?
    // but then how do we handle a simple type like "int" without generics or anything
    Simple(Symbol),
    Multi(Vec<TypeArgument>),
    FunctionLike(Vec<TypeArgument>, Option<Box<TypeArgument>>),
}

/// A type argument, i.e. when performing a specific generic call or specifying a variable's type
///
/// ```ignore
/// a: int = id<int>(15);
///
/// a: int = 15;
/// a: int | string = if true { "jinko" } else { 14 };
///
/// func f(arg: Vector<int>) {}
/// func g(arg: Tuple<int, float>) {}
/// func h(arg: Tuple<int, Tuple<Vector<float>, string>>) {}
/// func apply_fn(arg: int, fn: func(int) -> int) -> int { fn(arg) }
/// ```
#[derive(Debug, Clone)]
// TODO: Rename? `Type`?
pub struct TypeArgument {
    pub kind: TypeKind,
    // the generics and location are common fields between the multiple kinds, so they are lifted out of the `TypeKind` enum
    pub location: SpanTuple,
    pub generics: Vec<TypeArgument>,
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
    VarDeclaration {
        mutable: bool,
        to_declare: Symbol,
        value: Box<Ast>,
    },
    VarAssign {
        to_assign: Symbol,
        value: Box<Ast>,
    },
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
        let stmts = self.fold(stmts, Self::visit)?;

        Ok(Ast {
            location,
            node: Node::Block {
                stmts,
                last_is_expr,
            },
        })
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

    fn visit_function(
        &mut self,
        location: SpanTuple,
        kind: FunctionKind,
        decl: Declaration,
        block: Option<Box<Ast>>,
    ) -> Result<Ast, Error> {
        let block = self.optional(block, Self::boxed)?;

        Ok(Ast {
            location,
            node: Node::Function { kind, decl, block },
        })
    }

    fn visit_type(
        &mut self,
        location: SpanTuple,
        name: Symbol,
        generics: Vec<GenericArgument>,
        fields: Vec<TypedValue>,
        with: Option<Box<Ast>>,
    ) -> Result<Ast, Error> {
        let with = self.optional(with, Self::boxed)?;

        Ok(Ast {
            location,
            node: Node::Type {
                name,
                generics,
                fields,
                with,
            },
        })
    }

    fn visit_type_instantiation(
        &mut self,
        location: SpanTuple,
        Call { to, generics, args }: Call,
    ) -> Result<Ast, Error> {
        let args = self.fold(args, Self::visit)?;

        Ok(Ast {
            location,
            node: Node::TypeInstantiation(Call { to, generics, args }),
        })
    }

    fn visit_function_call(
        &mut self,
        location: SpanTuple,
        Call { to, generics, args }: Call,
    ) -> Result<Ast, Error> {
        let args = self.fold(args, Self::visit)?;

        Ok(Ast {
            location,
            node: Node::FunctionCall(Call { to, generics, args }),
        })
    }

    fn visit_method_call(
        &mut self,
        location: SpanTuple,
        instance: Box<Ast>,
        Call { to, generics, args }: Call,
    ) -> Result<Ast, Error> {
        // FIXME: Should this be an accumulating spot?
        let instance = self.boxed(instance)?;
        let args = self.fold(args, Self::visit)?;

        Ok(Ast {
            location,
            node: Node::MethodCall {
                instance,
                call: Call { to, generics, args },
            },
        })
    }

    fn visit_binary_op(
        &mut self,
        location: SpanTuple,
        op: Operator,
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    ) -> Result<Ast, Error> {
        let lhs = self.boxed(lhs)?;
        let rhs = self.boxed(rhs)?;

        Ok(Ast {
            location,
            node: Node::BinaryOp(op, lhs, rhs),
        })
    }

    fn visit_field_access(
        &mut self,
        location: SpanTuple,
        instance: Box<Ast>,
        field: Symbol,
    ) -> Result<Ast, Error> {
        let instance = self.boxed(instance)?;

        Ok(Ast {
            location,
            node: Node::FieldAccess(instance, field),
        })
    }

    fn visit_if_else(
        &mut self,
        location: SpanTuple,
        if_condition: Box<Ast>,
        if_block: Box<Ast>,
        else_block: Option<Box<Ast>>,
    ) -> Result<Ast, Error> {
        let if_condition = self.boxed(if_condition)?;
        let if_block = self.boxed(if_block)?;
        let else_block = self.optional(else_block, Self::boxed)?;

        Ok(Ast {
            location,
            node: Node::IfElse {
                if_condition,
                if_block,
                else_block,
            },
        })
    }

    fn visit_var_declaration(
        &mut self,
        location: SpanTuple,
        mutable: bool,
        to_declare: Symbol,
        value: Box<Ast>,
    ) -> Result<Ast, Error> {
        let value = self.boxed(value)?;

        Ok(Ast {
            location,
            node: Node::VarDeclaration {
                mutable,
                to_declare,
                value,
            },
        })
    }

    fn visit_var_assign(
        &mut self,
        location: SpanTuple,
        to_assign: Symbol,
        value: Box<Ast>,
    ) -> Result<Ast, Error> {
        let value = self.boxed(value)?;

        Ok(Ast {
            location,
            node: Node::VarAssign { to_assign, value },
        })
    }

    fn visit_var_or_empty_type(&mut self, location: SpanTuple, name: Symbol) -> Result<Ast, Error> {
        Ok(Ast {
            location,
            node: Node::VarOrEmptyType(name),
        })
    }

    fn visit_return(
        &mut self,
        location: SpanTuple,
        to_return: Option<Box<Ast>>,
    ) -> Result<Ast, Error> {
        let to_return = self.optional(to_return, Self::boxed)?;

        Ok(Ast {
            location,
            node: Node::Return(to_return),
        })
    }

    fn visit_constant(&mut self, location: SpanTuple, value: Value) -> Result<Ast, Error> {
        Ok(Ast {
            location,
            node: Node::Constant(value),
        })
    }

    fn visit_empty(&mut self, location: SpanTuple) -> Result<Ast, Error> {
        Ok(Ast {
            location,
            node: Node::Empty,
        })
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
                name,
                generics,
                fields,
                with,
            } => self.visit_type(ast.location, name, generics, fields, with),
            Node::TypeInstantiation(call) => self.visit_type_instantiation(ast.location, call),
            Node::FunctionCall(call) => self.visit_function_call(ast.location, call),
            Node::MethodCall { instance, call } => {
                self.visit_method_call(ast.location, instance, call)
            }
            Node::BinaryOp(op, lhs, rhs) => self.visit_binary_op(ast.location, op, lhs, rhs),
            Node::FieldAccess(instance, field) => {
                self.visit_field_access(ast.location, instance, field)
            }
            Node::IfElse {
                if_condition,
                if_block,
                else_block,
            } => self.visit_if_else(ast.location, if_condition, if_block, else_block),
            Node::VarDeclaration {
                mutable,
                to_declare,
                value,
            } => self.visit_var_declaration(ast.location, mutable, to_declare, value),
            Node::VarAssign { to_assign, value } => {
                self.visit_var_assign(ast.location, to_assign, value)
            }
            Node::VarOrEmptyType(name) => self.visit_var_or_empty_type(ast.location, name),
            Node::Loop(kind, block) => self.visit_loop(ast.location, kind, block),
            Node::Return(to_return) => self.visit_return(ast.location, to_return),
            Node::Constant(value) => self.visit_constant(ast.location, value),
            Node::Empty => self.visit_empty(ast.location),
        }
    }

    // Sorry... but this is a convenience method. We need to do a lot of Box-unwrapping
    // and Box-wrapping back and forth when visiting nodes. So to avoid always doing
    // `self.visit(*ast).map(Box::new)` all the time, which looks even worse when
    // the node to visit is an `Option<Box<Ast>>`, we have this convenience function.
    #[allow(clippy::boxed_local)]
    fn boxed(&mut self, ast: Box<Ast>) -> Result<Box<Ast>, Error> {
        self.visit(*ast).map(Box::new)
    }

    fn optional<T>(
        &mut self,
        opt: Option<T>,
        mapper: impl FnOnce(&mut Self, T) -> Result<T, Error>,
    ) -> Result<Option<T>, Error> {
        match opt {
            Some(v) => mapper(self, v).map(Some),
            None => Ok(None),
        }
    }

    fn fold<T>(
        &mut self,
        values: Vec<T>,
        mapper: impl Fn(&mut Self, T) -> Result<T, Error>,
    ) -> Result<Vec<T>, Error> {
        let (values, errs) = values.into_iter().map(|value| mapper(self, value)).fold(
            (Vec::new(), Vec::new()),
            |(acc, err_acc), value| match value {
                Ok(v) => (acc.with(v), err_acc),
                Err(e) => (acc, err_acc.with(e)),
            },
        );

        if errs.is_empty() {
            Ok(values)
        } else {
            Err(Error::new(ErrKind::Multiple(errs)))
        }
    }
}
