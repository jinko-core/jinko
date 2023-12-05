//! Very simple Ast builder module, not suitable for general use - this is only for builtin functions

use crate::BuiltinType;

use ast::{Ast, Declaration, FunctionKind, Node, Type, TypeFields, TypeKind, TypedValue};
use location::SpanTuple;
use symbol::Symbol;

pub fn argument(name: &str, ty: Type) -> TypedValue {
    TypedValue {
        location: SpanTuple::builtin(),
        symbol: Symbol::from(name),
        ty,
    }
}

pub fn ty_arg(ty: BuiltinType) -> Type {
    Type {
        location: SpanTuple::builtin(),
        generics: vec![],
        kind: TypeKind::Simple(Symbol::from(ty.name())),
    }
}

pub fn ty(ty: BuiltinType) -> Ast {
    Ast {
        location: SpanTuple::builtin(),
        node: Node::Type {
            name: Symbol::from(ty.name()),
            generics: vec![],
            fields: TypeFields::None,
            with: None,
        },
    }
}

pub fn function(name: &str, args: Vec<TypedValue>, return_type: Option<Type>) -> Ast {
    Ast {
        location: SpanTuple::builtin(),
        node: Node::Function {
            kind: FunctionKind::Extern,
            decl: Declaration {
                name: Symbol::from(name),
                generics: vec![],
                args,
                return_type,
            },
            block: None,
        },
    }
}
