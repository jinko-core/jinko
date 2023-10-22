//! Very simple Ast builder module, not suitable for general use - this is only for builtin functions

use crate::BuiltinType;

use ast::{Ast, Declaration, FunctionKind, Node, TypeArgument, TypeKind, TypedValue};
use location::SpanTuple;
use symbol::Symbol;

pub fn argument(name: &str, ty: TypeArgument) -> TypedValue {
    TypedValue {
        location: SpanTuple::builtin(),
        symbol: Symbol::from(name),
        ty,
    }
}

pub fn ty_arg(ty: BuiltinType) -> TypeArgument {
    TypeArgument {
        location: SpanTuple::builtin(),
        generics: vec![],
        kind: TypeKind::Ty(Symbol::from(ty.name())),
    }
}

pub fn ty(ty: BuiltinType) -> Ast {
    Ast {
        location: SpanTuple::builtin(),
        node: Node::Type {
            name: Symbol::from(ty.name()),
            generics: vec![],
            fields: vec![],
            with: None,
        },
    }
}

pub fn function(name: &str, args: Vec<TypedValue>, return_type: Option<TypeArgument>) -> Ast {
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
