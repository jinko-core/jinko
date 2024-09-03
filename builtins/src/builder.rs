//! Very simple Ast builder module, not suitable for general use - this is only for builtin functions

use crate::BuiltinType;

use ast::{Ast, Declaration, FunctionKind, Node, Type, TypeContent, TypeKind, TypedValue};
use location::SpanTuple;
use symbol::Symbol;

pub fn argument(name: &str, ty: Type) -> TypedValue {
    TypedValue {
        location: SpanTuple::builtin(),
        symbol: Symbol::from(name),
        ty,
    }
}

pub fn type_symbol(symbol: &str) -> Type {
    Type {
        kind: TypeKind::Simple(Symbol::from(symbol)),
        location: SpanTuple::builtin(),
        generics: vec![],
    }
}

pub fn builtin_type_symbol(ty: BuiltinType) -> Type {
    Type {
        kind: TypeKind::Simple(Symbol::from(ty.name())),
        location: SpanTuple::builtin(),
        generics: vec![],
    }
}

fn union_content(types: Vec<Type>) -> TypeContent {
    TypeContent::Alias(Type {
        kind: TypeKind::Multi(types),
        location: SpanTuple::builtin(),
        generics: vec![],
    })
}

pub fn union_type(ty: BuiltinType, variants: Vec<Type>) -> Ast {
    Ast {
        location: SpanTuple::builtin(),
        node: Node::Type {
            name: Symbol::from(ty.name()),
            fields: union_content(variants),
            generics: vec![],
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
