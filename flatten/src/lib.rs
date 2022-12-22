use ast::{Ast, Node as AstNode};
use fir::Fir;
use symbol::Symbol;

pub trait FlattenAst: Sized {
    fn flatten(self) -> Fir<Symbol>;
}

impl FlattenAst for ast::Ast {
    fn flatten(self) -> Fir<Symbol> {
        let Ast { location, node } = self;

        match node {
            AstNode::Block(_) => todo!(),
            AstNode::Incl { source, as_path } => todo!(),
            AstNode::Function { kind, decl, block } => todo!(),
            AstNode::Type {
                name,
                generics,
                fields,
                with,
            } => todo!(),
            AstNode::TypeInstantiation(_) => todo!(),
            AstNode::FunctionCall(_) => todo!(),
            AstNode::MethodCall { instance, call } => todo!(),
            AstNode::BinaryOp(_, _, _) => todo!(),
            AstNode::FieldAccess(_, _) => todo!(),
            AstNode::IfElse {
                if_condition,
                if_block,
                else_block,
            } => todo!(),
            AstNode::VarAssign {
                mutable,
                to_assign,
                value,
            } => todo!(),
            AstNode::Var(_) => todo!(),
            AstNode::VarOrEmptyType(_) => todo!(),
            AstNode::Loop(_, _) => todo!(),
            AstNode::Return(_) => todo!(),
            AstNode::Constant(_) => todo!(),
        }
    }
}
