//! The goal of the flatten crate is to produce a flat representation of a given syntax tree.
//! This means going from an [`Ast`] to an [`Fir`]. The goal of this crate is not to name-resolve:
//! Instead, this crate will produce an [`Fir`] containing enough information to perform name
//! resolution strictly on an instance of [`Fir`].
//!
//! Let's take the following, invalid jinko program:
//! ```text
//! func f() -> int {
//!     return 15;
//! }
//!
//! f();
//! f(15);
//! ```
//!
//! This is the corresponding simplified [`Ast`] representation:
//!
//! ```text
//! Ast {
//!     Block(
//!         [
//!             Function {
//!                 kind: Func,
//!                 decl: Declaration {
//!                     name: "f",
//!                     generics: [],
//!                     args: [],
//!                     return_type: Some(
//!                         TypeArgument {
//!                             generics: [],
//!                             kind: "int",
//!                         },
//!                     ),
//!                 },
//!                 block: Some(
//!                     Block( [ Return(Constant(Integer(15)) ] ),
//!                 ),
//!             },
//!             FunctionCall(
//!                 to: "f",
//!                 generics: [],
//!                 args: [],
//!             ),
//!             FunctionCall(
//!                 to: "f",
//!                 generics: [],
//!                 args: [ Constant(Integer(15)) ],
//!             ),
//!         ],
//!     ),
//! }
//! ```
//!
//! We want to turn this into an instance of [`Fir`], so a *flat* representation of the above
//! [`Ast`]. Let's go through it step by step:
//!
//! First, we visit a Block. We go through all of its statements (visit them) and then we will add the
//! block to the [`Fir`]. We do this since a [`Kind::Statements`] only contains *references* to the statements
//! it contains, meaning the statements have to be defined within the [`Fir`] before we define the block.
//!
//! Programmatically, this means something along the lines of `for stmt in block { fir = visit(fir, stmt) }; fir.append(block.to_node())`
//!
//! The first statement of that block is a function declaration. We have to
//! visit all of its generics, append them to the [`Fir`]. then visit all of the
//! arguments, append them to the [`Fir`], then the return type, and finally visit
//! the functions's block. Let's go through that. Note that at this point, our [`Fir`] is still empty!
//!
//! Our function has no generics, and no arguments. This is easy. However, it has a return type. This return type must be a reference to an
//! existing type (which we'll resolve in a later pass - here, we are not interested in whether or not this type is valid). Let's create that reference add it to the [`Fir`].
//!
//! ```text
//! Fir [
//!     (Origin::0, TypeUsage(RefIdx::Unresolved),
//! ]
//! ```
//!
//! If we want to create our function in the [`Fir`], we need references to the
//! arguments (there's zero of them), generics (likewise), return type (we have
//! that one, it's `0`), and block, which is not currently present.
//!
//! Let's visit it. Similarly to the original block, it will only contain
//! references to its statements. Let's add each of them. The first one is a return statement, which, similarly,
//! can only store a reference to an existing value, which we visit.
//!
//! We're now on a "leaf node": the constant value `15`. This node does not contain any references and simply... exists.
//! We create it and append it to the [`Fir`].
//!
//! ```text
//! Fir [
//!     (Origin::0, TypeUsage(RefIdx::Unresolved)),
//!     (Origin::1, Constant(RefIdx::Unresolved)), // we don't know the type of `15` yet
//! ]
//! ```
//!
//! We can now create the return statement.
//!
//! ```text
//! Fir [
//!     (Origin::0, TypeUsage(RefIdx::Unresolved)),
//!     (Origin::1, Constant(RefIdx::Unresolved)), // we don't know the type of `15` yet
//!     (Origin::2, Return(RefIdx::Ref(1))), // reference to Origin::1
//! ]
//! ```
//!
//! Which lets us create the block
//!
//! ```text
//! Fir [
//!     (Origin::0, TypeUsage(RefIdx::Unresolved)),
//!     (Origin::1, Constant(RefIdx::Unresolved)), // we don't know the type of `15` yet
//!     (Origin::2, Return(RefIdx::Ref(1))), // reference to Origin::1
//!     (Origin::3, Block { stmts: [RefIdx::Ref(2)] }), // only one statement, the `return` one
//! ]
//! ```
//!
//! Which means we now have enough to create our function and add it to the [`Fir`].
//!
//! ```text
//! Fir [
//!     (Origin::0, TypeUsage(RefIdx::Unresolved)),
//!     (Origin::1, Constant(RefIdx::Unresolved)), // we don't know the type of `15` yet
//!     (Origin::2, Return(RefIdx::Ref(1))), // reference to Origin::1
//!     (Origin::3, Block { stmts: [ RefIdx::Ref(2) ] }), // only one statement, the `return` one
//!     (Origin::4, Function {
//!             args: [],
//!             generics: [],
//!             return_ty: Some(RefIdx::Ref(0)),
//!             block: Some(RefIdx::Ref(3)),
//!         }
//!     ),
//! ]
//! ```
//!
//! If we keep going and add the missing calls, we get something like the following:
//!
//! ```text
//! Fir [
//!     (Origin::0, TypeUsage(RefIdx::Unresolved)),
//!     (Origin::1, Constant(RefIdx::Unresolved)), // we don't know the type of `15` yet
//!     (Origin::2, Return(RefIdx::Ref(1))), // reference to Origin::1
//!     (Origin::3, Block { stmts: [RefIdx::Ref(2)] }), // only one statement, the `return` one
//!     (Origin::4, Function {
//!             args: [],
//!             generics: [],
//!             return_ty: Some(RefIdx::Ref(0)),
//!             block: Some(RefIdx::Ref(3)),
//!         }
//!     ),
//!     (Origin::5, Call(to: RefIdx::Unresolved, args: [])),
//!     (Origin::6, Constant(RefIdx::Unresolved)), // we don't know that both `15` nodes can be factored together yet
//!     (Origin::7, Call(to: RefIdx::Unresolved, args: [ RefIdx::Ref(6) ])),
//! ]
//! ```

use ast::{
    Ast, Call, Declaration, GenericArgument, LoopKind, Node as AstNode, TypeArgument, TypedValue,
};
use fir::{Fir, Kind, Node, OriginIdx, RefIdx};
use location::SpanTuple;
use symbol::Symbol;

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

trait OriginExt {
    /// Returns the new value
    fn increment(&mut self) -> Self;
}

impl OriginExt for OriginIdx {
    fn increment(&mut self) -> OriginIdx {
        self.0 += 1;

        *self
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct FlattenData {
    pub symbol: Option<Symbol>,
    pub location: Option<SpanTuple>, // FIXME: Remove the option
    pub scope: usize,
}

struct Ctx {
    pub fir: Fir<FlattenData>,
    pub origin: OriginIdx,
    pub scope: usize,
}

impl Ctx {
    fn append(mut self, data: FlattenData, kind: Kind) -> (Ctx, RefIdx) {
        let next = self.origin.increment();
        let node = Node {
            data,
            origin: next,
            kind,
        };

        (
            Ctx {
                fir: self.fir.append(node),
                ..self
            },
            RefIdx::Resolved(next),
        )
    }

    fn handle_generic_node(self, generic: &GenericArgument) -> (Ctx, RefIdx) {
        // FIXME: Needs AST to be fixed
        // let (ctx, default_ty) = self.handle_ty_node(generic.ty);

        let data = FlattenData {
            symbol: Some(generic.name.clone()),
            location: None, // FIXME
            scope: self.scope,
        };
        let kind = Kind::Generic { default: None }; // FIXME: Invalid

        self.append(data, kind)
    }

    fn handle_ty_node(self, ty: &TypeArgument) -> (Ctx, RefIdx) {
        let (ctx, generics) = self.visit_fold(ty.generics.iter(), Ctx::handle_ty_node);

        let data = FlattenData {
            symbol: match &ty.kind {
                ast::TypeKind::Ty(s) => Some(s.clone()),
                ast::TypeKind::FunctionLike(_, _) => Some(Symbol::from("func")), // FIXME: Invalid but w/ever for now
            },
            location: None, // FIXME: Invalid
            scope: ctx.scope,
        };
        let kind = Kind::Type {
            generics,
            fields: vec![],
        };

        ctx.append(data, kind)
    }

    fn scoped(mut self, f: impl Fn(Ctx) -> (Ctx, RefIdx)) -> (Ctx, RefIdx) {
        self.scope += 1;

        let (mut ctx, idx) = f(self);

        ctx.scope -= 1;

        (ctx, idx)
    }

    fn visit_binary_op(
        self,
        location: &SpanTuple,
        op: &ast::Operator,
        lhs: &Ast,
        rhs: &Ast,
    ) -> (Ctx, RefIdx) {
        // This desugars `l + r` to `+(l, r)`.
        // Is it valid? We assume that later on during type resolution
        // and actual code generation/interpretation, a builtin method
        // will be added to the interpreter:
        //
        // ```
        // func +[L, R, O](lhs: L, rhs: R) -> O { /* builtin */ }
        // ````
        // FIXME: Is that valid?

        let (ctx, l_id) = self.visit(lhs);
        let (ctx, r_id) = ctx.visit(rhs);

        let data = FlattenData {
            symbol: Some(Symbol::from(match op {
                ast::Operator::Add => "+",
                ast::Operator::Sub => "-",
                ast::Operator::Mul => "*",
                ast::Operator::Div => "/",
                ast::Operator::Lt => "<",
                ast::Operator::Gt => ">",
                ast::Operator::LtEq => "<=",
                ast::Operator::GtEq => ">=",
                ast::Operator::Equals => "==",
                ast::Operator::NotEquals => "!=",
            })),
            location: Some(location.clone()),
            // FIXME: Is that the correct scope? Add unit test
            scope: ctx.scope,
        };

        // FIXME: Should this be a call? Is this a good idea?
        let kind = Kind::Call {
            to: RefIdx::Unresolved,
            generics: vec![],
            args: vec![l_id, r_id],
        };

        ctx.append(data, kind)
    }

    fn visit_block(self, location: &SpanTuple, stmts: &[Ast], last_is_expr: bool) -> (Ctx, RefIdx) {
        self.scoped(|ctx| {
            let (ctx, refs) = if let Some((maybe_return, nodes)) = stmts.split_last() {
                let (ctx, refs) = ctx.visit_fold(nodes.iter(), Ctx::visit);
                let (ctx, idx) = ctx.visit(maybe_return);

                // If the block contains a last expression, transform it into a return
                let (ctx, last_idx) = if last_is_expr {
                    let data = FlattenData {
                        symbol: None,
                        location: Some(maybe_return.location.clone()),
                        scope: ctx.scope,
                    };
                    let kind = Kind::Return(Some(idx));

                    ctx.append(data, kind)
                } else {
                    (ctx, idx)
                };

                (ctx, refs.with(last_idx))
            } else {
                (ctx, vec![])
            };

            let data = FlattenData {
                symbol: None,
                location: Some(location.clone()),
                scope: ctx.scope,
            };
            let kind = Kind::Statements(refs);

            ctx.append(data, kind)
        })
    }

    fn visit_field_access(
        self,
        location: &SpanTuple,
        instance: &Ast,
        field_name: &Symbol,
    ) -> (Ctx, RefIdx) {
        let (ctx, instance) = self.visit(instance);

        let data = FlattenData {
            // FIXME: Isn't this actually more part of typechecking than name resolution?
            symbol: Some(field_name.clone()),
            location: Some(location.clone()),
            scope: ctx.scope,
        };

        let kind = Kind::TypeOffset {
            instance,
            field: RefIdx::Unresolved,
        };

        ctx.append(data, kind)
    }

    fn visit_function_call(
        self,
        location: &SpanTuple,
        Call { to, generics, args }: &Call,
    ) -> (Ctx, RefIdx) {
        let (ctx, generics) = self.visit_fold(generics.iter(), Ctx::handle_ty_node);
        let (ctx, args) = ctx.visit_fold(args.iter(), Ctx::visit);

        let data = FlattenData {
            symbol: Some(to.clone()),
            location: Some(location.clone()),
            scope: ctx.scope,
        };
        let kind = Kind::Call {
            to: RefIdx::Unresolved,
            generics,
            args,
        };

        ctx.append(data, kind)
    }

    fn visit_fold<T>(
        self,
        iter: impl Iterator<Item = T>,
        visitor: impl Fn(Ctx, T) -> (Ctx, RefIdx),
    ) -> (Ctx, Vec<RefIdx>) {
        iter.fold((self, vec![]), |(ctx, refs), node| {
            let (ctx, new_ref) = visitor(ctx, node);

            (ctx, refs.with(new_ref))
        })
    }

    fn visit_function(
        self,
        location: &SpanTuple,
        Declaration {
            name,
            generics,
            args,
            return_type,
        }: &Declaration,
        block: &Option<Box<Ast>>,
    ) -> (Ctx, RefIdx) {
        self.scoped(|ctx| {
            let (ctx, generics) = ctx.visit_fold(generics.iter(), Ctx::handle_generic_node);
            let (ctx, args) = ctx.visit_fold(args.iter(), Ctx::visit_typed_value);
            let (ctx, return_type) = ctx.visit_opt(return_type.as_ref(), Ctx::handle_ty_node);
            let (ctx, block) = ctx.visit_opt(block.as_deref(), Ctx::visit);

            let data = FlattenData {
                symbol: Some(name.clone()),
                location: Some(location.clone()),
                scope: ctx.scope,
            };
            let kind = Kind::Function {
                generics,
                args,
                return_type,
                block,
            };

            ctx.append(data, kind)
        })
    }

    fn visit_if_else(
        self,
        location: &SpanTuple,
        condition: &Ast,
        if_block: &Ast,
        else_block: &Option<Box<Ast>>,
    ) -> (Ctx, RefIdx) {
        let (ctx, condition) = self.visit(condition);
        let (ctx, true_block) = ctx.visit(if_block);
        let (ctx, false_block) = ctx.visit_opt(else_block.as_deref(), Ctx::visit);

        let data = FlattenData {
            symbol: None,
            location: Some(location.clone()),
            scope: ctx.scope,
        };

        let kind = Kind::Conditional {
            condition,
            true_block,
            false_block,
        };

        ctx.append(data, kind)
    }

    fn visit_loop(self, location: &SpanTuple, kind: &LoopKind, block: &Ast) -> (Ctx, RefIdx) {
        let (ctx, condition) = match kind {
            // At this point, only `while` loops remain: `for` loops and `loop` loops will
            // have been desugared by an earlier pass on the AST itself.
            LoopKind::While(condition) => self.visit(condition),
            // FIXME: Should this get desugared earlier?
            // would make things easier...
            LoopKind::Infinite | LoopKind::For { .. } => {
                unreachable!(
                    "AST node `Loop` has not been desugared properly before flattening: {:?}",
                    block
                )
            }
        };

        let (ctx, block) = ctx.visit(block);

        let data = FlattenData {
            symbol: None,
            location: Some(location.clone()),
            scope: ctx.scope,
        };

        let kind = Kind::Loop { condition, block };

        ctx.append(data, kind)
    }

    fn visit_method_call(
        self,
        location: &SpanTuple,
        instance: &Ast,
        Call { to, generics, args }: &Call,
    ) -> (Ctx, RefIdx) {
        let (ctx, idx) = self.visit(instance);
        let (ctx, generics) = ctx.visit_fold(generics.iter(), Ctx::handle_ty_node);
        let (ctx, mut args) = ctx.visit_fold(args.iter(), Ctx::visit);

        args.insert(0, idx);

        let data = FlattenData {
            symbol: Some(to.clone()),
            location: Some(location.clone()),
            scope: ctx.scope,
        };
        let kind = Kind::Call {
            to: RefIdx::Unresolved,
            generics,
            args,
        };

        ctx.append(data, kind)
    }

    fn visit_opt<T>(
        self,
        node: Option<T>,
        visitor: impl Fn(Ctx, T) -> (Ctx, RefIdx),
    ) -> (Ctx, Option<RefIdx>) {
        match node {
            Some(node) => {
                let (ctx, idx) = visitor(self, node);
                (ctx, Some(idx))
            }
            None => (self, None),
        }
    }

    fn visit_return(self, to_return: &Option<Box<Ast>>, location: &SpanTuple) -> (Ctx, RefIdx) {
        let (ctx, idx) = self.visit_opt(to_return.as_deref(), Ctx::visit);

        let data = FlattenData {
            symbol: None,
            location: Some(location.clone()),
            scope: ctx.scope,
        };
        let kind = Kind::Return(idx);

        ctx.append(data, kind)
    }

    fn visit_type(
        self,
        location: &SpanTuple,
        name: &Symbol,
        generics: &[GenericArgument],
        fields: &[TypedValue],
        _with: &Option<Box<Ast>>,
    ) -> (Ctx, RefIdx) {
        let (ctx, generics) = self.visit_fold(generics.iter(), Ctx::handle_generic_node);
        let (ctx, fields) = ctx.visit_fold(fields.iter(), Ctx::visit_typed_value);

        // FIXME: Handle `with` properly

        let data = FlattenData {
            symbol: Some(name.clone()),
            location: Some(location.clone()),
            scope: ctx.scope,
        };

        let kind = Kind::Type { generics, fields };

        ctx.append(data, kind)
    }

    fn visit_typed_value(
        self,
        TypedValue {
            location,
            symbol,
            ty,
        }: &TypedValue,
    ) -> (Ctx, RefIdx) {
        let (ctx, ty) = self.handle_ty_node(ty);

        let data = FlattenData {
            symbol: Some(symbol.clone()),
            location: Some(location.clone()),
            scope: ctx.scope,
        };
        let kind = Kind::TypedValue {
            value: RefIdx::Unresolved, // FIXME: That's not valid, right?
            ty,
        };

        ctx.append(data, kind)
    }

    fn visit_var_declaration(
        self,
        location: &SpanTuple,
        _mutable: &bool,
        to_declare: &Symbol,
        value: &Ast,
    ) -> (Ctx, RefIdx) {
        // A declaration consists of two things
        // 1. Instantiating the value which we'll assign
        // 2. Creating the first reference to that value - "binding" it

        let (ctx, to_bind) = self.visit(value);

        let data = FlattenData {
            symbol: Some(to_declare.clone()),
            location: Some(location.clone()),
            scope: ctx.scope,
        };

        let kind = Kind::Binding { to: to_bind };

        ctx.append(data, kind)
    }

    fn visit_var_assign(
        self,
        location: &SpanTuple,
        to_assign: &Symbol,
        value: &Ast,
    ) -> (Ctx, RefIdx) {
        // visit a variable or an empty type, but it's not possible to have an
        // empty type here.
        let (ctx, to) = self.visit_var_or_empty_type(location, to_assign);
        let (ctx, from) = ctx.visit(value);

        let data = FlattenData {
            symbol: None,
            location: Some(location.clone()),
            scope: ctx.scope,
        };

        let kind = Kind::Assignment { to, from };

        ctx.append(data, kind)
    }

    fn visit_var_or_empty_type(self, location: &SpanTuple, sym: &Symbol) -> (Ctx, RefIdx) {
        let data = FlattenData {
            symbol: Some(sym.clone()),
            location: Some(location.clone()),
            scope: self.scope,
        };

        let kind = Kind::TypedValue {
            value: RefIdx::Unresolved,
            ty: RefIdx::Unresolved,
        };

        self.append(data, kind)
    }

    fn visit_type_instantiation(
        self,
        location: &SpanTuple,
        Call {
            to,
            generics,
            args: fields,
        }: &Call,
    ) -> (Ctx, RefIdx) {
        let (ctx, generics) = self.visit_fold(generics.iter(), Ctx::handle_ty_node);
        let (ctx, fields) = ctx.visit_fold(fields.iter(), Ctx::visit);

        let data = FlattenData {
            symbol: Some(to.clone()),
            location: Some(location.clone()),
            scope: ctx.scope,
        };
        let kind = Kind::Instantiation {
            to: RefIdx::Unresolved,
            generics,
            fields,
        };

        ctx.append(data, kind)
    }

    fn visit(self, ast: &Ast) -> (Ctx, RefIdx) {
        let loc = &ast.location;

        match &ast.node {
            AstNode::Block {
                stmts,
                last_is_expr,
            } => self.visit_block(loc, stmts, *last_is_expr),
            AstNode::Function { decl, block, .. } => self.visit_function(loc, decl, block),
            AstNode::Type {
                name,
                generics,
                fields,
                with,
            } => self.visit_type(loc, name, generics, fields, with),
            AstNode::TypeInstantiation(call) => self.visit_type_instantiation(loc, call),
            AstNode::FunctionCall(call) => self.visit_function_call(loc, call),
            AstNode::MethodCall { instance, call } => self.visit_method_call(loc, instance, call),
            AstNode::BinaryOp(op, lhs, rhs) => self.visit_binary_op(loc, op, lhs, rhs),
            AstNode::FieldAccess(instance, field_name) => {
                self.visit_field_access(loc, instance, field_name)
            }
            AstNode::IfElse {
                if_condition,
                if_block,
                else_block,
            } => self.visit_if_else(loc, if_condition, if_block, else_block),
            AstNode::VarDeclaration {
                mutable,
                to_declare,
                value,
            } => self.visit_var_declaration(loc, mutable, to_declare, value),
            AstNode::VarAssign { to_assign, value } => self.visit_var_assign(loc, to_assign, value),
            AstNode::VarOrEmptyType(sym) => self.visit_var_or_empty_type(loc, sym),
            AstNode::Loop(kind, block) => self.visit_loop(loc, kind, block),
            AstNode::Return(value) => self.visit_return(value, loc),
            // Leaf node
            AstNode::Constant(_) => {
                let data = FlattenData {
                    symbol: None,
                    location: Some(ast.location.clone()),
                    scope: self.scope,
                };
                // FIXME: Aren't we losing the actual constant here?
                let kind = Kind::Constant(RefIdx::Unresolved);
                self.append(data, kind)
            }
            AstNode::Incl { .. } => {
                unreachable!("invalid AST state: `incl` expressions still present")
            }
            // FIXME: Is that correct?
            AstNode::Empty => (self, RefIdx::Unresolved),
        }
    }
}

impl FlattenAst for ast::Ast {
    fn flatten(&self) -> Fir<FlattenData> {
        let ctx = Ctx {
            fir: Fir::default(),
            origin: OriginIdx::default(),
            scope: 0,
        };

        let (ctx, _last_ref) = ctx.visit(self);

        // FIXME: [cfg(not(release))] yada yada
        ctx.fir.check();

        ctx.fir
    }
}

pub trait FlattenAst: Sized {
    fn flatten(&self) -> Fir<FlattenData>;
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! ast {
        ($($tok:tt)*) => {
            xparser::parse(
                stringify!($($tok)*),
                location::Source::Input(stringify!($($tok)*)))
            .unwrap()
        }
    }

    #[test]
    fn block_order() {
        let block = ast! {
            { 14; 15; }
        };

        let fir = block.flatten();

        assert!(matches!(
            fir.nodes.get(&OriginIdx(1)).unwrap().kind,
            Kind::Constant(_)
        ));

        assert!(matches!(
            fir.nodes.get(&OriginIdx(2)).unwrap().kind,
            Kind::Constant(_)
        ));

        assert!(matches!(
            fir.nodes.get(&OriginIdx(3)).unwrap().kind,
            Kind::Statements(_)
        ));
    }

    #[test]
    fn block_expr() {
        let block = ast! {
            { 15 }
        };

        let fir = block.flatten();

        let block = fir.nodes.get(&OriginIdx(3)).unwrap();
        let stmts = match &block.kind {
            Kind::Statements(stmts) => stmts,
            _ => unreachable!(),
        };
        let ret_idx = match stmts[0] {
            RefIdx::Resolved(i) => i,
            RefIdx::Unresolved => unreachable!(),
        };

        assert!(matches!(
            fir.nodes.get(&ret_idx).unwrap().kind,
            Kind::Return(_),
        ))
    }

    #[test]
    fn where_expr() {
        let fir = ast! {
            where x = 15;
            where mut y = x;
        }
        .flatten();

        let fifteen = &fir.nodes.get(&OriginIdx(1)).unwrap().kind;
        let x = &fir.nodes.get(&OriginIdx(2)).unwrap().kind;

        assert!(matches!(fifteen, Kind::Constant(_)));
        assert!(matches!(x, Kind::Binding { .. }));

        let x = &fir.nodes.get(&OriginIdx(3)).unwrap().kind;
        let y = &fir.nodes.get(&OriginIdx(4)).unwrap().kind;

        assert!(matches!(x, Kind::TypedValue { .. }));
        assert!(matches!(y, Kind::Binding { .. }));
    }
}
