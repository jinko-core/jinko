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

use ast::{Ast, Call, Declaration, GenericArgument, Node as AstNode, TypeArgument, TypedValue};
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

#[derive(Debug, Default)]
pub struct FlattenData {
    pub symbol: Option<Symbol>,
    pub location: Option<SpanTuple>, // FIXME: Remove the option
    pub scope: u64,
}

struct Ctx {
    pub fir: Fir<FlattenData>,
    pub origin: OriginIdx,
    pub scope: u64,
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

    fn scoped(mut self, f: impl Fn(Ctx) -> (Ctx, RefIdx)) -> (Ctx, RefIdx) {
        self.scope += 1;

        let (mut ctx, idx) = f(self);

        ctx.scope -= 1;

        (ctx, idx)
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

    fn visit_fn_call(
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

    fn visit(self, ast: &Ast) -> (Ctx, RefIdx) {
        match &ast.node {
            AstNode::Block {
                stmts,
                last_is_expr,
            } => self.visit_block(&ast.location, stmts, *last_is_expr),
            AstNode::Function { decl, block, .. } => {
                self.visit_function(&ast.location, decl, block)
            }
            AstNode::Type {
                name: _,
                generics: _,
                fields: _,
                with: _,
            } => todo!(),
            AstNode::TypeInstantiation(_) => todo!(),
            AstNode::FunctionCall(call) => self.visit_fn_call(&ast.location, call),
            AstNode::MethodCall { instance, call } => {
                self.visit_method_call(&ast.location, instance, call)
            }
            AstNode::BinaryOp(_, _, _) => todo!(),
            AstNode::FieldAccess(_, _) => todo!(),
            AstNode::IfElse {
                if_condition: _,
                if_block: _,
                else_block: _,
            } => todo!(),
            AstNode::VarAssign {
                mutable: _,
                to_assign: _,
                value: _,
            } => todo!(),
            AstNode::Var(_) => todo!(),
            AstNode::VarOrEmptyType(_) => todo!(),
            AstNode::Loop(_, _) => todo!(),
            AstNode::Return(value) => self.visit_return(value, &ast.location),
            // Leaf node
            AstNode::Constant(_) => {
                let data = FlattenData {
                    symbol: None,
                    location: Some(ast.location.clone()),
                    scope: self.scope,
                };
                let kind = Kind::Constant(RefIdx::Unresolved);
                self.append(data, kind)
            }
            // TODO: will we still have Incl at this level? Probably not
            AstNode::Incl { .. } => {
                unreachable!("invalid AST state: `incl` expressions still present")
            }
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
    use ast::Value;
    use location::{Location, Source};

    macro_rules! fake_loc {
        () => {
            SpanTuple::with_source_ref(Source::Empty, Location::new(1, 1), Location::new(1, 1))
        };
    }

    #[test]
    fn block_order() {
        // { 14; 15; }
        let block = Ast {
            location: fake_loc!(),
            node: AstNode::Block {
                stmts: vec![
                    Ast {
                        location: fake_loc!(),
                        node: AstNode::Constant(Value::Integer(14)),
                    },
                    Ast {
                        location: fake_loc!(),
                        node: AstNode::Constant(Value::Integer(15)),
                    },
                ],
                last_is_expr: false,
            },
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
        // { 15 }
        let block = Ast {
            location: fake_loc!(),
            node: AstNode::Block {
                stmts: vec![Ast {
                    location: fake_loc!(),
                    node: AstNode::Constant(Value::Integer(15)),
                }],
                last_is_expr: true,
            },
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
}
