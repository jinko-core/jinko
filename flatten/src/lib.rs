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
//! block to the [`Fir`]. We do this since a [`Kind::Block`] only contains *references* to the statements
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

use ast::{Ast, Declaration, GenericArgument, Node as AstNode, TypeArgument, TypedValue};
use fir::{Fir, Kind, Node, OriginIdx, RefIdx};
use location::SpanTuple;
use symbol::Symbol;

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

    fn visit(self, ast: &Ast) -> (Ctx, RefIdx) {
        match &ast.node {
            AstNode::Block(nodes) => self.visit_block(ast.location.clone(), nodes),
            AstNode::Function { decl, block, .. } => {
                self.visit_function(ast.location.clone(), decl, block)
            }
            AstNode::Type {
                name,
                generics,
                fields,
                with,
            } => todo!(),
            AstNode::TypeInstantiation(_) => todo!(),
            AstNode::FunctionCall(ast::Call { to, generics, args }) => {
                self.visit_fn_call(ast.location.clone(), to, generics, args)
            }
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
            AstNode::Return(value) => self.visit_return(value, ast.location.clone()),
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

    fn visit_block(self, location: SpanTuple, nodes: &[Ast]) -> (Ctx, RefIdx) {
        let (ctx, refs) = self.visit_fold(nodes.iter(), Ctx::visit);

        let data = FlattenData {
            symbol: None,
            location: Some(location),
            scope: ctx.scope,
        };
        let kind = Kind::Statements(refs);

        ctx.append(data, kind)
    }

    fn visit_fn_call(
        self,
        location: SpanTuple,
        to: &Symbol,
        generics: &[TypeArgument],
        args: &[Ast],
    ) -> (Ctx, RefIdx) {
        let (ctx, generics) = self.visit_fold(generics.iter(), Ctx::handle_ty_node);
        let (ctx, args) = ctx.visit_fold(args.iter(), Ctx::visit);

        let data = FlattenData {
            symbol: Some(to.clone()),
            location: Some(location),
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
        mut self,
        location: SpanTuple,
        Declaration {
            name,
            generics,
            args,
            return_type,
        }: &Declaration,
        block: &Option<Box<Ast>>,
    ) -> (Ctx, RefIdx) {
        self.scope += 1;

        let (ctx, generics) = self.visit_fold(generics.iter(), Ctx::handle_generic_node);
        let (ctx, args) = ctx.visit_fold(args.iter(), Ctx::visit_typed_value);
        let (ctx, return_type) = ctx.visit_opt(return_type.as_ref(), Ctx::handle_ty_node);
        let (mut ctx, block) = ctx.visit_opt(block.as_deref(), Ctx::visit);

        let data = FlattenData {
            symbol: Some(name.clone()),
            location: Some(location),
            scope: ctx.scope,
        };
        let kind = Kind::Function {
            generics,
            args,
            return_type,
            block,
        };

        ctx.scope -= 1;

        ctx.append(data, kind)
    }

    fn visit_return(self, to_return: &Option<Box<Ast>>, location: SpanTuple) -> (Ctx, RefIdx) {
        let (ctx, idx) = self.visit_opt(to_return.as_deref(), Ctx::visit);

        let data = FlattenData {
            symbol: None,
            location: Some(location),
            scope: ctx.scope,
        };
        let kind = Kind::Return(idx);

        ctx.append(data, kind)
    }

    // TODO: Now: How do we improve the API? This sucks ass and is tedious to extend, and error-prone

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
