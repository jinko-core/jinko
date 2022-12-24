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

use ast::{Ast, Declaration, Node as AstNode, TypeArgument};
use fir::{Fir, Kind, Node, OriginIdx, RefIdx};
use location::SpanTuple;
use symbol::Symbol;

#[derive(Debug, Default)]
pub struct FlattenData {
    pub symbol: Option<Symbol>,
    pub location: Option<SpanTuple>, // FIXME: Remove the option
    pub scope: u64,
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

fn visit_block(
    fir: Fir<FlattenData>,
    location: SpanTuple,
    nodes: &[Ast],
    current_origin: OriginIdx,
    current_scope_level: u64,
) -> (Fir<FlattenData>, RefIdx) {
    let mut origin = current_origin;

    let (fir, refs) = nodes.iter().fold((fir, vec![]), |(fir, refs), node| {
        let next = origin.next();
        origin = next;

        let (fir, new_ref) = visit(fir, node, next, current_scope_level + 1);

        (fir, refs.with(new_ref))
    });

    let origin = origin.next();
    let node = Node {
        data: FlattenData {
            symbol: None,
            location: Some(location),
            scope: current_scope_level,
        },
        origin,
        kind: Kind::Statements(refs),
    };

    (fir.append(node), RefIdx::Resolved(origin))
}

fn handle_ty_node(
    fir: Fir<FlattenData>,
    ty: &TypeArgument,
    current_origin: OriginIdx,
    current_scope_level: u64,
) -> (Fir<FlattenData>, RefIdx) {
    todo!()
}

fn visit_function(
    fir: Fir<FlattenData>,
    location: SpanTuple,
    Declaration {
        name,
        generics,
        args,
        return_type,
    }: &Declaration,
    block: &Option<Box<Ast>>,
    current_origin: OriginIdx,
    current_scope_level: u64,
) -> (Fir<FlattenData>, RefIdx) {
    let mut origin = current_origin;
    let fn_scope = current_scope_level + 1;

    let (fir, generics) = generics.iter().fold((fir, vec![]), |(fir, refs), node| {
        let next = origin.next();
        origin = next;

        let node = Node {
            data: FlattenData {
                symbol: Some(node.name.clone()),
                location: Some(location.clone()), // FIXME: This needs to be the GenericArgument's location
                scope: fn_scope,
            },
            origin: next,
            kind: Kind::Generic { default: None }, // FIXME: Use default properly
        };

        (fir.append(node), refs.with(RefIdx::Resolved(next)))
    });

    let (fir, args) = args.iter().fold((fir, vec![]), |(fir, refs), arg| {
        let next = origin.next();
        origin = next;

        let (fir, ty_node_idx) = handle_ty_node(fir, &arg.ty, origin, fn_scope); // should these return Nodes instead?
                                                                                 // would be way more ergonomic
        let node = Node {
            data: FlattenData {
                symbol: Some(arg.symbol.clone()),
                location: Some(location.clone()), // FIXME: Use the arg's location
                scope: fn_scope,
            },
            origin,
            kind: Kind::TypedValue {
                value: todo!(), // FIXME: What do we put here? Since this is a
                // declaration and not a usage, the type in `Fir::TypedValue` doesn't make a lot
                // of sense
                ty: ty_node_idx,
            },
        };

        (fir.append(node), refs.with(RefIdx::Resolved(next)))
    });

    // FIXME: Handle return type

    let (fir, block_id) = if let Some(block) = block {
        visit(fir, block, current_origin, fn_scope)
    } else {
        (fir, RefIdx::Unresolved) // FIXME: Invalid
    };

    todo!();
}

fn visit(
    fir: Fir<FlattenData>,
    ast: &Ast,
    current_origin: OriginIdx,
    current_scope_level: u64,
) -> (Fir<FlattenData>, RefIdx) {
    match &ast.node {
        AstNode::Block(nodes) => visit_block(
            fir,
            ast.location.clone(),
            nodes,
            current_origin,
            current_scope_level,
        ),
        AstNode::Function { decl, block, .. } => visit_function(
            fir,
            ast.location.clone(),
            decl,
            block,
            current_origin,
            current_scope_level,
        ),
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
        // Leaf node
        AstNode::Constant(_) => (
            fir.append(Node {
                data: FlattenData {
                    symbol: None,
                    location: Some(ast.location.clone()),
                    scope: current_scope_level,
                },
                origin: current_origin,
                kind: Kind::Constant(RefIdx::Unresolved),
            }),
            RefIdx::Resolved(current_origin),
        ),
        // TODO: will we still have Incl at this level? Probably not
        AstNode::Incl { .. } => {
            unreachable!("invalid AST state: `incl` expressions still present")
        }
    }
}

impl FlattenAst for ast::Ast {
    fn flatten(&self) -> Fir<FlattenData> {
        let (fir, _last_ref) = visit(Fir::default(), self, OriginIdx::default(), 0);

        fir
    }
}
