use error::Error;
use fir::{Fallible, Fir, Node, OriginIdx, RefIdx, Traversal};
use flatten::FlattenData;

use crate::{Type, TypeCtx, TypeVariable};

/// This pass takes care of shortening each link within the type context as much as possible. As explained in
/// the previous module ([`crate::typer`]), a list will be built within the [`TypeCtx`] containing "type links".
/// This pass will resolve these links and make each node point to its *actual* type.
pub(crate) struct Actual<'ctx>(pub(crate) &'ctx mut TypeCtx);

fn innermost_type(ctx: &TypeCtx, linked_node: RefIdx) -> Option<Type> {
    let linked_node = linked_node.expect_resolved();

    // so can we replace all of this by a permanent lookup until we find a Type::Actual?
    // probably, right?
    // and we won't need the FIR as a parameter anymore
    let mut to_lookup = linked_node;
    loop {
        let ty = ctx.types.get(&to_lookup);

        match ty {
            Some(ty) => match ty {
                Some(ty) => match ty {
                    TypeVariable::Actual(ty) => return Some(ty.clone()), // FIXME: Remove clone
                    TypeVariable::Reference(r) => {
                        to_lookup = r.expect_resolved();
                    }
                },
                None => return None,
            },
            None => todo!(),
        }
    }

    //     // if the context contains an entry for `linked_node`, then we use this - otherwise, we simply
    //     // get the node from the FIR
    //     let to_lookup = &ctx
    //         .types
    //         .get(&linked_node)
    //         .and_then(|ty| ty.map(|t| t.ref_idx().expect_resolved()))
    //         .unwrap_or(linked_node);

    //     let linked_node = &fir.nodes[to_lookup];

    //     let inner_opt = |fir, opt| match opt {
    //         Some(opt) => innermost_type(ctx, fir, opt),
    //         None => None,
    //     };

    //     match &linked_node.kind {
    //         // shouldn't we instead stop when we find a TypeVariable::Actual?
    //         // these are the *only* terminal branches
    //         Kind::RecordType { .. } | Kind::UnionType { .. } => {
    //             Some(Type::One(RefIdx::Resolved(linked_node.origin)))
    //         }
    //         Kind::Assignment { .. } => None,
    //         // if a typed value has no specific type, but points to a value, use this as the source of the type
    //         Kind::TypedValue {
    //             ty: RefIdx::Unresolved,
    //             value,
    //             // can we set `ty` here or not? probably not
    //             // we need to :(
    //         } => innermost_type(ctx, fir, *value),
    //         Kind::Constant(ty)
    //         | Kind::TypeReference(ty)
    //         | Kind::TypedValue { ty, .. }
    //         | Kind::Binding { to: ty }
    //         | Kind::Instantiation { to: ty, .. }
    //         | Kind::Call { to: ty, .. }
    //         | Kind::Conditional { true_block: ty, .. } => innermost_type(ctx, fir, *ty),
    //         Kind::Function {
    //             return_type: ty, ..
    //         }
    //         | Kind::Return(ty) => inner_opt(fir, *ty),
    //         Kind::Statements(stmts) => inner_opt(fir, stmts.last().copied()),
    //         // FIXME: What to do here?
    //         Kind::Generic { .. } => todo!(),
    //         Kind::Loop { .. } => todo!(),
    //         Kind::TypeOffset { .. } => todo!(),
    //     }
}

impl<'ctx> Actual<'ctx> {
    /// Recursively try and resolve a type link within the type context. This will update the given node's type
    /// within the type context.
    fn resolve_link(&mut self, to_resolve: OriginIdx) -> Fallible<Error> {
        // if any node has not been through "Typer" before, this is an interpreter error
        let link = self.0.types.get(&to_resolve).unwrap();

        // void nodes are already fully typed, so we don't take care of them
        if let Some(TypeVariable::Reference(ty_ref)) = link {
            // FIXME: Rework. that's very ugly
            let innermost_ty = innermost_type(self.0, *ty_ref);
            self.0
                .types
                .insert(
                    to_resolve,
                    Some(TypeVariable::Actual(innermost_ty.unwrap())),
                )
                .unwrap();
        }

        Ok(())
    }
}

impl<'ctx> Traversal<FlattenData<'_>, Error> for Actual<'ctx> {
    fn traverse_node(&mut self, _: &Fir<FlattenData>, node: &Node<FlattenData>) -> Fallible<Error> {
        self.resolve_link(node.origin)
    }
}
