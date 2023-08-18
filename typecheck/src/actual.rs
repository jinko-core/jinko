use error::Error;
use fir::{Fallible, Fir, Kind, Node, OriginIdx, RefIdx, Traversal};
use flatten::FlattenData;

use crate::{Type, TypeCtx};

/// This pass takes care of shortening each link within the type context as much as possible. As explained in
/// the previous module ([`crate::typer`]), a list will be built within the [`TypeCtx`] containing "type links".
/// This pass will resolve these links and make each node point to its *actual* type.
pub(crate) struct Actual<'ctx>(pub(crate) &'ctx mut TypeCtx);

fn innermost_type(fir: &Fir<FlattenData>, linked_node: RefIdx) -> Option<Type> {
    let linked_node = &fir.nodes[&linked_node.expect_resolved()];

    let inner_opt = |fir, opt| match opt {
        Some(opt) => innermost_type(fir, opt),
        None => None,
    };

    match &linked_node.kind {
        // these are the *only* terminal branches
        Kind::Type { .. } => Some(Type::One(RefIdx::Resolved(linked_node.origin))),
        Kind::Assignment { .. } => None,
        Kind::Constant(ty)
        | Kind::TypeReference(ty)
        | Kind::TypedValue { ty, .. }
        | Kind::Binding { to: ty }
        | Kind::Instantiation { to: ty, .. }
        | Kind::Call { to: ty, .. }
        | Kind::Conditional { true_block: ty, .. } => innermost_type(fir, *ty),
        Kind::Function {
            return_type: ty, ..
        }
        | Kind::Return(ty) => inner_opt(fir, *ty),
        Kind::Statements(stmts) => inner_opt(fir, stmts.last().copied()),
        // FIXME: What to do here?
        Kind::Generic { .. } => todo!(),
        Kind::Loop { .. } => todo!(),
        Kind::TypeOffset { .. } => todo!(),
    }
}

impl<'ctx> Actual<'ctx> {
    /// Recursively try and resolve a type link within the type context. This will update the given node's type
    /// within the type context.
    fn resolve_link(&mut self, fir: &Fir<FlattenData>, to_resolve: OriginIdx) -> Fallible<Error> {
        // if any node has not been through "Typer" before, this is an interpreter error
        let link = self.0.types.get(&to_resolve).unwrap();

        // void nodes are already fully typed, so we don't take care of them
        if let Some(Type::One(ty_ref)) = link {
            let innermost_ty = innermost_type(fir, *ty_ref);
            self.0.types.insert(to_resolve, innermost_ty).unwrap();
        }

        Ok(())
    }
}

impl<'ctx> Traversal<FlattenData<'_>, Error> for Actual<'ctx> {
    fn traverse_node(
        &mut self,
        fir: &Fir<FlattenData>,
        node: &Node<FlattenData>,
    ) -> Fallible<Error> {
        self.resolve_link(fir, node.origin)
    }
}
