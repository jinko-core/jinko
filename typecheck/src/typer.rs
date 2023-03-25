use error::Error;
use fir::{Fallible, Fir, Node, RefIdx, Traversal};
use flatten::FlattenData;

use crate::{Type, TypeCtx};

/// This pass takes care of "typing" each node in the [`Fir`], but to a non-terminal type. More explanation can be found
/// in the documentation for [`Typer::ty`].
pub(crate) struct Typer<'ctx>(pub(crate) &'ctx mut TypeCtx);

impl<'ctx> Typer<'ctx> {
    /// Assign a type to a node. This type can either be void ([`None`]) in the case of a declaration or void
    /// statement, or may be a "type linked list": a reference to a type defined elsewhere in the [`Fir`].
    /// Let's consider a block of multiple statements, the last of which being a function call. The type of a
    /// block is the type of its last statement, which is often a return statement. In that case the return statement
    /// will be of the same type as the function call. Said function call is of the type returned by the function
    /// it will call. You can see how we'll go down this list of type to figure out the actual, final type
    /// of each node in the [`Fir`]. This is however done in another traversal on the [`Fir`] called "Actual" and defined
    /// in another module.
    fn ty(&mut self, node: &Node<FlattenData>, ty: Option<&RefIdx>) -> Fallible<Error> {
        let ty = ty.map(|refidx| Type::One(*refidx));

        // Having non-unique ids in the Fir is an interpreter error
        // Or should we return an error here?
        assert!(self.0.types.insert(node.origin, ty).is_none());

        Ok(())
    }
}

impl Traversal<FlattenData<'_>, Error> for Typer<'_> {
    fn traverse_constant(
        &mut self,
        _fir: &Fir<FlattenData>,
        node: &Node<FlattenData>,
        _constant: &RefIdx,
    ) -> Fallible<Error> {
        // switch on the constant's kind - and this is a *declare* spot, so we must use
        // TypeData::from(data).declares(ty). This way, things like blocks returning constants can simply
        // depend on the type returned by the constant.
        // TODO: Is that all we need?
        // TODO: We need data from the AST at this point. Either the node or what kind of
        // constant it is (if it is one)

        // For constants, how will we look up the basic primitive type nodes before assigning them
        // here? Just a traversal and we do that based on name? Or will they need to be builtin at this point?
        // Some types, like string, int, char, are builtin multi types and will *need* to be builtin.
        // `bool` on the other hand, can be a multi type implemented within the standard library.
        self.ty(node, None)
    }

    fn traverse_node(
        &mut self,
        fir: &Fir<FlattenData>,
        node: &Node<FlattenData>,
    ) -> Fallible<Error> {
        match &node.kind {
            fir::Kind::Constant(c) => self.traverse_constant(fir, node, c),
            // Declarations and assignments are void
            fir::Kind::Type { .. }
            | fir::Kind::Function { .. }
            | fir::Kind::Binding { .. }
            | fir::Kind::Assignment { .. } => self.ty(node, None),
            // These nodes all refer to other nodes, type references or typed values. They will need
            // to be flattened later on.
            fir::Kind::TypeReference(ty)
            | fir::Kind::TypedValue { value: ty, .. }
            | fir::Kind::Instantiation { to: ty, .. }
            | fir::Kind::Call { to: ty, .. }
            | fir::Kind::Conditional { true_block: ty, .. } => self.ty(node, Some(ty)),
            // Returns are a bit special as they can already be void
            fir::Kind::Return(ty) => self.ty(node, ty.as_ref()),
            // Blocks are the same type as their last stmt, or void if it does not exist
            fir::Kind::Statements(stmts) => self.ty(node, stmts.last()),
            // TODO: Figure out what to do with these
            fir::Kind::Generic { .. } | fir::Kind::TypeOffset { .. } | fir::Kind::Loop { .. } => {
                Ok(())
            }
        }
    }
}
