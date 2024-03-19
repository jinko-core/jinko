use std::collections::HashMap;

use crate::Scope;

use fir::{Fallible, Fir, Kind, Node, OriginIdx, RefIdx, Traversal};
use flatten::FlattenData;

pub(crate) struct Scoper {
    /// The current scope we are visiting, which we will use when we assign a scope to each
    /// node in [`Scoper::scope`]
    pub(crate) current_scope: Scope,
    /// Map of each node to the scope it is contained in. This will be built progressively as
    /// we visit each node
    pub(crate) enclosing_scope: HashMap<OriginIdx, Scope>,
}

// TODO: Rename
#[derive(Debug)]
pub struct ScoperError;

impl Scoper {
    /// Set the enclosing scope of `to_scope` to the current scope
    fn scope(&mut self, to_scope: &Node<FlattenData>) {
        self.enclosing_scope
            .insert(to_scope.origin, self.current_scope);
    }

    /// Enter a new scope, replacing the context's current scope. This returns the old scope,
    /// which you will need to reuse when you exit the scoped node you are visiting
    fn enter_scope(&mut self, new_scope: OriginIdx) -> OriginIdx {
        self.current_scope.replace(new_scope)
    }

    // TODO: Move this function in `Traversal`?
    fn maybe_visit_child(
        &mut self,
        fir: &Fir<FlattenData<'_>>,
        ref_idx: &RefIdx,
    ) -> Fallible<ScoperError> {
        match ref_idx {
            RefIdx::Resolved(origin) => self.traverse_node(fir, &fir[origin]),
            // we skip unresolved nodes here
            RefIdx::Unresolved => Ok(()),
        }
    }
}

// FIXME: This can be replaced by a TreeLike
impl<'ast> Traversal<FlattenData<'ast>, ScoperError> for Scoper {
    fn traverse_assignment(
        &mut self,
        fir: &Fir<FlattenData<'ast>>,
        _node: &Node<FlattenData<'ast>>,
        to: &RefIdx,
        from: &RefIdx,
    ) -> Fallible<ScoperError> {
        self.maybe_visit_child(fir, to)?;
        self.maybe_visit_child(fir, from)
    }

    fn traverse_function(
        &mut self,
        fir: &Fir<FlattenData<'ast>>,
        node: &Node<FlattenData<'ast>>,
        generics: &[RefIdx],
        args: &[RefIdx],
        return_ty: &Option<RefIdx>,
        block: &Option<RefIdx>,
    ) -> Fallible<ScoperError> {
        let old = self.enter_scope(node.origin);

        generics
            .iter()
            .for_each(|generic| self.maybe_visit_child(fir, generic).unwrap());

        args.iter()
            .for_each(|arg| self.maybe_visit_child(fir, arg).unwrap());

        block.map(|definition| self.maybe_visit_child(fir, &definition));
        return_ty.map(|ty| self.maybe_visit_child(fir, &ty));

        self.enter_scope(old);

        Ok(())
    }

    fn traverse_statements(
        &mut self,
        fir: &Fir<FlattenData<'ast>>,
        node: &Node<FlattenData<'ast>>,
        stmts: &[RefIdx],
    ) -> Fallible<ScoperError> {
        // TODO: Ugly but can we do anything better? Can we have types which force you to exit a scope if you enter one?
        let old = self.enter_scope(node.origin);

        stmts
            .iter()
            .for_each(|stmt| self.maybe_visit_child(fir, stmt).unwrap());

        self.enter_scope(old);

        Ok(())
    }

    fn traverse_node(
        &mut self,
        fir: &Fir<FlattenData<'ast>>,
        node: &Node<FlattenData<'ast>>,
    ) -> Fallible<ScoperError> {
        self.scope(node);

        match &node.kind {
            Kind::Binding { to, ty } => {
                to.map_or(Ok(()), |node| self.maybe_visit_child(fir, &node))?;
                self.maybe_visit_child(fir, ty)
            }
            Kind::TypeReference(sub_node)
            | Kind::TypeOffset {
                instance: sub_node, ..
            } => self.maybe_visit_child(fir, sub_node),
            Kind::NodeRef(to) => self.maybe_visit_child(fir, to),
            Kind::RecordType { fields: subs, .. } | Kind::UnionType { variants: subs, .. } => {
                let old = self.enter_scope(node.origin);

                subs.iter().for_each(|sub| {
                    // FIXME: Is unwrap okay here?
                    self.maybe_visit_child(fir, sub).unwrap();
                });

                self.enter_scope(old);

                Ok(())
            }
            Kind::Generic { default } => {
                default.map_or(Ok(()), |def| self.maybe_visit_child(fir, &def))
            }
            Kind::Assignment { to, from } => self.traverse_assignment(fir, node, to, from),
            Kind::Instantiation {
                to,
                generics,
                fields,
            } => {
                self.maybe_visit_child(fir, to)?;
                generics
                    .iter()
                    .for_each(|generic| self.maybe_visit_child(fir, generic).unwrap());

                fields
                    .iter()
                    .for_each(|field| { self.maybe_visit_child(fir, field) }.unwrap());

                Ok(())
            }
            Kind::Call { to, generics, args } => {
                self.maybe_visit_child(fir, to)?;
                generics
                    .iter()
                    .for_each(|generic| self.maybe_visit_child(fir, generic).unwrap());
                args.iter()
                    .for_each(|arg| self.maybe_visit_child(fir, arg).unwrap());

                Ok(())
            }
            Kind::Function {
                generics,
                args,
                return_type,
                block,
            } => self.traverse_function(fir, node, generics, args, return_type, block),
            Kind::Statements(stmts) => self.traverse_statements(fir, node, stmts),
            Kind::Conditional {
                condition,
                true_block,
                false_block,
            } => {
                self.maybe_visit_child(fir, condition)?;
                self.maybe_visit_child(fir, true_block)?;
                false_block.map_or(Ok(()), |else_block| {
                    self.maybe_visit_child(fir, &else_block)
                })
            }
            Kind::Return(sub_node) => {
                sub_node.map_or(Ok(()), |node| self.maybe_visit_child(fir, &node))
            }
            Kind::Loop { condition, block } => {
                self.maybe_visit_child(fir, condition)?;
                self.maybe_visit_child(fir, block)?;

                Ok(())
            }
            // nothing to do for constants, other than scoping them
            Kind::Constant(_) => Ok(()),
        }
    }

    /// Nothing to do here, right? This function should never be called
    /// FIXME: Add documentation
    fn traverse(&mut self, _fir: &Fir<FlattenData<'ast>>) -> Fallible<Vec<ScoperError>> {
        unreachable!()
    }
}
