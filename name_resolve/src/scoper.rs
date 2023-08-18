// FIXME: Documentation

use std::collections::HashMap;

use fir::{Fallible, Fir, Kind, Node, OriginIdx, RefIdx, Traversal};
use flatten::FlattenData;

pub(crate) struct Scoper {
    pub(crate) current_scope: OriginIdx, // TODO: Wrap in a new type with a .replace() method
    pub(crate) enclosing_scope: HashMap<OriginIdx, OriginIdx>,
}

impl Scoper {
    /// Set the enclosing scope of `to_scope` to the current scope
    fn scope(&mut self, to_scope: &Node<FlattenData>) {
        self.enclosing_scope
            .insert(to_scope.origin, self.current_scope);
    }

    fn maybe_visit_child(&mut self, fir: &Fir<FlattenData<'_>>, ref_idx: &RefIdx) -> Fallible<()> {
        match ref_idx {
            RefIdx::Resolved(origin) => self.traverse_node(fir, &fir[origin]),
            // we skip unresolved nodes here
            RefIdx::Unresolved => Ok(()),
        }
    }

    fn visit_field_instantiation(
        &mut self,
        fir: &Fir<FlattenData<'_>>,
        ref_idx: &RefIdx,
    ) -> Fallible<()> {
        self.maybe_visit_child(fir, ref_idx)

        // let field_instantiation = dbg!(&fir[ref_idx]);

        // if let Kind::TypedValue { value: _, ty } = field_instantiation.kind {
        //     // do not visit the `value` - it's always unresolved at this point
        //     self.maybe_visit_child(fir, &ty)
        // } else {
        //     unreachable!()
        // }
    }
}

impl<'ast> Traversal<FlattenData<'ast>, () /* FIXME: Ok? */> for Scoper {
    fn traverse_function(
        &mut self,
        fir: &Fir<FlattenData<'ast>>,
        node: &Node<FlattenData<'ast>>,
        generics: &[RefIdx],
        args: &[RefIdx],
        return_ty: &Option<RefIdx>,
        block: &Option<RefIdx>,
    ) -> Fallible<()> {
        // TODO: Factor in a function
        let old_scope = self.current_scope;
        self.current_scope = node.origin;

        generics
            .iter()
            .for_each(|generic| self.maybe_visit_child(fir, generic).unwrap());

        args.iter()
            .for_each(|arg| self.maybe_visit_child(fir, arg).unwrap());

        block.map(|definition| self.maybe_visit_child(fir, &definition));
        return_ty.map(|ty| self.maybe_visit_child(fir, &ty));

        self.current_scope = old_scope;

        Ok(())
    }

    fn traverse_statements(
        &mut self,
        fir: &Fir<FlattenData<'ast>>,
        node: &Node<FlattenData<'ast>>,
        stmts: &[RefIdx],
    ) -> Fallible<()> {
        let old_scope = self.current_scope;
        self.current_scope = node.origin;

        stmts
            .iter()
            .for_each(|stmt| self.maybe_visit_child(fir, stmt).unwrap());

        self.current_scope = old_scope;

        Ok(())
    }

    fn traverse_node(
        &mut self,
        fir: &Fir<FlattenData<'ast>>,
        node: &Node<FlattenData<'ast>>,
    ) -> Fallible<()> {
        self.scope(node);

        match &node.kind {
            Kind::TypeReference(sub_node)
            | Kind::TypeOffset {
                instance: sub_node, ..
            }
            | Kind::Binding { to: sub_node } => self.maybe_visit_child(fir, sub_node),
            Kind::TypedValue { value, ty } => {
                self.maybe_visit_child(fir, value)?;
                self.maybe_visit_child(fir, ty)
            }
            Kind::Type { fields, .. } => {
                fields.iter().for_each(|field| {
                    // Factor in a function?
                    let old_scope = self.current_scope;
                    self.current_scope = node.origin;

                    self.maybe_visit_child(fir, field).unwrap();

                    self.current_scope = old_scope;
                });

                Ok(())
            }
            Kind::Generic { default } => default
                .map(|def| self.maybe_visit_child(fir, &def))
                .ok_or(())?,
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
                    .for_each(|field| self.visit_field_instantiation(fir, field).unwrap());

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
                false_block
                    .map(|else_block| self.maybe_visit_child(fir, &else_block))
                    .ok_or(())?
            }
            Kind::Return(sub_node) => sub_node
                .map(|node| self.maybe_visit_child(fir, &node))
                .ok_or(())?,
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
    fn traverse(&mut self, _fir: &Fir<FlattenData<'ast>>) -> Fallible<Vec<()>> {
        unreachable!()
    }
}
