// pub trait Scoper {

// }

use std::collections::HashMap;

use fir::{Fallible, Fir, Kind, Node, OriginIdx, RefIdx, Traversal};
use flatten::FlattenData;

pub(crate) struct Scoper {
    pub(crate) current_scope: RefIdx,
    pub(crate) enclosing_scope: HashMap<OriginIdx, RefIdx>,
}

impl Scoper {
    /// Set the enclosing scope of `to_scope` to the current scope
    fn scope(&mut self, to_scope: &Node<FlattenData>) {
        self.enclosing_scope
            .insert(to_scope.origin, self.current_scope);
    }
}

impl<'ast> Traversal<FlattenData<'ast>, () /* FIXME: Ok? */> for Scoper {
    fn traverse_function(
        &mut self,
        fir: &Fir<FlattenData<'ast>>,
        _node: &Node<FlattenData<'ast>>,
        generics: &[RefIdx],
        args: &[RefIdx],
        return_ty: &Option<RefIdx>,
        block: &Option<RefIdx>,
    ) -> Fallible<()> {
        let old_scope = self.current_scope;

        generics
            .iter()
            .for_each(|generic| self.traverse_node(fir, &fir[generic]).unwrap());

        args.iter()
            .for_each(|arg| self.traverse_node(fir, &fir[arg]).unwrap());

        block.map(|definition| self.traverse_node(fir, &fir[&definition]));
        return_ty.map(|ty| self.traverse_node(fir, &fir[&ty]));

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
        self.current_scope = RefIdx::Resolved(node.origin);

        stmts
            .iter()
            // this is safe since the Scoper will never throw an error // FIXME: Is that true?
            .for_each(|stmt| self.traverse_node(fir, &fir[stmt]).unwrap());

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
            Kind::Constant(sub_node)
            | Kind::TypeReference(sub_node)
            | Kind::TypedValue {
                value: sub_node, ..
            }
            | Kind::Instantiation { to: sub_node, .. }
            | Kind::TypeOffset {
                instance: sub_node, ..
            }
            | Kind::Binding { to: sub_node } => self.traverse_node(fir, &fir[sub_node]),
            Kind::Type { fields, .. } => {
                fields
                    .iter()
                    .for_each(|field| self.traverse_node(fir, &fir[field]).unwrap());

                Ok(())
            }
            Kind::Generic { default } => default
                .map(|def| self.traverse_node(fir, &fir[&def]))
                .ok_or(())?,
            Kind::Assignment { to, from } => self.traverse_assignment(fir, node, to, from),
            Kind::Call { to, generics, args } => {
                self.traverse_node(fir, &fir[to])?;
                generics
                    .iter()
                    .for_each(|generic| self.traverse_node(fir, &fir[generic]).unwrap());
                args.iter()
                    .for_each(|arg| self.traverse_node(fir, &fir[arg]).unwrap());

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
                self.traverse_node(fir, &fir[condition])?;
                self.traverse_node(fir, &fir[true_block])?;
                false_block
                    .map(|else_block| self.traverse_node(fir, &fir[&else_block]))
                    .ok_or(())?
            }
            Kind::Return(sub_node) => sub_node
                .map(|node| self.traverse_node(fir, &fir[&node]))
                .ok_or(())?,
            Kind::Loop { condition, block } => {
                self.traverse_node(fir, &fir[condition])?;
                self.traverse_node(fir, &fir[block])?;

                Ok(())
            }
        }
    }

    /// Nothing to do here, right? This function should never be called
    /// FIXME: Add documentation
    fn traverse(&mut self, _fir: &Fir<FlattenData<'ast>>) -> Fallible<Vec<()>> {
        unreachable!()
    }
}
