use crate::{Fallible, Fir, Kind, Node, RefIdx};

/// All of the helpers and visitors call back into [`Traversal::traverse_node`] which then dispatches
/// to the proper subfunctions.
pub trait Traversal<T, E> {
    fn traverse_constant(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _constant: &RefIdx,
    ) -> Fallible<E> {
        Ok(())
    }

    fn traverse_type_reference(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _reference: &RefIdx,
    ) -> Fallible<E> {
        Ok(())
    }

    fn traverse_typed_value(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _value: &RefIdx,
        _ty: &RefIdx,
    ) -> Fallible<E> {
        Ok(())
    }

    fn traverse_generic(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _default: &Option<RefIdx>,
    ) -> Fallible<E> {
        Ok(())
    }

    fn traverse_record_type(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _generics: &[RefIdx],
        _fields: &[RefIdx],
    ) -> Fallible<E> {
        Ok(())
    }

    fn traverse_union_type(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _generics: &[RefIdx],
        _variants: &[RefIdx],
    ) -> Fallible<E> {
        Ok(())
    }

    fn traverse_function(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _generics: &[RefIdx],
        _args: &[RefIdx],
        _return_ty: &Option<RefIdx>,
        _block: &Option<RefIdx>,
    ) -> Fallible<E> {
        Ok(())
    }

    fn traverse_binding(&mut self, _fir: &Fir<T>, _node: &Node<T>, _to: &RefIdx) -> Fallible<E> {
        Ok(())
    }

    fn traverse_instantiation(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _to: &RefIdx,
        _generics: &[RefIdx],
        _fields: &[RefIdx],
    ) -> Fallible<E> {
        Ok(())
    }

    fn traverse_type_offset(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _instance: &RefIdx,
        _offset: &RefIdx,
    ) -> Fallible<E> {
        Ok(())
    }

    fn traverse_assignment(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _to: &RefIdx,
        _from: &RefIdx,
    ) -> Fallible<E> {
        Ok(())
    }

    fn traverse_call(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _to: &RefIdx,
        _generics: &[RefIdx],
        _args: &[RefIdx],
    ) -> Fallible<E> {
        Ok(())
    }

    fn traverse_statements(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _stmts: &[RefIdx],
    ) -> Fallible<E> {
        Ok(())
    }

    fn traverse_condition(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _condition: &RefIdx,
        _true_block: &RefIdx,
        _false_block: &Option<RefIdx>,
    ) -> Fallible<E> {
        Ok(())
    }

    fn traverse_loop(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _condition: &RefIdx,
        _block: &RefIdx,
    ) -> Fallible<E> {
        Ok(())
    }

    fn traverse_return(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _expr: &Option<RefIdx>,
    ) -> Fallible<E> {
        Ok(())
    }

    fn traverse_node(&mut self, fir: &Fir<T>, node: &Node<T>) -> Fallible<E> {
        match &node.kind {
            Kind::Constant(c) => self.traverse_constant(fir, node, c),
            Kind::TypeReference(r) => self.traverse_type_reference(fir, node, r),
            Kind::NodeRef { value, ty } => self.traverse_typed_value(fir, node, value, ty),
            Kind::Generic { default } => self.traverse_generic(fir, node, default),
            Kind::RecordType { generics, fields } => {
                self.traverse_record_type(fir, node, generics, fields)
            }
            Kind::UnionType { generics, variants } => {
                self.traverse_union_type(fir, node, generics, variants)
            }
            Kind::Function {
                generics,
                args,
                return_type,
                block,
            } => self.traverse_function(fir, node, generics, args, return_type, block),
            Kind::Assignment { to, from } => self.traverse_assignment(fir, node, to, from),
            Kind::Binding { to } => self.traverse_binding(fir, node, to),
            Kind::Instantiation {
                to,
                generics,
                fields,
            } => self.traverse_instantiation(fir, node, to, generics, fields),
            Kind::TypeOffset { instance, field } => {
                self.traverse_type_offset(fir, node, instance, field)
            }
            Kind::Call { to, generics, args } => self.traverse_call(fir, node, to, generics, args),
            Kind::Statements(stmts) => self.traverse_statements(fir, node, stmts),
            Kind::Conditional {
                condition,
                true_block,
                false_block,
            } => self.traverse_condition(fir, node, condition, true_block, false_block),
            Kind::Loop { condition, block } => self.traverse_loop(fir, node, condition, block),
            Kind::Return(expr) => self.traverse_return(fir, node, expr),
        }
    }

    fn traverse(&mut self, fir: &Fir<T>) -> Fallible<Vec<E>> {
        let errs = fir
            .nodes
            .values()
            .fold(Vec::new(), |mut errs: Vec<E>, node| {
                match self.traverse_node(fir, node) {
                    Ok(_) => errs,
                    Err(e) => {
                        errs.push(e);
                        errs
                    }
                }
            });

        if errs.is_empty() {
            Ok(())
        } else {
            Err(errs)
        }
    }
}
