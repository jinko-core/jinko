use crate::{Fallible, Fir, IterError, Kind, Node, RefIdx};

use std::fmt::Debug;

/// All of the helpers and visitors call back into [`visit_node`]
pub trait Visitor<T: Debug, E: IterError> {
    fn visit_constant(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _constant: &RefIdx,
    ) -> Fallible<E> {
        Ok(())
    }

    fn visit_type_reference(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _reference: &RefIdx,
    ) -> Fallible<E> {
        Ok(())
    }

    fn visit_typed_value(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _value: &RefIdx,
        _ty: &RefIdx,
    ) -> Fallible<E> {
        Ok(())
    }

    fn visit_generic(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _default: &Option<RefIdx>,
    ) -> Fallible<E> {
        Ok(())
    }

    fn visit_type(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _generics: &[RefIdx],
        _fields: &[RefIdx],
    ) -> Fallible<E> {
        Ok(())
    }

    fn visit_function(
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

    fn visit_instantiation(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _to: &RefIdx,
        _generics: &[RefIdx],
        _fields: &[RefIdx],
    ) -> Fallible<E> {
        Ok(())
    }

    fn visit_call(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _to: &RefIdx,
        _generics: &[RefIdx],
        _args: &[RefIdx],
    ) -> Fallible<E> {
        Ok(())
    }

    fn visit_statements(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _stmts: &[RefIdx],
    ) -> Fallible<E> {
        Ok(())
    }

    fn visit_return(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _expr: &Option<RefIdx>,
    ) -> Fallible<E> {
        Ok(())
    }

    fn visit_node(&mut self, fir: &Fir<T>, node: &Node<T>) -> Fallible<E> {
        match &node.kind {
            Kind::Constant(c) => self.visit_constant(fir, node, c),
            Kind::TypeReference(r) => self.visit_type_reference(fir, node, r),
            Kind::TypedValue { value, ty } => self.visit_typed_value(fir, node, value, ty),
            Kind::Generic { default } => self.visit_generic(fir, node, default),
            Kind::Type { generics, fields } => self.visit_type(fir, node, generics, fields),
            Kind::Function {
                generics,
                args,
                return_type,
                block,
            } => self.visit_function(fir, node, generics, args, return_type, block),
            Kind::Instantiation {
                to,
                generics,
                fields,
            } => self.visit_instantiation(fir, node, to, generics, fields),
            Kind::Call { to, generics, args } => self.visit_call(fir, node, to, generics, args),
            Kind::Statements(stmts) => self.visit_statements(fir, node, stmts),
            Kind::Return(expr) => self.visit_return(fir, node, expr),
        }
    }

    fn visit(&mut self, fir: &Fir<T>) -> Fallible<E> {
        let errs = fir
            .nodes
            .values()
            .fold(Vec::new(), |mut errs: Vec<E>, node| {
                match self.visit_node(fir, node) {
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
            Err(E::aggregate(errs))
        }
    }
}
