use crate::{Fir, IterError, Kind, Node, OriginIdx, RefIdx};

// TODO: Probably the last Fir trait we need is a `MultiMapper` trait which returns `Result<Vec<Node<U>>, E>`s
pub trait Mapper<T, U: Default, E: IterError> {
    fn map_constant(
        &mut self,
        _data: T,
        origin: OriginIdx,
        constant: RefIdx,
    ) -> Result<Node<U>, E> {
        Ok(Node {
            data: U::default(),
            origin,
            kind: Kind::Constant(constant),
        })
    }

    fn map_type_reference(
        &mut self,
        _data: T,
        origin: OriginIdx,
        reference: RefIdx,
    ) -> Result<Node<U>, E> {
        Ok(Node {
            data: U::default(),
            origin,
            kind: Kind::TypeReference(reference),
        })
    }

    fn map_typed_value(
        &mut self,
        _data: T,
        origin: OriginIdx,
        value: RefIdx,
        ty: RefIdx,
    ) -> Result<Node<U>, E> {
        Ok(Node {
            data: U::default(),
            origin,
            kind: Kind::TypedValue { value, ty },
        })
    }

    fn map_generic(
        &mut self,
        _data: T,
        origin: OriginIdx,
        default: Option<RefIdx>,
    ) -> Result<Node<U>, E> {
        Ok(Node {
            data: U::default(),
            origin,
            kind: Kind::Generic { default },
        })
    }

    fn map_type(
        &mut self,
        _data: T,
        origin: OriginIdx,
        generics: Vec<RefIdx>,
        fields: Vec<RefIdx>,
    ) -> Result<Node<U>, E> {
        Ok(Node {
            data: U::default(),
            origin,
            kind: Kind::Type { generics, fields },
        })
    }

    fn map_function(
        &mut self,
        _data: T,
        origin: OriginIdx,
        generics: Vec<RefIdx>,
        args: Vec<RefIdx>,
        return_type: Option<RefIdx>,
        block: Option<RefIdx>,
    ) -> Result<Node<U>, E> {
        Ok(Node {
            data: U::default(),
            origin,
            kind: Kind::Function {
                generics,
                args,
                return_type,
                block,
            },
        })
    }

    fn map_instantiation(
        &mut self,
        _data: T,
        origin: OriginIdx,
        to: RefIdx,
        generics: Vec<RefIdx>,
        fields: Vec<RefIdx>,
    ) -> Result<Node<U>, E> {
        Ok(Node {
            data: U::default(),
            origin,
            kind: Kind::Instantiation {
                to,
                generics,
                fields,
            },
        })
    }

    fn map_type_offset(
        &mut self,
        _data: T,
        origin: OriginIdx,
        instance: RefIdx,
        field: RefIdx,
    ) -> Result<Node<U>, E> {
        Ok(Node {
            data: U::default(),
            origin,
            kind: Kind::TypeOffset { instance, field },
        })
    }

    fn map_assignment(
        &mut self,
        _data: T,
        origin: OriginIdx,
        to: RefIdx,
        from: RefIdx,
    ) -> Result<Node<U>, E> {
        Ok(Node {
            data: U::default(),
            origin,
            kind: Kind::Assignment { to, from },
        })
    }

    fn map_call(
        &mut self,
        _data: T,
        origin: OriginIdx,
        to: RefIdx,
        generics: Vec<RefIdx>,
        args: Vec<RefIdx>,
    ) -> Result<Node<U>, E> {
        Ok(Node {
            data: U::default(),
            origin,
            kind: Kind::Call { to, generics, args },
        })
    }

    fn map_statements(
        &mut self,
        _data: T,
        origin: OriginIdx,
        stmts: Vec<RefIdx>,
    ) -> Result<Node<U>, E> {
        Ok(Node {
            data: U::default(),
            origin,
            kind: Kind::Statements(stmts),
        })
    }

    fn map_condition(
        &mut self,
        _data: T,
        origin: OriginIdx,
        condition: RefIdx,
        true_block: RefIdx,
        false_block: Option<RefIdx>,
    ) -> Result<Node<U>, E> {
        Ok(Node {
            data: U::default(),
            origin,
            kind: Kind::Conditional {
                condition,
                true_block,
                false_block,
            },
        })
    }

    fn map_loop(
        &mut self,
        _data: T,
        origin: OriginIdx,
        condition: RefIdx,
        block: RefIdx,
    ) -> Result<Node<U>, E> {
        Ok(Node {
            data: U::default(),
            origin,
            kind: Kind::Loop { condition, block },
        })
    }

    fn map_return(
        &mut self,
        _data: T,
        origin: OriginIdx,
        expr: Option<RefIdx>,
    ) -> Result<Node<U>, E> {
        Ok(Node {
            data: U::default(),
            origin,
            kind: Kind::Return(expr),
        })
    }

    fn map_node(&mut self, node: Node<T>) -> Result<Node<U>, E> {
        match node.kind {
            Kind::Constant(c) => self.map_constant(node.data, node.origin, c),
            Kind::TypeReference(r) => self.map_type_reference(node.data, node.origin, r),
            Kind::TypedValue { value, ty } => {
                self.map_typed_value(node.data, node.origin, value, ty)
            }
            Kind::Generic { default } => self.map_generic(node.data, node.origin, default),
            Kind::Type { generics, fields } => {
                self.map_type(node.data, node.origin, generics, fields)
            }
            Kind::Function {
                generics,
                args,
                return_type,
                block,
            } => self.map_function(node.data, node.origin, generics, args, return_type, block),
            Kind::Instantiation {
                to,
                generics,
                fields,
            } => self.map_instantiation(node.data, node.origin, to, generics, fields),
            Kind::TypeOffset { instance, field } => {
                self.map_type_offset(node.data, node.origin, instance, field)
            }
            Kind::Assignment { to, from } => self.map_assignment(node.data, node.origin, to, from),
            Kind::Call { to, generics, args } => {
                self.map_call(node.data, node.origin, to, generics, args)
            }
            Kind::Statements(stmts) => self.map_statements(node.data, node.origin, stmts),
            Kind::Conditional {
                condition,
                true_block,
                false_block,
            } => self.map_condition(node.data, node.origin, condition, true_block, false_block),
            Kind::Loop { condition, block } => {
                self.map_loop(node.data, node.origin, condition, block)
            }
            Kind::Return(expr) => self.map_return(node.data, node.origin, expr),
        }
    }

    fn map(&mut self, fir: Fir<T>) -> Result<Fir<U>, E> {
        let (fir, errs) = fir.nodes.into_values().fold(
            (Fir::default(), Vec::new()),
            |(new_fir, mut errs), node| match self.map_node(node) {
                Ok(node) => (new_fir.append(node), errs),
                Err(e) => {
                    errs.push(e);
                    (new_fir, errs)
                }
            },
        );

        if errs.is_empty() {
            Ok(fir)
        } else {
            Err(E::aggregate(errs))
        }
    }
}