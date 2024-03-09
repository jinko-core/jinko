use crate::{Fir, Kind, Node, OriginIdx, RefIdx};

// FIXME: Should we still be able to emit errors?
pub trait TreeLike<'ast, 'fir, T> {
    fn visit_many(&mut self, fir: &Fir<T>, many: &[RefIdx]) {
        many.iter().for_each(|r| self.visit_reference(fir, r))
    }

    fn visit_optional(&mut self, fir: &Fir<T>, reference: &Option<RefIdx>) {
        if let Some(r) = reference {
            self.visit_reference(fir, r)
        }
    }

    fn visit_reference(&mut self, fir: &Fir<T>, reference: &RefIdx) {
        self.visit(fir, &reference.expect_resolved())
    }

    fn visit_constant(&mut self, fir: &Fir<T>, _node: &Node<T>, c: &RefIdx) {
        self.visit_reference(fir, c)
    }

    fn visit_type_reference(&mut self, fir: &Fir<T>, _node: &Node<T>, to: &RefIdx) {
        self.visit_reference(fir, to)
    }

    fn visit_typed_value(&mut self, fir: &Fir<T>, _node: &Node<T>, value: &RefIdx, ty: &RefIdx) {
        self.visit_reference(fir, value);
        self.visit_reference(fir, ty);
    }

    fn visit_generic(&mut self, fir: &Fir<T>, _node: &Node<T>, default: &Option<RefIdx>) {
        self.visit_optional(fir, default)
    }

    fn visit_record_type(
        &mut self,
        fir: &Fir<T>,
        _node: &Node<T>,
        generics: &[RefIdx],
        fields: &[RefIdx],
    ) {
        self.visit_many(fir, generics);
        self.visit_many(fir, fields);
    }

    fn visit_union_type(
        &mut self,
        fir: &Fir<T>,
        _node: &Node<T>,
        generics: &[RefIdx],
        variants: &[RefIdx],
    ) {
        self.visit_many(fir, generics);
        self.visit_many(fir, variants);
    }

    fn visit_function(
        &mut self,
        fir: &Fir<T>,
        _node: &Node<T>,
        generics: &[RefIdx],
        args: &[RefIdx],
        return_type: &Option<RefIdx>,
        block: &Option<RefIdx>,
    ) {
        self.visit_many(fir, generics);
        self.visit_many(fir, args);
        self.visit_optional(fir, return_type);
        self.visit_optional(fir, block);
    }

    fn visit_binding(&mut self, fir: &Fir<T>, _node: &Node<T>, to: &RefIdx) {
        self.visit_reference(fir, to)
    }

    fn visit_assignment(&mut self, fir: &Fir<T>, _node: &Node<T>, to: &RefIdx, from: &RefIdx) {
        self.visit_reference(fir, to);
        self.visit_reference(fir, from);
    }

    fn visit_instantiation(
        &mut self,
        fir: &Fir<T>,
        _node: &Node<T>,
        to: &RefIdx,
        generics: &[RefIdx],
        fields: &[RefIdx],
    ) {
        self.visit_reference(fir, to);
        self.visit_many(fir, generics);
        self.visit_many(fir, fields);
    }

    fn visit_type_offset(
        &mut self,
        fir: &Fir<T>,
        _node: &Node<T>,
        instance: &RefIdx,
        field: &RefIdx,
    ) {
        self.visit_reference(fir, instance);
        self.visit_reference(fir, field);
    }

    fn visit_call(
        &mut self,
        fir: &Fir<T>,
        _node: &Node<T>,
        to: &RefIdx,
        generics: &[RefIdx],
        args: &[RefIdx],
    ) {
        self.visit_reference(fir, to);
        self.visit_many(fir, generics);
        self.visit_many(fir, args);
    }

    fn visit_conditional(
        &mut self,
        fir: &Fir<T>,
        _node: &Node<T>,
        condition: &RefIdx,
        true_block: &RefIdx,
        false_block: &Option<RefIdx>,
    ) {
        self.visit_reference(fir, condition);
        self.visit_reference(fir, true_block);
        self.visit_optional(fir, false_block);
    }

    fn visit_loop(&mut self, fir: &Fir<T>, _node: &Node<T>, condition: &RefIdx, block: &RefIdx) {
        self.visit_reference(fir, condition);
        self.visit_reference(fir, block);
    }

    fn visit_statements(&mut self, fir: &Fir<T>, _node: &Node<T>, stmts: &[RefIdx]) {
        self.visit_many(fir, stmts)
    }

    fn visit_return(&mut self, fir: &Fir<T>, _node: &Node<T>, value: &Option<RefIdx>) {
        self.visit_optional(fir, value)
    }

    fn visit(&mut self, fir: &Fir<T>, start: &OriginIdx) {
        let node = &fir[start];

        match &node.kind {
            Kind::Constant(c) => self.visit_constant(fir, node, c),
            Kind::TypeReference(to) => self.visit_type_reference(fir, node, to),
            Kind::TypedValue { value, ty } => self.visit_typed_value(fir, node, value, ty),
            Kind::Generic { default } => self.visit_generic(fir, node, default),
            Kind::RecordType { generics, fields } => {
                self.visit_record_type(fir, node, generics, fields)
            }
            Kind::UnionType { generics, variants } => {
                self.visit_union_type(fir, node, generics, variants)
            }
            Kind::Function {
                generics,
                args,
                return_type,
                block,
            } => self.visit_function(fir, node, generics, args, return_type, block),
            Kind::Binding { to } => self.visit_binding(fir, node, to),
            Kind::Assignment { to, from } => self.visit_assignment(fir, node, to, from),
            Kind::Instantiation {
                to,
                generics,
                fields,
            } => self.visit_instantiation(fir, node, to, generics, fields),
            Kind::TypeOffset { instance, field } => {
                self.visit_type_offset(fir, node, instance, field)
            }
            Kind::Call { to, generics, args } => self.visit_call(fir, node, to, generics, args),
            Kind::Conditional {
                condition,
                true_block,
                false_block,
            } => self.visit_conditional(fir, node, condition, true_block, false_block),
            Kind::Loop { condition, block } => self.visit_loop(fir, node, condition, block),
            Kind::Statements(stmts) => self.visit_statements(fir, node, stmts),
            Kind::Return(value) => self.visit_return(fir, node, value),
        }
    }
}
