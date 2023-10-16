pub mod instance;

use std::collections::HashMap;

use instance::Instance;

// FIXME: Missing doc

use fir::{Fir, Kind, Node, OriginIdx, RefIdx};
use flatten::FlattenData;

pub trait Interpret {
    fn interpret(&self) -> Option<Instance>;
}

impl Interpret for Fir<FlattenData<'_>> {
    fn interpret(&self) -> Option<Instance> {
        let mut fire = Fire {
            values: HashMap::new(),
        };

        // Start the fire >:)
        fire.start(self)
    }
}

// FIXME: How do we deal with the fact that we're acting on a Flat representation?
// FIXME: Is the [`Fir`] stable? Can we just access the last node as the entry point to the [`Fir`]?
// then it makes everything super easy actually
struct Fire {
    // this will also take care of garbage collection so we must be careful
    values: HashMap<OriginIdx, Instance>,
}

impl Fire {
    // these two functions should probalby be part of a gc struct, which should be contained within the Fire
    fn allocate(&mut self, key: OriginIdx, value: Instance) {
        // if we allocate the same value twice, this is an interpreter error
        // FIXME: this is not specifically true - e.g. calling a function twice will allocate twice to the function's bindings, which is fine?
        self.values.insert(key, value);
    }

    fn copy(&mut self, to_copy: &RefIdx, key: OriginIdx) {
        if let Some(instance) = self.values.get(&to_copy.expect_resolved()) {
            self.allocate(key, instance.clone())
        }
    }

    fn transfer(&mut self, to_move: &RefIdx, key: OriginIdx) {
        self.copy(to_move, key)
    }

    fn lookup(&self, key: &OriginIdx) -> Option<&Instance> {
        self.values.get(key)
    }

    // TODO: Move outside of the impl block?
    fn perform_extern_call(
        &self,
        _fir: &Fir<FlattenData<'_>>,
        node: &Node<FlattenData<'_>>,
        args: &[RefIdx],
    ) -> Option<Instance> {
        let ast = node.data.ast.node();
        let name = match &ast.node {
            ast::Node::Function {
                decl: ast::Declaration { name, .. },
                ..
            } => name,
            other => {
                dbg!(other);
                unreachable!()
            }
        };

        if name.access() == "println" {
            args.iter().for_each(|arg| {
                let value = self.values.get(&arg.expect_resolved()).unwrap();
                if let Instance::String(s) = value {
                    println!("{s}");
                } else {
                    unreachable!("typecheck didn't catch this error. this is an interpreter bug.");
                }
            })
        }

        None
    }

    fn fire_block(
        &mut self,
        fir: &Fir<FlattenData<'_>>,
        node: &Node<FlattenData<'_>>,
        stmts: &[RefIdx],
    ) {
        // How do we deal with returns in this system?
        stmts.iter().for_each(|node| {
            let node = &fir.nodes[&node.expect_resolved()];
            self.fire_node(fir, node);
        });

        if let Some(last_stmt) = stmts.last() {
            if let Kind::Return(_) = &fir.nodes[&last_stmt.expect_resolved()].kind {
                self.transfer(last_stmt, node.origin)
            }
        }
    }

    // FIXME: Does this need "_c"?
    fn fire_constant(&mut self, node: &Node<FlattenData<'_>>, _c: &RefIdx) {
        use ast::{Node, Value};

        let ast = node.data.ast.node();

        let instance = match &ast.node {
            Node::Constant(Value::Integer(value)) => Instance::from(*value),
            Node::Constant(Value::Str(s)) => Instance::from(s),
            Node::Constant(Value::Bool(b)) => Instance::from(b),
            _ => unreachable!(),
        };

        self.allocate(node.origin, instance);
    }

    fn fire_call(
        &mut self,
        fir: &Fir<FlattenData<'_>>,
        node: &Node<FlattenData<'_>>,
        to: &RefIdx,
        args: &[RefIdx],
    ) {
        let def = &fir.nodes[&to.expect_resolved()];
        let (block, def_args) = match &def.kind {
            Kind::Function { block, args, .. } => (block, args),
            _ => unreachable!(),
        };

        args.iter().enumerate().for_each(|(i, arg)| {
            self.fire_node(fir, &fir.nodes[&arg.expect_resolved()]);
            self.transfer(arg, def_args[i].expect_resolved());
        });

        // FIXME: We need to add bindings here between the function's variables and the arguments given to the call
        match block {
            None => {
                let result = self.perform_extern_call(fir, def, args);
                if let Some(instance) = result {
                    self.allocate(node.origin, instance)
                }
            }
            Some(block) => {
                self.fire_node_ref(fir, block); // what to do here?
                self.transfer(block, node.origin);
            }
        }
    }

    fn fire_binding(
        &mut self,
        fir: &Fir<FlattenData<'_>>,
        node: &Node<FlattenData<'_>>,
        to: &RefIdx,
    ) {
        self.fire_node_ref(fir, to);

        self.transfer(to, node.origin);
    }

    fn fire_typed_value(
        &mut self,
        fir: &Fir<FlattenData<'_>>,
        node: &Node<FlattenData<'_>>,
        value: &RefIdx,
        ty: &RefIdx,
    ) {
        // what do we do here when we have a `value` but no `ty`?
        match ty {
            // this is a transfer
            RefIdx::Unresolved => self.transfer(value, node.origin),
            // this is an allocate?
            RefIdx::Resolved(ty) => {
                let tyref = &fir.nodes[ty];
                let fields = match &tyref.kind {
                    Kind::Type { fields, .. } => fields,
                    // FIXME: here we need to decide part of our copy/move semantics
                    Kind::TypeReference(_) => return,
                    other => {
                        dbg!(other);
                        unreachable!()
                    }
                };

                // TODO: can we just check if value == ty?
                let instance = if fields.is_empty() {
                    Instance::empty()
                } else {
                    unreachable!()
                };

                // FIXME: Handle result here
                // FIXME: Should this be a transfer?
                self.allocate(node.origin, instance);
            }
        }
    }

    fn fire_return(
        &mut self,
        fir: &Fir<FlattenData<'_>>,
        node: &Node<FlattenData<'_>>,
        expr: &Option<RefIdx>,
    ) {
        if let Some(returned) = expr {
            self.fire_node_ref(fir, returned);

            self.transfer(returned, node.origin);
        } // FIXME: Allocate None otherwise?
    }

    fn fire_node_ref(&mut self, fir: &Fir<FlattenData<'_>>, node_ref: &RefIdx) {
        let node = &fir.nodes[&node_ref.expect_resolved()];

        self.fire_node(fir, node)
    }

    fn fire_node(&mut self, fir: &Fir<FlattenData<'_>>, node: &Node<FlattenData<'_>>) {
        match &node.kind {
            Kind::Constant(c) => self.fire_constant(node, c),
            Kind::Statements(stmts) => self.fire_block(fir, node, stmts),
            Kind::Call {
                to,
                args,
                .. /* FIXME: Generics should be empty at this point */
            } => self.fire_call(fir, node, to, args),
            Kind::Binding { to } => self.fire_binding(fir, node, to),
            Kind::TypedValue { value, ty } => self.fire_typed_value(fir, node, value, ty),
            Kind::Return(expr) => self.fire_return(fir, node, expr),
            // Kind::TypeReference(r) => self.traverse_type_reference(fir, node, r),
            // Kind::Generic { default } => self.traverse_generic(fir, node, default),
            // Kind::Type { generics, fields } => self.traverse_type(fir, node, generics, fields),
            // Kind::Function {
            //     generics,
            //     args,
                // return_type,
            //     block,
            // } => self.traverse_function(fir, node, generics, args, return_type, block),
            // Kind::Assignment { to, from } => self.traverse_assignment(fir, node, to, from),
            // Kind::Instantiation {
            //     to,
            //     generics,
            //     fields,
            // } => self.traverse_instantiation(fir, node, to, generics, fields),
            // Kind::TypeOffset { instance, field } => {
            //     self.traverse_type_offset(fir, node, instance, field)
            // }
            // Kind::Conditional {
            //     condition,
            //     true_block,
            //     false_block,
            // } => self.traverse_condition(fir, node, condition, true_block, false_block),
            // Kind::Loop { condition, block } => self.traverse_loop(fir, node, condition, block),
            _ => {},
        }
    }

    fn start(&mut self, fir: &Fir<FlattenData<'_>>) -> Option<Instance> {
        // FIXME: No unwrap here
        let entry_point = fir.nodes.last_key_value().unwrap();
        let entry_point = &entry_point.1;
        let stmts = match &entry_point.kind {
            Kind::Statements(stmts) => stmts,
            _ => unreachable!(
                "{}:{}: expected list of statements for node {entry_point:?}. this is an interpreter error.",
                file!(),
                line!(),
            ),
        };

        self.fire_block(fir, entry_point, stmts);

        self.lookup(&entry_point.origin).cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use flatten::FlattenAst;
    use name_resolve::NameResolve;
    use typecheck::TypeCheck;

    macro_rules! ast {
        ($($toks:tt)*) => {
            xparser::ast!(
                type char;
                type bool;
                type int;
                type float;
                type string;
                $($toks)*
            )
        }
    }

    macro_rules! fir {
        ($ast:expr) => {
            $ast.flatten().name_resolve().unwrap().type_check().unwrap()
        };
    }

    #[test]
    fn last_value() {
        let ast = ast! {
            "jinko"
        };

        let result = fir!(ast).interpret();
        assert_eq!(result, Some(Instance::from("jinko")))
    }

    #[test]
    fn call() {
        let ast = ast! {
            func id(x: int) -> int { x }

            id(15)
        };

        let result = fir!(ast).interpret();
        assert_eq!(result, Some(Instance::from(15)))
    }

    #[test]
    fn nested_call() {
        let ast = ast! {
            func id(x: string) -> string { x }

            id(id(id(id("jinko"))))
        };

        let result = fir!(ast).interpret();
        assert_eq!(result, Some(Instance::from("jinko")))
    }
}
