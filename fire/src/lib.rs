//! [`Fire`] stands for [`Fir`] Executor, or [`Fir`] Engine. Its role is to interpret code represented in an [`Fir`].

// TODO:
//     How do we deal with returns in this system?
//     How do we deal with them when they are nested in an `if` block or equivalent? Something like the `ControlFlow` enum as a return value? Either keep going or return until RefIdx?
//     Keep information in the context? (`self`)
// TODO: Transform all `RefIdx` to `OriginIdx` before this? Add a trait extension to make RefIdx: Deref<OriginIdx> specifically here?

pub mod instance;

use std::collections::HashMap;

use instance::Instance;

use fir::{Fir, Kind, Node, OriginIdx, RefIdx};
use flatten::FlattenData;

pub trait Interpret {
    fn interpret(&self) -> Option<Instance>;
}

impl Interpret for Fir<FlattenData<'_>> {
    fn interpret(&self) -> Option<Instance> {
        let mut fire = Fire {
            gc: GarbaJKollector::new(),
            fir: self,
        };

        // Start the fire >:)
        fire.start()
    }
}

/// It's called [`GarbajKollector`] because this way you have "jk" in the middle of the word :)
struct GarbaJKollector(HashMap<OriginIdx, Instance>);

// FIXME: Add documentation for all methods
impl GarbaJKollector {
    fn new() -> GarbaJKollector {
        GarbaJKollector(HashMap::new())
    }

    fn allocate(&mut self, key: OriginIdx, value: Instance) {
        // if we allocate the same value twice, this is an interpreter error
        // FIXME: this is not specifically true - e.g. calling a function twice will allocate twice to the function's bindings, which is fine?
        self.0.insert(key, value);
    }

    fn copy(&mut self, to_copy: &RefIdx, key: OriginIdx) {
        if let Some(instance) = self.0.get(&to_copy.expect_resolved()) {
            self.allocate(key, instance.clone())
        }
    }

    fn transfer(&mut self, to_move: &RefIdx, key: OriginIdx) {
        self.copy(to_move, key)
    }

    fn lookup(&self, key: &OriginIdx) -> Option<&Instance> {
        self.0.get(key)
    }
}

struct Fire<'ast, 'fir> {
    // this will also take care of garbage collection so we must be careful
    gc: GarbaJKollector,
    // TODO: Store a &Fir here
    fir: &'fir Fir<FlattenData<'ast>>,
}

// Keep that on the back burner for now on... this works, but since the API might change a lot especially vis a vis return types for `fire_*` functions,
// we shouldn't use it yet
// struct Access<'ctx, 'ast, 'fir>(&'ctx mut Fire<'ast, 'fir>, &'fir Node<FlattenData<'ast>>);

// impl<'ctx, 'ast, 'fir> Access<'ctx, 'ast, 'fir> {
//     fn fire(&mut self) {
//         self.0.fire_node(self.1)
//     }
// }

impl<'ast, 'fir> Fire<'ast, 'fir> {
    // FIXME: Documentation
    // TODO: Rename? `node`? `node_from_ref`? `rnode`? `ref`? `view`?
    // should this return an `AccessedNode` or w/ever which we can then `.fire()`?
    fn access(&self, r: &RefIdx) -> &'fir Node<FlattenData<'ast>> {
        &self.fir.nodes[&r.expect_resolved()]
    }

    // TODO: Rename: `access_origin`?
    fn access_resolved(&self, resolved: &OriginIdx) -> &'fir Node<FlattenData<'ast>> {
        &self.fir.nodes[&resolved]
    }

    // TODO: Move outside of the impl block?
    fn perform_extern_call(
        &self,
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
                let value = self.gc.lookup(&arg.expect_resolved()).unwrap();
                if let Instance::String(s) = value {
                    println!("{s}");
                } else {
                    unreachable!("typecheck didn't catch this error. this is an interpreter bug.");
                }
            })
        }

        None
    }

    fn fire_block(&mut self, node: &Node<FlattenData<'_>>, stmts: &[RefIdx]) {
        stmts.iter().for_each(|node| {
            self.fire_node(self.access(node));
        });

        if let Some(last_stmt) = stmts.last() {
            if let Kind::Return(_) = self.access(last_stmt).kind {
                self.gc.transfer(last_stmt, node.origin)
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

        self.gc.allocate(node.origin, instance);
    }

    fn fire_call(&mut self, node: &Node<FlattenData<'_>>, to: &RefIdx, args: &[RefIdx]) {
        let def = self.access(to);
        let (block, def_args) = match &def.kind {
            Kind::Function { block, args, .. } => (block, args),
            _ => unreachable!(),
        };

        args.iter().enumerate().for_each(|(i, arg)| {
            self.fire_node(self.access(arg));
            self.gc.transfer(arg, def_args[i].expect_resolved());
        });

        // FIXME: We need to add bindings here between the function's variables and the arguments given to the call
        match block {
            None => {
                let result = self.perform_extern_call(def, args);
                if let Some(instance) = result {
                    self.gc.allocate(node.origin, instance)
                }
            }
            Some(block) => {
                self.fire_node_ref(block); // what to do here?
                self.gc.transfer(block, node.origin);
            }
        }
    }

    fn fire_binding(&mut self, node: &Node<FlattenData<'_>>, to: &RefIdx) {
        self.fire_node_ref(to);

        self.gc.transfer(to, node.origin);
    }

    fn fire_typed_value(&mut self, node: &Node<FlattenData<'_>>, value: &RefIdx, ty: &RefIdx) {
        // what do we do here when we have a `value` but no `ty`?
        match ty {
            // this is a transfer
            RefIdx::Unresolved => self.gc.transfer(value, node.origin),
            // this is an allocate?
            RefIdx::Resolved(ty) => {
                let tyref = self.access_resolved(ty);
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
                self.gc.allocate(node.origin, instance);
            }
        }
    }

    fn fire_return(&mut self, node: &Node<FlattenData<'_>>, expr: &Option<RefIdx>) {
        if let Some(returned) = expr {
            self.fire_node_ref(returned);

            self.gc.transfer(returned, node.origin);
        } // FIXME: Allocate None otherwise?
    }

    fn fire_node_ref(&mut self, node_ref: &RefIdx) {
        self.fire_node(self.access(node_ref))
    }

    fn fire_node(&mut self, node: &Node<FlattenData<'_>>) {
        match &node.kind {
            Kind::Constant(c) => self.fire_constant(node, c),
            Kind::Statements(stmts) => self.fire_block( node, stmts),
            Kind::Call {
                to,
                args,
                .. /* FIXME: Generics should be empty at this point */
            } => self.fire_call( node, to, args),
            Kind::Binding { to } => self.fire_binding( node, to),
            Kind::TypedValue { value, ty } => self.fire_typed_value( node, value, ty),
            Kind::Return(expr) => self.fire_return( node, expr),
            // Kind::TypeReference(r) => self.traverse_type_reference( node, r),
            // Kind::Generic { default } => self.traverse_generic( node, default),
            // Kind::Type { generics, fields } => self.traverse_type( node, generics, fields),
            // Kind::Function {
            //     generics,
            //     args,
                // return_type,
            //     block,
            // } => self.traverse_function( node, generics, args, return_type, block),
            // Kind::Assignment { to, from } => self.traverse_assignment( node, to, from),
            // Kind::Instantiation {
            //     to,
            //     generics,
            //     fields,
            // } => self.traverse_instantiation( node, to, generics, fields),
            // Kind::TypeOffset { instance, field } => {
            //     self.traverse_type_offset( node, instance, field)
            // }
            // Kind::Conditional {
            //     condition,
            //     true_block,
            //     false_block,
            // } => self.traverse_condition( node, condition, true_block, false_block),
            // Kind::Loop { condition, block } => self.traverse_loop( node, condition, block),
            _ => {},
        }
    }

    fn start(&mut self) -> Option<Instance> {
        let entry_point = self.fir.nodes.last_key_value()?;
        let entry_point = &entry_point.1;
        let stmts = match &entry_point.kind {
            Kind::Statements(stmts) => stmts,
            _ => unreachable!(
                "{}:{}: expected list of statements for node {entry_point:?}. this is an interpreter error.",
                file!(),
                line!(),
            ),
        };

        self.fire_block(entry_point, stmts);

        self.gc.lookup(&entry_point.origin).cloned()
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
