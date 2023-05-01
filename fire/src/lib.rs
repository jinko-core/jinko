use std::collections::HashMap;

// FIXME: Missing doc
// FIXME: Execution is very tree-like, isn't it
// FIXME: How do we get the last value of a Statements?
// FIXME: How do Returns work in this system?
// FIXME: How do we use the value returned by a function call for example?
// where x = id(15);

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

// FIXME: This is invalid
// what's a type? at runtime?
// just the hash of the type? -> that's good enough
//     what's the hash of a type?
// for a string -> the hash of this string
// for a char/int/bool -> the actual value (an i64)
// for a float -> eeeeeeeh? typecheck error?
//     introduce a safe-float type in the stdlib?
// for other types -> needs to be a unique hash -> based on source location and FirId?
type Type = &'static str;

// an instance needs to be unique
// needs to be hashable
// we need the type of the value - it needs to be known at all times
// FIXME: We can probably improve this type by specializing it more - turning it into a sum type differentiating between
// FIXME: We need to be very careful about what a "Clone" means here
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Instance {
    pub(crate) ty: Type,
    pub(crate) data: Vec<u8>,
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
        assert!(self.values.insert(key, value).is_none());
    }

    fn copy(&mut self, to_copy: &RefIdx, key: OriginIdx) {
        if let Some(instance) = self.values.get(&to_copy.unwrap()) {
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
        node: &Node<FlattenData<'_>>, // allocate a value for this node's origin
        args: &[RefIdx],
    ) {
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
                let value = self.values.get(&arg.unwrap()).unwrap();
                // FIXME: Ugly as SIN
                println!("{}", unsafe {
                    String::from_utf8_unchecked(value.data.clone())
                });
            })
        }

        // this should allocate data

        // None
    }

    // FIXME: Sholud this return a Result<Option<Instance>, Error>?
    fn fire_block(
        &mut self,
        fir: &Fir<FlattenData<'_>>,
        _node: &Node<FlattenData<'_>>,
        stmts: &[RefIdx],
    ) {
        // How do we deal with returns in this system?
        stmts.iter().for_each(|node| {
            let node = &fir.nodes[&node.unwrap()];
            self.fire_node(fir, node);
        });

        // FIXME: This is invalid, isn't it?
        // FIXME: or should we just return () here?
        // FIXME: If the last value is a return, we need to have this node's ID refer to the value as well
    }

    // FIXME: This can return an Instance, right?
    // FIXME: Does this need "self"?
    // FIXME: Does this need "fir"?
    fn fire_constant(
        &mut self,
        _fir: &Fir<FlattenData<'_>>,
        node: &Node<FlattenData<'_>>,
        _c: &RefIdx,
    ) {
        let ast = node.data.ast.node();

        let instance = match &ast.node {
            ast::Node::Constant(ast::Value::Integer(value)) => Instance {
                ty: "int",
                // origin: node.origin,
                data: value.to_le_bytes().to_vec(),
            },
            ast::Node::Constant(ast::Value::Str(s)) => Instance {
                ty: "string",
                // orgin: node.origin,
                data: s.clone().into_bytes(),
            },
            _ => unreachable!(),
        };

        self.allocate(node.origin, instance);
    }

    fn fire_call(
        &mut self,
        fir: &Fir<FlattenData<'_>>,
        _node: &Node<FlattenData<'_>>,
        to: &RefIdx,
        args: &[RefIdx],
    ) {
        let def = &fir.nodes[&to.unwrap()];
        let (block, def_args) = match &def.kind {
            Kind::Function { block, args, .. } => (block, args),
            _ => unreachable!(),
        };

        args.iter().enumerate().for_each(|(i, arg)| {
            self.fire_node(fir, &fir.nodes[&arg.unwrap()]);
            self.transfer(arg, def_args[i].unwrap());
        });

        // FIXME: We need to add bindings here between the function's variables and the arguments given to the call
        match block {
            None => self.perform_extern_call(fir, def, args),
            Some(block) => self.fire_node_ref(fir, block), // what to do here?
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
        _value: &RefIdx,
        ty: &RefIdx,
    ) {
        let tyref = &fir.nodes[&ty.unwrap()];
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
            Instance {
                ty: "empty type",
                data: Vec::new(),
            }
        } else {
            unreachable!()
        };

        // FIXME: Handle result here
        // FIXME: Should this be a transfer?
        self.allocate(node.origin, instance);
    }

    fn fire_node_ref(&mut self, fir: &Fir<FlattenData<'_>>, node_ref: &RefIdx) {
        let node = &fir.nodes[&node_ref.unwrap()];

        self.fire_node(fir, node)
    }

    // FIXME: Do we actually need to return an `Instance` here? We should just be able to lookup the rvalue's id or w/ever in the ctx
    // `where x = call()`
    // -> we perform the call, allocate data, and then look it up when accessing `x`? Is `x` a reference to call()? a move? that ties in the move semantics right?
    // where we would "move" the value from the call's OriginId to x's OriginId
    fn fire_node(&mut self, fir: &Fir<FlattenData<'_>>, node: &Node<FlattenData<'_>>) {
        dbg!(&node.data.ast);

        match &node.kind {
            Kind::Constant(c) => self.fire_constant(fir, node, c),
            Kind::Statements(stmts) => self.fire_block(fir, node, stmts),
            Kind::Call {
                to,
                args,
                .. /* FIXME: Generics should be empty at this point */
            } => self.fire_call(fir, node, to, args),
            Kind::Binding { to } => self.fire_binding(fir, node, to),
            Kind::TypedValue { value, ty } => self.fire_typed_value(fir, node, value, ty),
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
            // Kind::Return(expr) => self.traverse_return(fir, node, expr),
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
    fn call() {
        let ast = ast! {
            func id(x: int) -> int { x }

            id(15)
        };

        let result = fir!(ast).interpret();
    }
}
