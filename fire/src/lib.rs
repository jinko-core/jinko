use std::collections::HashMap;

// FIXME: Execution is very tree-like, isn't it
// FIXME: How do we get the last value of a Statements?
// FIXME: How do Returns work in this system?
// FIXME: How do we use the value returned by a function call for example?
// where x = id(15);

use error::Error;
use fir::{Fallible, Fir, Kind, Mapper, Node, OriginIdx, RefIdx, Traversal};
use flatten::FlattenData;

pub trait Interpret {
    fn interpret(&self) -> Result<(), Error>;
}

impl Interpret for Fir<FlattenData<'_>> {
    fn interpret(&self) -> Result<(), Error> {
        let mut fire = Fire {
            values: HashMap::new(),
        };

        fire.traverse(self)?;

        Ok(())
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
struct Instance {
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
    fn allocate(&mut self, key: OriginIdx, value: Instance) {
        // if we allocate the same value twice, this is an interpreter error
        assert!(self.values.insert(key, value).is_none());
    }

    fn copy(&mut self, to_copy: &RefIdx, key: OriginIdx) {
        // if we haven't allocated this value beforehand, this is an interpreter error
        let value = self.values.get(&to_copy.unwrap()).unwrap();

        // FIXME: This is very invalid - we should not be cloning the instance here.
        // this all depends on the copy/move/persistent semantics we'll choose
        self.allocate(key, value.clone());
    }

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
                let value = self.values.get(&arg.unwrap()).unwrap();
                // FIXME: Ugly as SIN
                println!("{}", unsafe {
                    String::from_utf8_unchecked(value.data.clone())
                });
            })
        }

        None
    }

    // FIXME: Sholud this return a Result<Option<Instance>, Error>?
    fn run_block(&mut self, fir: &Fir<FlattenData<'_>>, node: &RefIdx) -> Option<Instance> {
        let stmts = &fir.nodes[&node.unwrap()];
        let stmts = match &stmts.kind {
            Kind::Statements(stmts) => stmts,
            _ => unreachable!(
                "{}:{}: expected list of statements for node {node:?}. this is an interpreter error.",
                file!(),
                line!(),
            ),
        };

        let errs = stmts.iter().fold(Vec::new(), |mut errs, node| {
            let node = &fir.nodes[&node.unwrap()];
            match self.traverse_node(fir, node) {
                Ok(_) => errs,
                Err(e) => {
                    errs.push(e);
                    errs
                }
            }
        });

        // FIXME: This is invalid, isn't it?
        // FIXME: or should we just return () here?
        None
    }

    // TODO: It would be good to have a "copy/move" function which actually performs the behavior we want (copy or move)
    // and we could call it on the sites where it matters
}

// is this a Mapper or a Traverse? Traverse probably
// or can it be a Mapper? -> has to be a traverse because we can reuse nodes I think?
impl<'ast> Mapper<FlattenData<'ast>, FlattenData<'ast>, Error> for Fire {}

impl Traversal<FlattenData<'_>, Error> for Fire {
    fn traverse_call(
        &mut self,
        fir: &Fir<FlattenData<'_>>,
        _node: &Node<FlattenData<'_>>,
        _to: &RefIdx,
        _generics: &[RefIdx],
        args: &[RefIdx],
    ) -> Fallible<Error> {
        let def = &fir.nodes[&_to.unwrap()];
        let (block, def_args) = match &def.kind {
            Kind::Function { block, args, .. } => (block, args),
            _ => unreachable!(),
        };

        args.iter().enumerate().for_each(|(i, arg)| {
            self.traverse_node(fir, &fir.nodes[&arg.unwrap()]).unwrap();
            self.copy(arg, def_args[i].unwrap());
        });

        match block {
            None => self.perform_extern_call(fir, def, args),
            Some(block) => self.run_block(fir, block),
        };
        // FIXME: We need to add bindings here between the function's variables and the arguments given to the call

        Ok(())
    }

    fn traverse_constant(
        &mut self,
        _fir: &Fir<FlattenData<'_>>,
        node: &Node<FlattenData<'_>>,
        _constant: &RefIdx,
    ) -> Fallible<Error> {
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

        // FIXME: Handle result here
        self.allocate(node.origin, instance);

        Ok(())
    }

    fn traverse_typed_value(
        &mut self,
        fir: &Fir<FlattenData<'_>>,
        node: &Node<FlattenData<'_>>,
        _value: &RefIdx,
        ty: &RefIdx,
    ) -> Fallible<Error> {
        let tyref = &fir.nodes[&ty.unwrap()];
        let fields = match &tyref.kind {
            Kind::Type { fields, .. } => fields,
            // FIXME: here we need to decide part of our copy/move semantics
            Kind::TypeReference(_) => return Ok(()),
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
        self.allocate(node.origin, instance);

        Ok(())
    }

    fn traverse_binding(
        &mut self,
        _fir: &Fir<FlattenData<'_>>,
        node: &Node<FlattenData<'_>>,
        to: &RefIdx,
    ) -> Fallible<Error> {
        // FIXME: This is very invalid
        self.copy(to, node.origin);

        Ok(())
    }

    // FIXME: Remove this function. Use Fire::run_block instead
    fn traverse(&mut self, fir: &Fir<FlattenData<'_>>) -> Fallible<Vec<Error>> {
        // FIXME: No unwrap here
        let entry_point = fir.nodes.last_key_value().unwrap();
        let stmts = match &entry_point.1.kind {
            Kind::Statements(stmts) => stmts,
            _ => unreachable!(),
        };

        let errs = stmts.iter().fold(Vec::new(), |mut errs, node| {
            let node = &fir.nodes[&node.unwrap()];
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

#[cfg(test)]
mod tests {}
