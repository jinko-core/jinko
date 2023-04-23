use std::collections::HashMap;

// FIXME: Execution is very tree-like, isn't it

use error::{ErrKind, Error};
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
#[derive(PartialEq, Eq, Debug)]
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
    fn perform_extern_call(
        &self,
        _fir: &Fir<FlattenData<'_>>,
        node: &Node<FlattenData<'_>>,
        args: &[RefIdx],
    ) -> Result<Option<Instance>, Error> {
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

        Ok(None)
    }

    fn execute_stmts(
        &mut self,
        fir: &Fir<FlattenData<'_>>,
        node: &RefIdx,
    ) -> Result<Option<Instance>, Error> {
        let stmts = &fir.nodes[&node.unwrap()];
        let stmts = match &stmts.kind {
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
            // FIXME: Invalid
            Ok(None)
        } else {
            Err(Error::new(ErrKind::Multiple(errs)))
        }
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

        args.iter()
            .for_each(|arg| self.traverse_node(fir, &fir.nodes[&arg.unwrap()]).unwrap());

        let _block = match &def.kind {
            Kind::Function { block: None, .. } => self.perform_extern_call(fir, def, args),
            Kind::Function {
                block: Some(block), ..
            } => {
                self.execute_stmts(fir, block)
                // self.perform_extern_call(_fir, def, args),
            }
            _ => unreachable!(),
        };

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
        self.values.insert(node.origin, instance);

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
        self.values.insert(node.origin, instance);

        Ok(())
    }

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
