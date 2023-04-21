use std::collections::HashMap;

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
// for a float -> eeeeeeeh?
// for other types -> needs to be a unique hash -> based on source location and FirId?
type Type = &'static str;

// an instance needs to be unique
// needs to be hashable
// we need the type of the value - it needs to be known at all times
#[derive(PartialEq, Eq, Debug)]
struct Instance {
    pub(crate) ty: Type,
    pub(crate) data: Vec<u8>,
}

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
}

// is this a Mapper or a Traverse? Traverse probably
// or can it be a Mapper?
impl<'ast> Mapper<FlattenData<'ast>, FlattenData<'ast>, Error> for Fire {}

impl Traversal<FlattenData<'_>, Error> for Fire {
    fn traverse_call(
        &mut self,
        _fir: &Fir<FlattenData<'_>>,
        _node: &Node<FlattenData<'_>>,
        _to: &RefIdx,
        _generics: &[RefIdx],
        args: &[RefIdx],
    ) -> Fallible<Error> {
        let def = &_fir.nodes[&_to.unwrap()];
        let _block = match &def.kind {
            Kind::Function { block: None, .. } => self.perform_extern_call(_fir, def, args),
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

    fn traverse_node(
        &mut self,
        fir: &Fir<FlattenData<'_>>,
        node: &Node<FlattenData<'_>>,
    ) -> Fallible<Error> {
        match &node.kind {
            Kind::Constant(c) => self.traverse_constant(fir, node, c),
            Kind::TypeReference(r) => self.traverse_type_reference(fir, node, r),
            Kind::TypedValue { value, ty } => self.traverse_typed_value(fir, node, value, ty),
            Kind::Generic { default } => self.traverse_generic(fir, node, default),
            Kind::Type { generics, fields } => self.traverse_type(fir, node, generics, fields),
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
}

#[cfg(test)]
mod tests {}
