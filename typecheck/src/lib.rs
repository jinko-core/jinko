use error::{ErrKind, Error};
use fir::{Fallible, Fir, Kind, Mapper, Node, OriginIdx, Pass, RefIdx, Traversal};
use flatten::FlattenData;
use location::SpanTuple;
use symbol::Symbol;

pub struct TypeCtx;

pub trait TypeCheck<T>: Sized {
    fn type_check(self) -> Result<T, Error>;
}

impl TypeCheck<Fir<TypeData>> for Fir<FlattenData> {
    fn type_check(self) -> Result<Fir<TypeData>, Error> {
        TypeCtx.pass(self)
    }
}

#[derive(Default, Debug)]
enum Ty {
    // Should these be OriginIdx?

    // TODO: Should this be a RefIdx?
    // TODO: Add Index[&RefIdx] and Index[&u64] to Fir
    // A declaration can have no return type, such as a void function
    Dec(Option<RefIdx>),
    // mmmmh... so this is something like an Up should look up and look up and look up until
    // it finds a Dec. And that's how you get your actual type because say the following code
    // func a() -> int { 15 }; { a() /* block */ };
    // the type of the block *uses* the type of the last expression, which also *uses* the
    // type of the function's dec which is a dec. So that's how we get the *actual* type. But
    // is this necessary? Should we simplify this and just keep a hashmap in the TypeCtx?
    Use(RefIdx),
    #[default] // FIXME: is that okay? Do we need it?
    Unknown,
}

#[derive(Default, Debug)]
pub struct TypeData {
    // FIXME: Does this need to be an option?
    ty: Option<Ty>,
    symbol: Option<Symbol>,
    loc: Option<SpanTuple>,
}

impl From<FlattenData> for TypeData {
    fn from(value: FlattenData) -> Self {
        TypeData {
            ty: None,
            symbol: value.symbol,
            loc: value.location,
        }
    }
}

impl TypeData {
    fn uses(self, ty: RefIdx) -> TypeData {
        TypeData {
            ty: Some(Ty::Use(ty)),
            ..self
        }
    }

    fn declares(self, ty: Option<RefIdx>) -> TypeData {
        TypeData {
            ty: Some(Ty::Dec(ty)),
            ..self
        }
    }

    // Shouldn't this return a different type so we can use it with a mapper?
    // can we actually use it with a mapper... when the mapper consumes the Fir?
    // should this be a traversal instead?
    // How do we use this? This is the right algorithm to flatten things up, but
    // is that what we want to do? Maybe just in a latter Traversal which checks
    // that types match? And we don't have to do memoization for now, but could
    // by keeping a hashmap within the TypeCtx?
    fn actual_type(&self, fir: &Fir<TypeData>) -> Option<OriginIdx> {
        match &self.ty {
            // declarations are void
            Some(Ty::Dec(_)) | None => None,
            Some(Ty::Use(ty_ref)) => {
                if let RefIdx::Resolved(origin) = ty_ref {
                    let refers_to = &fir.nodes[origin];

                    // if we're in a `Use` case again, recursively flatten. Otherwise,
                    // we've reached the leaf node (the actual declaration this type refers to)
                    match refers_to.data.ty {
                        Some(Ty::Use(inner)) => TypeData {
                            ty: Some(Ty::Use(inner)),
                            symbol: None,
                            loc: None,
                        }
                        .actual_type(fir),
                        Some(Ty::Dec(_)) => Some(refers_to.origin),
                        None => None,
                        Some(Ty::Unknown) => panic!("this is an interpreter error!"),
                    }
                } else {
                    panic!("this is an interpreter error: unresolved `RefIdx` in `TypeData`")
                }
            }
            // TODO: how to get the function's name here?
            Some(Ty::Unknown) => panic!("this is an interpreter error in `TypeData::actual_type`"),
        }
    }
}

// How do we map from a FlattenData to a TypeData and do we actually need to? We don't need
// types for every step of the execution, only for instantiation which is done in very few
// places (bindings... and that's it? constants?). but at the same time it is pretty fucking
// pog to look at a node and get its type straight up. So probably what we could do is have a
// TypeKind enum? or does that make no sense? and have things like "this is a function so its
// type is void, but it returns `int` and calls to it should return `int` as well"
// This is a "typing" pass, so it should maybe be named accordingly? And it happens before the
// "checking" pass.
impl Mapper<FlattenData, TypeData, Error> for TypeCtx {
    fn map_constant(
        &mut self,
        data: FlattenData,
        origin: OriginIdx,
        constant: RefIdx,
    ) -> Result<Node<TypeData>, Error> {
        // switch on the constant's kind - and this is a *declare* spot, so we must use
        // TypeData::from(data).declares(ty). This way, things like blocks returning constants can simply
        // depend on the type returned by the constant.
        // TODO: Is that all we need?
        // TODO: We need data from the AST at this point. Either the node or what kind of
        // constant it is (if it is one)
        Ok(Node {
            data: TypeData::from(data).declares(None /* FIXME: Wrong */),
            origin,
            kind: Kind::Constant(constant),
        })
    }

    fn map_type_reference(
        &mut self,
        data: FlattenData,
        origin: OriginIdx,
        reference: RefIdx,
    ) -> Result<Node<TypeData>, Error> {
        Ok(Node {
            data: TypeData::from(data).uses(reference),
            origin,
            kind: Kind::TypeReference(reference),
        })
    }

    fn map_typed_value(
        &mut self,
        data: FlattenData,
        origin: OriginIdx,
        value: RefIdx,
        ty: RefIdx,
    ) -> Result<Node<TypeData>, Error> {
        Ok(Node {
            data: TypeData::from(data).uses(ty),
            origin,
            kind: Kind::TypedValue { value, ty },
        })
    }

    fn map_generic(
        &mut self,
        data: FlattenData,
        origin: OriginIdx,
        default: Option<RefIdx>,
    ) -> Result<Node<TypeData>, Error> {
        // FIXME: What do we need to do here?
        Ok(Node {
            data: TypeData::from(data),
            origin,
            kind: Kind::Generic { default },
        })
    }

    fn map_type(
        &mut self,
        data: FlattenData,
        origin: OriginIdx,
        generics: Vec<RefIdx>,
        fields: Vec<RefIdx>,
    ) -> Result<Node<TypeData>, Error> {
        Ok(Node {
            // a type declaration has no type
            data: TypeData::from(data).declares(Some(RefIdx::Resolved(origin))),
            origin,
            kind: Kind::Type { generics, fields },
        })
    }

    fn map_function(
        &mut self,
        data: FlattenData,
        origin: OriginIdx,
        generics: Vec<RefIdx>,
        args: Vec<RefIdx>,
        return_type: Option<RefIdx>,
        block: Option<RefIdx>,
    ) -> Result<Node<TypeData>, Error> {
        Ok(Node {
            // likewise, defining a function has no type, but can be looked up by calls
            data: TypeData::from(data).declares(return_type),
            origin,
            kind: Kind::Function {
                generics,
                args,
                return_type,
                block,
            },
        })
    }

    fn map_binding(
        &mut self,
        data: FlattenData,
        origin: OriginIdx,
        to: RefIdx,
    ) -> Result<Node<TypeData>, Error> {
        // FIXME: What do we do here?
        // A binding declare something that can be looked up, right?
        // so which type do we put here? just.. itself? It's a binding
        // to a typed value, correct?
        Ok(Node {
            data: TypeData::from(data).uses(to),
            origin,
            kind: Kind::Binding { to },
        })
    }

    fn map_instantiation(
        &mut self,
        data: FlattenData,
        origin: OriginIdx,
        to: RefIdx,
        generics: Vec<RefIdx>,
        fields: Vec<RefIdx>,
    ) -> Result<Node<TypeData>, Error> {
        Ok(Node {
            data: TypeData::from(data).uses(to),
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
        data: FlattenData,
        origin: OriginIdx,
        instance: RefIdx,
        field: RefIdx,
    ) -> Result<Node<TypeData>, Error> {
        // FIXME: What do we do here?
        Ok(Node {
            data: TypeData::from(data),
            origin,
            kind: Kind::TypeOffset { instance, field },
        })
    }

    fn map_assignment(
        &mut self,
        data: FlattenData,
        origin: OriginIdx,
        to: RefIdx,
        from: RefIdx,
    ) -> Result<Node<TypeData>, Error> {
        Ok(Node {
            // an assignment has no type, and it doesn't declare a type either. It's just void.
            // FIXME: should we remove this function entirely?
            data: TypeData::from(data),
            origin,
            kind: Kind::Assignment { to, from },
        })
    }

    fn map_call(
        &mut self,
        data: FlattenData,
        origin: OriginIdx,
        to: RefIdx,
        generics: Vec<RefIdx>,
        args: Vec<RefIdx>,
    ) -> Result<Node<TypeData>, Error> {
        Ok(Node {
            // how do we get the return type of the function call here?
            data: TypeData::from(data).uses(to),
            origin,
            kind: Kind::Call { to, generics, args },
        })
    }

    fn map_statements(
        &mut self,
        data: FlattenData,
        origin: OriginIdx,
        stmts: Vec<RefIdx>,
    ) -> Result<Node<TypeData>, Error> {
        // FIXME: Do we need to do anything here?
        // or is it handled by the `Return` node?
        // I think we need to check whether the last statement is typed or not,
        // and if it is, return the same type. And this is jinko specific because
        // we have block expressions
        let data = match stmts.last() {
            Some(ty_ref) => TypeData::from(data).uses(*ty_ref),
            None => TypeData::from(data),
        };

        Ok(Node {
            data,
            origin,
            kind: Kind::Statements(stmts),
        })
    }

    fn map_condition(
        &mut self,
        data: FlattenData,
        origin: OriginIdx,
        condition: RefIdx,
        true_block: RefIdx,
        false_block: Option<RefIdx>,
    ) -> Result<Node<TypeData>, Error> {
        Ok(Node {
            data: TypeData::from(data).uses(true_block),
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
        data: FlattenData,
        origin: OriginIdx,
        condition: RefIdx,
        block: RefIdx,
    ) -> Result<Node<TypeData>, Error> {
        Ok(Node {
            // what types should lopos have?
            data: TypeData::from(data),
            origin,
            kind: Kind::Loop { condition, block },
        })
    }

    fn map_return(
        &mut self,
        data: FlattenData,
        origin: OriginIdx,
        expr: Option<RefIdx>,
    ) -> Result<Node<TypeData>, Error> {
        let data = match expr {
            Some(expr_ty) => TypeData::from(data).uses(expr_ty),
            None => TypeData::from(data),
        };

        Ok(Node {
            data,
            origin,
            kind: Kind::Return(expr),
        })
    }
}

fn type_mismatch(
    loc: &Option<SpanTuple>,
    fir: &Fir<TypeData>,
    expected: &OriginIdx,
    got: &OriginIdx,
) -> Error {
    let expected_ty = fir.nodes[expected].data.symbol.as_ref().unwrap();
    let got_ty = fir.nodes[got].data.symbol.as_ref().unwrap();

    Error::new(ErrKind::TypeChecker)
        .with_msg(format!(
            "type mismatch found: expected `{expected_ty}`, got `{got_ty}``"
        ))
        .with_loc(loc.clone()) // FIXME: Missing hint
}

impl Traversal<TypeData, Error> for TypeCtx {
    // TODO: When typechecking calls, typecheck arguments as well

    fn traverse_function(
        &mut self,
        fir: &Fir<TypeData>,
        node: &Node<TypeData>,
        _generics: &[RefIdx],
        _args: &[RefIdx],
        return_ty: &Option<RefIdx>,
        block: &Option<RefIdx>,
    ) -> Fallible<Error> {
        match (return_ty, block) {
            (Some(RefIdx::Resolved(return_ty)), Some(RefIdx::Resolved(block_ty))) => {
                let return_ty = fir.nodes[return_ty].data.actual_type(fir);
                let block_ty = fir.nodes[block_ty].data.actual_type(fir);

                if return_ty != block_ty {
                    Err(type_mismatch(
                        &node.data.loc,
                        fir,
                        &return_ty.unwrap(),
                        &block_ty.unwrap(),
                    ))
                } else {
                    Ok(())
                }
            }
            _ => Ok(()),
        }
    }
}

impl Pass<FlattenData, TypeData, Error> for TypeCtx {
    fn pre_condition(_fir: &Fir<FlattenData>) {}

    fn post_condition(_fir: &Fir<TypeData>) {}

    fn transform(&mut self, fir: Fir<FlattenData>) -> Result<Fir<TypeData>, Error> {
        // Typing pass
        let typed_fir = self.map(fir).unwrap(); /* FIXME: No unwrap */

        // Checking pass
        self.traverse(&typed_fir)?;

        Ok(typed_fir)
    }
}

#[cfg(test)]
mod tests {}
