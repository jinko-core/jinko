// TODO: At the moment, the main issue with this typechecker is that it isn't *flat*. It does
// not benefit from all of the cool stuff of the Fir and ends up doing recursive calls to figure
// out the proper type of something. Is there a way to circumvent this or not? Or is that how we
// are supposed to do things?

use error::{ErrKind, Error};
use fir::{Fallible, Fir, Kind, Mapper, Node, OriginIdx, Pass, RefIdx, Traversal};
use flatten::FlattenData;
use location::SpanTuple;
use symbol::Symbol;

pub struct TypeCtx;

pub trait TypeCheck<T>: Sized {
    #[deprecated = "experimental recursive typecheck algorithm"]
    fn type_check(self) -> Result<T, Error>;
}

impl TypeCheck<Fir<TypeData>> for Fir<FlattenData<'_>> {
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

#[derive(Debug)]
pub struct TypeData {
    // FIXME: Does this need to be an option?
    ty: Option<Ty>,
    symbol: Option<Symbol>,
    loc: SpanTuple,
}

impl From<FlattenData<'_>> for TypeData {
    fn from(value: FlattenData) -> Self {
        TypeData {
            ty: None,
            symbol: value.ast.symbol().cloned(),
            loc: value.ast.location().clone(),
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
        dbg!(&self.ty);
        match &self.ty {
            // declarations are void
            Some(Ty::Dec(_)) | None => None,
            Some(Ty::Use(ty_ref)) => {
                if let RefIdx::Resolved(origin) = ty_ref {
                    let refers_to = &fir.nodes[origin];

                    // if we're in a `Use` case again, recursively flatten. Otherwise,
                    // we've reached the leaf node (the actual declaration this type refers to)
                    match refers_to.data.ty {
                        Some(Ty::Use(_)) => refers_to.data.actual_type(fir),
                        Some(Ty::Dec(declared_ty)) => declared_ty.map(|idx| match idx {
                            RefIdx::Unresolved => panic!("interpreter error!"),
                            RefIdx::Resolved(idx) => idx,
                        }),
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
impl Mapper<FlattenData<'_>, TypeData, Error> for TypeCtx {
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
            // For constants, how will we look up the basic primitive type nodes before assigning them
            // here? Just a traversal and we do that based on name? Or will they need to be builtin at this point?
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

    // fn map_node_ref(
    //     &mut self,
    //     data: FlattenData,
    //     origin: OriginIdx,
    //     to: RefIdx,
    // ) -> Result<Node<TypeData>, Error> {
    // Ok(Node {
    //     data: TypeData::from(data).uses(ty),
    //     origin,
    //     kind: Kind::NodeRef(to),
    // })
    // }

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

    fn map_record_type(
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
            kind: Kind::RecordType { generics, fields },
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
        to: Option<RefIdx>,
        ty: Option<RefIdx>,
    ) -> Result<Node<TypeData>, Error> {
        // FIXME: What do we do here?
        // A binding declare something that can be looked up, right?
        // so which type do we put here? just.. itself? It's a binding
        // to a typed value, correct?
        Ok(Node {
            // FIXME: This is wrong
            data: TypeData::from(data),
            origin,
            kind: Kind::Binding { to, ty },
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
    loc: &SpanTuple,
    fir: &Fir<TypeData>,
    expected: &Option<OriginIdx>,
    got: &Option<OriginIdx>,
) -> Error {
    let get_symbol = |idx| fir.nodes[&idx].data.symbol.clone().unwrap();

    let expected_ty = expected.map_or(Symbol::from("void"), get_symbol);
    let got_ty = got.map_or(Symbol::from("void"), get_symbol);

    Error::new(ErrKind::TypeChecker)
        .with_msg(format!(
            "type mismatch found: expected `{expected_ty}`, got `{got_ty}`"
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
        // TODO: Make this whole block return an Option<Error> instead?
        match (return_ty, block) {
            (None, Some(RefIdx::Resolved(block_ty))) => {
                let block_ty = fir.nodes[block_ty].data.actual_type(fir);

                if block_ty.is_some() {
                    Err(type_mismatch(&node.data.loc, fir, &None, &block_ty))
                } else {
                    Ok(())
                }
            }
            (Some(RefIdx::Resolved(return_ty)), Some(RefIdx::Resolved(block_ty))) => {
                let return_ty = fir.nodes[return_ty].data.actual_type(fir);
                let block_ty = fir.nodes[block_ty].data.actual_type(fir);

                // This has an issue with the `nested_call` testcase, as one resolves to the type declaration and another
                // to the type reference which refers that declaration.
                if return_ty != block_ty {
                    Err(type_mismatch(&node.data.loc, fir, &return_ty, &block_ty))
                } else {
                    Ok(())
                }
            }
            _ => Ok(()),
        }
    }
}

impl Pass<FlattenData<'_>, TypeData, Error> for TypeCtx {
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
mod tests {
    use super::*;
    use flatten::FlattenAst;
    use name_resolve::NameResolve;
    use xparser::ast;

    macro_rules! fir {
        ($ast:expr) => {
            $ast.flatten().name_resolve().unwrap()
        };
    }

    #[ignore]
    #[test]
    fn easy() {
        let ast = ast! {
            type Marker0;

            func foo() -> Marker0 {
                Marker0
            }
        };
        let fir = fir!(ast).type_check();

        // TODO: Add assertions making sure that the type of the block and block's last stmt are typerefs to Marker0;
        assert!(fir.is_ok());
    }

    #[ignore]
    #[test]
    fn mismatch() {
        let ast = ast! {
            type Marker0;
            type Marker1;

            func foo() -> Marker0 {
                Marker1
            }
        };
        let fir = fir!(ast).type_check();

        assert!(fir.is_err());
    }

    #[ignore]
    #[test]
    fn nested_call() {
        let ast = ast! {
            type Marker0;

            func foo() -> Marker0 { Marker0 }
            func bar() -> Marker0 { foo() }
        };
        let fir = fir!(ast).type_check();

        assert!(fir.is_ok());
    }
}
