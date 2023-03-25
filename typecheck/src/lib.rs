mod actual;
mod checker;
mod primitives;
mod typer;

use std::collections::HashMap;

use error::Error;
use fir::{Fir, OriginIdx, Pass, RefIdx, Traversal};
use flatten::FlattenData;

use actual::Actual;
use checker::Checker;
use typer::Typer;

use primitives::PrimitiveTypes;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum Type {
    One(RefIdx),
}

pub(crate) struct TypeCtx {
    // primitive type declaration
    pub(crate) primitives: PrimitiveTypes,
    // mapping from declaration to type
    pub(crate) types: HashMap<OriginIdx, Option<Type>>,
}

pub trait TypeCheck<T>: Sized {
    fn type_check(self) -> Result<T, Error>;
}

impl<'ast> TypeCheck<Fir<FlattenData<'ast>>> for Fir<FlattenData<'ast>> {
    fn type_check(self) -> Result<Fir<FlattenData<'ast>>, Error> {
        let primitives = primitives::find(&self)?;

        TypeCtx {
            primitives,
            types: HashMap::new(),
        }
        .pass(self)
    }
}

impl<'ast> Pass<FlattenData<'ast>, FlattenData<'ast>, Error> for TypeCtx {
    fn pre_condition(_fir: &Fir<FlattenData>) {}

    fn post_condition(_fir: &Fir<FlattenData>) {}

    fn transform(&mut self, fir: Fir<FlattenData<'ast>>) -> Result<Fir<FlattenData<'ast>>, Error> {
        // Typing pass
        Typer(self).traverse(&fir)?;
        Actual(self).traverse(&fir)?;
        Checker(self).traverse(&fir)?;

        Ok(fir)
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

    #[test]
    fn invalid_nested() {
        let ast = ast! {
            type Marker0;
            type Marker1;

            func foo() -> Marker0 { Marker0 }
            func bar() -> Marker0 { foo() }
            func baz() -> Marker1 { bar() }

            func qux() -> Marker0 { baz() }
        };
        let fir = fir!(ast).type_check();

        assert!(fir.is_err());
    }

    #[test]
    fn assignment_valid() {
        let ast = ast! {
            type Marker0;

            where mut a = Marker0;
            a = Marker0;

            where mut b = Marker0;
            b = a;
        };
        let fir = fir!(ast).type_check();

        assert!(fir.is_ok());
    }

    #[test]
    fn assignment_invalid() {
        let ast = ast! {
            type Marker0;
            type Marker1;

            where mut a = Marker0;
            a = Marker1;
        };
        let fir = fir!(ast).type_check();

        assert!(fir.is_err());
    }
}
