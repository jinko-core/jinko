mod actual;
mod checker;
mod typer;

use std::collections::HashMap;

use error::Error;
use fir::{Fir, OriginIdx, Pass, RefIdx, Traversal};
use flatten::FlattenData;

use typer::Typer;

pub(crate) enum Type {
    Void,
    One(RefIdx),
}

#[derive(Default)]
pub(crate) struct TypeCtx {
    // mapping from declaration to type
    pub(crate) types: HashMap<OriginIdx, Type>,
}

pub trait TypeCheck<T>: Sized {
    fn type_check(self) -> Result<T, Error>;
}

impl TypeCheck<Fir<FlattenData>> for Fir<FlattenData> {
    fn type_check(self) -> Result<Fir<FlattenData>, Error> {
        TypeCtx::default().pass(self)
    }
}

impl Pass<FlattenData, FlattenData, Error> for TypeCtx {
    fn pre_condition(_fir: &Fir<FlattenData>) {}

    fn post_condition(_fir: &Fir<FlattenData>) {}

    fn transform(&mut self, fir: Fir<FlattenData>) -> Result<Fir<FlattenData>, Error> {
        // Typing pass
        Typer(self).traverse(&fir).unwrap(); /* FIXME: No unwrap */

        // Checking pass
        // self.traverse(&typed_fir)?;

        // Ok(typed_fir)

        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! fir {
        ($($tok:tt)*) => {
            {
                let ast = xparser::parse(
                    stringify!($($tok)*),
                    location::Source::Input(stringify!($($tok)*)))
                .unwrap();

                let fir = flatten::FlattenAst::flatten(&ast);
                name_resolve::NameResolve::name_resolve(fir).unwrap()
            }
        }
    }

    #[test]
    fn easy() {
        let fir = fir! {
            type Marker0;

            func foo() -> Marker0 {
                Marker0
            }
        }
        .type_check();

        // TODO: Add assertions making sure that the type of the block and block's last stmt are typerefs to Marker0;
        assert!(fir.is_ok());
    }

    #[test]
    fn mismatch() {
        let fir = fir! {
            type Marker0;
            type Marker1;

            func foo() -> Marker0 {
                Marker1
            }
        }
        .type_check();

        assert!(fir.is_err());
    }

    #[test]
    fn nested_call() {
        let fir = fir! {
            type Marker0;

            func foo() -> Marker0 { Marker0 }
            func bar() -> Marker0 { foo() }
        }
        .type_check();

        assert!(fir.is_ok());
    }
}
