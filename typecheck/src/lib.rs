mod actual;
mod checker;
mod primitives;
mod typer;

use std::{
    collections::{HashMap, HashSet},
    iter,
};

use error::{ErrKind, Error};
use fir::{Fir, Incomplete, Mapper, OriginIdx, Pass, RefIdx, Traversal};
use flatten::FlattenData;

use actual::Actual;
use checker::Checker;
use typer::Typer;

use primitives::PrimitiveTypes;

/// This is the base structure that our typechecker - a type "interpreter" - will play with.
/// In `jinko`, the type of a variable is a set of elements of kind `type`. So this structure can
/// be thought of as a simple set of actual, monomorphized types.
// TODO: for now, let's not think about optimizations - let's box and clone and blurt bytes everywhere
#[derive(Clone, Debug, Eq, PartialEq)]
// TODO: We might have to turn this into an enum - `ActualType(Set<RefIdx>) | TypeReference(RefIdx)`
pub(crate) struct Type(HashSet<RefIdx>);

impl Type {
    pub fn new(set: HashSet<RefIdx>) -> Type {
        Type(set)
    }

    // TODO: Rename? one? simple? unique? what's the opposite of `sum` or `multi`?
    pub fn single(fir_type: RefIdx) -> Type {
        let mut set = HashSet::new();
        set.insert(fir_type);

        Type(set)
    }
}

impl FromIterator<RefIdx> for Type {
    fn from_iter<T: IntoIterator<Item = RefIdx>>(iter: T) -> Type {
        Type(iter.into_iter().collect())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum TypeVariable {
    Actual(Type),
    Reference(RefIdx), // specifically we're interested in `type_ctx.type_of(< that refidx >)`
}

impl TypeVariable {}

pub(crate) struct TypeCtx {
    // primitive type declaration
    pub(crate) primitives: PrimitiveTypes,
    // mapping from declaration to type
    pub(crate) types: HashMap<OriginIdx, Option<TypeVariable>>,
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
        let fir = Typer(self).map(fir);

        let mut type_errs = None;

        let fir = match fir {
            Ok(fir) => fir,
            Err(Incomplete { carcass, errs }) => {
                type_errs = Some(Error::new(ErrKind::Multiple(errs)));
                carcass
            }
        };

        Actual(self).traverse(&fir)?;
        Checker(self).traverse(&fir)?;

        match type_errs {
            Some(e) => Err(e),
            None => Ok(fir),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use builtins::AppendAstBuiltins;
    use flatten::FlattenAst;
    use name_resolve::NameResolve;

    // helper macro which declares our primitive types and calls into [`xparser::ast`]
    macro_rules! ast {
        ($($toks:tt)*) => {
            xparser::ast!(
                type char;
                type bool;
                type int;
                type float;
                type string;
                $($toks)*
            ).append_builtins().unwrap()
        }
    }

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

    #[test]
    fn constant_bool() {
        let ast = ast! {
            func foo() -> bool { true }
            func bar() -> bool { false }
        };

        let fir = fir!(ast).type_check();

        assert!(fir.is_ok());
    }

    #[test]
    fn constant_int() {
        let ast = ast! {
            func foo() -> int { 15 }
            func bar() -> int { foo() }
        };

        let fir = fir!(ast).type_check();

        assert!(fir.is_ok());
    }

    #[test]
    fn constants() {
        let ast = ast! {
            func foo() -> char { 'a' }
            func bar() -> string { "multichars" }
            func baz() -> float { 14.4 }
        };

        let fir = fir!(ast).type_check();

        assert!(fir.is_ok());
    }

    #[test]
    fn issue618() {
        let ast = ast! {
            func foo(x: string) {}

            foo("27");
            foo("28");
        };

        let fir = fir!(ast).type_check();

        assert!(fir.is_ok())
    }

    #[test]
    fn typeck_function_call_argument_count_mismatch() {
        let ast = ast! {
            func foo(one: int, two: int) -> int { one }

            foo(15)
        };

        let fir = fir!(ast).type_check();

        assert!(fir.is_err());
    }

    #[test]
    fn typeck_function_call_argument_count_match() {
        let ast = ast! {
            func foo(one: int, two: int) -> int { one }

            foo(15, 14)
        };

        let fir = fir!(ast).type_check();

        assert!(fir.is_ok());
    }

    #[test]
    fn typeck_method_call() {
        let ast = ast! {
            func foo(one: string, two: int) -> int { two }

            "hoo".foo(15)
        };

        let fir = fir!(ast).type_check();

        assert!(fir.is_ok());
    }

    #[test]
    fn typeck_method_call2() {
        let ast = ast! {
            func foo(one: string, two: int, three: char) -> int { two }

            "hoo".foo(15, 14)
        };

        let fir = fir!(ast).type_check();

        assert!(fir.is_err());
    }

    #[test]
    fn typeck_call_complex_arg() {
        let ast = ast! {
            type Marker;

            func take_marker(m: Marker) {}
            func get_marker() -> Marker { Marker }

            take_marker(Marker);

            where m = Marker;
            take_marker(m);

            take_marker(get_marker());

            get_marker().take_marker();
        };

        let fir = fir!(ast).type_check();

        assert!(fir.is_ok())
    }

    #[test]
    fn typeck_binop() {
        let ast = ast! {
            func take_int(a: int) -> int { a }
            func square(x: int) -> int { x * x }
            func add(l: int, r: int) -> int { l + r }

            take_int(4);
            take_int(15 + 4);
            square(2);
            add(square(2), 6);
        };

        let fir = fir!(ast).type_check();

        assert!(fir.is_ok());
    }

    #[test]
    fn typeck_binop_valid() {
        let ast = ast! {
            5 + 15
        };

        let fir = fir!(ast).type_check();

        assert!(fir.is_ok());
    }

    #[test]
    fn typeck_cmp_binop_valid() {
        let ast = ast! {
            "foo" == "boo"
        };

        let fir = fir!(ast).type_check();

        assert!(fir.is_ok());
    }

    #[test]
    fn typeck_binop_invalid() {
        let ast = ast! {
            5.4 + 15
        };

        let fir = fir!(ast).type_check();

        assert!(fir.is_err());
    }

    #[test]
    fn typeck_cmp_binop_invalid() {
        let ast = ast! {
            "foo" == 15
        };

        let fir = fir!(ast).type_check();

        assert!(fir.is_err());
    }

    #[test]
    fn typeck_cmp_binop_invalid_on_string() {
        let ast = ast! {
            "foo" >= "fah"
        };

        let fir = fir!(ast).type_check();

        assert!(fir.is_err());
    }

    #[test]
    fn typeck_cmp_binop_valid_on_string() {
        let ast = ast! {
            "foo" != "fah"
        };

        let fir = fir!(ast).type_check();

        assert!(fir.is_ok());
    }
}
