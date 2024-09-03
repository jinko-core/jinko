mod actual;
mod checker;
mod collectors;
mod typemap;
mod typer;
mod widen;

use std::collections::{HashMap, HashSet};

use error::{ErrKind, Error};
use fir::{Fir, Incomplete, Kind, Mapper, OriginIdx, Pass, RefIdx, Traversal};
use flatten::FlattenData;

use actual::Actual;
use checker::Checker;
use typer::Typer;
use widen::widen;

use collectors::{
    constants::ConstantCollector,
    primitives::{self, PrimitiveTypes},
};

#[derive(Clone, Debug, Eq, PartialEq)]
// FIXME: Should that be a hashset RefIdx or OriginIdx?
// probably OriginIdx
// FIXME: Switch to HashSet<OriginIdx>
pub struct TypeSet(HashSet<RefIdx>);

impl TypeSet {
    // TODO: Rename or improve `Type`'s API
    pub fn contains(&self, other: &TypeSet) -> bool {
        // FIXME: This is quite ugly
        !other.0.iter().any(|elt| !self.0.contains(elt))
    }

    pub fn empty() -> TypeSet {
        TypeSet(HashSet::new())
    }

    pub fn merge(self, other: TypeSet) -> TypeSet {
        TypeSet(other.0.into_iter().fold(self.0, |mut set, entry| {
            set.insert(entry);
            set
        }))
    }
}

/// This is the base structure that our typechecker - a type "interpreter" - will play with.
/// In `jinko`, the type of a variable is a set of elements of kind `type`. So this structure can
/// be thought of as a simple set of actual, monomorphized types. There is one complication in that
/// the language recognizes a couple of magic types: `int`, `string` and `char` should be treated as
/// sets of all possible literals of that type. So we can imagine that `char` should actually be defined
/// as such:
///
/// ```rust,ignore
/// type char = '0' | '1' | '2' ... 'a' | 'b' | 'c' ... | <last_unicode_char_ever>;
/// ```
///
/// This is of course not a realistic definition to put in our standard library (and it gets worse for `string`)
/// so these types have to be handled separately.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Type(OriginIdx, TypeSet);

impl Type {
    pub fn origin(&self) -> OriginIdx {
        self.0
    }

    pub fn builtin(set: HashSet<RefIdx>) -> Type {
        Type(OriginIdx(u64::MAX), TypeSet(set))
    }

    pub fn record(origin: OriginIdx) -> Type {
        let mut set = HashSet::new();
        // FIXME: Switch to keeping HashSet<OriginIdx> instead
        set.insert(RefIdx::Resolved(origin));

        Type(origin, TypeSet(set))
    }

    pub fn union(origin: OriginIdx, variants: impl Iterator<Item = RefIdx>) -> Type {
        Type(origin, TypeSet(variants.collect()))
    }

    pub fn set(&self) -> &TypeSet {
        &self.1
    }

    pub fn is_superset_of(&self, other: &Type) -> bool {
        self.set().contains(other.set())
    }

    pub fn can_widen_to(&self, superset: &Type) -> bool {
        superset.set().contains(self.set())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum TypeVariable {
    Union(OriginIdx),
    Record(OriginIdx),
    Reference(RefIdx), // specifically we're interested in `type_ctx.type_of(< that refidx >)`
}

type TypeLinkMap = HashMap<OriginIdx, TypeVariable>;

pub(crate) struct TypeCtx<T> {
    // primitive type declaration
    pub(crate) primitives: PrimitiveTypes,
    // mapping from declaration to type
    // FIXME: Explain why this is needed
    pub(crate) types: T,
}

pub trait TypeCheck<T>: Sized {
    fn type_check(self) -> Result<T, Error>;
}

impl<'ast> TypeCheck<Fir<FlattenData<'ast>>> for Fir<FlattenData<'ast>> {
    fn type_check(mut self) -> Result<Fir<FlattenData<'ast>>, Error> {
        let primitives = primitives::find(&self)?;

        let mut const_collector = ConstantCollector::new();
        const_collector.traverse(&self)?;

        // We can now build our primitive union types. Because the first TypeCtx deals
        // with [`TypeVariable`]s, it's not possible to directly create a TypeSet - so
        // we can do that later on during typechecking, right before the actual
        // typechecking. An alternative is to modify the [`Fir`] directly by creating
        // new nodes for these primitive unions, which is probably a little cleaner and
        // less spaghetti.
        let mk_constant_types = |set: HashSet<RefIdx>| set.into_iter().collect();

        self[primitives.int_type].kind = Kind::UnionType {
            generics: vec![],
            variants: mk_constant_types(const_collector.integers),
        };
        self[primitives.float_type].kind = Kind::UnionType {
            generics: vec![],
            variants: mk_constant_types(const_collector.floats),
        };
        self[primitives.char_type].kind = Kind::UnionType {
            generics: vec![],
            variants: mk_constant_types(const_collector.characters),
        };
        self[primitives.string_type].kind = Kind::UnionType {
            generics: vec![],
            variants: mk_constant_types(const_collector.strings),
        };
        self[primitives.bool_type].kind = Kind::UnionType {
            generics: vec![],
            variants: mk_constant_types(const_collector.bools),
        };

        TypeCtx {
            primitives,
            types: HashMap::new(),
        }
        .pass(self)
    }
}

impl<'ast> Pass<FlattenData<'ast>, FlattenData<'ast>, Error> for TypeCtx<TypeLinkMap> {
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

        let ctx = Actual::resolve_type_links(self, &fir)?;
        // FIXME: Improve the API?
        let mut ctx = widen(&fir, ctx);

        Checker(&mut ctx).traverse(&fir)?;

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
                type unit;
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

            "hoo".foo('a', 14)
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

    #[test]
    fn typeset_makes_sense() {
        let superset = Type(
            OriginIdx(4),
            TypeSet(
                [
                    RefIdx::Resolved(OriginIdx(0)),
                    RefIdx::Resolved(OriginIdx(1)),
                    RefIdx::Resolved(OriginIdx(2)),
                    RefIdx::Resolved(OriginIdx(3)),
                ]
                .into_iter()
                .collect(),
            ),
        );
        let set = Type(
            OriginIdx(5),
            TypeSet(
                [
                    RefIdx::Resolved(OriginIdx(0)),
                    RefIdx::Resolved(OriginIdx(1)),
                ]
                .into_iter()
                .collect(),
            ),
        );
        let single = Type::record(OriginIdx(0));
        let empty = Type(OriginIdx(7), TypeSet(HashSet::new()));

        // FIXME: Decide on empty's behavior

        assert!(set.can_widen_to(&superset));
        assert!(superset.is_superset_of(&set));

        assert!(single.can_widen_to(&set));
        assert!(set.is_superset_of(&single));

        assert!(single.can_widen_to(&superset));
        assert!(superset.is_superset_of(&single));
    }

    #[test]
    fn nullable_union_type() {
        let ast = ast! {
            type Nil;
            type Nullable = int | Nil;

            func nil_unit() -> Nullable { Nil }
            func nil_just(i: int) -> Nullable { i }
        };
        let fir = fir!(ast).type_check();

        assert!(fir.is_ok());
    }

    #[test]
    fn nullable_union_type_valid_use() {
        let ast = ast! {
            type Nothing;
            type NullableInt = int | Nothing;

            func g(n: NullableInt) {}

            where x = 16;

            g(15);
            g(x);
            g(Nothing);
        };

        let fir = fir!(ast).type_check();

        assert!(fir.is_ok());
    }

    #[test]
    fn nullable_union_type_invalid_use() {
        let ast = ast! {
            type Nothing;
            type NullableInt = 15 | Nothing;

            func g(n: NullableInt) {}

            where x = 16;

            g(15);
            g(x);
            g(Nothing);
        };

        let fir = fir!(ast).type_check();

        assert!(fir.is_err());
    }
}
