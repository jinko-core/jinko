//! The name-resolve module takes care of resolving each "name" within a `jinko` program to
//! its definition site. You can think of it as a function, which will take an unresolved
//! [`Fir`] as input, and output a new [`Fir`] where each node points to the definition it
//! uses. To do this, multiple passes are applied on the input [`Fir`].
//!
//! 1. We start by "scoping" *all* of the definitions and usages in the program.
//! This is done via the [`Scoper`] type, which will assign an enclosing scope to
//! each node of our [`Fir`]. This is important, since name resolution in `jinko`
//! can go "backwards" or "upwards". You can access a definition in an *outer*
//! scope from an *inner* scope. This means that the outermost scope is able to
//! use definitions from the outermost scope. But if this "enclosing" or "parent"
//! relationship does not exist, then the usage is invalid:
//!
//! ```text
//! {
//!     func foo() {}
//! }
//! {
//!     { foo() }
//!     // this scope does not have `foo`'s scope as a parent, so it will need to error out later on
//! }
//! ```
//!
//! 2. We collect all of the definitions within the program using the [`Declarator`] struct.
//! A definition can be a function definition, a new type, as well as a new binding. This
//! "definition collection" pass will only error out if a definition is present twice, e.g.:
//!
//! ```text
//! func foo() {}
//! func foo(different: int, arguments: int) -> string { "oh no" }
//! ```
//!
//! 2. Finally, we resolve all *usages* to their *definitions* using the [`Resolver`] type.
//! If a usage cannot be resolved to a definition, then it is an error. Similarly, if a usage
//! can be resolved to more than one definitions, we error out. The resolver does not take care
//! of resolving complex usages, such as methods, generic function calls or specialization.

use std::{collections::HashMap, mem, ops::Index};

use colored::Colorize;
use error::{ErrKind, Error};
use fir::{Fallible, Fir, Incomplete, Kind, Mapper, OriginIdx, Pass, Traversal};
use flatten::FlattenData;
use location::SpanTuple;
use symbol::Symbol;

mod declarator;
mod resolver;
mod scoper;

use declarator::Declarator;
use resolver::{LatentResolver, ResolveKind, Resolver};
use scoper::Scoper;

/// Error reported when an item (variable, function, type) was already declared
/// in the current scope.
struct UniqueError(OriginIdx, &'static str);

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub(crate) struct Scope(pub(crate) OriginIdx);

impl Scope {
    pub fn replace(&mut self, new: OriginIdx) -> OriginIdx {
        mem::replace(&mut self.0, new)
    }

    pub fn origin(&self) -> OriginIdx {
        self.0
    }
}

/// This data structure maps each node from the [`Fir`] to the scope which contains it. This
/// makes finding the definition associated with a name very easy, as we can simply look at the
/// name's enclosing scope, and look for definitions. If no suitable definition, we look at the
/// parent scope of this scope, and repeat the process, until we find a definition or exhaust
/// valid parents. This struct keeps a reference on a map, making it cheap to copy and pass around.
#[derive(Clone, Copy, Debug)]
struct EnclosingScope<'enclosing>(&'enclosing HashMap<OriginIdx, Scope>);

impl Index<OriginIdx> for EnclosingScope<'_> {
    type Output = Scope;

    fn index(&self, index: OriginIdx) -> &Self::Output {
        &self.0[&index]
    }
}

type Bindings = HashMap<Symbol, OriginIdx>;

/// Each scope in the [`scopes`] map contains the bindings associated with a given scope,
/// meaning that each scope contains a list of definitions. A definition can be thought of as the
/// mapping of a name ([`Symbol`]) to the node's index in the [`Fir`].
#[derive(Clone, Debug)]
struct FlatScope<'enclosing> {
    scopes: HashMap<Scope, Bindings>,
    enclosing_scope: EnclosingScope<'enclosing>,
}

/// Allow iterating on a [`FlatScope`] by going through the chain of enclosing scopes
struct FlatIterator<'scope, 'enclosing>(Option<Scope>, &'scope FlatScope<'enclosing>);

trait LookupIterator<'scope, 'enclosing> {
    fn lookup_iterator(&'scope self, starting_scope: Scope) -> FlatIterator<'scope, 'enclosing>;
}

impl<'scope, 'enclosing> LookupIterator<'scope, 'enclosing> for FlatScope<'enclosing> {
    fn lookup_iterator(&'scope self, starting_scope: Scope) -> FlatIterator<'scope, 'enclosing> {
        FlatIterator(Some(starting_scope), self)
    }
}

impl<'scope, 'enclosing> Iterator for FlatIterator<'scope, 'enclosing> {
    type Item = &'scope Bindings;

    fn next(&mut self) -> Option<Self::Item> {
        let cursor = self.0;
        let bindings = cursor.and_then(|scope| self.1.scopes.get(&scope));

        self.0 = cursor
            // TODO: Factor this in a method?
            .and_then(|current| self.1.enclosing_scope.0.get(&current.origin()))
            .copied();

        bindings
    }
}

impl<'enclosing> FlatScope<'enclosing> {
    fn lookup(&self, name: &Symbol, starting_scope: Scope) -> Option<&OriginIdx> {
        self.lookup_iterator(starting_scope)
            .find_map(|bindings| bindings.get(name))
    }

    fn insert(&mut self, name: Symbol, idx: OriginIdx, scope: Scope) -> Result<(), OriginIdx> {
        // we need to use the innermost scope here, not `lookup`
        if let Some(existing) = self
            .scopes
            .get(&scope)
            .expect("interpreter error: there should always be at least one outer scope")
            .get(&name)
        {
            return Err(*existing);
        }

        self.scopes.entry(scope).or_default().insert(name, idx);

        Ok(())
    }
}

/// A scope map keeps track of the currently available scopes and the current depth
/// level.
#[derive(Clone, Debug)]
struct ScopeMap<'enclosing> {
    pub bindings: FlatScope<'enclosing>,
    pub functions: FlatScope<'enclosing>,
    pub types: FlatScope<'enclosing>,
}

struct NameResolveCtx<'enclosing> {
    enclosing_scope: EnclosingScope<'enclosing>,
    mappings: ScopeMap<'enclosing>,
}

impl<'enclosing> NameResolveCtx<'enclosing> {
    fn new(enclosing_scope: EnclosingScope<'enclosing>) -> NameResolveCtx {
        let empty_scope_map: HashMap<Scope, Bindings> = enclosing_scope
            .0
            .values()
            .map(|scope_idx| (*scope_idx, Bindings::new()))
            .collect();

        NameResolveCtx {
            enclosing_scope,
            mappings: ScopeMap {
                bindings: FlatScope {
                    scopes: empty_scope_map.clone(),
                    enclosing_scope,
                },
                functions: FlatScope {
                    scopes: empty_scope_map.clone(),
                    enclosing_scope,
                },
                types: FlatScope {
                    scopes: empty_scope_map,
                    enclosing_scope,
                },
            },
        }
    }
}

/// Extension type of [`Error`] to be able to implement [`IterError`].
enum NameResolutionError {
    NonUnique(SpanTuple, OriginIdx, &'static str),
    Unresolved(ResolveKind, Symbol, SpanTuple),
    AmbiguousBinding(OriginIdx, OriginIdx, SpanTuple),
    UnresolvedBinding(Symbol, SpanTuple),
    Multiple(Vec<NameResolutionError>),
}

impl NameResolutionError {
    fn finalize(self, fir: &Fir<FlattenData>, _mappings: &ScopeMap) -> Error {
        // we need the FIR... how do we access nodes otherwise??
        match self {
            NameResolutionError::Multiple(errs) => Error::new(ErrKind::Multiple(
                errs.into_iter()
                    .map(|e| e.finalize(fir, _mappings))
                    .collect(),
            )),
            NameResolutionError::NonUnique(loc, origin, kind_str) => {
                let existing = &fir.nodes[&origin].data;
                let sym = existing.ast.symbol().unwrap();

                Error::new(ErrKind::NameResolution)
                    .with_msg(format!("{kind_str} `{sym}` already defined in this scope"))
                    .with_loc(loc)
                    .with_hint(
                        Error::hint()
                            .with_msg(format!("`{sym}` is also defined here"))
                            .with_loc(existing.ast.location().clone()),
                    )
            }
            NameResolutionError::Unresolved(kind, sym, location) => {
                // TODO: Extract possible values based on `kind` and `sym`

                // let map = match kind {
                //     UnresolvedKind::Call => todo!(),
                //     UnresolvedKind::Type => todo!(),
                //     UnresolvedKind::Var => todo!(),
                // }

                // let hints = self.mappings.keys().filter_map(|key| {
                //     if distance::levenshtein(key.symbol_unchecked().access(), err_sym.access()) < 2 {
                //         Some(
                //             Error::hint()
                //                 .with_msg(format!(
                //                     "maybe you meant `{}`?",
                //                     key.symbol_unchecked()
                //                 ))
                //                 .with_loc(key.location().clone()),
                //         )
                //     } else {
                //         None
                //     }
                // });

                // FIXME: factor this with the typechecker for the colorization of the type in purple
                // this should probably be part of the `error` module, with a function like `error::format::ty`
                Error::new(ErrKind::NameResolution)
                    .with_msg(format!("unresolved {kind}: `{}`", sym.access().purple()))
                    .with_loc(location)
            }
            NameResolutionError::AmbiguousBinding(lhs, rhs, location) => {
                let lhs = &fir.nodes[&lhs].data;
                let rhs = &fir.nodes[&rhs].data;
                let sym = lhs.ast.symbol().unwrap();

                Error::new(ErrKind::NameResolution)
                    .with_msg(format!("resolution of `{sym}` is ambiguous"))
                    .with_loc(location)
                    .with_hint(
                        Error::hint()
                            .with_msg(String::from("could point to this binding..."))
                            .with_loc(lhs.ast.location().clone()),
                    )
                    .with_hint(
                        Error::hint()
                            .with_msg(String::from("...or this empty type"))
                            .with_loc(rhs.ast.location().clone()),
                    )
            }
            NameResolutionError::UnresolvedBinding(sym, location) => {
                // TODO: Go through mappings again to find a relevant type or var which could work

                Error::new(ErrKind::NameResolution)
                    .with_msg(format!("unresolved binding to `{sym}`"))
                    .with_loc(location)
                    .with_hint(
                        Error::hint().with_msg(format!("searched for empty type named `{sym}`")),
                    )
                    .with_hint(
                        Error::hint().with_msg(format!("searched for binding named `{sym}`")),
                    )
            }
        }
    }

    /// Compose a [`UniqueError`] into a proper [`Error`] of kind [`ErrKind::NameResolution`]
    fn non_unique(
        location: &SpanTuple,
        UniqueError(origin, kind_str): UniqueError,
    ) -> NameResolutionError {
        NameResolutionError::NonUnique(location.clone(), origin, kind_str)
    }

    fn unresolved(
        kind: ResolveKind,
        sym: Option<&Symbol>,
        location: &SpanTuple,
    ) -> NameResolutionError {
        NameResolutionError::Unresolved(kind, sym.cloned().unwrap(), location.clone())
    }

    fn ambiguous_binding(
        lhs: OriginIdx,
        rhs: OriginIdx,
        location: &SpanTuple,
    ) -> NameResolutionError {
        NameResolutionError::AmbiguousBinding(lhs, rhs, location.clone())
    }

    fn unresolved_binding(sym: Option<&Symbol>, location: &SpanTuple) -> NameResolutionError {
        NameResolutionError::UnresolvedBinding(sym.cloned().unwrap(), location.clone())
    }
}

impl<'enclosing> NameResolveCtx<'enclosing> {
    fn scope(fir: &Fir<FlattenData>) -> HashMap<OriginIdx, Scope> {
        let root = fir.nodes.last_key_value().unwrap();

        let mut scoper = Scoper {
            current_scope: Scope(*root.0),
            enclosing_scope: HashMap::new(),
        };

        let Kind::Statements(stmts) = &root.1.kind else {
            unreachable!()
        };

        stmts
            .iter()
            .for_each(|stmt| scoper.traverse_node(fir, &fir[stmt]).unwrap());

        scoper.enclosing_scope
    }

    fn insert_definitions(&mut self, fir: &Fir<FlattenData>) -> Fallible<Vec<NameResolutionError>> {
        Declarator(self).traverse(fir)
    }

    fn resolve_nodes<'ast>(
        &mut self,
        fir: Fir<FlattenData<'ast>>,
    ) -> Result<Fir<FlattenData<'ast>>, Incomplete<FlattenData<'ast>, NameResolutionError>> {
        let mut resolver = Resolver {
            ctx: self,
            required_parents: Vec::new(),
        };

        let fir = resolver.map(fir)?;

        let latent_map =
            resolver
                .required_parents
                .into_iter()
                .fold(HashMap::new(), |mut map, reqd| {
                    let required_node = &fir[&reqd];

                    let scope = match required_node.kind {
                        Kind::Instantiation { to, .. } => Scope(to.expect_resolved()),
                        _ => unreachable!(),
                    };

                    map.insert(reqd, scope);

                    map
                });

        LatentResolver {
            ctx: self,
            latent_map,
        }
        .map(fir)
    }
}

impl<'ast, 'enclosing> Pass<FlattenData<'ast>, FlattenData<'ast>, Error>
    for NameResolveCtx<'enclosing>
{
    fn pre_condition(_fir: &Fir<FlattenData>) {}

    fn post_condition(_fir: &Fir<FlattenData>) {}

    fn transform(&mut self, fir: Fir<FlattenData<'ast>>) -> Result<Fir<FlattenData<'ast>>, Error> {
        let definition = self.insert_definitions(&fir);
        let definition = definition
            .map_err(|errs| NameResolutionError::Multiple(errs).finalize(&fir, &self.mappings));

        let resolution = self.resolve_nodes(fir);

        match (definition, resolution) {
            (Ok(_), Ok(fir)) => Ok(fir),
            (Ok(_), Err(Incomplete { carcass, errs })) => {
                Err(NameResolutionError::Multiple(errs).finalize(&carcass, &self.mappings))
            }
            (Err(e), Ok(_fir)) => Err(e),
            (Err(e1), Err(Incomplete { carcass, errs })) => {
                let multi_err = Error::new(ErrKind::Multiple(vec![
                    e1,
                    NameResolutionError::Multiple(errs).finalize(&carcass, &self.mappings),
                ]));
                Err(multi_err)
            }
        }
    }
}

pub trait NameResolve<'ast> {
    fn name_resolve(self) -> Result<Fir<FlattenData<'ast>>, Error>;
}

impl<'ast> NameResolve<'ast> for Fir<FlattenData<'ast>> {
    fn name_resolve(self) -> Result<Fir<FlattenData<'ast>>, Error> {
        // TODO: Ugly asf
        let enclosing_scope = NameResolveCtx::scope(&self);
        let mut ctx = NameResolveCtx::new(EnclosingScope(&enclosing_scope));

        ctx.transform(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fir::{Kind, RefIdx};
    use flatten::FlattenAst;
    use xparser::ast;

    #[test]
    fn declaration() {
        let ast = ast! {
            where a = 15;
            where b = a;
        };

        let fir = ast.flatten().name_resolve().unwrap();

        let a = &fir.nodes[&OriginIdx(2)];
        let a_reference = &fir.nodes[&OriginIdx(3)];

        assert!(matches!(a_reference.kind, Kind::TypedValue { .. }));
        let Kind::TypedValue {
            value: a_reference, ..
        } = a_reference.kind
        else {
            unreachable!()
        };

        assert_eq!(a_reference, RefIdx::Resolved(a.origin));
    }

    #[test]
    fn function_call() {
        let ast = ast! {
            func a() {}

            a();
        };

        let fir = ast.flatten().name_resolve().unwrap();

        // find the unique call and definition
        let def = fir
            .nodes
            .values()
            .find(|node| matches!(node.kind, Kind::Function { .. }));
        let call = fir
            .nodes
            .values()
            .find(|node| matches!(node.kind, Kind::Call { .. }))
            .map(|node| match node.kind {
                Kind::Call { to, .. } => to,
                _ => unreachable!(),
            });

        assert_eq!(call.unwrap(), RefIdx::Resolved(def.unwrap().origin));
    }

    #[test]
    fn type_def() {
        let ast = ast! {
            type T;

            where x = T;
        };

        let fir = ast.flatten().name_resolve().unwrap();

        // find the unique call and definition
        let def = fir
            .nodes
            .values()
            .find(|node| matches!(node.kind, Kind::RecordType { .. }));
        let (value, ty) = fir
            .nodes
            .values()
            .find(|node| matches!(node.kind, Kind::TypedValue { .. }))
            .map(|node| match node.kind {
                Kind::TypedValue { value, ty } => (value, ty),
                _ => unreachable!(),
            })
            .unwrap();

        assert_eq!(value, RefIdx::Resolved(def.unwrap().origin));
        assert_eq!(ty, RefIdx::Resolved(def.unwrap().origin));
    }

    #[test]
    fn ambiguous_var() {
        let ast = ast! {
            where x = X;
        };

        let fir = ast.flatten().name_resolve();

        assert!(fir.is_err());
    }

    #[test]
    fn function_argument() {
        let ast = ast! {
            type int;

            func id(x: int) -> int { x }
        };

        let fir = ast.flatten().name_resolve();

        assert!(fir.is_ok());
    }

    #[test]
    fn complex() {
        let ast = ast! {
            type int;

            func id(x: int) -> int {
                x
            }

            type Id(value: int);

            where x = Id(value: 15);
            where y = 14;
            where z = id(y);
        };

        let fir = ast.flatten().name_resolve();

        assert!(fir.is_ok());
    }

    #[test]
    fn builtin_type() {
        let ast = ast! {
            type bool;
            type true;
            type false;

            func foo() -> bool { true }
        };

        let fir = ast.flatten().name_resolve();

        if let Err(e) = &fir {
            e.emit();
        }

        assert!(fir.is_ok());
    }

    #[test]
    fn scoped_resolution() {
        let ast = ast! {
            type Marker;

            {
                type Marker;
                where x = Marker;
            }
        };

        let fir = ast.flatten().name_resolve().unwrap();

        let marker_1 = dbg!(&fir.nodes[&OriginIdx(1)]);
        let marker_2 = dbg!(&fir.nodes[&OriginIdx(2)]);
        let x_value = dbg!(&fir.nodes[&OriginIdx(3)]);

        assert!(matches!(x_value.kind, Kind::TypedValue { .. }));
        assert!(matches!(marker_1.kind, Kind::RecordType { .. }));
        assert!(matches!(marker_2.kind, Kind::RecordType { .. }));

        match x_value.kind {
            Kind::TypedValue { value, ty } => {
                assert_eq!(value, ty);
                assert_eq!(ty, RefIdx::Resolved(marker_2.origin));
                assert_eq!(value, RefIdx::Resolved(marker_2.origin));
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn fail_resolution_to_fn_arg() {
        let ast = ast! {
            type A;

            func f(a: A) {}

            where x = a; // invalid
        };

        let fir = ast.flatten().name_resolve();

        assert!(fir.is_err());
    }

    #[test]
    fn issue615() {
        let ast = ast! {
            type Foo;

            type A(field: Foo);
            type B(field: Foo);
        };

        let fir = ast.flatten().name_resolve();

        assert!(fir.is_ok())
    }

    #[test]
    fn multi_type_nameres_invalid() {
        let ast = ast! {
            type Foo = Bar | Baz;
        };

        let fir = ast.flatten().name_resolve();

        assert!(fir.is_err())
    }

    #[test]
    #[ignore]
    fn multi_type_nameres_valid() {
        let ast = ast! {
            type Bar;
            type Baz;
            type Foo = Bar | Baz;
        };

        let fir = ast.flatten().name_resolve();

        assert!(fir.is_ok())
    }

    #[test]
    fn multi_type_nameres_fn_arg_valid() {
        let ast = ast! {
            type Bar;
            type Baz;
            func foo(a: Bar | Baz) {}
        };

        let fir = ast.flatten().name_resolve();

        assert!(fir.is_ok())
    }

    #[test]
    fn multi_type_nameres_fn_arg_invalid() {
        let ast = ast! {
            type Bar;
            func foo(a: Bar | Baz) {}
        };

        let fir = ast.flatten().name_resolve();

        assert!(fir.is_err())
    }

    #[test]
    fn multi_type_in_record_nameres_valid() {
        let ast = ast! {
            type Bar;
            type Baz;
            type Foo(inner: Bar | Baz);
        };

        let fir = ast.flatten().name_resolve();

        assert!(fir.is_ok())
    }

    #[test]
    fn multi_type_in_record_nameres_invalid() {
        let ast = ast! {
            type Bar;
            type Foo(inner: Bar | Baz);
        };

        let fir = ast.flatten().name_resolve();

        assert!(fir.is_err())
    }
}
