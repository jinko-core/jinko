// them with.

// so we would like to traverse call points, and to build a map of declaration points with what to substitute them
// Find all the generic callpoints - find their associated declpoints - add that to the Map<Declpoint, Vec<(Subs, NewOriginIdx)>>
// TODO: How to handle children? We need to visit those as well and create a mono request too
// NOTE: We want to mono ALL children - even if they do not make use of the substitutions. Otherwise we'll probably run into issues, right?
// maybe we can revisit this later as a space optimization?

// e.g. let's look at the following code:
//
// ```text
// type Foo[T](a: T, b: T);
//
// where f = Foo[int](a: 15, b: 14);
// ```
// the FIR for this looks like the following:
//
// ```text
// {
//    1: Field(a, T)
//    2: Field(b, T)
//    3: Type(Foo, [T], [1, 2])
//    4: Assign(1, Constant(15))
//    5: Assign(2, Constant(14))
//    6: Instantiation(3 /* Foo */, [int], [4, 5])
// }
// ```
// NOTE: Do we need two maps? One for declpoints and one for callpoints? or is that not necessary and I'm going about it the wrong way
// the output we want is something like this:
// ```text
// { 1: Subs(T -> int), NewIdx(7) }
// { 2: Subs(T -> int), NewIdx(8) }
// { 3: Subs(T -> int), NewIdx(9) }
// ```
// then we'll have to figure out 1. how to do those substitutions 2. how to replace `3` by `9` (and others) in the FIR

use fir::iter::TreeLike;
use fir::{Fir, Kind, Node, OriginIdx, RefIdx};
use flatten::FlattenData;

use core::mem;
use std::collections::HashMap;
use std::marker::PhantomData;

#[derive(Debug, Copy, Clone)]
pub struct NewOrigin(OriginIdx);

#[derive(Debug)]
pub struct OriginVector(Vec<OriginIdx>);

impl From<&[RefIdx]> for OriginVector {
    fn from(references: &[RefIdx]) -> OriginVector {
        let origins = references.iter().map(|r| r.expect_resolved()).collect();

        OriginVector(origins)
    }
}

#[derive(Debug)]
pub struct Generics<T> {
    generics: OriginVector,
    _marker: PhantomData<T>,
}

impl<T> Generics<T> {
    pub fn new(generics: impl Into<OriginVector>) -> Generics<T> {
        Generics {
            generics: generics.into(),
            _marker: PhantomData,
        }
    }
}

#[derive(Debug)]
pub struct Decls(Generics<Decls>);
#[derive(Debug)]
pub struct Args(Generics<Args>);

#[derive(Debug)]
pub struct SubsTarget {
    from: Decls,
    to: Args,
}

impl SubsTarget {
    pub fn new(from: Decls, to: Args) -> SubsTarget {
        SubsTarget { from, to }
    }
}

#[derive(Default, Debug)]
pub struct Output {
    // TODO: Should we store OriginIdx instead of NewOrigins in both maps?
    pub(crate) to_mono: HashMap<OriginIdx, Vec<(SubsTarget, NewOrigin)>>,
    pub(crate) substitutions: HashMap<OriginIdx, NewOrigin>,
}

// TODO: We also need to keep a stack of Context (SubsitutionCtx?) in order to put in monomorphize requests for the statements of a function, and add these to the substitution list
pub struct Substitutions {
    output: Output,
    next_idx: OriginIdx,
}

impl Substitutions {
    // FIXME: Should this return Self?
    pub fn find<'ast>(fir: &Fir<FlattenData<'ast>>) -> Output {
        let next_idx = fir
            .nodes
            .last_key_value()
            .map(|kv| kv.0.next())
            .unwrap_or(OriginIdx(1));

        let mut ctx = Substitutions {
            output: Output::default(),
            next_idx,
        };

        ctx.visit_all(fir);

        ctx.output
    }

    pub fn next_origin(&mut self) -> NewOrigin {
        let new_next = self.next_idx.next();
        let next = mem::replace(&mut self.next_idx, new_next);

        NewOrigin(next)
    }

    #[must_use]
    // TODO: Rename
    fn add_mono_request(
        &mut self,
        fir: &Fir<FlattenData<'_>>,
        to: &RefIdx,
        generics: &[RefIdx],
    ) -> NewOrigin {
        let from = &fir[to];
        let from = match &from.kind {
            Kind::Function { generics, .. } => generics.as_slice(),
            _ => unreachable!(),
        };
        let next_origin = self.next_origin();
        let mono_request = (
            SubsTarget::new(Decls(Generics::new(from)), Args(Generics::new(generics))),
            next_origin,
        );

        self.output
            .to_mono
            .entry(to.expect_resolved())
            .or_insert_with(|| vec![])
            .push(mono_request);

        next_origin
    }

    fn substitute(&mut self, from: OriginIdx, to: NewOrigin) {
        assert!(self.output.substitutions.insert(from, to).is_none());
    }
}

impl<'ast> TreeLike<FlattenData<'ast>> for Substitutions {
    fn visit_call(
        &mut self,
        fir: &Fir<FlattenData<'ast>>,
        node: &Node<FlattenData<'ast>>,
        to: &RefIdx,
        generics: &[RefIdx],
        _args: &[RefIdx],
    ) {
        if !generics.is_empty() {
            let new_fn = self.add_mono_request(fir, to, generics);

            // how does that work actually? arguments are just regular expressions - we don't need to monomorphize them, actually, right?
            // we only need to change what they resolve to? how do we do that?? we don't need to create a new node for the call either, actually - we just change what it resolves to
            // that's gonna be one extra TreeLike?
            // self.visit_many(fir, args)

            // or we can actually store an extra map in the Substitutions - after all, this is also a substitution! - from the current OriginIdx to the one we just created

            self.substitute(node.origin, new_fn);
        }
    }

    fn visit_instantiation(
        &mut self,
        fir: &Fir<FlattenData<'ast>>,
        node: &Node<FlattenData<'ast>>,
        to: &RefIdx,
        generics: &[RefIdx],
        _fields: &[RefIdx],
    ) {
        if !generics.is_empty() {
            let new_type = self.add_mono_request(fir, to, generics);

            // self.visit_many(fir, fields)

            self.substitute(node.origin, new_type);
        }
    }
}
