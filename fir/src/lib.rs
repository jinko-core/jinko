//! FIR stands for Flat Intermediate Representation.
//! At first, the "graph" is empty: There are nodes, scattered (flattily) within the structure, and no edges between the nodes
//! (no links, no references, no origin points in fact).
//! Let's take the following jinko program:
//!
//! ```ignore
//! func f() { }
//!
//! f();
//! ```
//!
//! The [`Fir`] might look something like this:
//!
//! ```ignore
//! [
//!   {Origin::1, Declaration(args: [], return_type: Unresolved)},
//!   {Origin::2, Call(to: Unresolved, args: [])}
//! ]
//! ````
//!
//! Once name resolution is performed, we want the following graph:
//!
//! ```ignore
//! [
//!   {Origin::1, Declaration(args: [], return_type: Unresolved)},
//!   {Origin::2, Call(to: Reference::1, args: [])} // < here
//! ]
//! ````
//!
//! We still have no type information. Once the typecheckin pass is done, we want
//! something like that:
//!
//! ```ignore
//! [
//!   {Origin::1, Declaration(args: [], return_type: Reference::void_type)}, // < here
//!   {Origin::2, Call(to: Reference::1, args: [])}
//! ]
//! ````
//!
//! (Assuming that we have an Origin point for the various builtin types such as `void`,
//! `int`, `float`... so the graph would actually look something like that at this point,
//! with builtin type nodes having been inserted before type-checking.
//!
//! ```ignore
//! [
//!   {Origin::1, Declaration(args: [], return_type: Reference::3)}, // < here...
//!   {Origin::2, Call(to: Reference::1, args: [])},
//!   {Origin::3, Type(name: "void",   generics: [], fields: [])} // < ...and here
//!   {Origin::4, Type(name: "int",    generics: [], fields: [])} // < ...and here
//!   {Origin::5, Type(name: "float",  generics: [], fields: [])} // < ...and here
//!   {Origin::6, Type(name: "char",   generics: [], fields: [])} // < ...and here
//!   {Origin::7, Type(name: "bool",   generics: [], fields: [])} // < ...and here
//!   {Origin::8, Type(name: "string", generics: [], fields: [])} // < ...and here
//! ]
//! ````
//!
//! Their position in the [`Fir`] does not really matter, since the representation is... flat.

// FIXME:
// How do we represent types here? How would we perform type checking? How do we perform name resolution?
// Do we keep references? IDs?
// How do we implement our "contracts"? What would they look like here? This will probably end up being super
// jinko-specific
// Can we do something like only keep an ImmutableMap here (maybe more helpers but that's it) and then only
// have extension types which help rebuild a new Fir graph?
// And have a *lot* of enum variants which are then checked for existence during the various passes? i.e
// have `UnresolvedCall` and `UnresolvedVar` as variants, and then after name resolution make sure that
// they are not present anymore?
// fir.nodes.iter().for_each(|node| {
//     match node {
//         UnresolvedCall | UnresolvedVar => unreachable!(),
//         _ => {}
//     }
// })
//
// The base kind would thus be the equivalent of a "leaf" node - something which does not refer to any other nodes,
// and is only referred to. So in the case of jinko we can think of it as Types probably?
// Each variable, each function will refer to a type - that is typechecking.
// functions also refer to types after typechecking. So are the basic nodes types? Constants? These go together, and
// should maybe be grouped as "Values"? Where do we store them? How?
//
// We need trait extensions. This way the name resolver can keep its own mappings and update the Fir (-> return a new one)
// And for example the VM or FIR-e can keep some execution context as it runs through the VM.
// So the entry point of the trait extension would be, e.g `name_resolve` which would create a [`NameResolver`] ctx
// and visit the entire Fir, finally returning a complete one.
//
// How do represent this internally within the [`Kind`] enum? Do we keep something like
// enum Kind {
//     Declaration(...),
//     UnresolvedCall(Symbol, Vec<Node>, Vec<Node>),
//     ResolvedCall(FirIdx, Vec<FirIdx>, Vec<FirIdx>),
// }
// Should name resolution be done separately and Fir only works using FirIdx? Yes?
// Can we have that and have the typechecker work? By simply changing relations in the graph?
// Does that make sense? Does that indicate that for all types we must first keep a Option<Ty> which is set to None?
// Is this going to cause problems?

use std::collections::HashMap;
use std::hash::Hash;

use location::SpanTuple;
use symbol::Symbol;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum FirIdx {
    /// An unresolved index into the [`Fir`]. This indicates a step that has not been done yet: Either a
    /// declaration/origin point has not been visited yet, or a reference to a declaration has not been resolved
    /// yet
    Unresolved,
    /// An origin point - this is where a variable, type or function is defined. Later uses of that
    /// object (variable, type or function) are resolved to [`FirIdx::Reference`]s
    Origin(u64),
    /// A reference to a definition/origin point. The reference should only ever refer to [`FirIdx::Origin`] nodes.
    Reference(u64), // FIXME: Should this instead be a `Reference(&'idx FirIdx)`?
}

pub trait WithHashMap<K: Hash + Eq, V> {
    fn with(self, key: K, value: V) -> Self;
}

impl<K: Hash + Eq, V> WithHashMap<K, V> for HashMap<K, V> {
    fn with(mut self, key: K, value: V) -> HashMap<K, V> {
        self.insert(key, value);

        self
    }
}

#[derive(Debug, Clone)]
pub enum Kind {
    Declaration(Symbol),
    Call {
        to: FirIdx,
        generics: Vec<FirIdx>,
        args: Vec<FirIdx>,
    },
}

#[derive(Debug, Clone)]
pub struct Node {
    pub location: Option<SpanTuple>,
    pub kind: Kind,
}

/// An instance of [`Fir`] is similar to a graph, containing [`Node`]s and relationships binding them together.
#[derive(Default)]
pub struct Fir {
    // FIXME: We need better than a hashmap to represent a graph I think
    pub nodes: HashMap<FirIdx, Node>,
}

pub trait Pass {
    /// This function should panic if a condition fails to be upheld
    fn pre_condition(fir: &Fir);

    /// This function should panic if a condition fails to be upheld
    fn post_condition(fir: &Fir);

    fn pass<F: FnOnce(Fir) -> Result<Fir, u64>>(
        fir: Fir,
        f: F,
    ) -> Result<Fir, u64 /* FIXME: Use a proper Error type */> {
        Self::pre_condition(&fir);

        let fir = f(fir)?;

        Self::post_condition(&fir);

        Ok(fir)
    }
}
