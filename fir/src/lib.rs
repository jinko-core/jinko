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
//!   {OriginIdx::1, Declaration(args: [], return_type: RefIdx::Unresolved)},
//!   {OriginIdx::2, Call(to: RefIdx::Unresolved, args: [])}
//! ]
//! ````
//!
//! Once name resolution is performed, we want the following graph:
//!
//! ```ignore
//! [
//!   {OriginIdx::1, Declaration(args: [], return_type: RefIdx::Unresolved)},
//!   {OriginIdx::2, Call(to: RefIdx::Resolved(1), args: [])} // < here
//! ]
//! ````
//!
//! We still have no type information. Once the typecheckin pass is done, we want
//! something like that:
//!
//! ```ignore
//! [
//!   {OriginIdx::1, Declaration(args: [], return_type: RefIdx::Resolved(void_type)}, // < here
//!   {OriginIdx::2, Call(to: RefIdx::Resolved(1), args: [])}
//! ]
//! ````
//!
//! (Assuming that we have an OriginIdx point for the various builtin types such as `void`,
//! `int`, `float`... so the graph would actually look something like that at this point,
//! with builtin type nodes having been inserted before type-checking.
//!
//! ```ignore
//! [
//!   {OriginIdx::1, Declaration(args: [], return_type: RefIdx::Resolved(3)}, // < here...
//!   {OriginIdx::2, Call(to: RefIdx::Resolved(1), args: [])},
//!   {OriginIdx::3, Type(name: "void",   generics: [], fields: [])} // < ...and here
//!   {OriginIdx::4, Type(name: "int",    generics: [], fields: [])} // < ...and here
//!   {OriginIdx::5, Type(name: "float",  generics: [], fields: [])} // < ...and here
//!   {OriginIdx::6, Type(name: "char",   generics: [], fields: [])} // < ...and here
//!   {OriginIdx::7, Type(name: "bool",   generics: [], fields: [])} // < ...and here
//!   {OriginIdx::8, Type(name: "string", generics: [], fields: [])} // < ...and here
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
//     ResolvedCall(RefIdx, Vec<RefIdx>, Vec<RefIdx>),
// }
// Should name resolution be done separately and Fir only works using RefIdx? Yes?
// Can we have that and have the typechecker work? By simply changing relations in the graph?
// Does that make sense? Does that indicate that for all types we must first keep a Option<Ty> which is set to None?
// Is this going to cause problems?

use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

use location::SpanTuple;

/// A reference to another [`Node`] in the [`Fir`]. These references can be either resolved or unresolved, based
/// on the state of the [`Fir`].
/// For [`Node`]s whose [`Kind`] is a reference to a definition, for example a call to a function or the usage
/// of a variable, there should be an extra [`RefIdx`] stored in the [`Kind`], which should at first be
/// [`RefIdx::Unresolved`] before being changed to a [`RefIdx::Resolved`].
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum RefIdx {
    /// An unresolved index into the [`Fir`]. This indicates a step that has not been done yet: Either a
    /// declaration/origin point has not been visited yet, or a reference to a declaration has not been resolved
    /// yet
    Unresolved,
    /// A resolved reference to a definition/origin point.
    Resolved(OriginIdx),
}

/// Each [`Node`] in the [`Fir`] is its own [`OriginIdx`], which is an origin point. This is a bit wasteful
/// since most nodes aren't definitions and instead *refer* to definitions, but it makes it easy to refer to
/// call points or to emit errors.
/// An origin point - this is where a variable, type or function is defined. Later uses of that
/// object (variable, type or function) are resolved to [`RefIdx::Resolved`]s.
#[derive(Debug, Default, Clone, Copy, Hash, PartialEq, Eq)]
pub struct OriginIdx(pub u64);

impl OriginIdx {
    /// Get the next origin from an existing one. This allows [`Pass`]es to simply keep an [`OriginIdx`] in their context
    /// and repeatedly call [`next`] on it.
    pub fn next(&self) -> OriginIdx {
        OriginIdx(self.0 + 1)
    }
}

/// Helper trait to enable an immutable pattern on the [`Fir`]
trait WithHashMap<K: Hash + Eq, V> {
    fn with(self, key: K, value: V) -> Self;
}

impl<K: Hash + Eq, V> WithHashMap<K, V> for HashMap<K, V> {
    fn with(mut self, key: K, value: V) -> HashMap<K, V> {
        self.insert(key, value);

        self
    }
}

/// An abstraction over a statically typed value. This is similar to keeping a tuple of [`RefIdx`]s,
/// but provides some helper methods
#[derive(Debug, Clone)]
pub struct TypedValue(RefIdx, RefIdx);

impl TypedValue {
    /// Get the underlying [`RefIdx`] to the "value" contained in the [`TypedValue`]
    pub fn value(&self) -> RefIdx {
        self.0
    }

    /// Get the underlying [`RefIdx`] to the type contained in the [`TypedValue`]
    pub fn ty(&self) -> RefIdx {
        self.1
    }
}

#[derive(Debug, Clone)]
pub enum Kind {
    // FIXME: Figure out more leaf/basic block types
    Type {
        generics: Vec<RefIdx>, // to Kind::Generic
        fields: Vec<TypedValue>,
    },
    /// A statically typed constant. This can be either a literal or an empty type
    Constant(RefIdx), // to Kind::Type
    // FIXME: Should we keep a TypedValue here as kind and instead of keeping
    // Vec<TypedValue>, keep Vec<RefIdx> to Kind::TypedValue?
    Generic {
        default: Option<RefIdx>,
    },
    FnDeclaration {
        generics: Vec<RefIdx>, // to Kind::Generic
        args: Vec<TypedValue>,
    },
    Call {
        to: RefIdx,            // to Kind::FnDeclaration
        generics: Vec<RefIdx>, // to Kind::Type
        args: Vec<TypedValue>,
    },
    Instantiation {
        to: RefIdx,            // to Kind::Type
        generics: Vec<RefIdx>, // to Kind::Type
        fields: Vec<TypedValue>,
    },
}

#[derive(Debug, Clone)]
pub struct Node<T: Debug = ()> {
    pub data: T,
    pub origin: OriginIdx,
    // FIXME: Do we need to keep a Symbol here?
    // FIXME: Can we avoid that by doing mappings between the AST and FIR?
    // FIXME: Should we do something along the lines of Node<T = ()>? And keep an instance of T within the node?
    // FIXME: Is that going to be annoying to use? Do we actually need a Location here?
    pub location: Option<SpanTuple>,
    pub kind: Kind,
}

/// An instance of [`Fir`] is similar to a graph, containing [`Node`]s and relationships binding them together.
#[derive(Default)]
pub struct Fir<T: Debug = ()> {
    pub nodes: HashMap<OriginIdx, Node<T>>,
}

impl<T: Debug> Fir<T> {
    /// Append a new node to the [`Fir`]
    pub fn append(self, node: Node<T>) -> Fir<T> {
        Fir {
            nodes: self.nodes.with(node.origin, node),
        }
    }

    /// Check if the [`Fir`] only contains links between entities allowed to link together.
    /// For example, this asserts that there are no calls that have been resolved as calls to constant literals.
    ///
    /// # Panic
    ///
    /// This function panic if the provided [`Fir`] contains invalid relationships.
    pub fn check(&self) {
        macro_rules! check {
            ($ref:expr => $kind:pat, $node:expr) => {
                if let RefIdx::Resolved(origin) = $ref {
                    match self.nodes[&origin].kind {
                        $kind => {}
                        // FIXME: Do a nice error here
                        _ => panic!("invalid relationship detected in `Fir`:\n\n{:#?}\n\nrefers to\n\n{:#?}\n\n[reference: {:?}]", $node, self.nodes[&origin], $ref),
                    }
                }
            };
            (@$iter:expr => $kind:pat, $node:expr) => {
                $iter.iter().for_each(|value| check!(value => $kind, $node))
            };
        }

        self.nodes.iter().for_each(|kv| {
            let node = &kv.1;
            match &node.kind {
                Kind::Constant(r) => check!(r => Kind::Type { .. }, node),
                Kind::Generic {
                    default: Some(default),
                } => check!(default => Kind::Type { .. }, node),
                Kind::FnDeclaration {
                    generics,
                    args: _args,
                } => {
                    check!(@generics => Kind::Generic { .. }, node);
                    // check!(@args => Kind::TypedValue);
                }
                Kind::Call {
                    to,
                    generics,
                    args: _args,
                } => {
                    check!(to => Kind::FnDeclaration { .. }, node);
                    check!(@generics => Kind::Type { .. }, node);
                    // check!(@args => Kind::TypedValue);
                }
                Kind::Type {
                    generics,
                    fields: _fields,
                } => {
                    check!(@generics => Kind::Generic { .. }, node);
                    // check!(@fields => Kind::TypedValue);
                }
                Kind::Instantiation {
                    to,
                    generics,
                    fields: _fields,
                } => {
                    check!(to => Kind::Type { .. }, node);
                    check!(@generics => Kind::Type { .. }, node);
                    // check!(@fields => Kind::TypedValue);
                }
                // Nothing to do for generics without a default
                Kind::Generic { .. } => {}
            }
        })
    }
}

pub trait Pass<T: Debug = (), U: Debug = ()> {
    /// Each implementer of [`Pass`] should keep its own [`OriginIdx`] counter in order to supply the [`Fir`]
    /// with new nodes. This can be done by keeping an [`OriginIdx`] as part of the context, and repeatedly
    /// calling [`OriginIdx::next`] on it.
    fn next_origin(&mut self) -> OriginIdx;

    /// This function should panic if a condition fails to be upheld
    // FIXME: Add a #[cfg(not(release))] here
    fn pre_condition(fir: &Fir<T>);

    /// This function should panic if a condition fails to be upheld
    // FIXME: Add a #[cfg(not(release))] here
    fn post_condition(fir: &Fir<U>);

    /// The actual pass algorithm.
    // FIXME: Should this take an immutable context and return it?
    fn transform(&mut self, fir: Fir<T>) -> Fir<U>;

    // FIXME: Add documentation
    // FIXME: Should this take an immutable context and return it?
    fn pass(&mut self, fir: Fir<T>) -> Fir<U> {
        // FIXME: Add a #[cfg(not(release))] here
        Self::pre_condition(&fir);

        let new_fir = self.transform(fir);

        // FIXME: Add a #[cfg(not(release))] here
        Self::post_condition(&new_fir);
        // FIXME: Add a #[cfg(not(release))] here
        new_fir.check();

        new_fir
    }
}
