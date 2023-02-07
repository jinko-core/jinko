//! FIR stands for Flat Intermediate Representation.
//! At first, the "graph" is empty: There are nodes, scattered (flattily) within the structure, and no edges between the nodes
//! (no links, no references, no origin points in fact).
//! Let's take the following jinko program:
//!
//! ```text
//! func f() { }
//!
//! f();
//! ```
//!
//! The [`Fir`] might look something like this:
//!
//! ```text
//! [
//!   {OriginIdx::1, Declaration(args: [], return_type: RefIdx::Unresolved)},
//!   {OriginIdx::2, Call(to: RefIdx::Unresolved, args: [])}
//! ]
//! ````
//!
//! Once name resolution is performed, we want the following graph:
//!
//! ```text
//! [
//!   {OriginIdx::1, Declaration(args: [], return_type: RefIdx::Unresolved)},
//!   {OriginIdx::2, Call(to: RefIdx::Resolved(1), args: [])} // < here
//! ]
//! ````
//!
//! We still have no type information. Once the typecheckin pass is done, we want
//! something like that:
//!
//! ```text
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
//! ```text
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

mod checks;

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

pub type Fallible<E> = Result<(), E>;

/// Helper trait to enable an immutable pattern on the [`Fir`]
trait WithHashMap<K: Hash + Eq, V> {
    fn with(self, key: K, value: V) -> Self;
}

impl<K: Hash + Eq + Debug, V: Debug> WithHashMap<K, V> for HashMap<K, V> {
    fn with(mut self, key: K, value: V) -> HashMap<K, V> {
        if self.insert(key, value).is_some() {
            unreachable!("re-using already insert OriginIdx");
        };

        self
    }
}

// FIXME: Add proc-macro to generates the check automatically for each kind
#[derive(Debug, Clone)]
pub enum Kind {
    // FIXME: Figure out more leaf/basic block types
    // FIXME: Do we need a TypeArgument node?
    /// A statically typed constant. This can be either a literal or an empty type
    // TODO: Do we want newtype patterns so that references can only point to certain types? i.e TypeRef(RefIdx)
    Constant(RefIdx), // to Kind::TypeReference, // FIXME: Is TypeReference the play? Yes, but really that way?
    TypeReference(RefIdx), // to Kind::{Type, Generic}
    TypedValue {
        value: RefIdx, // to Kind::{Call, Instantiation, TypedValue, Constant}
        ty: RefIdx,    // to Kind::Type
    },
    Generic {
        default: Option<RefIdx>, // to Kind::Type
    },
    Type {
        generics: Vec<RefIdx>, // to Kind::Generic
        fields: Vec<RefIdx>,   // to Kind::TypedValue
    },
    Function {
        generics: Vec<RefIdx>,       // to Kind::Generic
        args: Vec<RefIdx>,           // to Kind::TypedValue
        return_type: Option<RefIdx>, // to Kind::Type,
        block: Option<RefIdx>,       // to Kind::Statements
    },
    /// A binding is immutable, however there can be multiple bindings. Assigning to a mutable
    /// variable can be thought of a second binding.
    Binding {
        to: RefIdx, // to Kind::{TypedValue, Instantiation, any expr?},
    },
    Assignment {
        to: RefIdx,   // to Kind::TypedValue
        from: RefIdx, // to FIXME: Can be anything?
    },
    Instantiation {
        to: RefIdx,            // to Kind::Type
        generics: Vec<RefIdx>, // to Kind::Type
        fields: Vec<RefIdx>,   // to Kind::TypedValue
    },
    TypeOffset {
        instance: RefIdx, // to Kind::TypedValue (and more! FIXME)
        field: RefIdx,    // FIXME
    },
    Call {
        to: RefIdx,            // to Kind::FnDeclaration
        generics: Vec<RefIdx>, // to Kind::Type
        args: Vec<RefIdx>,     // to Kind::TypedValue
    },
    Conditional {
        condition: RefIdx, // FIXME
        true_block: RefIdx,
        false_block: Option<RefIdx>,
    },
    Loop {
        condition: RefIdx, // FIXME
        block: RefIdx,     // to Kind::Statements
    },
    Statements(Vec<RefIdx>), // to any kind
    Return(Option<RefIdx>),  // to any kind
}

#[derive(Debug, Clone)]
pub struct Node<T: Debug = ()> {
    pub data: T,
    pub origin: OriginIdx,
    pub kind: Kind,
}

/// An instance of [`Fir`] is similar to a graph, containing [`Node`]s and relationships binding them together.
#[derive(Default, Debug)]
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
}

pub trait Pass<T: Debug, U: Debug, E> {
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
    fn transform(&mut self, fir: Fir<T>) -> Result<Fir<U>, E>;

    // FIXME: Add documentation
    // FIXME: Should this take an immutable context and return it?
    // FIXME: Should this return a Result<Fir<U>, E>?
    fn pass(&mut self, fir: Fir<T>) -> Result<Fir<U>, E> {
        // FIXME: Add a #[cfg(not(release))] here
        Self::pre_condition(&fir);

        let fir = self.transform(fir);

        // FIXME: Add a #[cfg(not(release))] here
        // otherwise just return `fir`
        fir.map(|fir| {
            Self::post_condition(&fir);
            fir.check();

            fir
        })
    }
}

// FIXME: Documentation
pub trait VisitError: Sized {
    // FIXME: Documentation
    fn simple() -> Self;

    // FIXME: Documentation
    fn aggregate(errs: Vec<Self>) -> Self;
}

/// All of the helpers and visitors call back into [`visit_node`]
pub trait Visitor<T: Debug, E: VisitError> {
    fn visit_constant(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _constant: &RefIdx,
    ) -> Fallible<E> {
        Ok(())
    }

    fn visit_type_reference(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _reference: &RefIdx,
    ) -> Fallible<E> {
        Ok(())
    }

    fn visit_typed_value(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _value: &RefIdx,
        _ty: &RefIdx,
    ) -> Fallible<E> {
        Ok(())
    }

    fn visit_generic(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _default: &Option<RefIdx>,
    ) -> Fallible<E> {
        Ok(())
    }

    fn visit_type(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _generics: &[RefIdx],
        _fields: &[RefIdx],
    ) -> Fallible<E> {
        Ok(())
    }

    fn visit_function(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _generics: &[RefIdx],
        _args: &[RefIdx],
        _return_ty: &Option<RefIdx>,
        _block: &Option<RefIdx>,
    ) -> Fallible<E> {
        Ok(())
    }

    fn visit_instantiation(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _to: &RefIdx,
        _generics: &[RefIdx],
        _fields: &[RefIdx],
    ) -> Fallible<E> {
        Ok(())
    }

    fn visit_call(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _to: &RefIdx,
        _generics: &[RefIdx],
        _args: &[RefIdx],
    ) -> Fallible<E> {
        Ok(())
    }

    fn visit_statements(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _stmts: &[RefIdx],
    ) -> Fallible<E> {
        Ok(())
    }

    fn visit_return(
        &mut self,
        _fir: &Fir<T>,
        _node: &Node<T>,
        _expr: &Option<RefIdx>,
    ) -> Fallible<E> {
        Ok(())
    }

    fn visit_node(&mut self, fir: &Fir<T>, node: &Node<T>) -> Fallible<E> {
        match &node.kind {
            Kind::Constant(c) => self.visit_constant(fir, node, c),
            Kind::TypeReference(r) => self.visit_type_reference(fir, node, r),
            Kind::TypedValue { value, ty } => self.visit_typed_value(fir, node, value, ty),
            Kind::Generic { default } => self.visit_generic(fir, node, default),
            Kind::Type { generics, fields } => self.visit_type(fir, node, generics, fields),
            Kind::Function {
                generics,
                args,
                return_type,
                block,
            } => self.visit_function(fir, node, generics, args, return_type, block),
            Kind::Instantiation {
                to,
                generics,
                fields,
            } => self.visit_instantiation(fir, node, to, generics, fields),
            Kind::Call { to, generics, args } => self.visit_call(fir, node, to, generics, args),
            Kind::Statements(stmts) => self.visit_statements(fir, node, stmts),
            Kind::Return(expr) => self.visit_return(fir, node, expr),
        }
    }

    fn visit(&mut self, fir: &Fir<T>) -> Fallible<E> {
        let errs = fir
            .nodes
            .values()
            .fold(Vec::new(), |mut errs: Vec<E>, node| {
                match self.visit_node(fir, node) {
                    Ok(_) => errs,
                    Err(e) => {
                        errs.push(e);
                        errs
                    }
                }
            });

        if errs.is_empty() {
            Ok(())
        } else {
            Err(E::aggregate(errs))
        }
    }
}
