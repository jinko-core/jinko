//! This module offers three types of iterators to transform a given [`Fir`]. They are equivalent
//! to `for_each` and `map`.
//!
//! 1. `traverse`
//!
//! The first one, [`Traversal`], offers the [`Traversal::traverse`] function, which
//! allows one to define how an [`Fir`] should be immutably traversed and viewed. It offers
//! aggregating information about the [`Fir`], which might be useful before applying
//! a transformation on it. It can also be used to easily check relationships or
//! individual nodes in the [`Fir`].
//!
//! 2. `mapper`
//!
//! [`Mapper`] offers a `map` operation on the [`Fir`]. Each node can be transformed into another
//! one through various operations.
//!
//! 3. `multi_mapper`
//!
//! The [`MultiMapper`] trait is useful for mapping nodes to multiple new nodes: For example,
//! monomorphization of a generic function might generate multiple nodes from a single one.

mod mapper;
mod multi_mapper;
mod traverse;

pub use mapper::Mapper;
pub use multi_mapper::MultiMapper;
pub use traverse::Traversal;

use crate::Fir;

/// An incomplete [`Fir`]. This is returned by the various mappers within this module. The
/// incomplete [`Fir`] contains all of the nodes for which the mapping operation succeeded,
/// which enables you to still perform error reporting, hinting, or to keep going with more
/// operations. You can also access the errors emitted by the mapping pass. This vector of
/// errors is guaranteed to not be empty.
pub struct Incomplete<T, E> {
    pub carcass: Fir<T>,
    pub errs: Vec<E>,
}

/// A fallible operation whose value in the [`Ok`] case does not matter
pub type Fallible<E> = Result<(), E>;
