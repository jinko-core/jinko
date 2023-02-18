mod mapper;
mod multi_mapper;
mod traverse;

pub use mapper::Mapper;
pub use multi_mapper::MultiMapper;
pub use traverse::Traversal;

use crate::Fir;

// FIXME: Documentation
pub struct Incomplete<T, E> {
    pub carcass: Fir<T>,
    pub errs: Vec<E>,
}
