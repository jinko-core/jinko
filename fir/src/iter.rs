mod mapper;
mod multi_mapper;
mod traverse;

pub use mapper::Mapper;
pub use multi_mapper::MultiMapper;
pub use traverse::Traversal;

use crate::Fir;

// FIXME: Documentation
pub trait IterError: Sized {
    // FIXME: Documentation
    fn aggregate(errs: Vec<Self>) -> Self;
}

// FIXME: Documentation
pub struct Incomplete<T, E>(pub Fir<T>, pub E);
