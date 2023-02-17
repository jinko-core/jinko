mod mapper;
mod multi_mapper;
mod traverse;

pub use mapper::Mapper;
pub use multi_mapper::MultiMapper;
pub use traverse::Traversal;

// FIXME: Documentation
pub trait IterError: Sized {
    // FIXME: Documentation
    fn aggregate(errs: Vec<Self>) -> Self;
}
