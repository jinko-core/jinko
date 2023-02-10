mod mapper;
mod multi_mapper;
mod visitor;

pub use mapper::Mapper;
pub use multi_mapper::MultiMapper;
pub use visitor::Visitor;

// FIXME: Documentation
pub trait IterError: Sized {
    // FIXME: Documentation
    fn simple() -> Self;

    // FIXME: Documentation
    fn aggregate(errs: Vec<Self>) -> Self;
}
