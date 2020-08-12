//! A `Value` is an instance in broccoli. It refers to primitive types as well as
//! complex ones. Any type can implement the `Value` trait if it wishes to be returned
//! by an instruction

pub mod constant;

pub use constant::Constant;

pub trait Value {
    /// The type contained inside the value
    type Contained;

    /// Return the value contained in the `Value`
    fn value(&self) -> Self::Contained;
}
