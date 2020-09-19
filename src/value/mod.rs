//! A `Value` is an instance in broccoli. It refers to primitive types as well as
//! complex ones. Any type can implement the `Value` trait if it wishes to be returned
//! by an instruction

use crate::instruction::Instruction;

mod jink_bool;
mod jink_char;
mod jink_float;
mod jink_int;
mod jink_string;

pub use jink_bool::JinkBool;
pub use jink_char::JinkChar;
pub use jink_float::JinkFloat;
pub use jink_int::JinkInt;
pub use jink_string::JinkString;

/// C is the type contained inside the `Value`
pub trait Value: Instruction {
    /// Return the value contained in the `Value`
    fn value<C>(&self) -> C
    where
        Self: Sized,
    {
        unreachable!("Cannot get value from Value. This is a bug")
    }

    /// Change the value contained in the `Value`
    fn set<C>(&mut self, value: C)
    where
        Self: Sized,
    {
        unreachable!("Cannot set value on Value. This is a bug")
    }
}
