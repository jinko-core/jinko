//! A `Value` is an instance in jinko. It refers to primitive types as well as
//! complex ones. Any type can implement the `Value` trait if it wishes to be returned
//! by an instruction

use crate::instruction::{Instruction, Operator};

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

/// Available primitive types
#[derive(Debug)]
#[repr(u8)]
pub enum ValueType {
    Bool,
    Int,
    Float,
    Char,
    Str,
}

/// C is the type contained inside the `Value`
pub trait Value: Instruction {
    /// Return the virtual type of the primitive value.
    ///
    /// ```
    /// let some_int = JinkInt::from(160);
    /// assert_eq!(some_int.vtype(), ValueType::Int);
    /// ```
    fn vtype(&self) -> ValueType;

    /// Call this function when an operation is not implemented, rather than implementing
    /// your own. This will format the error nicely.
    fn no_op(&self, other: &Self, op: Operator) -> Box<dyn Instruction> {
        unreachable!("NOP") // FIXME
    }

    /// Realize any operation implemented by the type, and return a new instance
    /// of a valid type. You cannot add multiple types together, except in one case:
    /// Adding a floating point number and an integer together. Doing that will
    /// return a new JinkFloat.
    ///
    /// ```
    /// let a = JinkInt::from(126);
    /// let b = JinkInt::from(4);
    ///
    /// let res = a.do_op(b, Operator::Add); // JinkInt(130)
    /// assert_eq!(res.vtype(), ValueType::Int);
    ///
    /// let f = JinkFloat::from(4.0);
    ///
    /// let res = a.do_op(f, Operator::Add); // JinkFloat(130.0)
    /// assert_eq!(res.vtype(), ValueType::Float);
    /// ```
    // FIXME: Implement behavior defined here ^
    fn do_op(&self, other: &Self, op: Operator) -> Box<dyn Instruction> {
        self.no_op(other, op)
    }
}
