//! A `Value` is an instance in jinko. It refers to primitive types as well as
//! complex ones. Any type can implement the `Value` trait if it wishes to be returned
//! by an instruction

use crate::instruction::{Instruction, Operator};
use crate::{Instance, JinkoError};

mod jink_constant;

pub use jink_constant::JinkConstant;

pub type JinkBool = JinkConstant<bool>;
pub type JinkInt = JinkConstant<i64>;
pub type JinkFloat = JinkConstant<f64>;
pub type JinkChar = JinkConstant<char>;
pub type JinkString = JinkConstant<String>;

/// C is the type contained inside the `Value`
pub trait Value: Instruction {
    /// Call this function when an operation is not implemented, rather than implementing
    /// your own. This will format the error nicely.
    fn no_op(&self, _other: &Self, _op: Operator) -> Result<Instance, JinkoError> {
        unreachable!("NOP") // FIXME
    }

    /// Realize any operation implemented by the type, and return a new instance
    /// of a valid type. You cannot add multiple types together, except in one case:
    /// Adding a floating point number and an integer together. Doing that will
    /// return a new JinkFloat.
    ///
    /// ```
    /// let interpreter = Interpreter::new();
    ///
    /// let a = JinkInt::from(126);
    /// let b = JinkInt::from(4);
    ///
    /// let res = a.do_op(b, Operator::Add); // JinkInt(130)
    /// assert_eq!(res.ty(), interpreter.get_type("int"));
    ///
    /// let f = JinkFloat::from(4.0);
    ///
    /// let res = a.do_op(f, Operator::Add); // JinkFloat(130.0)
    /// assert_eq!(res.ty(), interpreter.get_type("float"));
    /// ```
    // FIXME: Implement behavior defined here ^
    fn do_op(&self, other: &Self, op: Operator) -> Result<Instance, JinkoError> {
        self.no_op(other, op)
    }
}
