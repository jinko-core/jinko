//! A `Value` is a number instance in jinko. It refers to arithmetic primtive types, such
//! as Ints and Floats

use crate::{instruction::Operator, Instruction, JkError, ObjectInstance};

mod jk_constant;
mod string_interpolation;

pub use jk_constant::JkConstant;

pub type JkBool = JkConstant<bool>;
pub type JkInt = JkConstant<i64>;
pub type JkFloat = JkConstant<f64>;
pub type JkChar = JkConstant<char>;
pub type JkString = JkConstant<String>;

/// C is the type contained inside the `Value`
pub trait Value: Instruction {
    /// Call this function when an operation is not implemented, rather than implementing
    /// your own. This will format the error nicely.
    fn no_op(&self, _other: &Self, _op: Operator) -> Result<ObjectInstance, JkError> {
        unreachable!("NOP") // FIXME
    }

    /// Realize any operation implemented by the type, and return a new instance
    /// of a valid type. You cannot add multiple types together, except in one case:
    /// Adding a floating point number and an integer together. Doing that will
    /// return a new JkFloat.
    ///
    /// ```
    /// let interpreter = Interpreter::new();
    ///
    /// let a = JkInt::from(126);
    /// let b = JkInt::from(4);
    ///
    /// let res = a.do_op(b, Operator::Add); // JkInt(130)
    /// assert_eq!(res.ty(), interpreter.get_type("int"));
    ///
    /// let f = JkFloat::from(4.0);
    ///
    /// let res = a.do_op(f, Operator::Add); // JkFloat(130.0)
    /// assert_eq!(res.ty(), interpreter.get_type("float"));
    /// ```
    // FIXME: Implement behavior defined here ^
    fn do_op(&self, other: &Self, op: Operator) -> Result<ObjectInstance, JkError> {
        self.no_op(other, op)
    }
}
