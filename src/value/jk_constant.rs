use crate::instruction::{InstrKind, Instruction, Operator};
use crate::{
    FromObjectInstance, Interpreter, JkError, JkString, ObjectInstance, ToObjectInstance, Value,
};

use std::convert::TryFrom;

#[derive(Clone)]
/// A JkConstant represents a primitive type in Jinko. It is used in order to
/// implement integers, floating point numbers, characters, booleans and strings, as
/// well as raw byte values later for custom types.
pub struct JkConstant<T>(pub(crate) T);

// We can do a generic implementation instead of copy pasting it 5 times.
// However, this part of the rust compiler is still not ready
// (https://github.com/rust-lang/rust/issues/43408)
//
// Here is the generic implementation:
//
// ```
// impl<T: Sized> ToObjectInstance for JkConstant<T> {
//     fn to_instance(&self) -> ObjectInstance {
//         use std::mem::{size_of, transmute};
//
//         unsafe {
//             ObjectInstance::from_bytes(
//                 None,
//                 size_of::<T>(),
//                 &transmute::<T, [u8; size_of::<T>()]>(self.0),
//             )
//         }
//     }
// }
//
// /* (Plus a specific implementation for JkConstant<String>) */
// ```
//
// which fails with the following error message:
//
// ```
// error[E0277]: the size for values of type `T` cannot be known at compilation time
// --> src/value/jink_constant.rs:34:48
//     |
// 25  | impl <T: Sized> ToObjectInstance for JkConstant<T> {
//     |       - this type parameter needs to be `Sized`
//     ...
// 34  |         &transmute::<T, [u8; size_of::<T>()]>(self.0),
//     |                                        ^ doesn't have a size known at compile-time
// ```

/// Circumvents the need for a generic implementation (see comment).
/// Call it with the type contained in the JkConstant and the &str representation
///
/// ```
/// // Implements a JkConstant<i64> with type displayed as "int"
/// jk_primitive!(i64, "int");
/// ```
macro_rules! jk_primitive {
    // Special implementation for JkBool, in order to have as_bool()
    (bool) => {
        impl ToObjectInstance for JkConstant<bool> {
            fn to_instance(&self) -> ObjectInstance {
                use std::mem::{size_of, transmute};

                unsafe {
                    ObjectInstance::from_bytes(
                        Some("bool".to_string()), // FIXME
                        size_of::<bool>(),
                        &transmute::<bool, [u8; size_of::<bool>()]>(self.0),
                        None,
                    )
                }
            }
        }

        impl FromObjectInstance for JkConstant<bool> {
            fn from_instance(i: &ObjectInstance) -> Self {
                use std::mem::{size_of, transmute};

                unsafe {
                    Self::from(transmute::<[u8; size_of::<bool>()], bool>(
                        TryFrom::try_from(i.data()).unwrap(),
                    ))
                }
            }
        }

        impl Instruction for JkConstant<bool> {
            fn kind(&self) -> InstrKind {
                InstrKind::Expression(None)
            }

            fn print(&self) -> String {
                self.0.to_string()
            }

            fn as_bool(&self, _interpreter: &mut Interpreter) -> Result<bool, JkError> {
                Ok(self.0)
            }

            fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JkError> {
                interpreter.debug("CONSTANT", &self.0.to_string());

                // Since we cannot use the generic ToObjectInstance implementation, we also have to
                // copy paste our four basic implementations for jinko's primitive types...
                Ok(InstrKind::Expression(Some(self.to_instance())))
            }

            fn prefix(&mut self, _: &str) {}
        }
    };
    ($t:ty, $s:expr) => {
        impl ToObjectInstance for JkConstant<$t> {
            fn to_instance(&self) -> ObjectInstance {
                use std::mem::{size_of, transmute};

                unsafe {
                    ObjectInstance::from_bytes(
                        Some($s.to_string()), // FIXME
                        size_of::<$t>(),
                        &transmute::<$t, [u8; size_of::<$t>()]>(self.0),
                        None,
                    )
                }
            }
        }

        impl FromObjectInstance for JkConstant<$t> {
            fn from_instance(i: &ObjectInstance) -> Self {
                use std::mem::{size_of, transmute};

                unsafe {
                    Self::from(transmute::<[u8; size_of::<$t>()], $t>(
                        TryFrom::try_from(i.data()).unwrap(),
                    ))
                }
            }
        }

        impl Instruction for JkConstant<$t> {
            fn kind(&self) -> InstrKind {
                InstrKind::Expression(None)
            }

            fn print(&self) -> String {
                self.0.to_string()
            }

            fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JkError> {
                interpreter.debug("CONSTANT", &self.0.to_string());

                // Since we cannot use the generic ToObjectInstance implementation, we also have to
                // copy paste our four basic implementations for jinko's primitive types...
                Ok(InstrKind::Expression(Some(self.to_instance())))
            }

            fn prefix(&mut self, _: &str) {}
        }
    };
}

jk_primitive!(i64, "int");
jk_primitive!(f64, "float");
jk_primitive!(char, "char");
jk_primitive!(bool);

impl Value for JkConstant<i64> {
    fn do_op(&self, other: &Self, op: Operator) -> Result<ObjectInstance, JkError> {
        match op {
            Operator::Add => Ok(JkConstant::from(self.0 + other.0).to_instance()),
            Operator::Sub => Ok(JkConstant::from(self.0 - other.0).to_instance()),
            Operator::Mul => Ok(JkConstant::from(self.0 * other.0).to_instance()),
            Operator::Div => Ok(JkConstant::from(self.0 / other.0).to_instance()),
            _ => self.no_op(other, op),
        }
    }
}

impl Value for JkConstant<f64> {
    fn do_op(&self, other: &Self, op: Operator) -> Result<ObjectInstance, JkError> {
        match op {
            Operator::Add => Ok(JkConstant::from(self.0 + other.0).to_instance()),
            Operator::Sub => Ok(JkConstant::from(self.0 - other.0).to_instance()),
            Operator::Mul => Ok(JkConstant::from(self.0 * other.0).to_instance()),
            Operator::Div => Ok(JkConstant::from(self.0 / other.0).to_instance()),
            _ => self.no_op(other, op),
        }
    }
}

impl ToObjectInstance for JkString {
    fn to_instance(&self) -> ObjectInstance {
        ObjectInstance::from_bytes(
            Some("string".to_string()), // FIXME
            self.0.as_bytes().len(),
            self.0.as_bytes(),
            None,
        )
    }
}

impl FromObjectInstance for JkString {
    fn from_instance(i: &ObjectInstance) -> Self {
        // unchecked is safe because this instance came from a utf8 string in ToObjectInstance
        unsafe { JkString::from(String::from_utf8_unchecked(i.data().to_vec())) }
    }
}

impl Instruction for JkString {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression(None)
    }

    fn print(&self) -> String {
        format!("\"{}\"", self.0.clone())
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JkError> {
        interpreter.debug("CONSTANT", &self.0.to_string());

        Ok(InstrKind::Expression(Some(self.to_instance())))
    }

    fn prefix(&mut self, _: &str) {}
}

impl From<&str> for JkConstant<String> {
    fn from(s: &str) -> Self {
        JkConstant(s.to_string())
    }
}

impl<T> From<T> for JkConstant<T> {
    fn from(rust_value: T) -> Self {
        JkConstant(rust_value)
    }
}
