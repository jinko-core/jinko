use crate::{JkBool, JkChar, JkFloat, JkInt, JkString, Value, FromInstance, Instance, Interpreter, JkError, ToInstance};
use crate::instruction::{InstrKind, Instruction, Operator};

use std::convert::TryFrom;

#[derive(Clone)]
pub struct JkConstant<T>(pub(crate) T);

impl Instruction for JkInt {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression(None)
    }

    fn print(&self) -> String {
        self.0.to_string()
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JkError> {
        interpreter.debug("CONSTANT", &self.0.to_string());

        // Since we cannot use the generic ToInstance implementation, we also have to
        // copy paste our four basic implementations for jinko's primitive types...
        Ok(InstrKind::Expression(Some(self.to_instance())))
    }
}

impl Instruction for JkFloat {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression(None)
    }

    fn print(&self) -> String {
        self.0.to_string()
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JkError> {
        interpreter.debug("CONSTANT", &self.0.to_string());

        // Since we cannot use the generic ToInstance implementation, we also have to
        // copy paste our four basic implementations for jinko's primitive types...
        Ok(InstrKind::Expression(Some(self.to_instance())))
    }
}

impl Instruction for JkBool {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression(None)
    }

    fn print(&self) -> String {
        self.0.to_string()
    }

    fn as_bool(&self, _: &mut Interpreter) -> Result<bool, JkError> {
        Ok(self.0)
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JkError> {
        interpreter.debug("CONSTANT", &self.0.to_string());

        // Since we cannot use the generic ToInstance implementation, we also have to
        // copy paste our four basic implementations for jinko's primitive types...
        Ok(InstrKind::Expression(Some(self.to_instance())))
    }
}

impl Instruction for JkChar {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression(None)
    }

    fn print(&self) -> String {
        self.0.to_string()
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JkError> {
        interpreter.debug("CONSTANT", &self.0.to_string());

        // Since we cannot use the generic ToInstance implementation, we also have to
        // copy paste our four basic implementations for jinko's primitive types...
        Ok(InstrKind::Expression(Some(self.to_instance())))
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

        // Since we cannot use the generic ToInstance implementation, we also have to
        // copy paste our four basic implementations for jinko's primitive types...
        Ok(InstrKind::Expression(Some(self.to_instance())))
    }
}

// We can do a generic implementation instead of copy pasting it 5 times.
// However, this part of the rust compiler is still not ready
// (https://github.com/rust-lang/rust/issues/43408)
//
// Here is the generic implementation:
//
// ```
// impl<T: Sized> ToInstance for JkConstant<T> {
//     fn to_instance(&self) -> Instance {
//         use std::mem::{size_of, transmute};
//
//         unsafe {
//             Instance::from_bytes(
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
// 25  | impl <T: Sized> ToInstance for JkConstant<T> {
//     |       - this type parameter needs to be `Sized`
//     ...
// 34  |         &transmute::<T, [u8; size_of::<T>()]>(self.0),
//     |                                        ^ doesn't have a size known at compile-time
// ```

impl ToInstance for JkInt {
    fn to_instance(&self) -> Instance {
        use std::mem::{size_of, transmute};

        unsafe {
            Instance::from_bytes(
                Some("int".to_string()), // FIXME
                size_of::<i64>(),
                &transmute::<i64, [u8; size_of::<i64>()]>(self.0),
            )
        }
    }
}

impl ToInstance for JkFloat {
    fn to_instance(&self) -> Instance {
        use std::mem::{size_of, transmute};

        unsafe {
            Instance::from_bytes(
                Some("float".to_string()), // FIXME
                size_of::<f64>(),
                &transmute::<f64, [u8; size_of::<f64>()]>(self.0),
            )
        }
    }
}

impl ToInstance for JkBool {
    fn to_instance(&self) -> Instance {
        use std::mem::{size_of, transmute};

        unsafe {
            Instance::from_bytes(
                Some("bool".to_string()), // FIXME
                size_of::<bool>(),
                &transmute::<bool, [u8; size_of::<bool>()]>(self.0),
            )
        }
    }
}

impl ToInstance for JkChar {
    fn to_instance(&self) -> Instance {
        use std::mem::{size_of, transmute};

        unsafe {
            Instance::from_bytes(
                Some("char".to_string()), // FIXME
                size_of::<char>(),
                &transmute::<char, [u8; size_of::<char>()]>(self.0),
            )
        }
    }
}

impl ToInstance for JkString {
    fn to_instance(&self) -> Instance {
        Instance::from_bytes(
            Some("string".to_string()), // FIXME
            self.0.as_bytes().len(),
            self.0.as_bytes(),
        )
    }
}

// In the same vein, we also have to implement FromInstance this way...

impl FromInstance for JkInt {
    fn from_instance(i: &Instance) -> Self {
        use std::mem::{size_of, transmute};

        unsafe {
            JkInt::from(transmute::<[u8; size_of::<i64>()], i64>(
                TryFrom::try_from(i.data()).unwrap(),
            ))
        }
    }
}

impl FromInstance for JkFloat {
    fn from_instance(i: &Instance) -> Self {
        use std::mem::{size_of, transmute};

        unsafe {
            JkFloat::from(transmute::<[u8; size_of::<f64>()], f64>(
                TryFrom::try_from(i.data()).unwrap(),
            ))
        }
    }
}

impl FromInstance for JkBool {
    fn from_instance(i: &Instance) -> Self {
        use std::mem::{size_of, transmute};

        unsafe {
            JkBool::from(transmute::<[u8; size_of::<bool>()], bool>(
                TryFrom::try_from(i.data()).unwrap(),
            ))
        }
    }
}

impl FromInstance for JkChar {
    fn from_instance(i: &Instance) -> Self {
        use std::mem::{size_of, transmute};

        unsafe {
            JkChar::from(transmute::<[u8; size_of::<char>()], char>(
                TryFrom::try_from(i.data()).unwrap(),
            ))
        }
    }
}

impl FromInstance for JkString {
    fn from_instance(i: &Instance) -> Self {
        // unchecked is safe because this instance came from a utf8 string in ToInstance
        unsafe { JkString::from(String::from_utf8_unchecked(i.data().to_vec())) }
    }
}

// impl FromInstance for JkFloat {
//     fn from_instance(i: &Instance) -> Self {
//         use std::mem::{transmute, size_of};
//
//         JkFloat::from(transmute::<[u8; size_of::<f64>()], f64>(i.data()))
//     }
// }
//
// impl FromInstance for JkBool {
//     fn from_instance(i: &Instance) -> Self {
//         use std::mem::{transmute, size_of};
//
//         JkBool::from(transmute::<[u8; size_of::<bool>()], bool>(i.data()))
//     }
// }
//
// impl FromInstance for JkChar {
//     fn from_instance(i: &Instance) -> Self {
//         use std::mem::{transmute, size_of};
//
//         JkChar::from(transmute::<[u8; size_of::<char>()], char>(i.data()))
//     }
// }
//
// impl FromInstance for JkString {
//     fn from_instance(i: &Instance) -> Self {
//         use std::mem::{transmute, size_of};
//
//         JkString::from(transmute::<[u8; size_of::<String>()], String>(i.data()))
//     }
// }

impl Value for JkConstant<i64> {
    fn do_op(&self, other: &Self, op: Operator) -> Result<Instance, JkError> {
        match op {
            Operator::Add => Ok(JkConstant::from(self.0 + other.0).to_instance()),
            Operator::Sub => Ok(JkConstant::from(self.0 - other.0).to_instance()),
            Operator::Mul => Ok(JkConstant::from(self.0 * other.0).to_instance()),
            Operator::Div => Ok(JkConstant::from(self.0 / other.0).to_instance()),
            _ => self.no_op(other, op),
        }
    }
}

// FIXME: Avoid this copy paste, find a better/cleaner way to do it
impl Value for JkConstant<f64> {
    fn do_op(&self, other: &Self, op: Operator) -> Result<Instance, JkError> {
        match op {
            Operator::Add => Ok(JkConstant::from(self.0 + other.0).to_instance()),
            Operator::Sub => Ok(JkConstant::from(self.0 - other.0).to_instance()),
            Operator::Mul => Ok(JkConstant::from(self.0 * other.0).to_instance()),
            Operator::Div => Ok(JkConstant::from(self.0 / other.0).to_instance()),
            _ => self.no_op(other, op),
        }
    }
}

impl<T> From<T> for JkConstant<T> {
    fn from(rust_value: T) -> Self {
        JkConstant(rust_value)
    }
}

impl From<&str> for JkConstant<String> {
    fn from(s: &str) -> Self {
        JkConstant(s.to_string())
    }
}
