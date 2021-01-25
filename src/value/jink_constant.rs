use super::{JinkBool, JinkChar, JinkFloat, JinkInt, JinkString, Value};
use crate::instruction::{InstrKind, Instruction, Operator};
use crate::{FromInstance, Instance, Interpreter, JinkoError, ToInstance};
use std::convert::TryFrom;

#[derive(Clone)]
pub struct JinkConstant<T>(pub(crate) T);

impl Instruction for JinkInt {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression(None)
    }

    fn print(&self) -> String {
        self.0.to_string()
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JinkoError> {
        interpreter.debug("CONSTANT", &self.0.to_string());

        // Since we cannot use the generic ToInstance implementation, we also have to
        // copy paste our four basic implementations for jinko's primitive types...
        Ok(InstrKind::Expression(Some(self.to_instance())))
    }
}

impl Instruction for JinkFloat {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression(None)
    }

    fn print(&self) -> String {
        self.0.to_string()
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JinkoError> {
        interpreter.debug("CONSTANT", &self.0.to_string());

        // Since we cannot use the generic ToInstance implementation, we also have to
        // copy paste our four basic implementations for jinko's primitive types...
        Ok(InstrKind::Expression(Some(self.to_instance())))
    }
}

impl Instruction for JinkBool {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression(None)
    }

    fn print(&self) -> String {
        self.0.to_string()
    }

    fn as_bool(&self) -> bool {
        self.0
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JinkoError> {
        interpreter.debug("CONSTANT", &self.0.to_string());

        // Since we cannot use the generic ToInstance implementation, we also have to
        // copy paste our four basic implementations for jinko's primitive types...
        Ok(InstrKind::Expression(Some(self.to_instance())))
    }
}

impl Instruction for JinkChar {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression(None)
    }

    fn print(&self) -> String {
        self.0.to_string()
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JinkoError> {
        interpreter.debug("CONSTANT", &self.0.to_string());

        // Since we cannot use the generic ToInstance implementation, we also have to
        // copy paste our four basic implementations for jinko's primitive types...
        Ok(InstrKind::Expression(Some(self.to_instance())))
    }
}

impl Instruction for JinkString {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression(None)
    }

    fn print(&self) -> String {
        format!("\"{}\"", self.0.clone())
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JinkoError> {
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
// impl<T: Sized> ToInstance for JinkConstant<T> {
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
// /* (Plus a specific implementation for JinkConstant<String>) */
// ```
//
// which fails with the following error message:
//
// ```
// error[E0277]: the size for values of type `T` cannot be known at compilation time
// --> src/value/jink_constant.rs:34:48
//     |
// 25  | impl <T: Sized> ToInstance for JinkConstant<T> {
//     |       - this type parameter needs to be `Sized`
//     ...
// 34  |         &transmute::<T, [u8; size_of::<T>()]>(self.0),
//     |                                        ^ doesn't have a size known at compile-time
// ```

impl ToInstance for JinkInt {
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

impl ToInstance for JinkFloat {
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

impl ToInstance for JinkBool {
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

impl ToInstance for JinkChar {
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

impl ToInstance for JinkString {
    fn to_instance(&self) -> Instance {
        use std::mem::{size_of, transmute};

        unsafe {
            Instance::from_bytes(
                Some("string".to_string()), // FIXME
                size_of::<String>(),
                // FIXME: Avoid cloning
                &transmute::<String, [u8; size_of::<String>()]>(self.0.clone()),
            )
        }
    }
}

// In the same vein, we also have to implement FromInstance this way...

impl FromInstance for JinkInt {
    fn from_instance(i: &Instance) -> Self {
        use std::mem::{size_of, transmute};

        unsafe {
            JinkInt::from(transmute::<[u8; size_of::<i64>()], i64>(
                TryFrom::try_from(i.data()).unwrap(),
            ))
        }
    }
}

impl FromInstance for JinkFloat {
    fn from_instance(i: &Instance) -> Self {
        use std::mem::{size_of, transmute};

        unsafe {
            JinkFloat::from(transmute::<[u8; size_of::<f64>()], f64>(
                TryFrom::try_from(i.data()).unwrap(),
            ))
        }
    }
}

impl FromInstance for JinkBool {
    fn from_instance(i: &Instance) -> Self {
        use std::mem::{size_of, transmute};

        unsafe {
            JinkBool::from(transmute::<[u8; size_of::<bool>()], bool>(
                TryFrom::try_from(i.data()).unwrap(),
            ))
        }
    }
}

impl FromInstance for JinkChar {
    fn from_instance(i: &Instance) -> Self {
        use std::mem::{size_of, transmute};

        unsafe {
            JinkChar::from(transmute::<[u8; size_of::<char>()], char>(
                TryFrom::try_from(i.data()).unwrap(),
            ))
        }
    }
}

impl FromInstance for JinkString {
    fn from_instance(i: &Instance) -> Self {
        use std::mem::{size_of, transmute};

        unsafe {
            JinkString::from(transmute::<[u8; size_of::<String>()], String>(
                TryFrom::try_from(i.data()).unwrap(),
            ))
        }
    }
}

// impl FromInstance for JinkFloat {
//     fn from_instance(i: &Instance) -> Self {
//         use std::mem::{transmute, size_of};
//
//         JinkFloat::from(transmute::<[u8; size_of::<f64>()], f64>(i.data()))
//     }
// }
//
// impl FromInstance for JinkBool {
//     fn from_instance(i: &Instance) -> Self {
//         use std::mem::{transmute, size_of};
//
//         JinkBool::from(transmute::<[u8; size_of::<bool>()], bool>(i.data()))
//     }
// }
//
// impl FromInstance for JinkChar {
//     fn from_instance(i: &Instance) -> Self {
//         use std::mem::{transmute, size_of};
//
//         JinkChar::from(transmute::<[u8; size_of::<char>()], char>(i.data()))
//     }
// }
//
// impl FromInstance for JinkString {
//     fn from_instance(i: &Instance) -> Self {
//         use std::mem::{transmute, size_of};
//
//         JinkString::from(transmute::<[u8; size_of::<String>()], String>(i.data()))
//     }
// }

impl Value for JinkConstant<i64> {
    fn do_op(&self, other: &Self, op: Operator) -> Result<Instance, JinkoError> {
        match op {
            Operator::Add => Ok(JinkConstant::from(self.0 + other.0).to_instance()),
            Operator::Sub => Ok(JinkConstant::from(self.0 - other.0).to_instance()),
            Operator::Mul => Ok(JinkConstant::from(self.0 * other.0).to_instance()),
            Operator::Div => Ok(JinkConstant::from(self.0 / other.0).to_instance()),
            _ => self.no_op(other, op),
        }
    }
}

// FIXME: Avoid this copy paste, find a better/cleaner way to do it
impl Value for JinkConstant<f64> {
    fn do_op(&self, other: &Self, op: Operator) -> Result<Instance, JinkoError> {
        match op {
            Operator::Add => Ok(JinkConstant::from(self.0 + other.0).to_instance()),
            Operator::Sub => Ok(JinkConstant::from(self.0 - other.0).to_instance()),
            Operator::Mul => Ok(JinkConstant::from(self.0 * other.0).to_instance()),
            Operator::Div => Ok(JinkConstant::from(self.0 / other.0).to_instance()),
            _ => self.no_op(other, op),
        }
    }
}

impl<T> From<T> for JinkConstant<T> {
    fn from(rust_value: T) -> Self {
        JinkConstant(rust_value)
    }
}

impl From<&str> for JinkConstant<String> {
    fn from(s: &str) -> Self {
        JinkConstant(s.to_string())
    }
}
